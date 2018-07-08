/*
 * Logging support
 *
 *  Copyright (c) 2003 Fabrice Bellard
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#include "qemu/osdep.h"
#include "qemu/log.h"
#include "qemu/range.h"
#include "qemu/error-report.h"
#include "qapi/error.h"
#include "qemu/cutils.h"
#include "trace/control.h"
#include <sys/syscall.h>

static bool qemu_log_enabled1;
static __thread FILE *qemu_logfile1;
static __thread char logfile_buf[4096];
static char *logfilename;
int qemu_loglevel;
static GArray *debug_regions;

static int gettid(void)
{
    return syscall(SYS_gettid);
}

FILE *qemu_logfile0(void)
{
    FILE *f;

    if (!qemu_log_enabled1) {
        return NULL;
    }
    f = qemu_logfile1;
    if (f == NULL) {
        size_t len = strlen(logfilename) + 7;
        char *name = alloca(len);
        snprintf(name, len, "%s.%d", logfilename, gettid());

        qemu_logfile1 = f = fopen(name, "w");
        if (f == NULL) {
            perror(name);
            _exit(1);
        }
        /* Must avoid mmap() usage of glibc by setting a buffer by hand.  */
        setvbuf(f, logfile_buf, _IOLBF, sizeof(logfile_buf));
    }
    return f;
}

/* Returns true if qemu_log() will really write somewhere.  */
bool qemu_log_enabled(void)
{
    return qemu_log_enabled1;
}

/* Returns true if qemu_log() will write somewhere else than stderr.  */
bool qemu_log_separate(void)
{
    return qemu_logfile1 != stderr;
}

/* Return the number of characters emitted.  */
int qemu_log(const char *fmt, ...)
{
    FILE *f = qemu_logfile0();
    int ret = 0;
    if (f != NULL) {
        va_list ap;
        va_start(ap, fmt);
        ret = vfprintf(f, fmt, ap);
        va_end(ap);

        /* Don't pass back error results.  */
        if (ret < 0) {
            ret = 0;
        }
    }
    return ret;
}

/* enable or disable low levels log */
void qemu_set_log(int log_flags)
{
    qemu_loglevel = log_flags;
#ifdef CONFIG_TRACE_LOG
    qemu_loglevel |= LOG_TRACE;
#endif
    if (qemu_loglevel) {
        qemu_log_enabled1 = true;
    } else {
        qemu_log_enabled1 = false;
        qemu_log_close();
    }
}

void qemu_set_log_filename(const char *filename, Error **errp)
{
    g_free(logfilename);
    logfilename = g_strdup(filename);

    qemu_log_close();
    qemu_set_log(qemu_loglevel);
}

void qemu_log_needs_buffers(void)
{
}

/* Returns true if addr is in our debug filter or no filter defined
 */
bool qemu_log_in_addr_range(uint64_t addr)
{
    if (debug_regions) {
        int i = 0;
        for (i = 0; i < debug_regions->len; i++) {
            Range *range = &g_array_index(debug_regions, Range, i);
            if (range_contains(range, addr)) {
                return true;
            }
        }
        return false;
    } else {
        return true;
    }
}


void qemu_set_dfilter_ranges(const char *filter_spec, Error **errp)
{
    gchar **ranges = g_strsplit(filter_spec, ",", 0);
    int i;

    if (debug_regions) {
        g_array_unref(debug_regions);
        debug_regions = NULL;
    }

    debug_regions = g_array_sized_new(FALSE, FALSE,
                                      sizeof(Range), g_strv_length(ranges));
    for (i = 0; ranges[i]; i++) {
        const char *r = ranges[i];
        const char *range_op, *r2, *e;
        uint64_t r1val, r2val, lob, upb;
        struct Range range;

        range_op = strstr(r, "-");
        r2 = range_op ? range_op + 1 : NULL;
        if (!range_op) {
            range_op = strstr(r, "+");
            r2 = range_op ? range_op + 1 : NULL;
        }
        if (!range_op) {
            range_op = strstr(r, "..");
            r2 = range_op ? range_op + 2 : NULL;
        }
        if (!range_op) {
            error_setg(errp, "Bad range specifier");
            goto out;
        }

        if (qemu_strtou64(r, &e, 0, &r1val)
            || e != range_op) {
            error_setg(errp, "Invalid number to the left of %.*s",
                       (int)(r2 - range_op), range_op);
            goto out;
        }
        if (qemu_strtou64(r2, NULL, 0, &r2val)) {
            error_setg(errp, "Invalid number to the right of %.*s",
                       (int)(r2 - range_op), range_op);
            goto out;
        }

        switch (*range_op) {
        case '+':
            lob = r1val;
            upb = r1val + r2val - 1;
            break;
        case '-':
            upb = r1val;
            lob = r1val - (r2val - 1);
            break;
        case '.':
            lob = r1val;
            upb = r2val;
            break;
        default:
            g_assert_not_reached();
        }
        if (lob > upb) {
            error_setg(errp, "Invalid range");
            goto out;
        }
        range_set_bounds(&range, lob, upb);
        g_array_append_val(debug_regions, range);
    }
out:
    g_strfreev(ranges);
}

void qemu_log_vprintf(const char *fmt, va_list va)
{
    FILE *f = qemu_logfile0();
    if (f != NULL) {
        vfprintf(f, fmt, va);
    }
}

/* fflush() the log file */
void qemu_log_flush(void)
{
    if (qemu_logfile1) {
        fflush(qemu_logfile1);
    }
}

/* Close the log file */
void qemu_log_close(void)
{
    if (qemu_logfile1) {
        if (qemu_logfile1 != stderr) {
            fclose(qemu_logfile1);
        }
        qemu_logfile1 = NULL;
    }
}

const QEMULogItem qemu_log_items[] = {
    { CPU_LOG_TB_OUT_ASM, "out_asm",
      "show generated host assembly code for each compiled TB" },
    { CPU_LOG_TB_IN_ASM, "in_asm",
      "show target assembly code for each compiled TB" },
    { CPU_LOG_TB_OP, "op",
      "show micro ops for each compiled TB" },
    { CPU_LOG_TB_OP_OPT, "op_opt",
      "show micro ops after optimization" },
    { CPU_LOG_TB_OP_IND, "op_ind",
      "show micro ops before indirect lowering" },
    { CPU_LOG_INT, "int",
      "show interrupts/exceptions in short format" },
    { CPU_LOG_EXEC, "exec",
      "show trace before each executed TB (lots of logs)" },
    { CPU_LOG_TB_CPU, "cpu",
      "show CPU registers before entering a TB (lots of logs)" },
    { CPU_LOG_TB_FPU, "fpu",
      "include FPU registers in the 'cpu' logging" },
    { CPU_LOG_MMU, "mmu",
      "log MMU-related activities" },
    { CPU_LOG_PCALL, "pcall",
      "x86 only: show protected mode far calls/returns/exceptions" },
    { CPU_LOG_RESET, "cpu_reset",
      "show CPU state before CPU resets" },
    { LOG_UNIMP, "unimp",
      "log unimplemented functionality" },
    { LOG_GUEST_ERROR, "guest_errors",
      "log when the guest OS does something invalid (eg accessing a\n"
      "non-existent register)" },
    { CPU_LOG_PAGE, "page",
      "dump pages at beginning of user mode emulation" },
    { CPU_LOG_TB_NOCHAIN, "nochain",
      "do not chain compiled TBs so that \"exec\" and \"cpu\" show\n"
      "complete traces" },
#ifdef CONFIG_PLUGIN
    { CPU_LOG_PLUGIN, "plugin", "output from TCG plugins\n"},
#endif
    { 0, NULL, NULL },
};

/* takes a comma separated list of log masks. Return 0 if error. */
int qemu_str_to_log_mask(const char *str)
{
    const QEMULogItem *item;
    int mask = 0;
    char **parts = g_strsplit(str, ",", 0);
    char **tmp;

    for (tmp = parts; tmp && *tmp; tmp++) {
        if (g_str_equal(*tmp, "all")) {
            for (item = qemu_log_items; item->mask != 0; item++) {
                mask |= item->mask;
            }
#ifdef CONFIG_TRACE_LOG
        } else if (g_str_has_prefix(*tmp, "trace:") && (*tmp)[6] != '\0') {
            trace_enable_events((*tmp) + 6);
            mask |= LOG_TRACE;
#endif
        } else {
            for (item = qemu_log_items; item->mask != 0; item++) {
                if (g_str_equal(*tmp, item->name)) {
                    goto found;
                }
            }
            goto error;
        found:
            mask |= item->mask;
        }
    }

    g_strfreev(parts);
    return mask;

 error:
    g_strfreev(parts);
    return 0;
}

void qemu_print_log_usage(FILE *f)
{
    const QEMULogItem *item;
    fprintf(f, "Log items (comma separated):\n");
    for (item = qemu_log_items; item->mask != 0; item++) {
        fprintf(f, "%-15s %s\n", item->name, item->help);
    }
#ifdef CONFIG_TRACE_LOG
    fprintf(f, "trace:PATTERN   enable trace events\n");
    fprintf(f, "\nUse \"-d trace:help\" to get a list of trace events.\n\n");
#endif
}
