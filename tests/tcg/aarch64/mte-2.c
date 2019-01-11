/*
 * Memory tagging, basic fail cases.
 */

#include <assert.h>
#include <signal.h>
#include <stdlib.h>

asm(".arch armv8.5-a+memtag");

int data[16 / sizeof(int)] __attribute__((aligned(16)));

void pass(int sig)
{
    exit(0);
}

int main(int ac, char **av)
{
    int *p0 = data;
    int *p1, *p2;
    long excl = 1;

    /* Create two differently tagged pointers.  */
    asm("irg %0,%1,%2" : "=r"(p1) : "r"(p0), "r"(excl));
    asm("gmi %0,%1,%0" : "+r"(excl) : "r" (p1));
    assert(excl != 1);
    asm("irg %0,%1,%2" : "=r"(p2) : "r"(p0), "r"(excl));
    assert(p1 != p2);

    /* Store the tag from the first pointer.  */
    asm("stg %0, [%0]" : : "r"(p1));

    *p1 = 0;
    signal(SIGSEGV, pass);
    *p2 = 0;

    assert(0);
}
