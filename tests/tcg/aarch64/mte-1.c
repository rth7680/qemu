/*
 * Memory tagging, basic pass cases.
 */

#include <assert.h>

asm(".arch armv8.5-a+memtag");

int data[16 / sizeof(int)] __attribute__((aligned(16)));

int main(int ac, char **av)
{
    int *p0 = data;
    int *p1, *p2;
    long c;

    asm("irg %0,%1,%2" : "=r"(p1) : "r"(p0), "r"(1));
    assert(p1 != p0);
    asm("subp %0,%1,%2" : "=r"(c) : "r"(p0), "r"(p1));
    assert(c == 0);

    asm("stg %0, [%0]" : : "r"(p1));
    asm("ldg %0, [%1]" : "=r"(p2) : "r"(p0), "0"(p0));
    assert(p1 == p2);

    return 0;
}
