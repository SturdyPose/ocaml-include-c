#ifndef TEST_H
#define TEST_H

typedef unsigned int u32;

#define ADD(x, y) int add(x,y);

ADD(int a, int b);

u32 someAdd(u32 a, u32 b);
u32 performSomething(u32* pu);

struct B;

struct A
{
    struct B* someBPointer;
    u32 foo;
};

void DoSomething(struct B* b);
struct B* Create();

#endif