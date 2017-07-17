#ifndef test_h
#define test_h

#define abort() (*(int*)0 = 1)

#define assert(x) { if (!(x)) { abort(); } }

#endif
