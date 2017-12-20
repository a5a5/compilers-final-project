


#include "./bdwgc/include/gc.h"     // change based on where the include file is. 
#include "stdio.h"
#include "stdlib.h"
#include "stdint.h"



#define TOGGLE_GC true //switch to false to turn off gc


#if TOGGLE_GC
    #define ALLOCATE(m) (u64*) GC_MALLOC(m);
#else 
    #define ALLOCATE(m) (u64*) malloc(m);
#endif


//non-pointer datums
#define INT_TAG 1
#define CHAR_TAG 2
#define ENUM_TAG 7
#define STR_TAG 3
#define SYM_TAG 4 //

//all elements with the ptr tag (000) should point to an a u64 array where the head has one of the below ptr tags.
#define PTR_TAG 0

//these tags are for the first element 
#define PROC_PTR 0//or PTR_CLO i guess. should be a ptr to a two element array. first element is just a dummy with this tag. this might not work since procs are never encoded. they are striaght pointers.
#define CONS_PTR 1
#define STR_PTR 2//strings
#define SYM_PTR 3 //unused
#define VEC_PTR 4
#define MAP_PTR 5 //unused
#define SET_PTR 6 //unused

// Hashes, Sets, gen records, can all be added here


#define V_VOID 39  //32 +7 (+7 is for anything enumerable other than null)
#define V_TRUE 31  //24 +7
#define V_FALSE 15 //8  +7
#define V_NULL 0



#define MASK64 0xffffffffffffffff // useful for tagging related operations

//only good for check ptr, char, ints

#define ASSERT_TAG(v,tag,msg) \
    if(((v)&7ULL) != (tag)) \
        fatal_err(msg);

#define ASSERT_VALUE(v,val,msg) \
    if(((u64)(v)) != (val))     \
        fatal_err(msg);


#define DECODE_INT(v) ((s32)((u32)(((v)&(7ULL^MASK64)) >> 32)))
#define ENCODE_INT(v) ((((u64)((u32)(v))) << 32) | INT_TAG)

#define DECODE_CHAR(v)  ( (uc32) (((v)&(7ULL^MASK64)) >> 32) )
#define ENCODE_CHAR(v) ((((u64) ((uc32)(v))) << 32) | CHAR_TAG)

#define DECODE_PTR(v) ((u64*)((v)&(7ULL^MASK64)))
#define ENCODE_PTR(v) (((u64)(v)) | PTR_TAG)

#define DECODE_PROC(v) ( (u64*) &DECODE_PTR(v)[1]  )
#define DECODE_CONS(v) ( (u64*) &DECODE_PTR(v)[1]  )
#define DECODE_STR(v) ( (u64*) &DECODE_PTR(v)[1]  )
#define DECODE_SYM(v) ( (u64*) &DECODE_PTR(v)[1]  )
#define DECODE_VEC(v) ( (u64*) &DECODE_PTR(v)[1]  )
#define DECODE_MAP(v) ( (u64*) &DECODE_PTR(v)[1]  )
#define DECODE_SET(v) ( (u64*) &DECODE_PTR(v)[1]  )

#define DECODE_SYMs(v) ((char*)((v)&(7ULL^MASK64)))
#define ENCODE_SYMs(v) (((u64)(v)) | SYM_TAG)
#define DECODE_STRs(v) ((char*)((v)&(7ULL^MASK64)))
#define ENCODE_STRs(v) (((u64)(v)) | STR_TAG)


// some apply-prim macros for expecting 1 argument or 2 arguments
#define GEN_EXPECT1ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 v0 = expect_args1(lst); \
        return g(v0); \
    } 

#define GEN_EXPECT2ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 rest; \
        u64 v0 = expect_cons(lst, &rest); \
        u64 v1 = expect_cons(rest, &rest); \
        if (rest != V_NULL) \
            fatal_err("prim applied on more than 2 arguments."); \
        return g(v0,v1);                                           \
    } 

#define GEN_EXPECT3ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 rest; \
        u64 v0 = expect_cons(lst, &rest); \
        u64 v1 = expect_cons(rest, &rest); \
        u64 v2 = expect_cons(rest, &rest); \
        if (rest != V_NULL) \
            fatal_err("prim applied on more than 2 arguments."); \
        return g(v0,v1,v2);                                        \
    } 





// No mangled names
extern "C"
{


typedef char32_t uc32;
typedef uint64_t u64;
typedef int64_t s64;
typedef uint32_t u32;
typedef int32_t s32;


    
// UTILS

u64* decode_closure(u64 tagged_closure) {
    printf("decoded closure");
    if ((tagged_closure&7) == PTR_TAG && (((u64*) DECODE_PTR(tagged_closure))[0]&7) == PROC_PTR)
        return DECODE_PROC(tagged_closure);
    else
        exit(1);
}


u64* alloc(const u64 m)
{
    //printf("allocated memory");

    return ALLOCATE(m);
}

void fatal_err(const char* msg)
{
    printf("library run-time error: ");
    printf("%s", msg);
    printf("\n");
    exit(1);
}

void print_u64(u64 i)
{
    printf("%lu\n", i);
}

u64 expect_args0(u64 args)
{
    if (args != V_NULL)
        fatal_err("Expected value: null (in expect_args0). Prim cannot take arguments.");
    return V_NULL;
}

u64 expect_args1(u64 args)
{
    ASSERT_TAG(args, PTR_TAG, "Expected cons value (in expect_args1). Prim applied on an empty argument list.");
    ASSERT_TAG( ((u64*)DECODE_PTR(args))[0], CONS_PTR, "Expected cons value (in expect_args1). Prim applied on an empty argument list.");
    u64* c = DECODE_CONS(args);
    ASSERT_VALUE((c[1]), V_NULL, "Expected null value (in expect_args1). Prim can only take 1 argument.");
    return c[0];
}

u64 expect_cons(u64 p, u64* rest)
{
    // pass a pair value p and a pointer to a word *rest                          
    // verifiies (cons? p), returns the value (car p) and assigns *rest = (cdr p) 
    ASSERT_TAG(p, PTR_TAG, "Expected a cons value. (expect_cons)");
    ASSERT_TAG( ((u64*)DECODE_PTR(p))[0], CONS_PTR, "Expected a cons value. (expect_cons)");
    u64* pp = DECODE_CONS(p);
    *rest = pp[1];
    return pp[0];
}

u64 expect_other(u64 v, u64* rest) //yeah... i dont plan on using this????
{
    // returns the runtime tag value
    // puts the untagged value at *rest
    ASSERT_TAG(v, PTR_TAG, "Expected a vector or special value. (expect_other)")
    
    u64* p = DECODE_PTR(v);
    *rest = p[1];
    return p[0];
}


/////// CONSTANTS
    
    
u64 const_init_int(s64 i)
{
    return ENCODE_INT((s32)i);
}

u64 const_init_void()
{
    return V_VOID;
}


u64 const_init_null()
{
    return V_NULL;
}


u64 const_init_true()
{
    return V_TRUE;
}

    
u64 const_init_false()
{
    return V_FALSE;
}

    
u64 const_init_string(const char* s) //honestly might just scrap this. will change all strings to a call to (string #\char ...)
{

    return ENCODE_STRs(s);
}
        
u64 const_init_symbol(const char* s) //idk about this one tho
{
    return ENCODE_SYMs(s);
}







/////////// PRIMS

    
///// effectful prims:



    
u64 prim_print_aux(u64 v) 
{
    if (v == V_NULL)
        printf("()");
    else if ((v&7) == SYM_TAG)
    {   // needs to handle escaping to be correct
        printf("%s", DECODE_SYMs(v));
    }
    else if ((v&7) == STR_TAG)
    {   // needs to handle escaping to be correct
        printf("\"%s\"", DECODE_STRs(v));
    }
    else if((v&7) == PTR_TAG) {
        u64 v2 = ((u64*) DECODE_PTR(v))[0];
        if ((v2&7) == CONS_PTR)
        {
            u64* p = DECODE_CONS(v);
            printf("(");
            prim_print_aux(p[0]);
            printf(" . ");
            prim_print_aux(p[1]);
            printf(")");
        }
        
        else if ((v2&7) == STR_PTR)
        {   // needs to handle escaping to be correct
            printf("haven't handled printing strs yet");
        }

        else if ((v2&7) == SYM_PTR)
        {   // needs to handle escaping to be correct
            printf("haven't handled printing syms yet");
        }

        else if ( (v2&7) == VEC_PTR)
        {
            printf("#(");
            u64* vec = (u64*) DECODE_VEC(v);
            u64 len = vec[0] >> 3;
            prim_print_aux(vec[1]);
            for (u64 i = 2; i <= len; ++i)
            {
                printf(",");
                prim_print_aux(vec[i]);
            }
            printf(")");
        }
        else
            printf("#<procedure>");


    }
    else if ((v&7) == INT_TAG)
    {
        printf("%d", (int)((s32)(v >> 32)));
    }
    else if ((v&7) == CHAR_TAG)
    {
        printf("%c", (char) ((uc32)(v >> 32)));
    }
    else
        printf("(print.. v); unrecognized value %lu", v);
    //...
    return V_VOID; 
}

u64 prim_print(u64 v) 
{
    if (v == V_NULL)
        printf("'()");
    else if ((v&7) == STR_TAG)
    {   // needs to handle escaping to be correct
        printf("\"%s\"", DECODE_STRs(v));
    }
    else if ((v&7) == SYM_TAG)
    {   // needs to handle escaping to be correct
        printf("'%s", DECODE_SYMs(v));
    }
    else if((v&7) == PTR_TAG) {
        u64 v2 = ((u64*) DECODE_PTR(v))[0];
        if ((v2&7) == CONS_PTR)
        {
            u64* p = DECODE_CONS(v);
            printf("'(");
            prim_print_aux(p[0]);
            printf(" . ");
            prim_print_aux(p[1]);
            printf(")");
        }
        
        else if ((v2&7) == STR_PTR)
        {   // needs to handle escaping to be correct
            printf("haven't handled printing char arrays yet");
        }

        else if ((v2&7) == SYM_PTR)
        {   // needs to handle escaping to be correct
            printf("haven't handled printing char arrays yet");
        }

        else if ( (v2&7) == VEC_PTR)
        {
            printf("'#(");
            u64* vec = (u64*) DECODE_VEC(v);
            u64 len = vec[0] >> 3;
            prim_print_aux(vec[1]);
            for (u64 i = 2; i <= len; ++i)
            {
                printf(",");
                prim_print_aux(vec[i]);
            }
            printf(")");
        }
        else
            printf("'#<procedure>");


    }
    else if ((v&7) == INT_TAG)
    {
        printf("%d", (int)((s32)(v >> 32)));
    }
    else if ((v&7) == CHAR_TAG)
    {
        printf("%c", (char) ((uc32)(v >> 32)));
    }
    else
        printf("(print.. v); unrecognized value %lu", v);
    //...
    return V_VOID; 
}
GEN_EXPECT1ARGLIST(applyprim_print,prim_print)


u64 prim_halt(u64 v) // halt
{
    prim_print(v); // display the final value
    printf("\n");
    exit(0);
    return V_NULL; 
}


u64 applyprim_vector(u64 lst)
{
    // pretty terrible, but works
    u64* buffer = ALLOCATE(512*sizeof(u64));
    u64 l = 0;
    while ( (lst&7) == PTR_TAG && (((u64*) DECODE_PTR(lst))[0]&7) == CONS_PTR && l < 512) 
        buffer[l++] = expect_cons(lst, &lst);
    u64* mem = (u64*) ALLOCATE((l + 1) * sizeof(u64));
    mem[0] = (l << 3) | VEC_PTR;
    for (u64 i = 0; i < l; ++i)
        mem[i+1] = buffer[i];
    delete [] buffer;
    return ENCODE_PTR(mem);
}



u64 prim_make_45vector(u64 lenv, u64 iv)
{
    ASSERT_TAG(lenv, INT_TAG, "first argument to make-vector must be an integer")
    
    const u64 l = DECODE_INT(lenv);
    u64* vec = (u64*) ALLOCATE((l + 1) * sizeof(u64));
    vec[0] = (l << 3) | VEC_PTR;
    for (u64 i = 1; i <= l; ++i)
        vec[i] = iv;
    return ENCODE_PTR(vec);
}
GEN_EXPECT2ARGLIST(applyprim_make_45vector, prim_make_45vector)


u64 prim_vector_45ref(u64 v, u64 i)
{
    ASSERT_TAG(i, INT_TAG, "second argument to vector-ref must be an integer")
    ASSERT_TAG(v, PTR_TAG, "first argument to vector-ref must be a vector") //lmao fix this
    ASSERT_TAG(((u64*)DECODE_PTR(v))[0], VEC_PTR, "first argument to vector-ref must be a vector")

    return ( (u64*) DECODE_VEC(v))[(DECODE_INT(i))];
}
GEN_EXPECT2ARGLIST(applyprim_vector_45ref, prim_vector_45ref)


u64 prim_vector_45set_33(u64 a, u64 i, u64 v)
{
    ASSERT_TAG(i, INT_TAG, "second argument to vector-ref must be an integer")
    ASSERT_TAG(a, PTR_TAG, "first argument to vector-ref must be a vector") //lmao fix this
    ASSERT_TAG(((u64*)DECODE_PTR(a))[0], VEC_PTR, "first argument to vector-ref must be a vector")
        
    ((u64*) (DECODE_VEC(a))) [DECODE_INT(i)] = v;
        
    return V_VOID;
}
GEN_EXPECT3ARGLIST(applyprim_vector_45set_33, prim_vector_45set_33)


///// void, ...

    
u64 prim_void()
{
    return V_VOID;
}


    



///// eq?, eqv?, equal?

    
u64 prim_eq_63(u64 a, u64 b)
{
    if (a == b)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT2ARGLIST(applyprim_eq_63, prim_eq_63)


u64 prim_eqv_63(u64 a, u64 b)
{
    if (a == b)
        return V_TRUE;
    //else if  // optional extra logic, see r7rs reference
    else
        return V_FALSE;
}
GEN_EXPECT2ARGLIST(applyprim_eqv_63, prim_eqv_63)

/*
u64 prim_equal_63(u64 a, u64 b)
{
    return 0;
}
GEN_EXPECT2ARGLIST(applyprim_equal_63, prim_equal_63)
*/


///// Other predicates


u64 prim_number_63(u64 a)
{
    // We assume that ints are the only number
    if ((a&7) == INT_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_number_63, prim_number_63)


u64 prim_integer_63(u64 a)
{
    if ((a&7) == INT_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_integer_63, prim_integer_63)


u64 prim_void_63(u64 a)
{
    if (a == V_VOID)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_void_63, prim_void_63)


u64 prim_procedure_63(u64 a)
{
    if ((a&7) == PTR_TAG && (((u64*) DECODE_PTR(a))[0]&7) == PROC_PTR)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_procedure_63, prim_procedure_63)


///// null?, cons?, cons, car, cdr


u64 prim_null_63(u64 p) // null?
{
    if (p == V_NULL)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_null_63, prim_null_63)    


u64 prim_cons_63(u64 p) // cons?
{
    if ((p&7) == PTR_TAG && (((u64*) DECODE_PTR(p))[0]&7) == CONS_PTR)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_cons_63, prim_cons_63)    


u64 prim_cons(u64 a, u64 b)
{
    //printf("cons");
    u64* p = ALLOCATE(3*sizeof(u64));
    p[0] = (u64) CONS_PTR;
    p[1] = a;
    p[2] = b;
    return ENCODE_PTR(p);
}
GEN_EXPECT2ARGLIST(applyprim_cons, prim_cons)


u64 prim_car(u64 p)
{
    u64 rest;
    u64 v0 = expect_cons(p,&rest);
    
    return v0;
}
GEN_EXPECT1ARGLIST(applyprim_car, prim_car)


u64 prim_cdr(u64 p)
{
    u64 rest;
    u64 v0 = expect_cons(p,&rest);
    
    return rest;
}
GEN_EXPECT1ARGLIST(applyprim_cdr, prim_cdr)


///// s32 prims, +, -, *, =, ...

    
u64 prim__43(u64 a, u64 b) // +
{
    ASSERT_TAG(a, INT_TAG, "(prim + a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim + a b); b is not an integer")

        //printf("sum: %d\n", DECODE_INT(a) + DECODE_INT(b));
    
    return ENCODE_INT(DECODE_INT(a) + DECODE_INT(b));
}

u64 applyprim__43(u64 p)
{
    if (p == V_NULL)
        return ENCODE_INT(0);
    else
    {
        ASSERT_TAG(p, PTR_TAG, "Tried to apply + on non list value.")
        ASSERT_TAG(((u64*)DECODE_PTR(p))[0], CONS_PTR, "Tried to apply + on non list value.")
        u64* pp = DECODE_CONS(p);
        return ENCODE_INT(DECODE_INT(pp[0]) + DECODE_INT(applyprim__43(pp[1])));
    }
}
    
u64 prim__45(u64 a, u64 b) // -
{
    ASSERT_TAG(a, INT_TAG, "(prim - a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim - a b); b is not an integer")
    
    return ENCODE_INT(DECODE_INT(a) - DECODE_INT(b));
}

u64 applyprim__45(u64 p)
{
    if (p == V_NULL)
        return ENCODE_INT(0);
    else
    {
        ASSERT_TAG(p, PTR_TAG, "Tried to apply + on non list value.")
        ASSERT_TAG(((u64*)DECODE_PTR(p))[0], CONS_PTR, "Tried to apply + on non list value.")
        u64* pp = DECODE_CONS(p);
        if (pp[1] == V_NULL)
            return ENCODE_INT(0 - DECODE_INT(pp[0]));
        else // ideally would be properly left-to-right
            return ENCODE_INT(DECODE_INT(pp[0]) - DECODE_INT(applyprim__43(pp[1])));
    }
}
    
u64 prim__42(u64 a, u64 b) // *
{
    ASSERT_TAG(a, INT_TAG, "(prim * a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim * a b); b is not an integer")
    
    return ENCODE_INT(DECODE_INT(a) * DECODE_INT(b));
}

u64 applyprim__42(u64 p)
{
    if (p == V_NULL)
        return ENCODE_INT(1);
    else
    {
        ASSERT_TAG(p, PTR_TAG, "Tried to apply + on non list value.")
        ASSERT_TAG(((u64*)DECODE_PTR(p))[0], CONS_PTR, "Tried to apply + on non list value.")
        u64* pp = DECODE_CONS(p);
        return ENCODE_INT(DECODE_INT(pp[0]) * DECODE_INT(applyprim__42(pp[1])));
    }
}
    
u64 prim__47(u64 a, u64 b) // /
{
    ASSERT_TAG(a, INT_TAG, "(prim / a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim / a b); b is not an integer")
    
    return ENCODE_INT(DECODE_INT(a) / DECODE_INT(b));
}
    
u64 prim__61(u64 a, u64 b)  // =
{
    ASSERT_TAG(a, INT_TAG, "(prim = a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim = a b); b is not an integer")
        
    if ((s32)((a&(7ULL^MASK64)) >> 32) == (s32)((b&(7ULL^MASK64)) >> 32))
        return V_TRUE;
    else
        return V_FALSE;
}

u64 prim__60(u64 a, u64 b) // <
{
    ASSERT_TAG(a, INT_TAG, "(prim < a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim < a b); b is not an integer")
    
    if ((s32)((a&(7ULL^MASK64)) >> 32) < (s32)((b&(7ULL^MASK64)) >> 32))
        return V_TRUE;
    else
        return V_FALSE;
}
    
u64 prim__60_61(u64 a, u64 b) // <=
{
    ASSERT_TAG(a, INT_TAG, "(prim <= a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim <= a b); b is not an integer")
        
    if ((s32)((a&(7ULL^MASK64)) >> 32) <= (s32)((b&(7ULL^MASK64)) >> 32))
        return V_TRUE;
    else
        return V_FALSE;
}

u64 prim_not(u64 a) 
{
    if (a == V_FALSE)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_not, prim_not)


}




