/*
                                        (License at bottom of file - MIT)
    ------------------------------------------------------------------------------------------------------------
                                                ** FILE NOTES **
    ------------------------------------------------------------------------------------------------------------

    First 200 lines or so are wyhash implementation **THIS IS NOT MY CODE** :
            (main repo: https://github.com/wangyi-fudan/wyhash author: 王一 Wang Yi <godspeed_china@yeah.net>)

    Below this point is the map header; then finally the implementation.
    Define SOL_HASH_MAP_IMPLEMENTATION in one c or cpp file (stb style) to include the source.

    Begin Ramblings:
        (I will take this out if someone ever actually sees this file other than me...)

    The hash map is based on the google 'swiss' map (https://abseil.io/about/design/swisstables) or the rust
    'hash brown' map (https://github.com/rust-lang/hashbrown). They are both very similar; to understand the
    implementation, look at either. I originally found out about them from buying a book, the book was pretty
    useless, but it came with a bunch of source code, and at the time when I did not understand reading source
    code, having a book to decrypt some of it was nice (the book *1). I wrote the same map in C++ right after
    I bought it. Lots of stuff wwas copy and pasted, but the main implementation was my own struggling through.
    That was like a week of hard work staring at source code and blog posts trying to wrap my head around it.
    Now It literally takes me about 30-40 mins to reimplement from scratch in a language I dont use, without
    types.

    That raptor engine source code is almost entirely where my coding style comes from, now further influenced
    by Casey and STB and Jon Blow. I wonder if it will have changed much by the time I move beyond complete
    noob status...?

    *1
    https://github.com/PacktPublishing/Mastering-Graphics-Programming-with-Vulkan


    ------------------------------------------------------------------------------------------------------------
                                                    ** API **
    ------------------------------------------------------------------------------------------------------------
                                *** WARNING!! This structure is not type safe!! ***

    @Note Insertion does not check if the value or key is present, it will just reinsert. You can easily
          implement the other behaviour with a find, check if null, then insert.
    @Note Deletion does not clear the actual data or hash, just the bit representation (see file notes).

    Example usage:
        Str_Dict dict = new_str_dict(16, Thing);

        Thing  thing   = {.x = 0, .y = 5};
        bool   success = str_dict_insert(&dict, "thing1", &thing); // return true
        Thing *p_thing = str_dict_find(&dict, "thing1"); // return non-null pointer

        p_thing = str_dict_delete(&dict, "thing1"); // return non-null pointer
        p_thing = str_dict_find(&dict, "thing1"); // return null pointer

        free_str_dict(&dict); // return void

    Front End Macros: (use these)
        #define new_str_dict(cap, type) ... // return a Str_Dict
        #define free_str_dict(dict)     ... // free it

        #define str_dict_insert(pmap, key, pelem) ... // insert into it
        #define str_dict_find(dict, key)          ... // get stuff back out
        #define str_dict_delete(dict, key)        ... // get stuff back out

    Struct Declaration:
        typedef struct Str_Dict {
            int cap;
            int remaining;
            int kv_stride;
            u8 *data;
        } Str_Dict;

    Front End Functions:
        inline static Str_Dict fn_new_str_dict(int cap, int elem_width) {..}
        inline static void fn_free_str_dict(Str_Dict *dict) {..}

        inline static bool  fn_str_dict_insert(Str_Dict *dict, const char *key, void *elem, int elem_width) {..}
        inline static void* fn_str_dict_find(Str_Dict *dict, const char* key) {..}
        inline static void* fn_str_dict_delete(Str_Dict *dict, u64 hash) {..}

    Back End Functions:
        bool  fn_str_dict_insert_hash(Str_Dict *dict, u64 hash, void *elem, int elem_width);
        void  fn_str_dict_if_full(Str_Dict *dict, int elem_width);
        void* fn_str_dict_find_hash(Str_Dict *dict, u64 hash);
        void* fn_str_dict_delete_hash(Str_Dict *dict, u64 hash);

*/
#ifndef SOL_STR_DICT_HPP_INCLUDE_GUARD_
#define SOL_STR_DICT_HPP_INCLUDE_GUARD_

#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <immintrin.h>

#ifndef SOL_TYPEDEF
typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
#define Max_s32  INT32_MAX
#endif

#ifndef SOL_ALIGN
static inline size_t align(size_t size, size_t alignment) {
  const size_t alignment_mask = alignment - 1;
  return (size + alignment_mask) & ~alignment_mask;
}
#endif

#ifndef SOL_BUILTIN_WRAPPERS
#ifndef _WIN32
inline static int count_trailing_zeros_u16(u16 a) {
    // @Note This has to be copied between compiler directives because gcc will not compile
    // tzcnt16 with only one leading undescore. I assume this is a compiler bug, because tzcnt32
    // and 64 both want one leading underscore...
    return (int)__tzcnt_u16(a);
}
inline static int pop_count16(u16 num) {
    u32 tmp = num;
    tmp &= 0x0000ffff; // just to be sure, I have been burned before;
    return (int)__builtin_popcount(tmp);
}
#else
inline static int count_trailing_zeros_u16(u16 a) {
    return (int)_tzcnt_u16(a);
}
inline static int pop_count16(u16 num) {
    return (int)__popcnt16(num);
}
#endif // WIN32 or not
#endif // SOL_BUILTIN_WRAPPERS

/*
                                               ** BEGIN WYHASH **

    -- This is not my (Solomon Brown's) code:
           see wyhash github link at file start and below, wyhash full license before dict implementation

*/

// This is free and unencumbered software released into the public domain under The Unlicense (http://unlicense.org/)
// main repo: https://github.com/wangyi-fudan/wyhash
// author: 王一 Wang Yi <godspeed_china@yeah.net>
// contributors: Reini Urban, Dietrich Epp, Joshua Haberman, Tommy Ettinger, Daniel Lemire, Otmar Ertl, cocowalla, leo-yuriev, Diego Barrios Romero, paulie-g, dumblob, Yann Collet, ivte-ms, hyb, James Z.M. Gao, easyaspi314 (Devin), TheOneric


#ifndef wyhash_final_version_4
#define wyhash_final_version_4

#ifndef WYHASH_CONDOM
//protections that produce different results:
//1: normal valid behavior
//2: extra protection against entropy loss (probability=2^-63), aka. "blind multiplication"
#define WYHASH_CONDOM 1
#endif

#ifndef WYHASH_32BIT_MUM
//0: normal version, slow on 32 bit systems
//1: faster on 32 bit systems but produces different results, incompatible with wy2u0k function
#define WYHASH_32BIT_MUM 0
#endif

//includes
#include <stdint.h>
#include <string.h>
#if defined(_MSC_VER) && defined(_M_X64)
  #include <intrin.h>
  #pragma intrinsic(_umul128)
#endif

//likely and unlikely macros
#if defined(__GNUC__) || defined(__INTEL_COMPILER) || defined(__clang__)
  #define _likely_(x)  __builtin_expect(x,1)
  #define _unlikely_(x)  __builtin_expect(x,0)
#else
  #define _likely_(x) (x)
  #define _unlikely_(x) (x)
#endif

//128bit multiply function
static inline uint64_t _wyrot(uint64_t x) { return (x>>32)|(x<<32); }
static inline void _wymum(uint64_t *A, uint64_t *B){
#if(WYHASH_32BIT_MUM)
  uint64_t hh=(*A>>32)*(*B>>32), hl=(*A>>32)*(uint32_t)*B, lh=(uint32_t)*A*(*B>>32), ll=(uint64_t)(uint32_t)*A*(uint32_t)*B;
  #if(WYHASH_CONDOM>1)
  *A^=_wyrot(hl)^hh; *B^=_wyrot(lh)^ll;
  #else
  *A=_wyrot(hl)^hh; *B=_wyrot(lh)^ll;
  #endif
#elif defined(__SIZEOF_INT128__)
  __uint128_t r=*A; r*=*B;
  #if(WYHASH_CONDOM>1)
  *A^=(uint64_t)r; *B^=(uint64_t)(r>>64);
  #else
  *A=(uint64_t)r; *B=(uint64_t)(r>>64);
  #endif
#elif defined(_MSC_VER) && defined(_M_X64)
  #if(WYHASH_CONDOM>1)
  uint64_t  a,  b;
  a=_umul128(*A,*B,&b);
  *A^=a;  *B^=b;
  #else
  *A=_umul128(*A,*B,B);
  #endif
#else
  uint64_t ha=*A>>32, hb=*B>>32, la=(uint32_t)*A, lb=(uint32_t)*B, hi, lo;
  uint64_t rh=ha*hb, rm0=ha*lb, rm1=hb*la, rl=la*lb, t=rl+(rm0<<32), c=t<rl;
  lo=t+(rm1<<32); c+=lo<t; hi=rh+(rm0>>32)+(rm1>>32)+c;
  #if(WYHASH_CONDOM>1)
  *A^=lo;  *B^=hi;
  #else
  *A=lo;  *B=hi;
  #endif
#endif
}

//multiply and xor mix function, aka MUM
static inline uint64_t _wymix(uint64_t A, uint64_t B){ _wymum(&A,&B); return A^B; }

//endian macros
#ifndef WYHASH_LITTLE_ENDIAN
  #if defined(_WIN32) || defined(__LITTLE_ENDIAN__) || (defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
    #define WYHASH_LITTLE_ENDIAN 1
  #elif defined(__BIG_ENDIAN__) || (defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    #define WYHASH_LITTLE_ENDIAN 0
  #else
    #warning could not determine endianness! Falling back to little endian.
    #define WYHASH_LITTLE_ENDIAN 1
  #endif
#endif

//read functions
#if (WYHASH_LITTLE_ENDIAN)
static inline uint64_t _wyr8(const uint8_t *p) { uint64_t v; memcpy(&v, p, 8); return v;}
static inline uint64_t _wyr4(const uint8_t *p) { uint32_t v; memcpy(&v, p, 4); return v;}
#elif defined(__GNUC__) || defined(__INTEL_COMPILER) || defined(__clang__)
static inline uint64_t _wyr8(const uint8_t *p) { uint64_t v; memcpy(&v, p, 8); return __builtin_bswap64(v);}
static inline uint64_t _wyr4(const uint8_t *p) { uint32_t v; memcpy(&v, p, 4); return __builtin_bswap32(v);}
#elif defined(_MSC_VER)
static inline uint64_t _wyr8(const uint8_t *p) { uint64_t v; memcpy(&v, p, 8); return _byteswap_uint64(v);}
static inline uint64_t _wyr4(const uint8_t *p) { uint32_t v; memcpy(&v, p, 4); return _byteswap_ulong(v);}
#else
static inline uint64_t _wyr8(const uint8_t *p) {
  uint64_t v; memcpy(&v, p, 8);
  return (((v >> 56) & 0xff)| ((v >> 40) & 0xff00)| ((v >> 24) & 0xff0000)| ((v >>  8) & 0xff000000)| ((v <<  8) & 0xff00000000)| ((v << 24) & 0xff0000000000)| ((v << 40) & 0xff000000000000)| ((v << 56) & 0xff00000000000000));
}
static inline uint64_t _wyr4(const uint8_t *p) {
  uint32_t v; memcpy(&v, p, 4);
  return (((v >> 24) & 0xff)| ((v >>  8) & 0xff00)| ((v <<  8) & 0xff0000)| ((v << 24) & 0xff000000));
}
#endif
static inline uint64_t _wyr3(const uint8_t *p, size_t k) { return (((uint64_t)p[0])<<16)|(((uint64_t)p[k>>1])<<8)|p[k-1];}
//wyhash main function
static inline uint64_t wyhash(const void *key, size_t len, uint64_t seed, const uint64_t *secret){
  const uint8_t *p=(const uint8_t *)key; seed^=_wymix(seed^secret[0],secret[1]);	uint64_t	a,	b;
  if(_likely_(len<=16)){
    if(_likely_(len>=4)){ a=(_wyr4(p)<<32)|_wyr4(p+((len>>3)<<2)); b=(_wyr4(p+len-4)<<32)|_wyr4(p+len-4-((len>>3)<<2)); }
    else if(_likely_(len>0)){ a=_wyr3(p,len); b=0;}
    else a=b=0;
  }
  else{
    size_t i=len;
    if(_unlikely_(i>48)){
      uint64_t see1=seed, see2=seed;
      do{
        seed=_wymix(_wyr8(p)^secret[1],_wyr8(p+8)^seed);
        see1=_wymix(_wyr8(p+16)^secret[2],_wyr8(p+24)^see1);
        see2=_wymix(_wyr8(p+32)^secret[3],_wyr8(p+40)^see2);
        p+=48; i-=48;
      }while(_likely_(i>48));
      seed^=see1^see2;
    }
    while(_unlikely_(i>16)){  seed=_wymix(_wyr8(p)^secret[1],_wyr8(p+8)^seed);  i-=16; p+=16;  }
    a=_wyr8(p+i-16);  b=_wyr8(p+i-8);
  }
  a^=secret[1]; b^=seed;  _wymum(&a,&b);
  return  _wymix(a^secret[0]^len,b^secret[1]);
}

//the default secret parameters
static const uint64_t _wyp[4] = {0xa0761d6478bd642full, 0xe7037ed1a0b428dbull, 0x8ebc6af09c88c6e3ull, 0x589965cc75374cc3ull};

//a useful 64bit-64bit mix function to produce deterministic pseudo random numbers that can pass BigCrush and PractRand
static inline uint64_t wyhash64(uint64_t A, uint64_t B){ A^=0xa0761d6478bd642full; B^=0xe7037ed1a0b428dbull; _wymum(&A,&B); return _wymix(A^0xa0761d6478bd642full,B^0xe7037ed1a0b428dbull);}

//The wyrand PRNG that pass BigCrush and PractRand
static inline uint64_t wyrand(uint64_t *seed){ *seed+=0xa0761d6478bd642full; return _wymix(*seed,*seed^0xe7037ed1a0b428dbull);}

//convert any 64 bit pseudo random numbers to uniform distribution [0,1). It can be combined with wyrand, wyhash64 or wyhash.
static inline double wy2u01(uint64_t r){ const double _wynorm=1.0/(1ull<<52); return (r>>12)*_wynorm;}

//convert any 64 bit pseudo random numbers to APPROXIMATE Gaussian distribution. It can be combined with wyrand, wyhash64 or wyhash.
static inline double wy2gau(uint64_t r){ const double _wynorm=1.0/(1ull<<20); return ((r&0x1fffff)+((r>>21)&0x1fffff)+((r>>42)&0x1fffff))*_wynorm-3.0;}

#ifdef	WYTRNG
#include <sys/time.h>
//The wytrand true random number generator, passed BigCrush.
static inline uint64_t wytrand(uint64_t *seed){
	struct	timeval	t;	gettimeofday(&t,0);
	uint64_t	teed=(((uint64_t)t.tv_sec)<<32)|t.tv_usec;
	teed=_wymix(teed^_wyp[0],*seed^_wyp[1]);
	*seed=_wymix(teed^_wyp[0],_wyp[2]);
	return _wymix(*seed,*seed^_wyp[3]);
}
#endif

#if(!WYHASH_32BIT_MUM)
//fast range integer random number generation on [0,k) credit to Daniel Lemire. May not work when WYHASH_32BIT_MUM=1. It can be combined with wyrand, wyhash64 or wyhash.
static inline uint64_t wy2u0k(uint64_t r, uint64_t k){ _wymum(&r,&k); return k; }
#endif

//make your own secret
static inline void make_secret(uint64_t seed, uint64_t *secret){
  uint8_t c[] = {15, 23, 27, 29, 30, 39, 43, 45, 46, 51, 53, 54, 57, 58, 60, 71, 75, 77, 78, 83, 85, 86, 89, 90, 92, 99, 101, 102, 105, 106, 108, 113, 114, 116, 120, 135, 139, 141, 142, 147, 149, 150, 153, 154, 156, 163, 165, 166, 169, 170, 172, 177, 178, 180, 184, 195, 197, 198, 201, 202, 204, 209, 210, 212, 216, 225, 226, 228, 232, 240 };
  for(size_t i=0;i<4;i++){
    uint8_t ok;
    do{
      ok=1; secret[i]=0;
      for(size_t j=0;j<64;j+=8) secret[i]|=((uint64_t)c[wyrand(&seed)%sizeof(c)])<<j;
      if(secret[i]%2==0){ ok=0; continue; }
      for(size_t j=0;j<i;j++) {
#if defined(__GNUC__) || defined(__INTEL_COMPILER) || defined(__clang__)
        if(__builtin_popcountll(secret[j]^secret[i])!=32){ ok=0; break; }
#elif defined(_MSC_VER) && defined(_M_X64)
        if(_mm_popcnt_u64(secret[j]^secret[i])!=32){ ok=0; break; }
#else
        //manual popcount
        uint64_t x = secret[j]^secret[i];
        x -= (x >> 1) & 0x5555555555555555;
        x = (x & 0x3333333333333333) + ((x >> 2) & 0x3333333333333333);
        x = (x + (x >> 4)) & 0x0f0f0f0f0f0f0f0f;
        x = (x * 0x0101010101010101) >> 56;
        if(x!=32){ ok=0; break; }
#endif
      }
    }while(!ok);
  }
}

#endif
                             /* WYHASH LICENCE */
/* The Unlicense
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to <http://unlicense.org/>
*/

                        /* BEGIN STR DICT HEADER */
typedef struct Str_Dict {
    int cap;
    int remaining;
    int kv_stride;
    u8 *data;
} Str_Dict;

#define STR_DICT_FULL        0b01111111
#define STR_DICT_EMPTY       0b11111111
#define STR_DICT_DELETED     0b10000000
#define STR_DICT_GROUP_WIDTH 16

                        /* Backend Functions*/
bool  fn_str_dict_insert_hash(Str_Dict *dict, u64 hash, void *elem, int elem_width);
void  fn_str_dict_if_full(Str_Dict *dict, int elem_width);
void* fn_str_dict_find_hash(Str_Dict *dict, u64 hash);
void* fn_str_dict_delete_hash(Str_Dict *dict, u64 hash);

                        /* Frontend Functions */
inline static Str_Dict fn_new_str_dict(int cap, int elem_width) {
    Str_Dict ret  = {};
    ret.cap       = align(cap, 16);
    ret.remaining = ((cap + 1) / 8) * 7;
    ret.kv_stride = align(8 + elem_width, 16);
    ret.data      = malloc(ret.cap + ret.cap * ret.kv_stride);
    memset(ret.data, STR_DICT_EMPTY, ret.cap);
    return ret;
}

inline static void fn_free_str_dict(Str_Dict *dict) {
    free(dict->data);
}

inline static bool fn_str_dict_insert(Str_Dict *dict, const char *key, void *elem, int elem_width) {
    if (dict->remaining == 0)
        fn_str_dict_if_full(dict, elem_width);

    u64 hash = wyhash(key, strlen(key), 0, _wyp);
    return fn_str_dict_insert_hash(dict, hash, elem, elem_width);
}

inline static void* fn_str_dict_find(Str_Dict *dict, const char* key) {
    u64 hash = wyhash(key, strlen(key), 0, _wyp);
    return fn_str_dict_find_hash(dict, hash);
}
inline static void* fn_str_dict_delete(Str_Dict *dict, const char* key) {
    u64 hash = wyhash(key, strlen(key), 0, _wyp);
    return fn_str_dict_delete_hash(dict, hash);
}

                        /* Frontend Macros */
#define new_str_dict(cap, type) fn_new_str_dict(cap, sizeof(type))
#define free_str_dict(dict) fn_free_str_dict(dict)

#define str_dict_insert(pmap, key, pelem) fn_str_dict_insert(pmap, key, pelem, sizeof(*pelem))
#define str_dict_find(dict, key) fn_str_dict_find(dict, key)
#define str_dict_delete(dict, key) fn_str_dict_delete(dict, key)

                    /* BEGIN STR DICT IMPLEMENTATION */
#ifdef SOL_STR_DICT_IMPLEMENTATION

bool fn_str_dict_insert_hash(Str_Dict *dict, u64 hash, void *elem, int elem_width) {
    int g_idx = (hash & (dict->cap - 1));
    g_idx    -= g_idx & 15;

    u8  *data   = dict->data;
    int  cap    = dict->cap;
    int  stride = dict->kv_stride;

    int  tz;
    u16  mask;
    u64 *phash;

    __m128i a;
    int inc = 0;
    while(inc < cap) {
        a    = _mm_load_si128((__m128i*)(data + g_idx));
        mask = _mm_movemask_epi8(a);

        if (!mask) {
            inc   += 16;
            g_idx += inc;
            g_idx &= cap - 1;
            continue;
        } else {
            tz = count_trailing_zeros_u16(mask);

            u8 top7 = (hash >> 57) & STR_DICT_FULL;
            data[g_idx + tz] = 0x0 | top7;

            phash  = (u64*)(data + cap + (stride * (tz + g_idx)));
           *phash  =  hash;
            memcpy(data + cap + (stride * (tz + g_idx)) + 8, elem, elem_width);

            dict->remaining -= 1;
            return true;
        }
    }
    return false;
}
void fn_str_dict_if_full(Str_Dict *dict, int elem_width) {
    assert(dict->cap * 2 > dict->cap && "mul overflow");

    u8 *old_data = dict->data;
    int old_cap  = dict->cap;

    dict->cap      *= 2;
    dict->data      = malloc(dict->cap + dict->cap * dict->kv_stride);
    dict->remaining = ((dict->cap + 1) / 8) * 7;

    memset(dict->data, STR_DICT_EMPTY, dict->cap);

    int stride = dict->kv_stride;

    int  pc;
    int  tz;
    u16  mask;
    u64 *phash;

    __m128i a;
    for(int i = 0; i < old_cap; i += 16) {
        a    = _mm_load_si128((__m128i*)(old_data + i));
        mask = ~(_mm_movemask_epi8(a));

        pc = pop_count16(mask);
        for(int j = 0; j < pc; ++j) {
            tz    = count_trailing_zeros_u16(mask);
            mask ^= 1 << tz;

            phash = (u64*)(old_data + old_cap + (stride * (tz + i)));
            assert(fn_str_dict_insert_hash(dict, *phash, (u8*)phash + 8, elem_width) && "dict grow failure");
        }
    }
    free(old_data);
}
void* fn_str_dict_find_hash(Str_Dict *dict, u64 hash) {
    u8 top7   = (hash >> 57) & STR_DICT_FULL;
    int g_idx = hash & (dict->cap - 1);
    g_idx -= g_idx & 15;

    u8 *data   = dict->data;
    int stride = dict->kv_stride;
    int cap    = dict->cap;

    int  pc;
    int  tz;
    u16  mask;
    u64 *phash;

    __m128i a;
    __m128i b = _mm_set1_epi8(top7);

    int inc = 0;
    while(inc < cap) {
        a    = _mm_load_si128((__m128i*)(data + g_idx));
        a    = _mm_cmpeq_epi8(a, b);

        mask = _mm_movemask_epi8(a);
        pc   = pop_count16(mask);

        for(int i = 0; i < pc; ++i) {
            tz    = count_trailing_zeros_u16(mask);
            mask ^= 1 << tz;
            phash = (u64*)(data + cap + (stride * (tz + g_idx)));
            if (*phash == hash)
                return (u8*)phash + 8;
        }
        g_idx += 16;
        g_idx &= cap - 1;
        inc   += 16;
    }
    return NULL;
}
void* fn_str_dict_delete_hash(Str_Dict *dict, u64 hash) {
    u8 top7   = (hash >> 57) & STR_DICT_FULL;
    int g_idx = hash & (dict->cap - 1);
    g_idx    -= g_idx & 15;

    __m128i a;
    __m128i b = _mm_set1_epi8(top7);

    u8 *data   = dict->data;
    int stride = dict->kv_stride;
    int cap    = dict->cap;

    int  pc;
    int  tz;
    u16  mask;
    u64 *phash;

    int inc = 0;
    while(inc < cap) {
        a    = _mm_load_si128((__m128i*)(data + g_idx));
        a    = _mm_cmpeq_epi8(a, b);

        mask = _mm_movemask_epi8(a);
        pc   = pop_count16(mask);

        for(int i = 0; i < pc; ++i) {
            tz    = count_trailing_zeros_u16(mask);
            mask ^= 1 << tz;
            phash = (u64*)(data + cap + (stride * (tz + g_idx)));
            if (*phash == hash) {
                data[g_idx + tz] = STR_DICT_DELETED;
                return (u8*)phash + 8;
            }
        }
        g_idx += 16;
        g_idx &= cap - 1;
        inc   += 16;
    }
    return NULL;
}
#endif // implementation guard
#endif // include guard
/*

    Copyright 2023 Solomon Carden-Brown (solomoncardenbrown@gmail.com)

    Permission is hereby granted, free of charge, to any person obtaining a copy of
    this software and associated documentation files (the “Software”), to deal in
    the Software without restriction, including without limitation the rights to
    use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is furnished to do
    so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.

*/
