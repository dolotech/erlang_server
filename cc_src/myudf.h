#ifndef MTEST_H_
#define MTEST_H_
#include <my_global.h>
#include <mysql.h>

my_bool show_hero_init(UDF_INIT *initid, UDF_ARGS *args, char *message);

char *show_hero(UDF_INIT *initid, UDF_ARGS *args,
            char *result, unsigned long *length,
            char *is_null, char *error);

void show_hero_deinit(UDF_INIT *initid);

my_bool show_item_init(UDF_INIT *initid, UDF_ARGS *args, char *message);

char *show_item(UDF_INIT *initid, UDF_ARGS *args,
            char *result, unsigned long *length,
            char *is_null, char *error);

void show_item_deinit(UDF_INIT *initid);

#endif

#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
            (((unsigned char*) (s))[1] << 16) | \
            (((unsigned char*) (s))[2] << 8)  | \
            (((unsigned char*) (s))[3]))

#define put_int32(i, s) do {((char*)(s))[0] = (char)((i) >> 24) & 0xff;   \
    ((char*)(s))[1] = (char)((i) >> 16) & 0xff;   \
    ((char*)(s))[2] = (char)((i) >> 8)  & 0xff;   \
    ((char*)(s))[3] = (char)(i)         & 0xff;} \
while (0)

#define get_int24(s) ((((unsigned char*) (s))[0] << 16) | \
            (((unsigned char*) (s))[1] << 8)  | \
            (((unsigned char*) (s))[2]))

#define put_int24(i, s) do {((char*)(s))[0] = (char)((i) >> 16) & 0xff;  \
    ((char*)(s))[1] = (char)((i) >> 8)  & 0xff;  \
    ((char*)(s))[2] = (char)(i)         & 0xff;} \
while (0)

#define get_int16(s) ((((unsigned char*)  (s))[0] << 8) | \
            (((unsigned char*)  (s))[1]))

#define put_int16(i, s) do {((char*)(s))[0] = (unsigned char)((i) >> 8) & 0xff;  \
    ((char*)(s))[1] = (unsigned char)(i)        & 0xff;} \
while (0)

#define get_int8(s) ((((unsigned char*)  (s))[0] ))

#define put_int8(i, s) do {((unsigned char*)(s))[0] = (i) & 0xff;} while (0)

#define get_int_b32(s) ((((unsigned char*) (s))[3] << 24) | \
            (((unsigned char*) (s))[2] << 16) | \
            (((unsigned char*) (s))[1] << 8)  | \
            (((unsigned char*) (s))[0]))

#define put_int_b32(i, s) do {((char*)(s))[3] = (char)((i) >> 24) & 0xff;   \
    ((char*)(s))[2] = (char)((i) >> 16) & 0xff;   \
    ((char*)(s))[1] = (char)((i) >> 8)  & 0xff;   \
    ((char*)(s))[0] = (char)(i)         & 0xff;} \
while (0)

#define get_int_b24(s) ((((unsigned char*) (s))[2] << 16) | \
            (((unsigned char*) (s))[1] << 8)  | \
            (((unsigned char*) (s))[0]))

#define put_int_b24(i, s) do {((char*)(s))[2] = (char)((i) >> 16) & 0xff;  \
    ((char*)(s))[1] = (char)((i) >> 8)  & 0xff;  \
    ((char*)(s))[0] = (char)(i)         & 0xff;} \
while (0)

#define get_int_b16(s) ((((unsigned char*)  (s))[1] << 8) | \
            (((unsigned char*)  (s))[0]))

#define put_int_b16(i, s) do {((char*)(s))[1] = (char)((i) >> 8) & 0xff;  \
    ((char*)(s))[0] = (char)(i)        & 0xff;} \
while (0)

#define get_int_b8(s) ((((unsigned char*)  (s))[0] ))

#define put_int_b8(i, s) do {((unsigned char*)(s))[0] = (i) & 0xff;} while (0)
