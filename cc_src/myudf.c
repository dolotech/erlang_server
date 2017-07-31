#include <string.h>
#include "myudf.h"

my_bool show_hero_init(UDF_INIT *initid, UDF_ARGS *args, char *message)
{
    if(args->arg_count < 1 || args->arg_count > 2){
        char *msg = "show_hero has 1 or 2 arguments!";
        strncpy(message, msg, strlen(msg) + 1);
        return 1;
    }

    initid->ptr = calloc(1, 1024);

    return 0;
}
char *show_hero(UDF_INIT *initid, UDF_ARGS *args,
            char *result, unsigned long *length,
            char *is_null, char *error)
{
    unsigned char *data = (char *)args->args[0];
    unsigned long len = args->lengths[0];
    unsigned char buffer[1024];
    int v = get_int8(data);
    data += 1;
    int tid = get_int32(data);
    data += 4;
    int job = get_int8(data);
    data += 1;
    int hp = get_int32(data);
    data += 4;
    int agi = get_int32(data);
    data += 4;
    int atk = get_int32(data);
    data += 4;
    int pos = get_int8(data);
    data += 1;
    int exp_max = get_int32(data);
    data += 4;
    int exp = get_int32(data);
    data += 4;
    int lev = get_int8(data);
    data += 1;
    int essence = get_int32(data);
    data += 4;
    int step = get_int8(data);
    data += 1;
    int growth = get_int8(data);
    data += 1;
    int skills_len = get_int16(data);
    data += 2;

    int index = 0;
    char list[1024];
    memset(list, '\0', 1024);
    char tmpstr[10];
    int tmplen;
    int skillId = 0, skillRate = 0, skillTags = 0, skillPos;

    while(index < skills_len){
        skillPos = get_int8(data);
        data += 1;
        skillId = get_int32(data);
        data += 4;
        skillRate = get_int8(data);
        data += 1;
        skillTags = get_int8(data);
        data += 1;
        tmplen = sprintf(tmpstr, ",{%u,%u,%u,%u}", 
                    skillPos, skillId, skillRate, skillTags);
        strncat(list, tmpstr, tmplen);
        index++;
    }

    if(2 == args->arg_count){
        char *field = (char *)args->args[1];
        unsigned long field_len = args->lengths[1];
        char field_name[field_len];
        memset(field_name, '\0', field_len);
        memcpy(field_name, field, field_len);
        field_name[field_len] = '\0';
        if(strcmp(field_name, "lev") == 0){
            *length = sprintf(buffer, "%u", lev);
        }else if(strcmp(field_name, "tid") == 0){
            *length = sprintf(buffer, "%u", tid);
        }else if(strcmp(field_name, "step") == 0){
            *length = sprintf(buffer, "%u", step);
        }else if(strcmp(field_name, "pos") == 0){
            *length = sprintf(buffer, "%u", pos);
        }else if(strcmp(field_name, "job") == 0){
            *length = sprintf(buffer, "%u", job);
        }else{
            *length = sprintf(buffer, "undef:%s", field_name);
        }
    }else{
        *length = sprintf( buffer, "[2]v:%u,tid:%u,job:%u, hp:%u,"
                    "agi:%u, atk:%u, pos:%u, exp_max:%u, exp:%u, lev:%u,"
                    "essence:%u, step:%u, growth:%u, skills:%u%s",
                    v,tid,job, hp, agi, atk, pos, exp_max, exp, lev, 
                    essence, step, growth, skills_len, list);
    }

    memcpy(initid->ptr, buffer, *length+1);
    return initid->ptr;
}

void show_hero_deinit(UDF_INIT *initid)
{
    free(initid->ptr);
}


my_bool show_item_init(UDF_INIT *initid, UDF_ARGS *args, char *message)
{
    if(args->arg_count < 1 || args->arg_count > 2){
        char *msg = "show_item has 1 or 2 arguments!";
        strncpy(message, msg, strlen(msg) + 1);
        return 1;
    }

    initid->ptr = calloc(1, 1024);

    return 0;
}
char *show_item(UDF_INIT *initid, UDF_ARGS *args,
            char *result, unsigned long *length,
            char *is_null, char *error)
{
    unsigned char *data = (char *)args->args[0];
    unsigned long len = args->lengths[0];
    unsigned char buffer[1024];
    int v = get_int8(data); data += 1;
    int tid = get_int32(data); data += 4;
    int sort = get_int8(data); data += 1;
    int hero_id = get_int32(data); data += 4;
    int job = get_int8(data); data += 1;
    int tab = get_int8(data); data += 1;
    int pos = get_int8(data); data += 1;
    int num_max = get_int16(data); data += 2;
    int num = get_int16(data); data += 2;
    int lev = get_int8(data); data += 1;
    int atime = get_int32(data); data += 4;
    int etime = get_int32(data); data += 4;
    int hp = get_int32(data); data += 4;
    int agi = get_int32(data); data += 4;
    int atk = get_int32(data); data += 4;
    int listlen = get_int16(data); data += 2;
    int index = 0;
    char list[1024];
    memset(list, '\0', 1024);
    char tmpstr[10];
    int tmplen;
    while(index < listlen){
        tmplen = sprintf(tmpstr, ",{%u}", get_int32(data));
        data += 4;
        strncat(list, tmpstr, tmplen);
        index++;
    }
    if(2 == args->arg_count){
        char *field = (char *)args->args[1];
        unsigned long field_len = args->lengths[1];
        char field_name[field_len];
        memset(field_name, '\0', field_len);
        memcpy(field_name, field, field_len);
        field_name[field_len] = '\0';
        if(strcmp(field_name, "lev") == 0){
            *length = sprintf(buffer, "%u", lev);
        }else if(strcmp(field_name, "tid") == 0){
            *length = sprintf(buffer, "%u", tid);
        }else if(strcmp(field_name, "sort") == 0){
            *length = sprintf(buffer, "%u", sort);
        }else if(strcmp(field_name, "hero_id") == 0){
            *length = sprintf(buffer, "%u", hero_id);
        }else{
            *length = sprintf(buffer, "undef:%s", field_name);
        }
    }else{
        *length = sprintf( buffer, 
                    "v:%u,tid:%u,sort:%u,hero_id:%u,job:%u,tab:%u,"
                    "pos:%u,num_max:%u,num:%u,lev:%u,"
                    "atime:%u,etime:%u,hp:%u,agi:%u,atk:%u,list:%u%s",
                    v,tid,sort,hero_id,job,tab,pos,num_max,num,lev,
                    atime,etime,hp,agi,atk,listlen,list);
    }
    memcpy(initid->ptr, buffer, *length+1);
    return initid->ptr;
}

void show_item_deinit(UDF_INIT *initid)
{
    free(initid->ptr);
}
