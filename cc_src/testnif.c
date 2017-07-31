#include <stdbool.h>
#include <math.h>
#include "erl_nif.h"


static ErlNifResourceType* testnif_RESOURCE = NULL;

typedef struct
{
} testnif_handle;

// Prototypes
static ERL_NIF_TERM testnif_new(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM testnif_myfunction(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM findPrime(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static bool isPrime(int i);

static ErlNifFunc nif_funcs[] =
{
    {"new", 0, testnif_new},
    {"myfunction", 1, testnif_myfunction},
    {"findPrime", 1, findPrime}
};

static ERL_NIF_TERM testnif_new(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[])
{
    testnif_handle* handle = enif_alloc_resource(testnif_RESOURCE,
                                                    sizeof(testnif_handle));
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}


static ERL_NIF_TERM testnif_myfunction(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    // printf("fprintf\n");
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM findPrime(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int n;
    if(!enif_get_int(env, argv[0], &n))
      return enif_make_badarg(env);
    else
    {
        int i;
        ERL_NIF_TERM res = enif_make_list(env, 0);
        for(i = 2; i < n; ++i)
        {
            if(isPrime(i))
              res = enif_make_list_cell(env, enif_make_int(env, i), res);
        }
        return res;
    }
}

static void testnif_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in testnif_handle */
    /* testnif_handle* handle = (testnif_handle*)arg; */
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                                                     "testnif_resource",
                                                     &testnif_resource_cleanup,
                                                     flags, NULL);
    if (rt == NULL)
        return -1;

    testnif_RESOURCE = rt;

    return 0;
}

static bool isPrime(int i)
{
        int j;
        int t = sqrt(i) + 1;
        for(j = 2; j <= t; ++j)
        {
                if(i % j == 0)
                        return false;
        }
        return true;
}

ERL_NIF_INIT(testnif, nif_funcs, &on_load, NULL, NULL, NULL);
