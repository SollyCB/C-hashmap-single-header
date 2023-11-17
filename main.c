#define SOL_STR_DICT_IMPLEMENTATION
#include "str_dict.h"

typedef struct Thing {int x,y;} Thing;

int main() {
    int cap = 16;
    Str_Dict dict = new_str_dict(cap, Thing);

    char buf[2];
    Thing thing;
    for(int i = 1; i < 255; ++i) {
        buf[0] = i;
        buf[1] = '\0';
        thing = (struct Thing){i, i};
        assert(str_dict_insert(&dict, buf, &thing) && "Insertion Failed");
    }

    Thing *pthing;
    for(int i = 1; i < 255; ++i) {
        buf[0] = i;
        buf[1] = '\0';
        pthing = str_dict_find(&dict, buf);

        assert(pthing && "Find Failed");

        assert(pthing->x == i && "That is not my card...");
        assert(pthing->y == i && "That is not my card...");

        printf("thing.x = %i, thing.y = %i\n", pthing->x, pthing->y);
    }

    for(int i = 1; i < 255; ++i) {
        buf[0] = i;
        buf[1] = '\0';
        pthing = str_dict_delete(&dict, buf);

        assert(pthing && "Find Failed");

        assert(pthing->x == i && "That is not my card...");
        assert(pthing->y == i && "That is not my card...");

        printf("thing.x = %i, thing.y = %i\n", pthing->x, pthing->y);

        pthing = str_dict_find(&dict, buf);
        assert(!pthing && "Delete Failed");
    }

    free_str_dict(&dict);
    return 0;
}
