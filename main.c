#define SOL_HASH_MAP_IMPLEMENTATION
#include "hash_map.h"

typedef struct Thing {int x,y;} Thing;

int main() {
    int cap = 16;
    Hash_Map map = new_hash_map(cap, Thing);

    char buf[2];
    Thing thing;
    for(int i = 1; i < 255; ++i) {
        buf[0] = i;
        buf[1] = '\0';
        thing = (struct Thing){i, i};
        assert(hash_map_insert_str(&map, buf, &thing) && "Insertion Failed");
    }

    Thing *pthing;
    for(int i = 1; i < 255; ++i) {
        buf[0] = i;
        buf[1] = '\0';
        pthing = hash_map_find_str(&map, buf);

        assert(pthing && "Find Failed");

        assert(pthing->x == i && "That is not my card...");
        assert(pthing->y == i && "That is not my card...");

        printf("thing.x = %i, thing.y = %i\n", pthing->x, pthing->y);
    }

    for(int i = 1; i < 255; ++i) {
        buf[0] = i;
        buf[1] = '\0';
        pthing = hash_map_delete_str(&map, buf);

        assert(pthing && "Find Failed");

        assert(pthing->x == i && "That is not my card...");
        assert(pthing->y == i && "That is not my card...");

        printf("thing.x = %i, thing.y = %i\n", pthing->x, pthing->y);

        pthing = hash_map_find_str(&map, buf);
        assert(!pthing && "Delete Failed");
    }

    free_hash_map(&map);
    return 0;
}
