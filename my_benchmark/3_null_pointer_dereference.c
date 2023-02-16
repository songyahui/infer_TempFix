
#include <sys/stat.h>
#include <sys/resource.h>
#include <sys/ioctl.h>
#include <execinfo.h>

typedef struct swHashMap_aux {
   
} swHashMap;


void sw_free ( ptr ) 
/*@ sw_free: 
    Require TRUE, 𝝐
    Ensure  TRUE, free  @*/
{
    if(ptr) {
        free(ptr); 
        ptr=NULL; 
        swWarn ( "free" ) ;
    }
}


void test () 
/*@ test: 
    Require TRUE, 𝝐
    Ensure  (hmap<0, malloc) \/ 
            ((!(hmap<0)) /\ root<0, malloc · malloc  · free) \/
            ((!(hmap<0)) /\ (!(root<0)), malloc · malloc  · free · free)
               @*/
{
    swHashMap *hmap = malloc(sizeof(swHashMap));
    if (hmap<0) {
        swWarn("malloc[1] failed."); 
        return NULL;
    }
    swHashMap *root = malloc(sizeof(swHashMap)); 
    if (root<0) {
        swWarn("malloc[2] failed."); 
        return NULL; // returns, hmap not freed
    }
    free(hmap);
    free(root);
}
