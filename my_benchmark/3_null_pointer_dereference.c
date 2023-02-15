swHashMap *hmap = sw_malloc(sizeof(swHashMap));
if (!hmap) {
swWarn("malloc[1] failed."); return NULL;
}
swHashMap_node *root =
sw_malloc(sizeof(swHashMap_node)); if (!root) {
    swWarn("malloc[2] failed."); 
return NULL; // returns, hmap not freed
}

#define sw_free ( ptr ) if(ptr) {
free(ptr); ptr=NULL; swWarn ( "free" ) ;
}