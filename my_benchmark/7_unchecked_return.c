
#include <sys/stat.h>
#include <sys/resource.h>
#include <sys/ioctl.h>
#include <execinfo.h>


typedef struct swString {
    char* str;
    int length;
   
} swString;

swString* helper () 
/*@ helper: 
    Require TRUE, 𝝐
    Ensure  (ret=NULL, emp) \/ (!(mtd=0), 𝝐)
 @*/
{
    return NULL;
}


void* malloc_syh( size_t size ){
    
    return NULL;
}


void test() 
/*@ test: 
    Require TRUE, 𝝐
    Ensure  (mtd=0, throwExc) \/ (!(mtd=0), 𝝐)
 @*/
{
    //swString *mtd =  helper (); 
 // method may be null
   // checkGuardedBy(mtd != null, id.toString());
    
    char* buf = (char*) malloc_syh(4); 
    
    
    return free(buf);
    //return mtd;
}



