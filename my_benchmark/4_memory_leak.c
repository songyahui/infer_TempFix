
#include <sys/stat.h>
#include <sys/resource.h>
#include <sys/ioctl.h>
#include <execinfo.h>


int helper () 
/*@ helper: 
    Require TRUE, 𝝐
    Ensure  (ret=NULL, emp) \/ (!(mtd=0), 𝝐)
 @*/
{
    return NULL;
}

void checkGuardedBy( cnd) 
/*@ checkGuardedBy: 
    Require TRUE, 𝝐
    Ensure  TRUE, throwExc
 @*/
{
    throwExc(); 
}

int locationMarker(){
    0;
}

void test() 
/*@ test: 
    Require TRUE, 𝝐
    Ensure  (mtd=0, throwExc) \/ (!(mtd=0), 𝝐)
 @*/
{
    int mtd =  helper (); 
 // method may be null
   // checkGuardedBy(mtd != null, id.toString());
    
    if (mtd==NULL) {
        locationMarker();
        //throwExc(); 
        locationMarker();
    }
    
    return mtd;
}



