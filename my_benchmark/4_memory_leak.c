
#include <sys/stat.h>
#include <sys/resource.h>
#include <sys/ioctl.h>
#include <execinfo.h>


int helper () {
    return NULL;
}

void checkGuardedBy( cnd) 
/*@ checkGuardedBy: 
    Require TRUE, 𝝐
    Ensure  TRUE, throwExc
 @*/
{
    if (!cnd) {
        throwExc(); 
    }
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
        locationMarker();
    }
    return mtd;
}



