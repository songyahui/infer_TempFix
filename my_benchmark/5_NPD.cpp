#include <iostream>
#include <stdexcept>
#include <sys/socket.h>  
#include <arpa/inet.h>
 #include <netdb.h>

using namespace std;


using namespace std;


typedef uint32_t in_addr_t;



void validate_addr_form(char *user_supplied_addr){
}



void threwNPDExc (int  *pointer1)
/*@ threwNPDExc: 
    Require TRUE, 𝝐
    Ensure  (pointer1=NULL, Exc(NPD)) \/ (!(pointer1=NULL), 𝝐)
 @*/
{
    if (pointer1 == NULL) {

        throw std::runtime_error("NPD");

    }
    return;
}



in_addr_t inet_addr(const char *cp);

struct hostent *gethostbyaddr(char *host_address,
                              int address_length,
                              int address_type);
/*@ gethostbyaddr: 
    Require TRUE, 𝝐
    Ensure  exists ret. (ret=NULL, gethostbyaddr) \/ (!(ret=NULL), gethostbyaddr)
 @*/

// https://cwe.mitre.org/data/definitions/476.html
void host_lookup(char *user_supplied_addr)
/*@ gethostbyaddr: 
    Require TRUE, 𝝐
    Ensure  exists hp. (hp=NULL, gethostbyaddr.Exc(NPD)) \/ 
    (!(hp=NULL), gethostbyaddr.strcpy)
 @*/
{
    struct hostent *hp;
    in_addr_t *addr;
    char hostname[64];
    in_addr_t inet_addr(const char *cp);

/*routine that ensures user_supplied_addr is in the right format for conversion */ 

    validate_addr_form(user_supplied_addr);
    * addr = inet_addr(user_supplied_addr);
    hp = gethostbyaddr( addr, sizeof(struct in_addr), AF_INET);
    strcpy(hostname, hp->h_name);
}


int main () {
    struct hostent *hp = NULL;

    char * name = hp-> h_name;
    return 1;
}
