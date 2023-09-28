#define SW_CHANNEL_MIN_MEM (1024*64)


// resource Leak


/*@ SSL_CTX_new(meth):
    Future ((ret=0), return(meth))@*/

/*@ return(arg):
    Future (TRUE, return(arg))@*/


/*@ ERR_raise():
    Future (TRUE, ERR_raise())@*/



SSL_new