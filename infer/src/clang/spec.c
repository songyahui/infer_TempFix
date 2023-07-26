
#define SW_CHANNEL_MIN_MEM (1024*64)


/*@ open(path): 
    Post (ret<0, 𝝐) \/ (ret>=0, open(ret))
    Future (ret>=0, (!close(ret))^* · close(ret) · (_)^* )  @*/


/*@ close(handler): 
    Post (TRUE, close(handler)) 
    Future  (TRUE, (!_(handler))^*)  @*/


//NPD
/*@ localtime(t): 
    Future  (ret=0, (!_(ret))^*)  @*/


/*@ malloc(path): 
    Future  (ret=0, (!_(ret))^*)  @*/

// Memory Leak
/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* ) \/ (ret=0, (!_(ret))^*) @*/


/*@ free(handler): 
    Post (TRUE, free(handler)) 
    Future  (TRUE, (!_(handler))^*)  @*/


/* swArray_new(p, i): 
    Future  (!(ret=0),  free(ret) )  @*/


/* swArray_free(arr): 
    Pre (TRUE, (_)^* · malloc(arr) · (_)^* ) */
