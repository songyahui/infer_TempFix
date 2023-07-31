
#define SW_CHANNEL_MIN_MEM (1024*64)


/*@ open(path): 
    Post (ret<0, 𝝐) \/ (ret>=0, open(ret))
    Future (ret>=0, (!close(ret))^* · close(ret) · (_)^* )  @*/

/*@ socket(domain, type, protocol): 
    Post (ret<0, 𝝐) \/ (ret>=0, socket(ret))
    Future (ret>=0, (!close(ret))^* · close(ret) · (_)^* )  @*/

/*@ swSocket_create(arg): 
    Post (ret<0, 𝝐) \/ (ret>=0, swSocket_create(ret))
    Future (ret>=0, (!close(ret))^* · close(ret) · (_)^* )  @*/

/*@ close(handler): 
    Post (TRUE, close(handler)) 
    Future  (TRUE, (!_(handler))^*)  @*/

/*@ fopen(path): 
    Post (ret<0, 𝝐) \/ (ret>=0, fopen(ret))
    Future (ret>=0, (!fclose(ret))^* · fclose(ret) · (_)^* )  @*/

/*@ fclose(handler): 
    Post (TRUE, fclose(handler)) 
    Future  (TRUE, (!_(handler))^*)  @*/

/*@ opendir(path): 
    Post (ret<0, 𝝐) \/ (ret>=0, opendir(ret))
    Future (ret>=0, (!closedir(ret))^* · closedir(ret) · (_)^* )  @*/

/*@ closedir(handler): 
    Post (TRUE, closedir(handler)) 
    Future  (TRUE, (!_(handler))^*)  @*/


// swClient_create

//NPD
/* localtime(t): 
    Future  (ret=0, (!_(ret))^*)  */


/* malloc(path): 
    Future  (ret=0, (!_(ret))^*)  */

// Memory Leak
/* malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* ) \/ (ret=0, (!_(ret))^*) */


/* free(handler): 
    Post (TRUE, free(handler)) 
    Future  (TRUE, (!_(handler))^*)  */


/* swArray_new(p, i): 
    Future  (!(ret=0),  free(ret) )  */


/* swArray_free(arr): 
    Pre (TRUE, (_)^* · malloc(arr) · (_)^* ) */
