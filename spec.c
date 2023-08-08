
#define SW_CHANNEL_MIN_MEM (1024*64)

// Resourse leak 
/*@ open(path): 
    Post (ret<0, 𝝐) \/ (ret>=0, open(ret))
    Future (ret>=0, (!close(ret))^* · close(ret) · (_)^* )  @*/

/*@ socket(domain, type, protocol): 
    Post (ret<0, 𝝐) \/ (ret>=0, socket(ret))
    Future (ret>=0, (!close(ret))^* · close(ret) · (_)^* )  @*/

/*@ swSocket_create(arg): 
    Post (ret<0, 𝝐) \/ (ret>=0, socket(ret))
    Future (ret>=0, (!close(ret))^* · close(ret) · (_)^* )  @*/

/*@ close(handler): 
    Post (TRUE, close(handler)) 
    Future  (TRUE, (!_(handler))^*)  @*/

/*@ fopen(path): 
    Post (ret<0, 𝝐) \/ (ret>0, fopen(ret))
    Future (ret>0, (!fclose(ret))^* · fclose(ret) · (_)^* )  @*/

/*@ fclose(handler): 
    Post (TRUE, fclose(handler)) 
    Future  (TRUE, (!_(handler))^*)  @*/

/*@ opendir(path): 
    Post (ret<0, 𝝐) \/ (ret>0, opendir(ret))
    Future (ret>0, (!closedir(ret))^* · closedir(ret) · (_)^* )  @*/

/*@ closedir(handler): 
    Post (TRUE, closedir(handler)) 
    Future  (TRUE, (!_(handler))^*)  @*/

// Memory bugs 
/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* ) \/ (ret=0, (!_(ret))^*) @*/

/*@ free(handler): 
    Post (TRUE, free(handler)) 
    Future  (TRUE, (!_(handler))^*)  @*/

/*@ swMalloc_alloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* ) \/ (ret=0, (!_(ret))^*) @*/


// NPD
/*@ localtime(t): 
    Future  (ret=0, (!_(ret))^*)  @*/

/*@ swReactor_get(t): 
    Future  (ret=0, (!_(ret))^*)  @*/
// --------------------
/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ swMalloc_alloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (ret=0, (!_(ret))^*) @*/
// --------------------
/*@ swoole_get_property(a, b): 
    Future  (ret=0, (!_(ret))^*)  @*/
// --------------------
/*@ swServer_connection_verify(a, b): 
    Future  (ret=0, (!_(ret))^*)  @*/

/*@ swServer_connection_get(a, b): 
    Future  (ret=0, (!_(ret))^*)  @*/
// --------------------
/*@ swServer_get_worker(a, b): 
    Future  (ret=0, (!_(ret))^*)  @*/

/*@ swWorker_free(handler): 
    Post (TRUE, free(handler))  @*/

/*@ kill(handler, b): 
    Post (TRUE, free(handler))  @*/


