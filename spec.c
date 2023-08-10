
#define SW_CHANNEL_MIN_MEM (1024*64)

/*@ open(path): 
    Post (ret<0, 𝝐) \/ (ret>=0, open(ret))
    Future (ret>=0, (!close(ret))^* · close(ret) · (_)^* )  @*/

/*@ socket(domain, type, protocol): 
    Post (ret<0, 𝝐) \/ (ret>=0, socket(ret))
    Future (ret>=0, (!close(ret))^* · close(ret) · (_)^* )  @*/


/*@ close(handler): 
    Post (TRUE, close(handler)) 
    Future  (TRUE, (!_(handler))^*)  @*/

/*@ fdopen(path): 
    Post (ret<0, 𝝐) \/ (ret>0, fdopen(ret))
    Future (ret>0, (!fclose(ret))^* · fdclose(ret) · (_)^* )  @*/
    
/*@ fdclose(handler): 
    Post (TRUE, fdclose(handler)) 
    Future  (TRUE, (!_(handler))^*)  @*/
