
#define SW_CHANNEL_MIN_MEM (1024*64)

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
