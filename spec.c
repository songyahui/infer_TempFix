
#define SW_CHANNEL_MIN_MEM (1024*64)

// Resourse leak 

/*@ rpl_open(path): 
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


//NPD


/*@ get_name_action(t): 
    Future  (ret=0, (!_(ret))^*)  @*/


/*@ call_chunkfun(t): 
    Future  (ret=0, (!_(ret))^*)  @*/

/*@ strchr(a, b): 
    Future  (ret=0, (!_(ret))^*)  @*/

/*@ format_find(a, b): 
    Future  (ret=0, (!_(ret))^*)  @*/

/*@ gettermname(): 
    Future  (ret=0, (!_(ret))^*)  @*/

/*@ put_string(a, b): 
    Post  (TRUE, put_string(b))  @*/
    
/*@ strlen(a): 
    Post  (TRUE, strlen(a))  @*/


/*@ strdup(a): 
    Post  (TRUE, strdup(a))  @*/

// Memory bugs 

/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/

/*@ free(handler): 
    Post (TRUE, free(handler)) 
  @*/

/*@ ping_set_data(a, b, c, d): 
    Post (b=0, 𝝐) \/ (!(b=0), malloc(b))
    Future (!(b=0), (!free(b))^* · free(b) · (_)^* )  @*/

/*@ env_opt_start_info(): 
    Post (b=0, 𝝐) \/ (!(b=0), malloc(b))
    Future (!(b=0), (!free(b))^* · free(b) · (_)^* )  @*/

  
