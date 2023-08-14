
#define SW_CHANNEL_MIN_MEM (1024*64)

// Memory bugs 
/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/

/*@ free(handler): 
    Post (TRUE, free(handler)) 
    Future  (TRUE, (!_(handler))^*)  @*/

/*@ regmatch_dup(a, b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), regmatch_dup(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/
