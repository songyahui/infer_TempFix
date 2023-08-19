
#define SW_CHANNEL_MIN_MEM (1024*64)

/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* ) \/ (ret=0, (!_(ret))^*) @*/

/*@ free(handler): 
    Post (TRUE, free(handler)) 
      @*/
