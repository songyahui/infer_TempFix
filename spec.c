
#define SW_CHANNEL_MIN_MEM (1024*64)

/* malloc(path): 
    Post (TRUE, malloc(ret))
    Future (TRUE, (!free(ret))^* · free(ret) · (_)^* ) @*/

/* free(handler): 
    Post (TRUE, free(handler)) 
    Future  (TRUE, (!_(handler))^* · (𝝐 \/ (malloc(handler) · (_)^*)))  @*/

/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ memset(a, b): 
    Post (TRUE, memset(a))  @*/

/*@ p11_array_new(a, b): 
    Post (TRUE, strcpy(a))  @*/


