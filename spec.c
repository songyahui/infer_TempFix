
#define SW_CHANNEL_MIN_MEM (1024*64)

// Memory bugs 
/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/

/*@ free(handler): 
    Post (TRUE, free(handler))   @*/

/*@ WavpackCloseFile(a): 
    Post (TRUE, CONSUME(a))   @*/

/*@ WavpackOpenFileInputEx64(a, b, c, d, e, f): 
    Post (TRUE, CONSUME(c))   @*/
    
/*@ init_words(a): 
    Post (TRUE, CONSUME(a))   @*/



