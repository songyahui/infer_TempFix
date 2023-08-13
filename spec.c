
#define SW_CHANNEL_MIN_MEM (1024*64)

// NPD

/*@ realloc(a,b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), realloc(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ regmatch_dup(a,b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), regmatch_dup(ret)) 
    Future (ret=0, (!_(ret))^*) @*/

/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ calloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), calloc(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ fdopen(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), fdopen(ret))
    Future (ret=0, (!_(ret))^*) @*/


/*@ strcmp(a, b): 
    Post (TRUE, strcmp(a))  @*/

/*@ p11_array_push(a): 
    Post (TRUE, p11_array_push(a))  @*/

/*@ p11_array_new(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), p11_array_new(ret))
    Future (ret=0, (!_(ret))^*) @*/

