
#define SW_CHANNEL_MIN_MEM (1024*64)

// Resourse leak 
/*@ open(path): 
    Post (ret<0, 𝝐) \/ (ret>=0, open(ret))
    Future (ret>=0, (!close(ret))^* · close(ret) · (_)^* )  @*/

/*@ socket(domain, type, protocol): 
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

/*@ fputs(a, b): 
    Post (TRUE, fputs(b))  @*/


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
