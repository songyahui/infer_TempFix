
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


// Resourse leak 
/*@ open(path): 
    Post (ret<0, 𝝐) \/ (ret>=0, open(ret))
    Future (ret>=0, (!close(ret))^* · close(ret) · (_)^* )  @*/

/*@ socket(domain, type, protocol): 
    Post (ret<0, 𝝐) \/ (ret>=0, socket(ret))
    Future (ret>=0, (!close(ret))^* · close(ret) · (_)^* )  @*/

/*@ close(handler): 
    Post (TRUE, close(handler))   @*/

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
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/

/*@ free(handler): 
    Post (TRUE, free(handler))   @*/

/*@ p11_index_free(handler): 
    Post (TRUE, CONSUME(handler))   @*/

/*@ p11_asn1_cache_free(handler): 
    Post (TRUE, CONSUME(handler))   @*/

/*@ p11_dict_new(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), p11_dict_new(ret))
    Future (!(ret=0), (_)^* · (free(ret) \/  p11_dict_clear(ret)  \/ p11_array_free(ret)) · (_)^* )  @*/

/*@ load_seq_of_oid_str(a, b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), load_seq_of_oid_str(ret))
    Future (!(ret=0), (_)^* · (free(ret) \/  p11_dict_clear(ret)  \/ p11_array_free(ret)) · (_)^* )  @*/

/*@ p11_url_decode(a, b, c, d, e): 
    Post (ret=0, 𝝐) \/ (!(ret=0), p11_url_decode(ret))
    Future (!(ret=0), (_)^* · (free(ret) \/  p11_dict_clear(ret)  \/ p11_array_free(ret)) · (_)^* )  @*/


/*@ p11_asn1_defs_load(): 
    Post (ret=0, 𝝐) \/ (!(ret=0), p11_asn1_defs_load(ret))
    Future (!(ret=0), (_)^* · (free(ret) \/  p11_dict_clear(ret)  \/ p11_array_free(ret)) · (_)^* )  @*/


/*@ p11_asn1_read(a, b, c): 
    Post (ret=0, 𝝐) \/ (!(ret=0), p11_asn1_read(ret))
    Future (!(ret=0), (_)^* · (free(ret) \/  p11_dict_clear(ret) ) · (_)^* )  @*/

/*@ p11_dict_clear(handler): 
    Post (TRUE, p11_dict_clear(handler))  @*/

/*@ p11_dict_free(handler): 
    Post (TRUE, free(handler))  @*/
 
/*@ memdup(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), memdup(ret))
    Future (!(ret=0), (!free(ret))^* · (free(ret)) · (_)^* )  @*/
 
 
/*@ p11_kit_pin_new_for_buffer(a, b, c): 
    Post (TRUE, CONSUME(a))  @*/
 
