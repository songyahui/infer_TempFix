
#define SW_CHANNEL_MIN_MEM (1024*64)

// Memory bugs 
/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/



/*@ free(handler): 
    Post (TRUE, free(handler))   @*/

/*@ p11_index_free(handler): 
    Post (TRUE, p11_index_free(handler))   @*/

    

/*@ p11_dict_new(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), p11_dict_new(ret))
    Future (!(ret=0), (_)^* · (free(ret) \/  p11_dict_clear(ret) \/ p11_dict_free(ret) \/ p11_array_free(ret)\/ p11_index_free(ret)) · (_)^* )  @*/

/*@ load_seq_of_oid_str(a, b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), load_seq_of_oid_str(ret))
    Future (!(ret=0), (_)^* · (free(ret) \/  p11_dict_clear(ret) \/ p11_dict_free(ret) \/ p11_array_free(ret)) · (_)^* )  @*/

/*@ p11_url_decode(a, b, c, d, e): 
    Post (ret=0, 𝝐) \/ (!(ret=0), p11_url_decode(ret))
    Future (!(ret=0), (_)^* · (free(ret) \/  p11_dict_clear(ret) \/ p11_dict_free(ret) \/ p11_array_free(ret)) · (_)^* )  @*/


/*@ p11_asn1_defs_load(): 
    Post (ret=0, 𝝐) \/ (!(ret=0), p11_asn1_defs_load(ret))
    Future (!(ret=0), (_)^* · (free(ret) \/  p11_dict_clear(ret) \/ p11_dict_free(ret) \/ p11_array_free(ret)) · (_)^* )  @*/


/*@ p11_asn1_read(a, b, c): 
    Post (ret=0, 𝝐) \/ (!(ret=0), p11_asn1_read(ret))
    Future (!(ret=0), (_)^* · (free(ret) \/  p11_dict_clear(ret) \/ p11_dict_free(ret)) · (_)^* )  @*/

/*@ p11_dict_clear(handler): 
    Post (TRUE, p11_dict_clear(handler))  @*/

/*@ p11_dict_free(handler): 
    Post (TRUE, p11_dict_free(handler))  @*/
 
/*@ memdup(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), memdup(ret))
    Future (!(ret=0), (!free(ret))^* · (free(ret)) · (_)^* )  @*/
 
 
/*@ p11_kit_pin_new_for_buffer(a, b, c): 
    Post (TRUE, CONSUME(a))  @*/
 
