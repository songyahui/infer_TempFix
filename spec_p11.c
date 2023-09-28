
#define SW_CHANNEL_MIN_MEM (1024*64)


/*@ open(path):
    Future (((ret>0)/\(!(ret=stdout)/\!(ret=stdin))), (!close(ret))^* · close(ret) · (_)^*)@*/

/*@ socket(domain, type, protocol):
    Future ((ret>0), (!close(ret))^* · close(ret) · (_)^*)@*/

/*@ close(handler):
    Post (TRUE, close(handler))@*/

/*@ fopen(path):
    Future (((ret>0)/\(!(ret=stdout)/\!(ret=stdin))), (!fclose(ret))^* · fclose(ret) · (_)^*)@*/

/*@ fclose(handler):
    Post (TRUE, fclose(handler))@*/

/*@ fdopen(path, b):
    Future (((ret>0)/\(!(ret=stdout)/\!(ret=stdin))), ((!fclose(ret))^* · fclose(ret) · (_)^* \/ (!close(path))^* · close(path) · (_)^*) ) \/ (ret=0, (!_(ret))^*) @*/


/*@ endmntent(handler):
    Post (TRUE, fclose(handler))@*/

/*@ fflush(handler):
    Post (TRUE, fclose(handler))@*/

/*@ opendir(path):
    Future ((ret>0), (!closedir(ret))^* · closedir(ret) · (_)^*)@*/

/*@ closedir(handler):
    Post (TRUE, closedir(handler))@*/

/*@ rpl_open(path):
    Future ((ret>0), (!close(ret))^* · close(ret) · (_)^*)@*/


/*@ malloc(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*) \/ ((ret=0), (!_(ret))^*)@*/

/*@ realloc(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*) \/ ((ret=0), (!_(ret))^*)@*/


/*@ strcmp(a, b): 
    Post (TRUE, strcmp(a))  @*/



/*@ regmatch_dup(a,b): 
    Future (ret=0, (!_(ret))^*) @*/


/*@ p11_array_push(a): 
    Post (TRUE, p11_array_push(a))  @*/

/*@ p11_array_new(path): 
    Future (ret=0, (!_(ret))^*) @*/


/*@ free(handler): 
    Post (TRUE, free(handler))   @*/



/*@ p11_dict_new(path): 
    Future (!(ret=0), (_)^* · (free(ret) \/  p11_dict_clear(ret)  \/ p11_array_free(ret)) · (_)^* )  @*/

/*@ load_seq_of_oid_str(a, b): 
    Future (!(ret=0), (_)^* · (free(ret) \/  p11_dict_clear(ret)  \/ p11_array_free(ret)) · (_)^* )  @*/

/*@ p11_url_decode(a, b, c, d, e): 
    Future (!(ret=0), (_)^* · (free(ret) \/  p11_dict_clear(ret)  \/ p11_array_free(ret)) · (_)^* )  @*/


/*@ p11_asn1_defs_load(): 
    Future (!(ret=0), (_)^* · (free(ret) \/  p11_dict_clear(ret)  \/ p11_array_free(ret)) · (_)^* )  @*/


/*@ p11_asn1_read(a, b, c): 
    Future (!(ret=0), (_)^* · (free(ret) \/  p11_dict_clear(ret) ) · (_)^* )  @*/

/*@ p11_dict_clear(handler): 
    Post (TRUE, p11_dict_clear(handler))  @*/

/*@ p11_dict_free(handler): 
    Post (TRUE, free(handler))  @*/
 
/*@ memdup(path): 
    Future (!(ret=0), (!free(ret))^* · (free(ret)) · (_)^* )  @*/
 
/*@ p11_index_free(handler): 
    Post (TRUE, CONSUME(handler))   @*/

/*@ insert_attribute(a, b, c): 
    Post (TRUE, CONSUME(b))   @*/

/*@ p11_asn1_cache_free(handler): 
    Post (TRUE, CONSUME(handler))   @*/
 
/*@ p11_kit_pin_new_for_buffer(a, b, c): 
    Post (TRUE, CONSUME(a))  @*/


/*@ hex_encode(data, n_data):
    Future (TRUE, (_)^*)@*/

/*@ hex_encode(data, n_data):
    Future (TRUE, (_)^*)@*/

/*@ modules_dup(modules):
    Future (TRUE, (_)^*)@*/

 /*@ _p11_conf_parse_file(filename, sb, flags):
    Future (TRUE, (_)^*)@*/


 /*@ managed_steal_sessions_inlock(sessions, matching_slot_id, slot_id, count):
    Future (TRUE, (_)^*)@*/

/*@ p11_log_subclass(lower, destroyer):
    Future (TRUE, (_)^*)@*/

/*@ managed_create_inlock(mod):
    Future (TRUE, (_)^*)@*/

/*@ p11_kit_module_get_name(module):
    Future (TRUE, (_)^*)@*/



/*@ p11_path_base(path):
    Future (TRUE, (_)^*)@*/


/*@ p11_enumerate_comment(ex, first):
    Future (TRUE, (_)^*)@*/

/*@ p11_attrs_find_valid(attrs, type):
    Future (TRUE, (_)^*)@*/
    
 







