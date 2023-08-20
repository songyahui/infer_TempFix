
#define SW_CHANNEL_MIN_MEM (1024*64)

// Memory leak 
/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/

/*@ sex_create_buffer(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/



/*@ rec_buf_new(a, b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/


/*@ sexalloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/


    
/*@ rec_mset_append(a): 
    Post (ret=0, 𝝐) \/ (!(a=0), malloc(a))
    Future (!(a=0), (!free(a))^* · free(a) · (_)^* )  @*/


/*@ sexensure_buffer_stack(a): 
    Post (a=0, 𝝐) \/ (!(a=0), malloc(a))
    Future (!(a=0), (!free(a))^* · free(a) · (_)^* )  @*/


/*@ mmalloca(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/

/*@ rec_parse_regexp(a, b, c): 
    Post (c=0, 𝝐) \/ (!(c=0), malloc(c))
    Future (!(c=0), (!free(c))^* · free(c) · (_)^* )  @*/



/*@ free(handler): 
    Post (TRUE, free(handler))   @*/

/*@ rec_fex_destroy(handler): 
    Post (TRUE, free(handler))   @*/


/*@ rec_record_destroy(handler): 
    Post (TRUE, free(handler))   @*/


/*@ rec_rset_destroy(handler): 
    Post (TRUE, free(handler))   @*/


/*@ rec_rset_type(path): 
Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/

/*@ rec_mset_add_sorted(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/
      
/*@ set_tz(path): 
Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/

#define SW_CHANNEL_MIN_MEM (1024*64)

// Resourse leak 
/*@ open(path): 
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



// NPD

/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ rec_fex_get(a, b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), rec_fex_get(ret))
    Future (ret=0, (!_(ret))^*) @*/


/*@ rec_db_new(): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (ret=0, (!_(ret))^*) @*/


/*@ rec_sex_new(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), rec_sex_new(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ fex_elem(a, b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), fex_elem(ret))
    Future (ret=0, (!_(ret))^*) @*/



/*@ realloc(a, b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), realloc(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ rec_parser_new(a, b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), rec_parser_new(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ rec_buf_new(a, b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), rec_buf_new(ret))
    Future (ret=0, (!_(ret))^*) @*/


/*@ rec_parser_new_str(a, b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), rec_parser_new_str(ret))
    Future (ret=0, (!_(ret))^*) @*/


/*@ rec_mset_get_at(a, b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), rec_mset_get_at(ret))
    Future (ret=0, (!_(ret))^*) @*/



/*@ call_chunkfun(a, b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), call_chunkfun(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ rec_db_get_rset_by_type(a, b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), rec_db_get_rset_by_type(ret))
    Future (ret=0, (!_(ret))^*) @*/



/*@ recutl_parse_db_from_file(a, b, c): 
    Post (TRUE, recutl_parse_db_from_file(c))  @*/

/*@ memset(a, b): 
    Post (TRUE, memset(a))  @*/

/*@ strcpy(a, b): 
    Post (TRUE, strcpy(a))  @*/

/*@ memcpy(a, b): 
    Post (TRUE, memcpy(a))  @*/


/*@ rec_parser_error(a): 
    Post (TRUE, rec_parser_error(a))  @*/


/*@ rec_parser_eof(a): 
    Post (TRUE, rec_parser_eof(a))  @*/

/*@ rec_record_source(a): 
    Post (TRUE, rec_record_source(a))  @*/

/*@ rec_rset_num_records(a): 
    Post (TRUE, rec_rset_num_records(a))  @*/


/*@ rec_buf_putc(a, b): 
    Post (TRUE, rec_buf_putc(b))  @*/

/*@ rec_fex_elem_field_name(a): 
    Post (TRUE, rec_fex_elem_field_name(a))  @*/

/*@ rec_buf_close(a): 
    Post (TRUE, rec_buf_close(a))  @*/

/*@ rec_sex_compile(a, b): 
    Post (TRUE, rec_buf_close(a))  @*/





// Memory leak 
/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/

/*@ sex_create_buffer(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/



/*@ rec_buf_new(a, b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/


/*@ sexalloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/


    
/*@ rec_mset_append(a): 
    Post (ret=0, 𝝐) \/ (!(a=0), malloc(a))
    Future (!(a=0), (!free(a))^* · free(a) · (_)^* )  @*/


/*@ sexensure_buffer_stack(a): 
    Post (a=0, 𝝐) \/ (!(a=0), malloc(a))
    Future (!(a=0), (!free(a))^* · free(a) · (_)^* )  @*/


/*@ mmalloca(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/

/*@ rec_parse_regexp(a, b, c): 
    Post (c=0, 𝝐) \/ (!(c=0), malloc(c))
    Future (!(c=0), (!free(c))^* · free(c) · (_)^* )  @*/



/*@ free(handler): 
    Post (TRUE, free(handler))   @*/

/*@ rec_fex_destroy(handler): 
    Post (TRUE, free(handler))   @*/


/*@ rec_record_destroy(handler): 
    Post (TRUE, free(handler))   @*/


/*@ rec_rset_destroy(handler): 
    Post (TRUE, free(handler))   @*/


/*@ rec_rset_type(path): 
Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/

/*@ rec_mset_add_sorted(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/
      
/*@ set_tz(path): 
Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/
