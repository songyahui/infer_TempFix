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
    Future (((ret>0)/\(!(ret=stdout)/\!(ret=stdin))), ((!fclose(ret))^* · fclose(ret) · (_)^* \/ (!close(path))^* · close(path) · (_)^*))@*/

/*@ endmntent(handler):
    Post (TRUE, fclose(handler))@*/

/*@ fflush(handler):
    Post (TRUE, fclose(handler))@*/

/*@ opendir(path):
    Future ((ret>0), (!closedir(ret))^* · closedir(ret) · (_)^*)@*/

/*@ closedir(handler):
    Post (TRUE, closedir(handler))@*/

/*@ decode_long_double(x, ep, mp):
    Future (TRUE, (_)^*)@*/

/*@ decode_double(x, ep, mp):
    Future (TRUE, (_)^*)@*/

/*@ rec_db_process_fex(db, rset, record, fex):
    Future (TRUE, (_)^*)@*/

/*@ rec_db_process_fex(db, rset, record, fex):
    Future (TRUE, (_)^*)@*/

/*@ rec_field_new(name, value):
    Future (TRUE, (_)^*)@*/

/*@ rec_extract_url(str):
    Future (TRUE, (_)^*)@*/

/*@ rec_extract_file(str):
    Future (TRUE, (_)^*)@*/

/*@ sexrealloc(ptr, size, yyscanner):
    Future (TRUE, (_)^*)@*/

/*@ malloc(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*) \/ ((ret=0), (!deref(ret))^*)@*/

/*@ realloc(path):
    Future ((ret=0), (!deref(ret))^*)@*/

/*@ memset(a, b):
    Post (TRUE, deref(a))@*/

/*@ strcpy(a, b):
    Post (TRUE, deref(a))@*/

/*@ memcpy(a, b):
    Post (TRUE, deref(a))@*/

/*@ rec_db_new():
    Future ((ret=0), (!deref(ret))^*)@*/

/*@ rec_sex_new(path):
    Future ((ret=0), (!deref(ret))^*)@*/

/*@ fex_elem(a, b):
    Future ((ret=0), (!deref(ret))^*)@*/

/*@ rec_parser_new(a, b):
    Future ((ret=0), (!deref(ret))^*)@*/

/*@ rec_buf_new(a, b):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*) \/ ((ret=0), (!deref(ret))^*)@*/

/*@ rec_parser_new_str(a, b):
    Future ((ret=0), (!deref(ret))^*)@*/

/*@ rec_mset_get_at(a, b):
    Future ((ret=0), (!deref(ret))^*)@*/

/*@ call_chunkfun(a, b):
    Future ((ret=0), (!deref(ret))^*)@*/

/*@ rec_db_get_rset_by_type(a, b):
    Future ((ret=0), (!deref(ret))^*)@*/

/*@ rec_parser_error(a):
    Post (TRUE, deref(a))@*/

/*@ rec_parser_eof(a):
    Post (TRUE, deref(a))@*/

/*@ rec_record_source(a):
    Post (TRUE, deref(a))@*/

/*@ rec_rset_num_records(a):
    Post (TRUE, deref(a))@*/

/*@ rec_buf_putc(a, b):
    Post (TRUE, deref(b))@*/

/*@ rec_fex_elem_field_name(a):
    Post (TRUE, deref(a))@*/

/*@ rec_sex_compile(a, b):
    Post (TRUE, deref(a))@*/

/*@ rec_db_query(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)@*/

/*@ sex_create_buffer(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)@*/

/*@ rec_buf_new(a, b):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)@*/

/*@ sexalloc(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)@*/

/*@ mmalloca(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)@*/

/*@ free(handler):
    Post (TRUE, free(handler))@*/

/*@ rec_rset_destroy(handler):
    Post (TRUE, free(handler))@*/

/*@ rec_field_destroy(handler):
    Post (TRUE, free(handler))@*/

/*@ rec_record_destroy(handler):
    Post (TRUE, free(handler))@*/

/*@ rec_fex_destroy(handler):
    Post (TRUE, free(handler))@*/

/*@ rec_field_new(a, b):
    Post (TRUE, CONSUME(a))@*/

/*@ rec_field_set_name(a, b):
    Post (TRUE, CONSUME(b))@*/

/*@ rec_field_destroy(a, b):
    Post (TRUE, CONSUME(deletion_mask))@*/

/*@ rec_rset_type(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)@*/

/*@ rec_mset_add_sorted(a, b, c):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)@*/

/*@ set_tz(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)@*/

