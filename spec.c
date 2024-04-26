#define SW_CHANNEL_MIN_MEM (1024*64)

/*@ close(handler):
    Post (TRUE, close(handler))@*/

/*@ fopen(path):
    Future (((ret>0)/\(!(ret=stdout)/\!(ret=stdin))), (!fclose(ret))^* · fclose(ret) · (_)^*)@*/

/*@ fclose(handler):
    Post (TRUE, fclose(handler))@*/

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

/*@ inet_ntoa(t):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ get_name_action(t):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ call_chunkfun(t):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ strchr(a, b):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ gettermname():
    Future ((ret=0), (!_(ret))^*)@*/

/*@ put_string(a, b):
    Post (TRUE, put_string(b))@*/

/*@ strlen(a):
    Post (TRUE, strlen(a))@*/

/*@ strdup(a):
    Post (TRUE, strdup(a))@*/

/*@ malloc(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)@*/

/*@ free(handler):
    Post (TRUE, free(handler))@*/

/*@ ping_set_data(a, b, c, d):
    Future (!(b=0), (!free(b))^* · free(b) · (_)^*)@*/

/*@ xgethostname():
    Future (TRUE, (_)^*)@*/

/*@ format_find(a, b):
    Future (TRUE, (_)^*)@*/

