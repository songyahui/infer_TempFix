
//----------------Grub

#define SW_CHANNEL_MIN_MEM (1024*64)

/* free(handler):
    Post (TRUE, free(handler))
    Future (TRUE, ((!free(handler))^* \/ (!free(handler))^* · malloc(handler) · (_)^*))@*/

/*@ yylex_destroy(handler):
    Post (TRUE, free(handler))
    Future (TRUE, ((!free(handler))^* \/ (!free(handler))^* · malloc(handler) · (_)^*))@*/


/*@ grub_free(handler):
    Post (TRUE, free(handler))
    Future (TRUE, ((!free(handler))^* \/ (!free(handler))^* · malloc(handler) · (_)^*))@*/

/*@ grub_xasprintf(arg):
    Post (TRUE, malloc(ret))@*/

/*@ yy_scan_string(line, ptr):
    Post (TRUE, malloc(line))@*/

/*@ malloc(path):
    Post (TRUE, malloc(ret))@*/

/*@ grub_malloc(path):
    Post (TRUE, malloc(ret))@*/