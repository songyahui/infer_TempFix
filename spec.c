
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

/*@ malloc(path):
    Post (TRUE, malloc(ret))@*/

/*@ grub_malloc(path):
    Post (TRUE, malloc(ret))@*/