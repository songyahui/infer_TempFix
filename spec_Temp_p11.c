#define SW_CHANNEL_MIN_MEM (1024*64)

/*@ free(handler):
    Post (TRUE, free(handler))
    Future (TRUE, ((!_(handler))^* \/ (!_(handler))^* · malloc(handler) · (_)^*))@*/

/*@ malloc(path):
    Post (TRUE, malloc(ret))@*/

/*@ calloc(path):
    Post (TRUE, malloc(ret))@*/

/*@ p11_kit_space_strdup(path):
    Post (TRUE, malloc(ret))
    Future (TRUE, (_)^*)@*/

/*@ format_uri(path):
    Post (TRUE, malloc(ret))@*/

/*@ make_unique_name(path):
    Post (TRUE, malloc(ret))@*/

/*@ lookup_extension(path):
    Post (TRUE, malloc(ret))@*/

/*@ hex_encode(path):
    Post (TRUE, malloc(ret))
    Future (TRUE, (_)^*)@*/

/*@ symlink_for_subject_old_hash(path):
    Post (TRUE, malloc(ret))@*/

/*@ strndup(path):
    Post (TRUE, malloc(ret))@*/
