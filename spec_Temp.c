#define SW_CHANNEL_MIN_MEM (1024*64)

/*@ close(handler):
    Post (TRUE, close(handler))
    Future (TRUE, ((!_(handler))^* \/ (!_(handler))^* · open(handler) · (_)^*))@*/

/*@ fclose(handler):
    Post (TRUE, fclose(handler))
    Future (TRUE, ((!_(handler))^* \/ (!_(handler))^* · fopen(handler) · (_)^*))@*/

/*@ fopen(path):
    Post (TRUE, fopen(ret))@*/

/*@ open(path):
    Post (TRUE, open(ret))@*/

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

//-----------LXC

#define SW_CHANNEL_MIN_MEM (1024*64)

/*@ free(handler):
    Post (TRUE, free(handler))
    Future (TRUE, ((!free(handler))^* \/ (!free(handler))^* · malloc(handler) · (_)^*))@*/

/*@ malloc(path):
    Post (TRUE, malloc(ret))@*/

/*@ calloc(path):
    Post (TRUE, malloc(ret))@*/

/*@ cgroup_to_absolute_path(path):
    Post (TRUE, malloc(ret))
    Future (TRUE, (_)^*)@*/

/*@ lxc_append_paths(path):
    Post (TRUE, malloc(ret))@*/

/*@ must_make_path(path):
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

/*@ close(handler):
    Post ((handler=-1), close(handler))
    Future (TRUE, ((!close(handler))^* \/ (!close(handler))^* · open(handler) · (_)^*))@*/

/*@ fclose(handler):
    Post ((handler=-1), close(handler))
    Future (TRUE, ((!fclose(handler))^* \/ (!fclose(handler))^* · fopen(handler) · (_)^*))@*/

/*@ closedir(handler):
    Post (TRUE, close(handler))
    Future (TRUE, ((!close(handler))^* \/ (!close(handler))^* · open(handler) · (_)^*))@*/

/*@ endmntent(handler):
    Post (TRUE, close(handler))
    Future (TRUE, ((!close(handler))^* \/ (!close(handler))^* · open(handler) · (_)^*))@*/

/*@ open(path):
    Post (TRUE, open(ret))@*/

/*@ socket(domain, type, protocol):
    Post (TRUE, open(ret))@*/

/*@ lxc_abstract_unix_connect(domain, type, protocol):
    Post (TRUE, open(ret))@*/

/*@ fopen(path):
    Post ((ret>=0), open(ret))@*/

/*@ opendir(path):
    Post (TRUE, open(ret))@*/

/*@ rpl_open(path):
    Post (TRUE, open(ret))@*/

/*@ fork():
    Post ((ret=ret), fork())@*/

/*@ read(path):
    Post (TRUE, read(path))@*/



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