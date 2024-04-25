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

/*@ lxc_append_paths(path):
    Post (TRUE, malloc(ret))@*/

/*@ must_make_path(path):
    Post (TRUE, malloc(ret))@*/

/*@ lookup_extension(path):
    Post (TRUE, malloc(ret))@*/

/*@ hex_encode(path):
    Post (TRUE, malloc(ret))

/*@ symlink_for_subject_old_hash(path):
    Post (TRUE, malloc(ret))@*/

/*@ strndup(path):
    Post (TRUE, malloc(ret))@*/

/*@ malloc(path):
    Post (TRUE, malloc(ret))@*/

/*@ calloc(path):
    Post (TRUE, malloc(ret))@*/

/*@ p11_kit_space_strdup(path):
    Post (TRUE, malloc(ret))

/*@ format_uri(path):
    Post (TRUE, malloc(ret))@*/

/*@ make_unique_name(path):
    Post (TRUE, malloc(ret))@*/

/*@ lookup_extension(path):
    Post (TRUE, malloc(ret))@*/

/*@ hex_encode(path):
    Post (TRUE, malloc(ret))

/*@ symlink_for_subject_old_hash(path):
    Post (TRUE, malloc(ret))@*/

/*@ strndup(path):
    Post (TRUE, malloc(ret))@*/

/*@ yylex_destroy(handler):
    Post (TRUE, free(handler))
    Future (TRUE, ((!free(handler))^* \/ (!free(handler))^* · malloc(handler) · (_)^*))@*/

/*@ grub_free(handler):
    Post ((handler=0), 𝝐) \/ (!(handler=0), free(handler))
    Future (TRUE, ((!free(handler))^* \/ (!free(handler))^* · malloc(handler) · (_)^*))@*/

/*@ grub_strcpy(a, b):
    Post (TRUE, malloc(b))@*/

/*@ grub_xasprintf(arg):
    Post (TRUE, malloc(ret))@*/

/*@ grub_strdup(arg):
    Post (TRUE, malloc(ret))@*/

/*@ yy_scan_string(line, ptr):
    Post (TRUE, malloc(line))@*/

/*@ malloc(path):
    Post (TRUE, malloc(ret))@*/

/*@ grub_malloc(path):
    Post (TRUE, malloc(ret))@*/
