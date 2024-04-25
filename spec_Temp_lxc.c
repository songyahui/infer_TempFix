
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




