
#define SW_CHANNEL_MIN_MEM (1024*64)

/*@ open(path):
    Future (((ret>0)/\(!(ret=stdout)/\!(ret=stdin))), (!close(ret))^* · close(ret) · (_)^*)@*/

/*@ socket(domain, type, protocol):
    Future ((ret>0), ((!socket(ret))^* · close(ret) · (_)^* \/ _ · close(ret) · socket(ret) · (_)^* · close(ret) · (_)^*))@*/

/*@ lxc_abstract_unix_connect(domain, type, protocol):
    Post (TRUE, socket(ret))@*/  

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



// NPD 
/*@ malloc(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)\/ ((ret=0), (!_(ret))^*)  @*/

/*@ get_timestamp(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*) \/ ((ret=0), (!_(ret))^*)@*/


/*@ lxc_string_split(path): 
    Future (ret=0, (!_(ret))^*) @*/

/*@ lxcapi_get_config_path(path): 
    Future (ret=0, (!_(ret))^*) @*/



/*@ strlen(a): 
    Post (TRUE, strlen(a))  @*/

/*@ construct_path(a, b): 
    Post (TRUE, construct_path(a))  @*/

/*@ sprintf(a, b, c): 
    Post (TRUE, sprintf(a))  @*/

/*@ lxc_list_init(a): 
    Post (TRUE, deref(a))  @*/


/*@ lxc_get_netdev_by_idx(a, b, c): 
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/



/*@ genlmsg_alloc(path): 
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/

/*@ free(handler): 
    Post (TRUE, free(handler))   @*/

/*@ genlmsg_free(handler):
    Post (TRUE, free(handler))@*/


/*@ lxc_container_free(handler): 
    Post (TRUE, free(handler))   @*/


/*@ lxc_free_handler(handler): 
    Post (TRUE, free(handler))   @*/

/*@ free_cgroup_settings(handler): 
    Post (TRUE, free(handler))   @*/

/*@ free_groupnames(handler): 
    Post (TRUE, free(handler))   @*/




/*@ lxc_list_add(a, b): 
    Post (TRUE, CONSUME(b))   @*/

/*@ bdev_put(handler): 
    Post (TRUE, CONSUME(handler))   @*/


/*@ lxc_list_add_tail(a, b): 
    Post (TRUE, CONSUME(b))   @*/




/*@ fdopen(a, b):
    Post (TRUE, CONSUME(a)) @*/






/*@ aufs_get_rootfs(rootfs_path, rootfslen):
    Future (TRUE, (_)^*)   @*/


/*@ get_btrfs_subvol_path(fd, dir_id, objid, name, name_len):
    Future (TRUE, (_)^*)   @*/

/*@ ovl_get_rootfs(rootfs_path, rootfslen):
    Future (TRUE, (_)^*)   @*/


/*@ cgroup_to_absolute_path(path): 
    Future (TRUE, (_)^*)   @*/

/*@ lxc_cpumask_to_cpulist(bitarr, nbits):
    Future (TRUE, (_)^*)   @*/


/*@ lxc_cpumask(buf, nbits):
    Future (TRUE, (_)^*)   @*/


/*@ read_file(fnam):
    Future (TRUE, (_)^*)   @*/


/*@ copy_global_config_value(p):
    Future (TRUE, (_)^*)   @*/

/*@ get_rundir():
    Future (TRUE, (_)^*)   @*/


/*@ ls_get_cgroup_item(c, item):
    Future (TRUE, (_)^*)   @*/


/*@ lxc_global_config_value(option_name):
    Future (TRUE, (_)^*)   @*/

/*@ ls_get_config_item(c, item, running):
    Future (TRUE, (_)^*)   @*/

/*@ lxc_append_paths(first, second):
    Future (TRUE, (_)^*)   @*/

/*@ lxc_va_arg_list_to_argv(ap, skip, do_strdup):
    Future (TRUE, (_)^*)   @*/


/*@ get_template_path(t):
    Future (TRUE, (_)^*)   @*/


