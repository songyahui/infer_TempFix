
#define SW_CHANNEL_MIN_MEM (1024*64)

// NPD 
/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ lxc_string_split(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), lxc_string_split(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ lxcapi_get_config_path(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), lxcapi_get_config_path(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ cgroup_to_absolute_path(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), cgroup_to_absolute_path(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ strlen(a): 
    Post (TRUE, strlen(a))  @*/

/*@ construct_path(a, b): 
    Post (TRUE, construct_path(a))  @*/

/*@ sprintf(a, b, c): 
    Post (TRUE, sprintf(a))  @*/

/*@ lxc_list_init(a): 
    Post (TRUE, lxc_list_init(a))  @*/


// Resourse leak, ignoring the future conditions for fclose and close
/*@ open(path): 
    Post (ret<0, 𝝐) \/ (ret>=0, open(ret))
    Future (ret>=0, (!close(ret))^* · close(ret) · (_)^* )  @*/

/*@ socket(domain, type, protocol): 
    Post (ret<0, 𝝐) \/ (ret>=0, socket(ret))
    Future (ret>=0, (!_(ret))^* · close(ret) · (_)^* )  @*/

/*@ close(handler): 
    Post (handler=-1, close(handler)) 
  @*/

/*@ fdopen(a,b): 
    Post (ret<0, 𝝐) \/ (ret>0, fdopen(ret))
    Future (ret>0, (!fclose(ret))^* · (fdclose(ret) \/ fclose(ret)) · (_)^* )  @*/
    
/*@ fdclose(handler): 
    Post (TRUE, fdclose(handler)) 
    Future  (TRUE, (!_(handler))^*)  @*/

/*@ fopen(path): 
    Post (TRUE, fdopen(ret))
    Future (ret>0, (!fclose(ret))^* · fclose(ret) · (_)^* )  @*/
    
/*@ fclose(handler): 
    Post (handler=-1, fclose(handler)) 
      @*/

/*@ lxc_abstract_unix_connect(a): 
    Post (TRUE, lxc_abstract_unix_connect(ret))  @*/

// Memory Leak

/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/


/*@ ls_recv_str(a, b): 
    Post (b=0, 𝝐) \/ (!(b=0), malloc(b))
    Future (!(b=0), (!free(b))^* · free(b) · (_)^* )  @*/


/*@ lxc_get_netdev_by_idx(a, b, c): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/



/*@ genlmsg_alloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* )  @*/

/*@ free(handler): 
    Post (TRUE, free(handler))   @*/

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

