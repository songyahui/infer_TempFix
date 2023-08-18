
#define SW_CHANNEL_MIN_MEM (1024*64)

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




    