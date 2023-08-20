
#define SW_CHANNEL_MIN_MEM (1024*64)


// NPD

/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (ret=0, (!_(ret))^*) @*/


/*@ grub_util_make_temporary_file(): 
    Post (ret=0, 𝝐) \/ (!(ret=0), grub_util_make_temporary_file(ret))
    Future (ret=0, (!_(ret))^*) @*/


/*@ grub_device_open(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), grub_device_open(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ grub_efiemu_mm_obtain_request(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), grub_efiemu_mm_obtain_request(ret))
    Future (ret=0, (!_(ret))^*) @*/
 

/*@ grub_procfs_rewind(a): 
    Post (a=0, 𝝐) \/ (!(a=0), grub_procfs_rewind(ret))
    Future (a=0, (!_(a))^*) @*/
 

/*@ failure_start(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), failure_start(ret))
    Future (ret=0, (!_(ret))^*) @*/
 
/*@ grub_util_fd_opendir(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), grub_util_fd_opendir(ret))
    Future (ret=0, (!_(ret))^*) @*/
 

/*@ realloc(a, b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), realloc(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ memset(a, b): 
    Post (TRUE, memset(a))  @*/

/*@ strcpy(a, b): 
    Post (TRUE, strcpy(a))  @*/

/*@ memcpy(a, b): 
    Post (TRUE, memcpy(a))  @*/

/*@ grub_gpt_read(a): 
    Post (TRUE, grub_gpt_read(a))  @*/

/*@ failure_append_vtext(a, b, c): 
    Post (TRUE, failure_append_vtext(a))  @*/

/*@ grub_util_fd_readdir(a): 
    Post (TRUE, grub_util_fd_readdir(a))  @*/

/*@ fprintf(a, b, c): 
    Post (TRUE, fprintf(a))  @*/

/*@ grub_memcpy(a, b, c): 
    Post (TRUE, grub_memcpy(b))  @*/


