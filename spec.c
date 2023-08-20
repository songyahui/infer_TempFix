
#define SW_CHANNEL_MIN_MEM (1024*64)

// Resourse leak 
/*@ open(path): 
    Post (ret<0, 𝝐) \/ (ret>=0, open(ret))
    Future (ret>=0, (!close(ret))^* · close(ret) · (_)^* )  @*/

/*@ grub_util_fopen(a, b): 
    Post (ret<0, 𝝐) \/ (ret>0, open(ret))
    Future ((ret=out)/\(ret>0)/\!(ret=stdout), (!fclose(ret))^* · fclose(ret) · (_)^* )  @*/

/*@ close(handler): 
    Post (TRUE, close(handler)) 
    Future  (TRUE, (!_(handler))^*)  @*/

/*@ fdopen(path): 
    Post (ret<0, 𝝐) \/ (ret>0, fdopen(ret))
    Future (ret>0, (!fclose(ret))^* · fclose(ret) · (_)^* )  @*/

/*@ fopen(path): 
    Post (ret<0, 𝝐) \/ (ret>0, fopen(ret))
    Future (ret>0, (!fclose(ret))^* · fclose(ret) · (_)^* )  @*/

/*@ fclose(handler): 
    Post (TRUE, fclose(handler)) 
     @*/



// Memory bugs 
/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* ) \/ (ret=0, (!_(ret))^*) @*/

/*@ free(handler): 
    Post (TRUE, free(handler)) 
    Future  (TRUE, (!_(handler))^*)  @*/



// NPD

/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ yyalloc(a, b): 
    Post (ret=0, 𝝐) \/ (!(ret=0), yyalloc(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ grub_util_fopen(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), grub_util_fopen(ret))
    Future (ret=0, (!_(ret))^*) @*/

/*@ get_header_from_pointer(a, b, c): 
    Post (c=0, 𝝐) \/ (!(c=0), get_header_from_pointer(c))
    Future (c=0, (!_(c))^*) @*/


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


