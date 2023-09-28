
#define SW_CHANNEL_MIN_MEM (1024*64)

/*@ open(path):
    Future (((ret>0)/\(!(ret=stdout)/\!(ret=stdin))), (!close(ret))^* · close(ret) · (_)^*)@*/

/*@ socket(domain, type, protocol):
    Future ((ret>0), (!close(ret))^* · close(ret) · (_)^*)@*/

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

/*@ grub_efiemu_mm_obtain_request (handle):
    Future ((ret=0), (!_(ret))^*)@*/


/*@ getSessionPlugins ():
    Future ((ret=0), (!_(ret))^*)@*/


/*@ sfPolicyUserDataGet (a, b):
    Future ((ret=0), (!_(ret))^*)@*/

    

    
/*@ strtoul(str, endptr, base):
    Future ((endptr=0), (!_(endptr))^*)@*/

/*@ failure_start(file, funp, line):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ grub_procfs_rewind(data):
    Future ((data=0), (!_(data))^*)@*/

/*@ grub_util_fd_opendir(name):
    Future ((ret=0), (!_(ret))^*)@*/


/*@ malloc(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*) \/ ((ret=0), (!_(ret))^*)@*/






/*@ free(handler): 
    Post (TRUE, free(handler))   @*/

/*@ xz_dec_reset(handler): 
    Post (TRUE, free(handler))   @*/



/*@ realloc(a, b): 
    Future ((ret=0), (!_(ret))^*)@*/

/*@ memset(a, b): 
    Post (TRUE, memset(a))  @*/

/*@ strcpy(a, b): 
    Post (TRUE, strcpy(a))  @*/

/*@ memcpy(a, b): 
    Post (TRUE, memcpy(a))  @*/

/*@ grub_util_fd_open(os_dev, flags):
    Future (TRUE, (_)^*)@*/

/*@ find_root_devices_from_libzfs(dir):
    Future (TRUE, (_)^*)@*/


/*@ grub_zalloc(size):
    Future (TRUE, (_)^*)@*/

/*@ find_term_state(term):
    Future (TRUE, (_)^*)@*/

/*@ link_layer_find_entry(proto, card):
    Future (TRUE, (_)^*)@*/


/*@ failure_append_vtext(failure, fmt, ap):
    Post (TRUE, deref(failure))  @*/


/*@ fprintf(a, fmt, ap):
    Post (TRUE, deref(a))  @*/

/*@ fdopen(a, b):
    Post (TRUE, CONSUME(a))@*/

/*@ WavpackCloseFile(a): 
    Post (TRUE, CONSUME(a))   @*/

/*@ WavpackOpenFileInputEx64(a, b, c, d, e, f): 
    Post (TRUE, CONSUME(c))   @*/
    
/*@ init_words(a): 
    Post (TRUE, CONSUME(a))   @*/


/*@ grub_util_fd_readdir(d):
    Post (TRUE, grub_util_fd_readdir(d))  @*/


/*@ grub_ohci_td_phys2virt(o, x):
    Future (TRUE, (_)^*)@*/


/*@ grub_load_public_key(f):
    Future (TRUE, (_)^*)@*/


/*@ grub_canonicalize_file_name(path):
    Future (TRUE, (_)^*)@*/

/*@ grub_partition_get_name(partition):
    Future (TRUE, (_)^*)@*/


/*@ grub_iso9660_convert_string(us, len):
    Future (TRUE, (_)^*)@*/


