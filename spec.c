#define SW_CHANNEL_MIN_MEM (1024*64)

/*@ open(path):
    Future ((ret>=0), (!close(ret))^* · close(ret) · (_)^*)@*/

/*@ socket(domain, type, protocol):
    Future ((ret>=0), (!close(ret))^* · close(ret) · (_)^*)@*/

/*@ close(handler):
    Post (TRUE, close(handler))@*/

/*@ fopen(path):
    Future ((ret>0), (!fclose(ret))^* · fclose(ret) · (_)^*)@*/

/*@ fdopen(path, b):
    Future ((ret>0), ((!fclose(ret))^* · fclose(ret) · (_)^* \/ (!close(path))^* · close(path) · (_)^*))@*/

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
    Future (ret>=0, (!close(ret))^* · close(ret) · (_)^* )  @*/

