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

/*@ fdopen(path, b):
    Future (((ret>0)/\(!(ret=stdout)/\!(ret=stdin))), ((!fclose(ret))^* · fclose(ret) · (_)^* \/ (!close(path))^* · close(path) · (_)^*)) \/ ((ret=0), (!_(ret))^*)@*/

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

/*@ x264_cli_get_csp(path):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ malloc(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*) \/ ((ret=0), (!_(ret))^*)@*/

/*@ free(handler):
    Post (TRUE, free(handler))@*/

