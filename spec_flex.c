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

/*@ malloc(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*) \/ ((ret=0), (!_(ret))^*)@*/

/*@ realloc(path):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ calloc(path):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ regmatch_dup(a, b):
    Future ((ret=0), (!_(ret))^*) \/ (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)@*/

/*@ strcmp(a, b):
    Post (TRUE, strcmp(a))@*/

/*@ fputs(a, b):
    Post (TRUE, fputs(b))@*/

/*@ free(handler):
    Post (TRUE, free(handler))@*/

