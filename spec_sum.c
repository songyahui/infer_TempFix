#define SW_CHANNEL_MIN_MEM (1024*64)


// resource Leak

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
    Post (TRUE, CONSUME(path))
    Future (((ret>0)/\(!(ret=stdout)/\!(ret=stdin))), ((!fclose(ret))^* · fclose(ret) · (_)^* \/ (!close(path))^* · close(path) · (_)^*))@*/


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

/*@ assignnullsyh(): 
    Future  (ret=0, (!_(ret))^*)  @*/


/*@ localtime(t):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ malloc(path):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ realloc(a, b):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ calloc(path):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ free(handler):
    Post (TRUE, free(handler))@*/

/*@ strcmp(a, b):
    Post (TRUE, strcmp(a))@*/

/*@ fputs(a, b):
    Post (TRUE, fputs(b))@*/

/*@ memset(a, b):
    Post (TRUE, memset(a))@*/

/*@ memcpy(a, b):
    Post (TRUE, memcpy(a))@*/

/*@ strcpy(a, b): 
    Post (TRUE, strcpy(a))  @*/

/*@ strlen(a):
    Post (TRUE, strlen(a))@*/

/*@ sprintf(a, b, c):
    Post (TRUE, sprintf(a))@*/
