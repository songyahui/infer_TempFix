#define SW_CHANNEL_MIN_MEM (1024*64)

/*@ open(path):
    Future ((ret>0), (!close(ret))^* · close(ret) · (_)^*)@*/

/*@ socket(domain, type, protocol):
    Future ((ret>0), (!close(ret))^* · close(ret) · (_)^*)@*/

/*@ swSocket_create(domain, type, protocol):
    Future ((ret>0), (!close(ret))^* · close(ret) · (_)^*)@*/

/*@ close(handler):
    Post (TRUE, close(handler))@*/

/*@ fopen(path):
    Future ((ret>0), (!fclose(ret))^* · fclose(ret) · (_)^*)@*/


/*@ fclose(handler):
    Post (TRUE, fclose(handler))@*/


/*@ fdopen(path, b):
    Future ((ret>0), ((!fclose(ret))^* · fclose(ret) · (_)^* \/ (!close(path))^* · close(path) · (_)^*))@*/


/*@ endmntent(handler):
    Post (TRUE, fclose(handler))@*/

/*@ fflush(handler):
    Post (TRUE, fclose(handler))@*/

/*@ opendir(path):
    Future ((ret>0), (!closedir(ret))^* · closedir(ret) · (_)^*)@*/

/*@ closedir(handler):
    Post (TRUE, closedir(handler))@*/

/*@ malloc(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*) \/ ((ret=0), (!_(ret))^*)@*/

/*@ emalloc(path):
    Post (!(ret=0), 𝝐)@*/

/*@ __builtin_constant_p(path):
    Post (!(ret=0), 𝝐)@*/

/*@ free(handler):
    Post (TRUE, free(handler))@*/

/*@ swMalloc_alloc(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*) \/ ((ret=0), (!_(ret))^*)@*/

/*@ swReactorSelect_add(a, b):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ localtime(t):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ swReactor_get(t):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ calloc(path):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ swoole_get_property(a, b):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ swServer_connection_verify(a, b):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ swServer_connection_get(a, b):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ swServer_get_worker(a, b):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ swWorker_free(handler):
    Post (TRUE, free(handler))@*/

/*@ swoole_set_object(server_object, serv):
    Post (TRUE, COMSUME(serv))@*/

