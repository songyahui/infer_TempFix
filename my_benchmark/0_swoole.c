//https://github.com/swoole/swoole-src/commit/e12c7db38c9737234695d35d9#diff-4e9df5e965eb2b2d000a6d20319c8efe5e97293404eba46fac8ccf33b6e79ecc

#include <sys/stat.h>
#include <sys/resource.h>
#include <sys/ioctl.h>
#include <execinfo.h>

#define SW_ERROR_FILE_EMPTY "SWOOLE_ERROR_FILE_EMPTY"
#define SW_LOG_TRACE "SWOOLE_LOG_TRACE"
#define SW_MAX_FILE_CONTENT 67108864  // for swoole_file_get_contents
#define SW_LOG_WARNING "SWOOLE_LOG_WARNING"
#define SW_ERROR_FILE_TOO_LARGE "SWOOLE_ERROR_FILE_TOO_LARGE"
#define O_RDONLY "O_RDONLY"
#define errno 110
#define EINTR 1

typedef struct swString {
    char* str;
    int length;
   
} swString;


/*@ open(path, mode): 
    Post (ret<0, open) \/ (ret>=0, open)
    Future (ret<0, (_)^*)  \/ (ret>=0, (!close(ret))^* · close(ret) · (_)^* )  
@*/

/*@ close(handler): 
    Post (TRUE, close(handler))   
@*/

/* swoole_error_log: 
    Post (TRUE, swoole_error_log)   
*/

swString* swoole_file_get_contents(char *filename)
{
    size_t filesize = swoole_file_size(filename);
    if (filesize < 0)
    {
        return NULL;
    }
    else if (filesize == 0)
    {
        swoole_error_log(SW_LOG_TRACE, SW_ERROR_FILE_EMPTY, "file[%s] is empty.", filename);
        return NULL;
    }
    else if (filesize > SW_MAX_FILE_CONTENT)
    {
        swoole_error_log(SW_LOG_WARNING, SW_ERROR_FILE_TOO_LARGE, "file[%s] is too large.", filename);
        return NULL;
    }
    int handler = open(filename, O_RDONLY);
    if (handler < 0)
    {
        swWarn("open(%s) failed. Error: %s[%d]", filename, strerror(errno), errno);
        return NULL;
    }
    swString *content = swString_new(filesize);
    if (!content)
    {
        close(handler);
        return NULL;
    }

    int readn = 0;
    int n;
    while(readn < filesize)
    {
        n = pread(handler, content->str + readn, filesize - readn, readn);
        if (n < 0)
        {
            if (errno == EINTR)
            {
                continue;
            }
            else
            {
                swSysError("pread(%d, %ld, %d) failed.", handler, filesize - readn, readn);
                swString_free(content);
                close(handler);
                return NULL;
            }
        }
        readn += n;
    }
    close(handler);
    content->length = readn;
    return content;
}
