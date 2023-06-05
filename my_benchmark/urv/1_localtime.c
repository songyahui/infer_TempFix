#include <time.h>


//p = localtime(&t);
/*@ localtime(t): 
    Post (ret=0, 𝝐) \/ (!(ret=0), localtime(ret)) 
    Future  (ret=0, (!_(ret))^*)  @*/

void swLog_put(int level, char *cnt)
{
      const char *level_str;
    char date_str[SW_LOG_DATE_STRLEN];
    char log_str[SW_LOG_BUFFER_SIZE];
    int n;

    time_t t;
    struct tm *p;
    t = time(NULL);
    p = localtime(&t);
    p->tm_year;
    snprintf(date_str, SW_LOG_DATE_STRLEN, "%d-%02d-%02d %02d:%02d:%02d", p->tm_year + 1900, p->tm_mon + 1, p->tm_mday, p->tm_hour, p->tm_min, p->tm_sec);

}