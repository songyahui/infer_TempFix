


/*@ returnChunkSize(t): 
    Post (ret>=0, 𝝐) \/ ((ret<0), returnChunkSize(ret)) 
    Future  (ret<0, (!_(ret))^*)  @*/


/*@ memcpy(t1, t2, t3): 
    Post (TRUE, memcpy(t3)) 
 @*/



int main() {
    int destBuf = 0;
    int temp = returnChunkSize((destBuf)-1); 
    // Fix: here needs a check 
    memcpy(destBuf, srcBuf, temp);

}


/*@ pthread_mutex_lock(t): 
    Post (ret>=0, 𝝐) \/ ((ret<0), pthread_mutex_lock(ret)) 
    Future  (ret<0, (!_(ret))^*)  @*/


/*@ pthread_mutex_unlock(t1): 
    Post (TRUE, pthread_mutex_unlock(t1)) 
 @*/


int f(pthread_mutex_t *mutex) {

    int result = pthread_mutex_lock(mutex);
    //if (result != 0 )
    // return result;

    pthread_mutex_unlock(result);


/* access shared resource */


    return 0; 
}


/*@ openFileToWrite(t): 
    Post (ret>=0, 𝝐) \/ ((ret<0), openFileToWrite(ret)) 
    Future  (ret<0, (!_(ret))^*)  @*/



/*@ pthread_mutex_unlock_write_to_file(t1): 
    Post (TRUE, pthread_mutex_unlock(t1)) 
    Future  (ret<0, (!_(ret))^*)
 @*/

 /*@ closeFile(t1): 
    Post (TRUE, closeFile(t1)) 
 @*/


int outputStringToFile(char *output, char *filename) {

    int fd = openFileToWrite(filename);
    int file = pthread_mutex_unlock_write_to_file (fd);
    int temp = closeFile(file);
    return temp;
}