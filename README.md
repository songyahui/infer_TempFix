<img src="website/static/img/logo.png" alt="logo" width="15%" />

# Infer ![build](https://github.com/facebook/infer/actions/workflows/install.yml/badge.svg) ![website](https://github.com/facebook/infer/actions/workflows/deploy.yml/badge.svg)

[Infer](http://fbinfer.com/) is a static analysis tool for Java,
C++, Objective-C, and C. Infer is written in [OCaml](https://ocaml.org/).

## Installation

Read our [Getting
Started](http://fbinfer.com/docs/getting-started) page for
details on how to install packaged versions of Infer. To build Infer
from source, see [INSTALL.md](./INSTALL.md).

## Contributing

See [CONTRIBUTING.md](./CONTRIBUTING.md).

## License

Infer is MIT-licensed.

Note: Enabling Java support may require you to download and install 
components licensed under the GPL.



./build-infer.sh java
./build-infer.sh clang
infer run -- javac examples/Hello.java
infer/bin/infer run -- clang -c examples/Hello.c  
infer/bin/infer run -- clang -c /Users/yahuis/Desktop/git/LightFTP/Source/ftpserv.c
infer/bin/infer run -- clang -c /Users/yahuis/Desktop/git/pure-ftpd/src/main.c



infer/bin/infer run --pulse-only -- clang -c ../../repair-benchmark/my_benchmark/small_test1.c


git show -s --format=%H

git ls-files | xargs cat | wc -l
git ls-files | xargs wc -l

# biabduction 
# repair 

bidirectional bug localization. 
program synthesis using deductions. 



infer/bin/infer run -- clang -c ../../repair-benchmark/swoole-src/src/core/base.c


1. is that ok to extract the code and analyses it separately 
2. most likely trace to have the repair. 
3. related works derived by "Static automated program repair for heap properties"



AST: 
/Users/yahuis/Desktop/git/infer/infer/src/atd/clang_ast_t.ml




/*@  open(path): 
    Post (ret<0, 𝝐) \/ (ret>=0, open(ret))
    Future (ret>=0, (!close(ret))^* · close(ret) · (_)^* )  @*/


/*@  close(handler): 
    Post (TRUE, close(handler)) 
    Future  (TRUE, (!_(handler))^*)  @*/


//NPD
/*@  localtime(t): 
    Future  (ret=0, (!_(ret))^*)  @*/


/*@  malloc(path): 
    Future  (ret=0, (!_(ret))^*)  @*/


/*@  swReactor_get(t, p): 
    Post (ret=0, 𝝐) \/ (!(ret=0), deref(reactor)) 
    Future  (ret=0, (!_(ret))^*)  @*/


// Memory Leak
/*@ malloc(path): 
    Post (ret=0, 𝝐) \/ (!(ret=0), malloc(ret))
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^* ) \/ (ret=0, (!_(ret))^*) @*/


/*@ free(handler): 
    Post (TRUE, free(handler)) 
    Future  (TRUE, (!_(handler))^*)  @*/

using the command: 
infer/bin/infer run --pulse-only -- clang -c ../../repair-benchmark/swoole-src/src/core/*


Done with the swoole benchmark, need to do the temporal bugs. 

Temporal bugs are: 
infer/bin/infer run --pulse-only -- clang -c my_benchmark/urv/2_returnChunkSize.c

infer/bin/infer run --pulse-only -- clang -c my_benchmark/3_Generalized_URV/FFmpeg-9ffa49496d1aae4cbbb387aac28a9e061a6ab0a6/adtsenc.c

infer/bin/infer run --pulse-only -- clang -c my_benchmark/3_Generalized_URV/mongo-c-driver-fc4eb63a0581dee88461059a9d61e45a6ed8c56a/mongoc-gridfs-file.c

infer/bin/infer run --pulse-only -- clang -c my_benchmark/1_peek.c 



/*@ bson_iter_array(t1, t2, t3): 
    Post (ret>=0, 𝝐) \/ ((ret<0), bson_init_static(t3)) 
    Future  (ret<0, (!_(t3))^*)  @*/


/*@ bson_init_static(t1, buf, t3): 
    Post (TRUE, bson_init_static(buf)) @*/


/**
 * _mongoc_gridfs_file_new_from_bson:
 *
 * creates a gridfs file from a bson object
 *
 * This is only really useful for instantiating a gridfs file from a server
 * side object
 */
mongoc_gridfs_file_t *
_mongoc_gridfs_file_new_from_bson (void *gridfs, const void *data)
{

   //const uint8_t *;
   int iter, buf_len, buf;

   int ret = bson_iter_array (&iter, &buf_len, &buf);
         /* fix liwenhua if (!bson_init_static (&file->bson_aliases, buf, buf_len)) {
            GOTO (failure);
         }*/
   bson_init_static (1, buf, buf_len);
   return ret;
}
