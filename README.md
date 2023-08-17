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




using the command: 
infer/bin/infer run --pulse-only -- clang -c ../../repair-benchmark/swoole-src/src/core/*


Done with the swoole benchmark, need to do the temporal bugs. 

Temporal bugs are: 
infer/bin/infer run --pulse-only -- clang -c my_benchmark/urv/2_returnChunkSize.c

infer/bin/infer run --pulse-only -- clang -c my_benchmark/3_Generalized_URV/FFmpeg-9ffa49496d1aae4cbbb387aac28a9e061a6ab0a6/adtsenc.c

infer/bin/infer run --pulse-only -- clang -c my_benchmark/3_Generalized_URV/mongo-c-driver-fc4eb63a0581dee88461059a9d61e45a6ed8c56a/mongoc-gridfs-file.c

infer/bin/infer run --pulse -- clang -c my_benchmark/1_peek.c 


Benchmarks:


../../infer_TempFix/infer/bin/infer run --pulse -- make --keep-going 

x264$ 

./configure --disable-asm
make -j20
../../infer_TempFix/infer/bin/infer run  -- make -j20
make clean
../../infer_TempFix/infer/bin/infer run  -- make -j20  


swoole.c:

phpize && \
./configure && \
make && make install

infer 0.15.0

VERSION=0.15.0; \
curl -sSL "https://github.com/facebook/infer/releases/download/v$VERSION/infer-linux64-v$VERSION.tar.xz" \
| sudo tar -C /opt -xJ && \
sudo ln -s "/opt/infer-linux64-v$VERSION/bin/infer" /usr/local/bin/infer


TODO: 
check all the true bugs and try to fix them. 
generate future conditions for functions. 




###########################
编译fio报static declaration of ‘gettid’ follows non-static错误解决方法
https://bbs.huaweicloud.com/forum/thread-186993-1-1.html

openEuler安装lmbench报netconfig.h: No such file or directory解决方法
https://bbs.huaweicloud.com/forum/thread-143131-1-1.html


brew install libdvdnav 

Snort 3 
https://linuxhint.com/compile-install-snort-from-source-code-ubuntu/


Turn on the infer analyze_and_report: "analyze_and_report"