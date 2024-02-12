<img src="website/static/img/ProveNFix.png" alt="logo" width="15%" />

# ProveNFix 

ProveNFix is a program analysis and repair tool for temporal properties for C language. 
ProveNFix is written in [OCaml](https://ocaml.org/), and built on top of the Meta [Infer](http://fbinfer.com/) tool. 

[Infer](http://fbinfer.com/) ![build](https://github.com/facebook/infer/actions/workflows/install.yml/badge.svg) ![website](https://github.com/facebook/infer/actions/workflows/deploy.yml/badge.svg) is a static analysis tool for Java,
C++, Objective-C, and C. Infer is written in [OCaml](https://ocaml.org/). 
Read Infer's [Getting
Started](http://fbinfer.com/docs/getting-started) page for
details on how to install packaged versions of Infer. 



## Build ProveNFix
```
opam switch create 4.14.0+flambda
eval $(opam env)  
git clone https://github.com/songyahui/infer_TempFix.git
cd infer_TempFix
./compile
```

## Run ProveNFix on the projects in experiments 1 and 2
### Step 1: Put the default spec in the file ["spec.c"](./spec.c).
 
### Step 2: Run ProveNFix
```
../../git/infer_TempFix/infer/bin/tempFix
``` 

### Step 3: Check out the analysis/repair results in the file ["TempFix-out/detail.txt"](./TempFix-out/detail.txt)

Here lists the pre-required commands for the benchmark projects

### [Swoole](https://github.com/swoole/swoole-src)
```
phpize
./configure
```


###  [lxc](https://github.com/lxc/lxc), [WavPack](https://github.com/dbry/WavPack), [p11-kit](https://github.com/p11-glue/p11-kit), [flex](https://github.com/westes/flex), [x264](https://github.com/mirror/x264), [recutils-1.8](https://ftp.gnu.org/gnu/recutils/), [inetutils-1.9.4](https://ftp.gnu.org/gnu/inetutils/), [snort-2.9.13](https://www.snort.org/downloads#), [grub](https://github.com/rhboot/grub2)
```
./autogen.sh 
./configure
```

## Experiment 3: Automatically detect double free errors


### Step1: Checkout the ProveNFix branch and re-build
```
git checkout doubleFreeClose
./compile
```

### Step 2: Put the default spec (["spec_Temp_grub.c"](./spec_Temp_grub.c), ["spec_Temp_lxc.c"](./spec_Temp_lxc.c), ["spec_Temp_p11.c"](./spec_Temp_p11.c)) in the file ["spec.c"](./spec.c), and run ProveNFix on the projects Grub, lex, and p11-kit respectively. 
 
```
../../git/infer_TempFix/infer/bin/tempFix
``` 

### Step 3: Check out the analysis/repair results in the file ["TempFix-out/detail.txt"](./TempFix-out/detail.txt)




## Experiment 4: generating specs for the OpenSSL project

### Step1: Checkout the ProveNFix branch and re-build
```
git checkout Infer_OpenSSL
./compile
```

### Get the source code and unzip [OpenSSL](https://github.com/openssl/openssl/releases/tag/openssl-3.1.2)
```
cd openssl-3.1.2
./Configure --prefix=/usr/local/ssl --openssldir=/usr/local/ssl \
    '-Wl,-rpath,$(LIBRPATH)'
```

### Add the first two specs in the file ["spec.c"](./spec.c).
```
#define SW_CHANNEL_MIN_MEM (1024*64)

/*@ return(arg):
    Post (TRUE, return(arg))@*/

/*@ ERR_new():
    Post (TRUE, ERR_new())@*/
```

### Run the ProveNFix
```
../../git/infer_TempFix/infer/bin/tempFix
```

### Check out generated specs in the file ["spec.c"](./spec.c).

----
Thanks for trying out ProveNFix. In case there are any issues, please contact Yahui Song (yahui_s@nus.edu.s)


<!--

# Others 


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





###########################
编译fio报static declaration of ‘gettid’ follows non-static错误解决方法
https://bbs.huaweicloud.com/forum/thread-186993-1-1.html

openEuler安装lmbench报netconfig.h: No such file or directory解决方法
https://bbs.huaweicloud.com/forum/thread-143131-1-1.html


brew install libdvdnav 

Snort 3 
https://linuxhint.com/compile-install-snort-from-source-code-ubuntu/


Turn on the infer analyze_and_report: "analyze_and_report"

1. reason for not finding all the TP, shown in infer. 

API-Misuse Detection Driven by Fine-Grained API-Constraint
Knowledge Graph


-->