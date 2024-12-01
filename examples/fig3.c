/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

struct st{
    int flag;
    int f;
};

int * foo ( struct st *p )
{ int * q ;
  if (p->flag ) q = malloc (1) ;
  else q = p->f ;
  return q ;
}

int main () {
  struct st p ; 
  int* q ;
  p.f = malloc (1) ;
  q = foo ( &p ) ;
  if (p.flag) free(q);
  free ( p.f ) ;} // double - free

// infer/bin/infer run -- clang -c examples/fig3.c
// check infer_TempFix/TempFix-out/detail.txt 