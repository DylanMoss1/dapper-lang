; ModuleID = 'module'
source_filename = "module"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"

define i32 @main() {
entry:
  ret i32 42
}

define i32 @lambda_0(i32 %0) {
entry:
  %x = alloca i32, align 4
  store i32 %0, ptr %x, align 4
  %x1 = load i32, ptr %x, align 4
  ret i32 %x1
}

define i32 @lambda_1(i32 %0) {
entry:
  %x = alloca i32, align 4
  store i32 %0, ptr %x, align 4
  %x1 = load i32, ptr %x, align 4
  %x2 = load i32, ptr %x, align 4
  %tmpadd = add i32 %x1, %x2
  ret i32 %tmpadd
}

define i32 @lambda_2(i32 %0) {
entry:
  %x = alloca i32, align 4
  store i32 %0, ptr %x, align 4
  %x1 = load i32, ptr %x, align 4
  %x2 = load i32, ptr %x, align 4
  %tmpadd = add i32 %x1, %x2
  %x3 = load i32, ptr %x, align 4
  %tmpadd4 = add i32 %tmpadd, %x3
  ret i32 %tmpadd4
}
