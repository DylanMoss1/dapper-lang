; ModuleID = 'module'
source_filename = "module"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"

define i32 @add(i32 %0, i32 %1) {
entry:
  %y = alloca i32, align 4
  %x = alloca i32, align 4
  store i32 %0, ptr %x, align 4
  store i32 %1, ptr %y, align 4
  %x1 = load i32, ptr %x, align 4
  %y2 = load i32, ptr %y, align 4
  %tmpadd = add i32 %x1, %y2
  ret i32 %tmpadd
}

define i32 @main() {
entry:
  %add_call = call i32 @add(i32 10, i32 32)
  ret i32 %add_call
}
