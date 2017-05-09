; ModuleID = 'greet_curried2.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.Fp_greetCurried_anon1 = type { void (%struct.Rec_greetCurried*, i8*)*, %struct.Rec_greetCurried* }
%struct.Rec_greetCurried = type { %struct.Rec_toplevel*, i8*, {}* }
%struct.Rec_toplevel = type { {}*, %struct.Fp_greetCurried_anon1* }
%struct.Rec_greetCurried_anon1 = type { %struct.Rec_greetCurried*, i8*, void (%struct.Rec_greetCurried*, i8*)* }

@.str = private unnamed_addr constant [5 x i8] c"%s, \00", align 1
@.str1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str2 = private unnamed_addr constant [6 x i8] c"Hello\00", align 1
@.str3 = private unnamed_addr constant [6 x i8] c"Heidi\00", align 1

; Function Attrs: nounwind uwtable
define %struct.Fp_greetCurried_anon1* @greetCurried(%struct.Rec_toplevel* %parent, i8* %greeting) #0 {
  %1 = alloca %struct.Rec_toplevel*, align 8
  %2 = alloca i8*, align 8
  %mine = alloca %struct.Rec_greetCurried*, align 8
  %fp = alloca %struct.Fp_greetCurried_anon1*, align 8
  store %struct.Rec_toplevel* %parent, %struct.Rec_toplevel** %1, align 8
  store i8* %greeting, i8** %2, align 8
  %3 = call noalias i8* @malloc(i64 24) #3
  %4 = bitcast i8* %3 to %struct.Rec_greetCurried*
  store %struct.Rec_greetCurried* %4, %struct.Rec_greetCurried** %mine, align 8
  %5 = load %struct.Rec_toplevel** %1, align 8
  %6 = load %struct.Rec_greetCurried** %mine, align 8
  %7 = getelementptr inbounds %struct.Rec_greetCurried* %6, i32 0, i32 0
  store %struct.Rec_toplevel* %5, %struct.Rec_toplevel** %7, align 8
  %8 = load i8** %2, align 8
  %9 = load %struct.Rec_greetCurried** %mine, align 8
  %10 = getelementptr inbounds %struct.Rec_greetCurried* %9, i32 0, i32 1
  store i8* %8, i8** %10, align 8
  %11 = call noalias i8* @malloc(i64 16) #3
  %12 = bitcast i8* %11 to %struct.Fp_greetCurried_anon1*
  store %struct.Fp_greetCurried_anon1* %12, %struct.Fp_greetCurried_anon1** %fp, align 8
  %13 = load %struct.Fp_greetCurried_anon1** %fp, align 8
  %14 = getelementptr inbounds %struct.Fp_greetCurried_anon1* %13, i32 0, i32 0
  store void (%struct.Rec_greetCurried*, i8*)* @greetCurried_anon1, void (%struct.Rec_greetCurried*, i8*)** %14, align 8
  %15 = load %struct.Rec_greetCurried** %mine, align 8
  %16 = load %struct.Fp_greetCurried_anon1** %fp, align 8
  %17 = getelementptr inbounds %struct.Fp_greetCurried_anon1* %16, i32 0, i32 1
  store %struct.Rec_greetCurried* %15, %struct.Rec_greetCurried** %17, align 8
  %18 = load %struct.Fp_greetCurried_anon1** %fp, align 8
  ret %struct.Fp_greetCurried_anon1* %18
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #1

; Function Attrs: nounwind uwtable
define void @greetCurried_anon1(%struct.Rec_greetCurried* %parent, i8* %name) #0 {
  %1 = alloca %struct.Rec_greetCurried*, align 8
  %2 = alloca i8*, align 8
  %mine = alloca %struct.Rec_greetCurried_anon1*, align 8
  store %struct.Rec_greetCurried* %parent, %struct.Rec_greetCurried** %1, align 8
  store i8* %name, i8** %2, align 8
  %3 = call noalias i8* @malloc(i64 24) #3
  %4 = bitcast i8* %3 to %struct.Rec_greetCurried_anon1*
  store %struct.Rec_greetCurried_anon1* %4, %struct.Rec_greetCurried_anon1** %mine, align 8
  %5 = load %struct.Rec_greetCurried** %1, align 8
  %6 = load %struct.Rec_greetCurried_anon1** %mine, align 8
  %7 = getelementptr inbounds %struct.Rec_greetCurried_anon1* %6, i32 0, i32 0
  store %struct.Rec_greetCurried* %5, %struct.Rec_greetCurried** %7, align 8
  %8 = load i8** %2, align 8
  %9 = load %struct.Rec_greetCurried_anon1** %mine, align 8
  %10 = getelementptr inbounds %struct.Rec_greetCurried_anon1* %9, i32 0, i32 1
  store i8* %8, i8** %10, align 8
  %11 = load %struct.Rec_greetCurried** %1, align 8
  %12 = getelementptr inbounds %struct.Rec_greetCurried* %11, i32 0, i32 1
  %13 = load i8** %12, align 8
  %14 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @.str, i32 0, i32 0), i8* %13)
  %15 = load %struct.Rec_greetCurried_anon1** %mine, align 8
  %16 = getelementptr inbounds %struct.Rec_greetCurried_anon1* %15, i32 0, i32 1
  %17 = load i8** %16, align 8
  %18 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str1, i32 0, i32 0), i8* %17)
  ret void
}

declare i32 @printf(i8*, ...) #2

; Function Attrs: nounwind uwtable
define i32 @main(i32 %argc, i8** %argv) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i8**, align 8
  %mine = alloca %struct.Rec_toplevel*, align 8
  store i32 %argc, i32* %1, align 4
  store i8** %argv, i8*** %2, align 8
  %3 = call noalias i8* @malloc(i64 16) #3
  %4 = bitcast i8* %3 to %struct.Rec_toplevel*
  store %struct.Rec_toplevel* %4, %struct.Rec_toplevel** %mine, align 8
  %5 = load %struct.Rec_toplevel** %mine, align 8
  %6 = getelementptr inbounds %struct.Rec_toplevel* %5, i32 0, i32 0
  %7 = bitcast {}** %6 to %struct.Fp_greetCurried_anon1* (%struct.Rec_toplevel*, i8*)**
  %8 = load %struct.Fp_greetCurried_anon1* (%struct.Rec_toplevel*, i8*)** %7, align 8
  %9 = load %struct.Rec_toplevel** %mine, align 8
  %10 = call %struct.Fp_greetCurried_anon1* %8(%struct.Rec_toplevel* %9, i8* getelementptr inbounds ([6 x i8]* @.str2, i32 0, i32 0))
  %11 = load %struct.Rec_toplevel** %mine, align 8
  %12 = getelementptr inbounds %struct.Rec_toplevel* %11, i32 0, i32 1
  store %struct.Fp_greetCurried_anon1* %10, %struct.Fp_greetCurried_anon1** %12, align 8
  %13 = load %struct.Rec_toplevel** %mine, align 8
  %14 = getelementptr inbounds %struct.Rec_toplevel* %13, i32 0, i32 1
  %15 = load %struct.Fp_greetCurried_anon1** %14, align 8
  %16 = getelementptr inbounds %struct.Fp_greetCurried_anon1* %15, i32 0, i32 0
  %17 = load void (%struct.Rec_greetCurried*, i8*)** %16, align 8
  %18 = load %struct.Rec_toplevel** %mine, align 8
  %19 = getelementptr inbounds %struct.Rec_toplevel* %18, i32 0, i32 1
  %20 = load %struct.Fp_greetCurried_anon1** %19, align 8
  %21 = getelementptr inbounds %struct.Fp_greetCurried_anon1* %20, i32 0, i32 1
  %22 = load %struct.Rec_greetCurried** %21, align 8
  call void %17(%struct.Rec_greetCurried* %22, i8* getelementptr inbounds ([6 x i8]* @.str3, i32 0, i32 0))
  ret i32 0
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"Ubuntu clang version 3.4-1ubuntu3 (tags/RELEASE_34/final) (based on LLVM 3.4)"}
