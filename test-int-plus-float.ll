; ModuleID = 'MatchaScript'

@fmt = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt, i32 0, i32 0), double add (double 3.050000e+00, i32 1))
  ret i32 0
}
