declare i32 @printf(i8*, ...)

@hellostr = private unnamed_addr constant [13 x i8] c"Hello world\0A\00"

define dso_local i32 @main(i32 %argc, i8** %argv) {
  %res = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @hellostr, i64 0, i64 0))
  ret i32 0
}
