define internal i32 @func() {
  ret i32 0
}

define dso_local i32 @main() {
  %1 = call i32 @func()
  ret i32 %1
}

