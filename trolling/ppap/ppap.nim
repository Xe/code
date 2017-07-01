type
  🖋 = object
  🍎 = object
  🍍 = object
  🍎🖋 = object
  🍍🖋 = object

proc `$`(p: 🖋): string = "🖋 "
proc `$`(a: 🍎): string = "🍎 "
proc `$`(p: 🍍): string = "🍍 "
proc `$`(ap: 🍎🖋): string = "🍎🖋 "
proc `$`(pp: 🍍🖋): string = "🍍🖋 "

echo 🖋(), 🍎(), " -> ", 🍎🖋()
echo 🖋(), 🍍(), " -> ", 🍍🖋()
echo 🍎🖋(), 🍍🖋()
echo 🖋(), 🍍(), 🍎(), 🖋()
