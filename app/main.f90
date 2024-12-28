program testing
use debugging_tools
use output_module
implicit none
double precision :: a_double = 2.0
type(Logger) :: logging


call logging%set_verbosity("MINIMAL")


call logging%verbose("VERBOSE " // to_string(a_double) // " here")
call logging%debug("DEBUG")
call logging%standard("standard")
call logging%info("info")
call logging%minimal("minimal")


call print_message("hello " + " another string " + "AAA")

end program testing
