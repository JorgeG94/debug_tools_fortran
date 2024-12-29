program testing
use debugging_tools
use pic_output
use mpi_f08
use pic_comm
implicit none
double precision :: a_double = 2.0
type(Logger) :: logging
integer(int32) :: size, mpi_rank
type(comm) :: world_comm

call MPI_Init()

world_comm = world()




if (world_comm%leader()) then

size = world_comm%size()
mpi_rank = world_comm%rank()
call print_message("size = " + to_string(size) + " rank = " + to_string(mpi_rank))
call logging%set_verbosity("MINIMAL")


call logging%verbose("VERBOSE " // to_string(a_double) // " here")
call logging%debug("DEBUG")
call logging%standard("standard")
call logging%info("info")
call logging%minimal("minimal")


call print_message("hello " + " another string " + "AAA")

end if
call MPI_Finalize()

end program testing
