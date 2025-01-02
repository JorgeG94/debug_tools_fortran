module pic_comm
   use mpi_f08
   use pic_types
   implicit none

   type :: comm
      private
      type(MPI_Comm) :: m_comm = MPI_COMM_NULL
      integer(kind=int32) :: m_size = -1
      integer(kind=int32) :: m_rank = -1
   contains
      procedure :: init => comm_init
      procedure :: finalize => comm_finalize
      procedure :: leader => comm_leader
      procedure :: rank => comm_rank
      procedure :: size => comm_size
      procedure :: is_null => comm_is_null
      procedure :: get => comm_get_mpi_comm
      procedure :: barrier => comm_barrier
   end type comm

contains

   function world() result(new_comm)
      type(Comm) :: new_comm
      type(mpi_comm) :: world_comm
      call mpi_comm_dup(MPI_COMM_WORLD, world_comm)
      call new_comm%init(world_comm)
   end function world

   function comm_leader(this) result(is_leader)
      class(comm), intent(in) :: this
      logical :: is_leader
      is_leader = (this%m_rank == 0)
   end function comm_leader

   function comm_size(this) result(size)
      class(comm), intent(in) :: this
      integer(kind=int32) :: size
      size = this%m_size
   end function comm_size

   function comm_rank(this) result(rank)
      class(comm), intent(in) :: this
      integer(kind=int32) :: rank
      rank = this%m_rank
   end function comm_rank

   function comm_is_null(this) result(is_null)
      class(comm), intent(in) :: this
      logical :: is_null
      is_null = (this%m_comm == MPI_COMM_NULL)
   end function comm_is_null

   function comm_get_mpi_comm(this) result(mpicomm)
      class(comm), intent(in) :: this
      type(MPI_Comm) :: mpicomm
      if (this%is_null()) then
         write (*, *) "Error: MPI communicator is null"
         stop
      else
         mpicomm = this%m_comm
      end if
   end function comm_get_mpi_comm

   function comm_split(this) result(new_comm)
      class(comm), intent(in) :: this
      type(comm) :: new_comm
      type(MPI_Comm) :: new_mpi_comm
      call mpi_comm_split_type(this%m_comm, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, new_mpi_comm)
      call new_comm%init(new_mpi_comm)
   end function comm_split

   subroutine comm_barrier(this)
      class(comm), intent(in) :: this
      call mpi_barrier(this%m_comm)
   end subroutine comm_barrier

   subroutine comm_init(this, mpicomm)
      class(comm), intent(inout) :: this
      type(mpi_comm), intent(in) :: mpicomm
      call mpi_comm_size(mpicomm, this%m_size)
      call mpi_comm_rank(mpicomm, this%m_rank)
      this%m_comm = mpicomm
   end subroutine comm_init

   subroutine comm_finalize(this)
      class(comm), intent(inout) :: this
      if (this%m_comm /= MPI_COMM_NULL) then
         call mpi_comm_free(this%m_comm)
         this%m_comm = MPI_COMM_NULL
         this%m_size = -1
         this%m_rank = -1
      end if
   end subroutine comm_finalize

end module pic_comm
