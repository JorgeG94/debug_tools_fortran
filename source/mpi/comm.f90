module pic_comm
   use mpi_f08
   use pic_types
   implicit none

   type :: comm
      private
      type(MPI_Comm) :: m_comm = MPI_COMM_NULL
      integer(int32) :: m_size
      integer(int32) :: m_rank
   contains
      procedure :: init => comm_init
      procedure :: finalize => comm_finalize
      procedure :: leader => comm_leader
      procedure :: rank => comm_rank
      procedure :: size => comm_size
   end type comm


contains

  function World() result(new_comm)
    type(Comm) :: new_comm
    type(MPI_Comm) :: world_comm
    call MPI_Comm_dup(MPI_COMM_WORLD, world_comm)
    call new_comm%init(world_comm)
  end function World

   subroutine Comm_init(this, mpicomm)
      class(comm), intent(inout) :: this
      type(MPI_Comm), intent(in) :: mpicomm
      call MPI_COMM_SIZE(mpicomm, this%m_size)
      call MPI_COMM_RANK(mpicomm, this%m_rank)
      this%m_comm = mpicomm
   end subroutine Comm_init

   subroutine Comm_finalize(this)
      class(comm), intent(inout) :: this
      if (this%m_comm /= MPI_COMM_NULL) then
         call MPI_Comm_free(this%m_comm)
         this%m_comm = MPI_COMM_NULL
         this%m_size = -1
         this%m_rank = -1
      end if
   end subroutine Comm_finalize

   function comm_leader(this) result(is_leader)
      class(comm), intent(in) :: this
      logical :: is_leader
      is_leader = (this%m_rank == 0)
   end function comm_leader

   function comm_size(this) result(size)
      class(comm), intent(in) :: this
      integer(int32) :: size
      size = this%m_size
   end function comm_size

   function comm_rank(this) result(rank)
      class(comm), intent(in) :: this
      integer(int32) :: rank
      rank = this%m_rank
   end function comm_rank

end module pic_comm
