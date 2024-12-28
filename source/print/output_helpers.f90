module pic_output_helpers
  use pic_types
  implicit none

contains

  subroutine print_asterisk_row(n)
    integer(kind=int64), intent(in) :: n
    integer(kind=int64) :: i
    do i = 1, n
      write(*, '(A)', advance='no') '*'
    end do
    write(*,*)  ! Move to the next line after printing the asterisks
  end subroutine print_asterisk_row


end module pic_output_helpers
