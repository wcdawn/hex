program hex
IMPLICIT NONE

integer :: inp
integer :: length
integer :: i
integer,dimension(:),allocatable :: pos

real(8) :: root
real(8) :: inp_real

logical :: prefix

character(1),dimension(:),allocatable :: hex_out


101 format(a) ! plain text descriptor
102 format(a,i3)

write(*,101) 'Enter integer to be converted'
read(*,*) inp
inp_real = real(inp,8)

root = log(inp_real)/log(16.0d0)
length = ceiling(root)
! do something about allocation of character
allocate(pos(length))
allocate(hex_out(length))

do i = length - 1,0,-1
	if (i .eq. 0) then
		pos(i) = inp
	else
		pos(i) = floor(inp_real / (16.0d0 ** real(i,8)))
		inp = inp - (pos(i) * (16 ** i))
		inp_real = real(inp,8)
	endif
	write(*,'(i2,x,e12.6)') pos(i), inp_real
enddo

do i = 0,length - 1
	select case (pos(i))
	case (0:9)
		write(hex_out(i),'(i1)') pos(i)
	case (10)
		hex_out(i) = 'A'
	case (11)
		hex_out(i) = 'B'
	case(12)
		hex_out(i) = 'C'
	case(13)
		hex_out(i) = 'D'
	case(14)
		hex_out(i) = 'E'
	case(15)
		hex_out(i) = 'F'
	endselect
enddo

prefix = .true.
if (prefix) then
	write(*,101,advance = 'no') '0x'
endif
do i = length-1,0,-1
	if (i .eq. 0) then
		write(*,101) hex_out(i)
	else
		write(*,101,advance = 'no') hex_out(i)
	endif
enddo




write(*,101)
write(*,101) 'INPUT ECHO'
write(*,102) 'inp - ', inp
write(*,102) 'length - ', length


end