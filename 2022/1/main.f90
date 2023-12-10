program day_1
   implicit none
   integer, dimension(300, 20) :: result
   integer, dimension(300) :: summed
   integer :: eof, i, out, size
   integer, external :: top3

   eof = 0
   result = 0
   i = 0

   ! read every block of ints from stdin
   do
      i = i + 1
      if (i > 300) then
         print *, 'Main read overflow: more than 300 entries read.'
         exit
      end if

      call read_block(result(i, :), eof)
      if (eof /= 0) exit
   end do

   ! sum results
   summed = sum(result, dim=2)

   ! print results
   print *, 'Part 1: ', maxval(summed)
   print *, 'Part 2: ', top3(summed)
end program day_1

! loops over entire input and returns the top 3 values from it
function top3(input) result(retval)
   implicit none
   integer, dimension(300), intent(in) :: input
   integer, dimension(3) :: topValues
   integer :: i, j, k, retval

   retval = 0
   topValues = 0

   do i = 1, 300
      do j = 1, 3
         if (input(i) > topValues(j)) then
            topValues(j) = input(i)
            exit
         end if
      end do
   end do

   retval = sum(topValues)
end function top3

! reads a single block of integers delimited by an empty line and returns
subroutine read_block(result, eof)
   implicit none
   integer, dimension(20), intent(out) :: result
   integer, intent(out) :: eof
   integer :: iostatus, n, parsedCalories
   character(len=10) :: line

   result = 0
   n = 0
   eof = 0
   parsedCalories = 0

   do
      read (*, '(A)', iostat=iostatus) line
      if (iostatus /= 0) then
         eof = iostatus
         exit
      else if (trim(line) == '') then
         exit
      end if

      read (line, '(I8)', iostat=iostatus) parsedCalories
      if (iostatus /= 0) then
         print *, 'Conversion error with iostat = ', iostatus
         exit
      end if

      n = n + 1
      if (n > 20) then
         print *, 'Read block overflow: more than 20 entries read.'
         exit
      end if

      result(n) = parsedCalories
   end do
end subroutine read_block
