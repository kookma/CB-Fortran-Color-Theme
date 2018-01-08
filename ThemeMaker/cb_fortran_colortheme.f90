program main
! Color Theme for Fortran Language in Code::Blocks
!
! This fortran code reads a text file containing the name and color specification
! of each syntax element of Fortran language and produces a theme.conf file to be
! imported into Code::Blocks configuration file (default.conf     )
! For more information take a look at
! https://github.com/kookma/CB-Fortran-Color-Theme
!
! Friday 2018-01-05


    implicit none

! Declaration types and variables

    type style
        !Define the style for every syntax element
        character(len=:), allocatable   :: name      !Style name, e.g number, identifier, comment, ...
        character(len=:), allocatable   :: stnum     !Style number in C::B conf file
        integer                         :: fcolor(3) !Foreground color
        integer                         :: bcolor(3) !Background color
    end type style

    integer, parameter :: number_style = 15 !number of element
    type(style) ::elstyles(number_style)

    character(len=80)            :: xml_file_output
    character(len=80)            :: colorspec_Input_File
    character(len=80)            :: line
    character(len=30)            :: theme_name
    integer                      :: fout, fin    !File identifier
    integer                      :: i, ios


    !The file to read color spec
    write(*,fmt='(a)') 'Enter the csv filename for color specification'
    write(*,fmt='(a)', advance='no') '[for example: CBF_Colorsp.csv]: '
    read*, colorspec_Input_File

    !The file to write the C::B theme e.g CBF_theme.conf
    write(*,fmt='(a)') 'The name for C::B theme conf file (without extension): '
    write(*,fmt='(a)', advance='no') '[for example: Dark_gray]: '
    read*, xml_file_output
    xml_file_output = trim(xml_file_output) // '.conf'



    !The name of your theme
    write(*,fmt='(a)') 'Enter a name for theme:'
    write(*,fmt='(a)', advance='no') '[for example: Dark_gray]: '
    read*, theme_name



    ! Open the conf file for output
    call open_file(fin, colorspec_Input_File, file_status='old')
    read(fin, '(a)') line !read the header

    ! Read color spec from input file
    do i=1, number_style
        read(fin, fmt='(a)', iostat=ios) line
        if ( is_iostat_end(ios) ) then
            print*, 'end of file'
            exit
        end if
        call tokenize(line, elstyles(i))
        elstyles(i)%stnum= num2str(i-1)
    end do


    !Open file to write the C::B theme configuration
    call open_file( fout, xml_file_output, file_status='replace')

    ! Produce the config file. This is an xml file with color specification
    ! for each syntax elemnt e.g comment, number, keyword, ...
    write( fout, '(a)')      '<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>'
    write( fout, '(a)')      '<CodeBlocksConfig version="1">'
    write( fout, '(4x, a)')  '<editor>'
    write( fout, '(8x, a)')  '<colour_sets>'
    write( fout, '(12x,a)')  '<' // lcase(trim(theme_name)) //'>'
    write( fout, '(16x,a)')  '<NAME>'
    write( fout, '(20x,a)')  '<str>'
    write( fout, '(24x,a)')  '<![CDATA[' //trim(theme_name) // ']]>'
    write( fout, '(20x,a)')  '</str>'
    write( fout, '(16x,a)')  '</NAME>'
    write( fout, '(16x,a)')  '<fortran>'

    do i=1, number_style
        write( fout, '(20x,a)' )  '<style' // elstyles(i)%stnum // '>'

        write( fout, '(24x,a)' )  '<NAME>'
        write( fout, '(28x,a)' )  '<str>'
        write( fout, '(32x,a)' )  '<![CDATA[' // trim(elstyles(i)%name) // ']]>'
        write( fout, '(28x,a)' )  '</str>'
        write( fout, '(24x,a)' )  '</NAME>'

        write( fout, '(24x,a)' )  '<FORE>'
        write( fout, '(28x,a)' )  '<colour r="' // num2str(elstyles(i)%fcolor(1)) //  &
            '" g="' // num2str(elstyles(i)%fcolor(2)) // &
            '" b="' // num2str(elstyles(i)%fcolor(3)) // '" />'
        write( fout, '(24x,a)' )  '</FORE>'

        write( fout, '(24x,a)' )  '<BACK>'
        write( fout, '(28x,a)' )  '<colour r="' // (num2str(elstyles(i)%bcolor(1))) //  &
            '" g="' // (num2str(elstyles(i)%bcolor(2))) // &
            '" b="' // (num2str(elstyles(i)%bcolor(3))) // '" />'
        write( fout, '(24x,a)' )  '</BACK>'

        write( fout, '(20x,a)' )  '</style' // elstyles(i)%stnum // '>'

    end do


    write( fout, '(20x,a)' )  '<NAME>'
    write( fout, '(24x,a)' )  '<str>'
    write( fout, '(28x,a)' )  '<![CDATA[Fortran]]>'
    write( fout, '(24x,a)' )  '</str>'
    write( fout, '(20x,a)' )  '</NAME>'
    write( fout, '(20x,a)' )  '<editor>'
    write( fout, '(24x,a)' )  '<keywords />'
    write( fout, '(20x,a)' )  '</editor>'


    write( fout, '(16x,a)' )  '</fortran>'


    write( fout, '(16x,a)' )  '<cc>'
    write( fout, '(20x,a)' )  '<editor>'
    write( fout, '(24x,a)' )  '<keywords>'
    write( fout, '(28x,a)' )  '<SET4>'
    write( fout, '(32x,a)' )  '<str>'
    write( fout, '(36x,a)' )  '<![CDATA[__cplusplus]]>'
    write( fout, '(32x,a)' )  '</str>'
    write( fout, '(28x,a)' )  '</SET4>'
    write( fout, '(24x,a)' )  '</keywords>'
    write( fout, '(20x,a)' )  '</editor>'
    write( fout, '(16x,a)' )  '</cc>'


    write( fout, '(12x,a)' )  '</' // lcase(trim(theme_name)) // '>'
    write( fout, '(8x,a)' )  '</colour_sets>'
    write( fout, '(4x,a)' )  '</editor>'
    write( fout, '(a)' )  '</CodeBlocksConfig>'

    ! Close the conf file
    close(fout)


contains
!-------------------------------------------------------------------------------
! The auxiliary procedure
!-------------------------------------------------------------------------------

    !..............................................................................
    subroutine tokenize(str, stl)
    !..............................................................................
        !tokenize, separate a string using "," delimiters
        !and store each statement in a style structure
        character(len=*), intent(in)    :: str
        type(style), intent(out)        :: stl
        !Local vars
        character, parameter          :: delimiter=','
        character(len=:), allocatable :: tmpstr
        integer ::  ind, i

        tmpstr=trim(str)

        !Extract the name
        ind = scan(tmpstr, delimiter)
        stl%name = tmpstr(1:ind-1)


        !Extract the foregroung rgb color
        do i=1,3
            tmpstr = tmpstr(ind+1:)
            ind = scan(tmpstr, delimiter)
            stl%fcolor(i) = str2num(tmpstr(1:ind-1))
        end do

        !Extract the background rgb color
        do i=1,2
            tmpstr = tmpstr(ind+1:)
            ind = scan(tmpstr, delimiter)
            stl%bcolor(i) = str2num(tmpstr(1:ind-1))
        end do
        tmpstr = tmpstr(ind+1:)
        stl%bcolor(3) = str2num(tmpstr)

    end subroutine tokenize


    !..............................................................................
    function str2num(strInput) result(numOut)
        !..............................................................................
        !convert a string to integer number
        !works for 3 digits integer
        character(len=*) , intent(in):: strInput
        integer :: numOut
        character(len=:), allocatable :: strfmt
        read(strInput, '(I3)') numOut

    end function str2num


    !..............................................................................
    function num2str(numInput) result(strOut)
    !..............................................................................
        ! convert an integer to string
        integer, intent(in)             :: numInput
        character(len=:), allocatable   :: strOut
        character(len=range(numInput)+2) ::strtmp  !Maximum number of digits, change if required

        write (strtmp, "(I0)") numInput
        strOut = trim(strtmp)
    end function num2str



    !..............................................................................
    function lcase(strInput)
    !..............................................................................
        ! Return the string (strInput) in lowercase
        character(len=*), intent(in) :: strInput
        character(len=len(strInput)):: lcase
        integer:: i
        integer:: n
        character(1):: chr

        do i=1, len(strInput)
            chr=strInput(i:i)
            n=ichar(chr)
            if (n >=65 .and. n <= 90) then
                lcase(i:i)=char(n+32)
            else
                lcase(i:i)=chr
            end if
        end do
    end function lcase



    subroutine open_file(file_unit, txtfileName, file_status)
        ! Create an output file, assign a file_unit
        ! for reading or writing

        integer, intent(out)            :: file_unit
        character(len=*), intent(in)    :: txtfileName
        character(len=*), intent(in)    :: file_status

        integer :: ierr

        ! Open the output file
        open ( Newunit = file_unit, file = txtfileName, status = file_status, iostat = ierr )

        if ( ierr /= 0 ) then
            print*, "cannot open file for output"
            stop
        end if
    end subroutine open_file


end program
