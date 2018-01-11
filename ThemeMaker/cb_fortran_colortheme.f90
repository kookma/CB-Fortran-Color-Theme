program main
    ! Color Theme for Fortran Language in Code::Blocks
    !
    ! This fortran code reads a comma separated file (CSV) containing the name, color specification
    ! and font style of each syntax element of Fortran language and produces a theme.conf file to be
    ! imported into Code::Blocks configuration file (default.conf)
    ! For more information take a look at
    ! https://github.com/kookma/CB-Fortran-Color-Theme
    !
    ! Friday 2018-01-05
    !
    !
    ! Rev 0.2
    ! Process several files in one run
    ! Font style added
    ! Thursday Jan 11, 2018


    implicit none

    ! Declaration types and variables

    type style
        !Define the style for every syntax element
        character(len=:), allocatable   :: name      !Style name, e.g number, identifier, comment, ...
        character(len=2)                :: stnum     !Style number in C::B conf file
        character(len=3)                :: fcolor(3) !Foreground color
        character(len=3)                :: bcolor(3) !Background color
        character(len=1)                :: fontstyle(3)
    end type style


    ! Local variables
    character(len=80)   :: xml_file_output
    character(len=80)   :: colorspec_Input_File
    character(len=30)   :: theme_name
    character(len=3)    :: ans


    do

        !The file to read color spec
        write(*,fmt='(a)') 'The csv filename for color specification (without extension):'
        write(*,fmt='(a)', advance='no') '[for example: CBF_Colorsp]: '
        read*, colorspec_Input_File

        !The output file
        xml_file_output      = trim(colorspec_Input_File) // '_theme.conf'
        colorspec_Input_File = trim(colorspec_Input_File) // '.csv'


        !The name of your theme
        write(*,fmt='(a)') 'Enter a name for theme:'
        write(*,fmt='(a)', advance='no') '[for example: Dark_gray]: '
        read*, theme_name

        !Process csv file and produce the color theme (.conf) file
        call make_CBFortran_conf_file()

        ! Check if there is more file to be created
        write(*,fmt='(a)', advance='no') 'do you want to create more colour theme? (yes, y): '
        read*, ans
        if ( ( trim(lcase(ans)) == 'y' ) .or. (lcase(ans) == 'yes') ) then
            continue
        else
            stop
        end if

    end do



contains

!-------------------------------------------------------------------------------
    subroutine make_CBFortran_conf_file()
!-------------------------------------------------------------------------------
    ! This subroutine processes the CSV input file and produce a color theme for
    ! Fortran language for Code::Blocks IDE.

        integer, parameter :: number_style = 15 !number of element
        type(style) ::elstyles(number_style)

        integer                      :: fout, fin    !File identifier
        character(len=80)            :: line
        integer                      :: i, ios

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
            write(elstyles(i)%stnum, '(I0)') i-1
            !  elstyles(i)%stnum= num2str(i-1)
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
        write( fout, '(20x,a)' )  '<style' // trim(elstyles(i)%stnum) // '>'

       if ( len_trim( elstyles(i)%fcolor(1) )  /= 0  ) then !Dont write the empty color spec, CB will use default setting
            write( fout, '(24x,a)' )  '<FORE>'
            write( fout, '(28x,a)' )  '<colour r="' // elstyles(i)%fcolor(1) //  &
                '" g="' // elstyles(i)%fcolor(2) // &
                '" b="' // elstyles(i)%fcolor(3) // '" />'
            write( fout, '(24x,a)' )  '</FORE>'
       end if

       if ( len_trim(elstyles(i)%bcolor(1))  /= 0 ) then !Dont write the empty color spec, CB will use default setting
            write( fout, '(24x,a)' )  '<BACK>'
            write( fout, '(28x,a)' )  '<colour r="' // elstyles(i)%bcolor(1) //  &
                '" g="' // elstyles(i)%bcolor(2) // &
                '" b="' // elstyles(i)%bcolor(3) // '" />'
            write( fout, '(24x,a)' )  '</BACK>'
       end if
       ! Process font style
       if ( len_trim(elstyles(i)%fontstyle(1))  /= 0 ) then  !Bold
           write( fout, '(24x,a)' )  '<BOLD bool="1" />'
       end if

       if ( len_trim(elstyles(i)%fontstyle(2))  /= 0 ) then !Italics
           write( fout, '(24x,a)' )  '<ITALICS bool="1" />'
       end if

       if ( len_trim(elstyles(i)%fontstyle(3))  /= 0 ) then !Underline
           write( fout, '(24x,a)' )  '<UNDERLINED bool="1" />'
       end if

        write( fout, '(24x,a)' )  '<NAME>'
        write( fout, '(28x,a)' )  '<str>'
        write( fout, '(32x,a)' )  '<![CDATA[' // trim(elstyles(i)%name) // ']]>'
        write( fout, '(28x,a)' )  '</str>'
        write( fout, '(24x,a)' )  '</NAME>'


        write( fout, '(20x,a)' )  '</style' // trim(elstyles(i)%stnum) // '>'

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


        write( fout, '(12x,a)' )  '</' // lcase(trim(theme_name)) // '>'
        write( fout, '(8x,a)' )  '</colour_sets>'
        write( fout, '(4x,a)' )  '</editor>'
        write( fout, '(a)' )  '</CodeBlocksConfig>'

        ! Close the conf file
        close(fout)



    end subroutine



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
            stl%fcolor(i) = tmpstr(1:ind-1)
        end do

        !Extract the background rgb color
        do i=1,3
            tmpstr = tmpstr(ind+1:)
            ind = scan(tmpstr, delimiter)
            stl%bcolor(i) = tmpstr(1:ind-1)
        end do

        !Extract the fontstyle
        do i=1,2
            tmpstr = tmpstr(ind+1:)
            ind = scan(tmpstr, delimiter)
            stl%fontstyle(i) = tmpstr(1:ind-1)
        end do
        tmpstr = tmpstr(ind+1:)
        stl%fontstyle(3) = tmpstr

    end subroutine tokenize


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


!-------------------------------------------------------------------------------
    subroutine open_file(file_unit, txtfileName, file_status)
!-------------------------------------------------------------------------------
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
