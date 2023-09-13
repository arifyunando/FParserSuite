program json_test
    use, intrinsic :: iso_fortran_env, wp => real64
    use fpsuite_json
    implicit none
    character(len=*),parameter :: dir = '../files/'
    character(len=*),parameter :: filename1 = 'test1.json'        
    character(len=*),parameter :: filename2 = 'test2.json'        
    character(len=*),parameter :: filename4 = 'test4.json'        
    character(len=*),parameter :: filename5 = 'test5.json'        
    
    call json_initialize(verbose=.false.)

    !run the tests:
    call test_1()
    call test_2()
    call test_3()
    call test_4()
    call test_5()  ! 
    call test_6()  ! these are attempting to read invalid json files
    call test_7()  ! indent test
    call test_8()  ! read from string test
    !call memory_leak_test()    
    
contains

    subroutine test_8()
        implicit none
        type(json_value), pointer :: p
        character(len=*), parameter ::                                          &
            str  = '{ "label": "foo",' // new_line(' ') // ' "value": "bar" }', &
            str2 = '{ "label": "foo",' // new_line(' ') //                      &
                   '  "value": "bar",' // new_line(' ') //                      &
                   '  "empty_array": [],' // new_line(' ') //                   &
                   '  "empty_object": {}' // new_line(' ') //                   &
                   '}'
        character(len=*), parameter :: str_invalid = '{ "label": "foo",'//new_line(' ')//' "value : "bar" }'

        call json_initialize()

        write(*,'(A)') ''
        write(*,'(A)') '=+================================'
        write(*,'(A)') ' EXAMPLE 8 : read JSON from string'
        write(*,'(A)') '==+==============================='
        write(*,'(A)') ''
            
        write(*,'(A)') '**************'
        write(*,'(A)') ' Valid test 1:'
        write(*,'(A)') '**************'
        write(*,'(A)') ''
        call json_parse(str=str, p=p)   ! read it from str
        call json_print(p,OUTPUT_UNIT)  ! print to console    
        call json_destroy(p)            ! cleanup
        if (json_failed()) call print_error_message()
        write(*,'(A)') ''

        write(*,'(A)') '**************'
        write(*,'(A)') ' Valid test 2:'
        write(*,'(A)') '**************'
        write(*,'(A)') ''
        call json_parse(str=str2, p=p)  ! read it from str
        call json_print(p,OUTPUT_UNIT)  ! print to console    
        call json_destroy(p)            ! cleanup
        if (json_failed()) call print_error_message()
        write(*,'(A)') ''
        
        write(*,'(A)') '**************'
        write(*,'(A)') ' Invalid test:'
        write(*,'(A)') '**************'
        write(*,'(A)') ''
        call json_parse(str=str_invalid, p=p) ! read it from str
        call json_print(p,OUTPUT_UNIT)        ! print to console    
        call json_destroy(p)                  ! cleanup
        if (json_failed()) call print_error_message()    
        write(*,'(A)') ''
    end subroutine test_8
    

    subroutine test_7()
        implicit none
        type(json_value), pointer :: root, a, b, c, d, e, e1, e2

        call json_initialize()

        write(*,'(A)') ''
        write(*,'(A)') '================================='
        write(*,'(A)') '   EXAMPLE 7 : indent test'
        write(*,'(A)') '================================='
        write(*,'(A)') ''
        
        !-----------------------
        ! jsonlint indention is
        !-----------------------
        !{
        !    "a": {
        !        "ints": [
        !            1,
        !            2,
        !            3
        !        ],
        !        "chars": [
        !            "a",
        !            "b",
        !            "c"
        !        ]
        !    },
        !    "b": {
        !        "c": {
        !            "val1": 1066
        !        }
        !    },
        !    "d": {
        !        "val2": 1815
        !    },
        !    "array": [
        !        {
        !            "int1": 1
        !        },
        !        {
        !            "int1": 1,
        !            "int2": 2
        !        }
        !    ]
        !}

        !create a json structure:
        call json_create_object(root,'root')    
        call json_create_object(a,'a')
            call json_add(a,'ints', [1,2,3])
        call json_create_object(b,'b')
            call json_add(a,'chars', ['a','b','c'])
        call json_create_object(c,'c')
            call json_add(c,'val1', 1066)
        call json_create_object(d,'d')
            call json_add(d,'val2', 1815)
        call json_create_array(e,'array')   !objects in an array
        call json_create_object(e1,'')
            call json_add(e1,'int1', 1)   
        call json_create_object(e2,'')
            call json_add(e2,'int1', 1)   
            call json_add(e2,'int2', 2)   
        call json_add(e,e1)
        call json_add(e,e2)
        call json_add(root,a)
        call json_add(root,b)
        call json_add(b,c)
        call json_add(root,d)
        call json_add(root,e)
        
        nullify(a)  !don't need these anymore
        nullify(b)
        nullify(c)
        nullify(d)
        nullify(e)
        nullify(e1)
        nullify(e2)
        
        call json_print(root,6)  !print to the console
        call json_destroy(root)  !cleanup
        if (json_failed()) call print_error_message()
    end subroutine test_7


    subroutine test_6()
        implicit none
        type(json_file) :: json 
        integer :: i  
        character(len=*),dimension(2),parameter :: files = ['invalid.json ',    &
                                                            'invalid2.json']

        call json_initialize()

        write(*,'(A)') ''
        write(*,'(A)') '================================='
        write(*,'(A)') '   EXAMPLE 6 : invalid JSON files'
        write(*,'(A)') '================================='
        write(*,'(A)') ''

        do i=1,2
            write(*,'(A)') ''
            write(*,'(A)') 'load file: '//trim(files(i))
            write(*,'(A)') ''
            call json%load_file(filename = dir//trim(files(i)))
            if (json_failed()) then
                call print_error_message()
            end if

            call json%destroy()
        end do
    end subroutine test_6


    subroutine test_5()
        implicit none
        integer :: vv
        integer,dimension(:),allocatable :: vvv
        real(wp) :: d
        type(json_file) :: json   
        logical :: found

        call json_initialize()

        write(*,'(A)') ''
        write(*,'(A)') '================================='
        write(*,'(A)') '   EXAMPLE 5'
        write(*,'(A)') '================================='
        write(*,'(A)') ''

        write(*,'(A)') 'load file...'
        call json%load_file(filename = dir//filename5)

        if (json_failed()) then
            call print_error_message()
        else
            write(*,'(A)') 'print file...'
            call json%print_file()

            ! extract data from the parsed value:
            write(*,'(A)') ''
            write(*,'(A)') 'extract data...'
            write(*,'(A)') '--------------------------'
            
            call json%get('Correl.ID2', vv, found)
            if (found) write(*,'(A,I5)') 'vv = ',vv

            call json%get('Correl.ID1', vvv, found)
            if (found) write(*,'(A,*(I5,1X))') 'vvv= ',vvv

            call json%get('Prior[3].mode', d, found)
            if (found) write(*,'(A,E30.16)') 'd  = ',d

            write(*,'(A)') ''
        end if

        call json%destroy()
    end subroutine test_5


    subroutine memory_leak_test()
        implicit none
        integer :: i

        call json_initialize()

        write(*,'(A)') ''
        write(*,'(A)') '================================='
        write(*,'(A)') '   MEMORY LEAK TEST'
        write(*,'(A)') '================================='
        write(*,'(A)') ''
    
        i = 0
        do
            i=i+1
            write(*,'(A,1X,I5)') '***********************', i

            call test_4()
        end do

    end subroutine memory_leak_test


    subroutine test_4()
        implicit none
        type(json_value), pointer :: p,inp
        type(json_file)           :: json
        character(len=10)         :: istr
        character(len=:), allocatable :: string
        integer :: i

        call json_initialize()

        write(*,'(A)') ''
        write(*,'(A)') '================================='
        write(*,'(A)') '   EXAMPLE 4'
        write(*,'(A)') '================================='
        write(*,'(A)') ''

        write(*,'(A)') ''
        write(*,'(A)') 'creating structure'

        ! create the value and associate the pointer and
        ! add the file name as the name of the overall structure
        call json_create_object(p,dir//filename4)     
                                                  
        ! config structure:
        call json_create_object(inp,'INPUTS')

        ! add just integers:
        do i=1,100
            write(istr,fmt='(I10)') i
            istr = adjustl(istr)
            call json_add(inp, 'x'//trim(istr),i)
        end do
        call json_add(p, inp)
        nullify(inp)

        write(*,'(A)') ''
        write(*,'(A)') 'write to file'

        ! write the file:
        call json_print(p,trim(dir//filename4))

        write(*,'(A)') ''
        write(*,'(A)') 'write to string'
        write(*,'(A)') ''
        
        ! write it to a string, and print to console:
        call json_print_to_string(p, string)
        write(*,'(A)') string
        deallocate(string)
        
        call json_destroy(p)

        ! reread file for checking
        write(*,'(A)') ''
        write(*,'(A)') 'read file'

        call json%load_file(filename = dir//filename4)
        if (json_failed()) call print_error_message()

        write(*,'(A)') ''
        write(*,'(A)') 'cleanup'

        call json%destroy()
    end subroutine test_4


    subroutine test_3()
        implicit none

        integer :: ival
        character(len=:),allocatable :: cval
        real(wp) :: rval
        type(json_file) :: json    !the JSON structure read from the file:
        integer :: i
        character(len=10) :: str
        real(wp),dimension(:),allocatable :: rvec

        call json_initialize()

        write(*,'(A)') ''
        write(*,'(A)') '================================='
        write(*,'(A)') '   EXAMPLE 3'
        write(*,'(A)') '================================='
        write(*,'(A)') ''

        ! parse the json file:
        write(*,'(A)') ''
        write(*,'(A)') 'parsing file: '//dir//filename2

        call json%load_file(filename = dir//filename2)

        if (json_failed()) then    !if there was an error reading the file

            call print_error_message()

        else

            write(*,'(A)') ''
            write(*,'(A)') 'reading data from file...'
            !get scalars:
            write(*,'(A)') ''
            call json%get('inputs.integer_scalar', ival)
            if (json_failed()) then
                call print_error_message()
            else
                write(*,'(A,1X,I5)') 'inputs.integer_scalar = ',ival
            end if
            !get one element from a vector:
            write(*,'(A)') ''
            call json%get('trajectory(1).DATA(2)', rval)
            if (json_failed()) then
                call print_error_message()
            else
                write(*,'(A,1X,F30.16)') 'trajectory(1).DATA(2) = ',rval
            end if
            !get vectors:
            do i=1,4

                write(str,fmt='(I10)') i
                str = adjustl(str)

                write(*,'(A)') ''
                call json%get('trajectory('//trim(str)//').VARIABLE', cval)
                if (json_failed()) then

                    call print_error_message()

                else

                    write(*,'(A)') 'trajectory('//trim(str)//').VARIABLE = '//trim(cval)

                    !...get the vector using the callback method:
                    call json%get('trajectory('//trim(str)//').DATA', rvec)
                    
                    if (json_failed()) then
                        call print_error_message()
                    else
                        write(*,'(A,1X,*(F30.16,1X))') 'trajectory('//trim(str)//').DATA = ',rvec
                    end if
                end if
            end do
        end if

        write(*,'(A)') ''
        write(*,'(A)') 'destroy...'
        call json%destroy()
    end subroutine test_3


    subroutine test_2()
        implicit none

        type(json_value),pointer    :: p, inp, traj

        integer :: iunit

        call json_initialize()

        write(*,'(A)') ''
        write(*,'(A)') '================================='
        write(*,'(A)') '   EXAMPLE 2'
        write(*,'(A)') '================================='
        write(*,'(A)') ''

        ! root:
        ! create the value and associate the pointer
        ! add the file name as the name of the overall structure
        call json_create_object(p,dir//filename2)    
                                                    
        write(*,'(A)') ''
        write(*,'(A)') 'initialize the structure...'

        ! config structure (object):
        call json_create_object(inp,'inputs')
        call json_add(p, inp)

        ! trajectory structure (array):
        call json_create_array(traj,'trajectory')
        call json_add(p, traj)

        write(*,'(A)') ''
        write(*,'(A)') 'adding some data to structure...'

        !add some variables:

        !input variables:
        call json_add(inp, 't0', 0.1_wp)
        call json_add(inp, 'tf', 1.1_wp)
        call json_add(inp, 'x0', 9999.000_wp)
        call json_add(inp, 'integer_scalar', 1)
        call json_add(inp, 'integer_array', [2,4,99])
        call json_add(inp, 'names', ['aaa','bbb','ccc'])
        call json_add(inp, 'logical_scalar', .true.)
        call json_add(inp, 'logical_vector', [.true., .false., .true.])
        nullify(inp)

        !trajectory variables:
        call add_variables_to_input(                                            &
            traj, 'Rx', 'km', 'J2000', 'EARTH',                                 &
            [1.0_wp, 2.0_wp, 3.0_wp]                                            &
        )
        call add_variables_to_input(                                            &
            traj, 'Ry', 'km', 'J2000', 'EARTH',                                 &
            [10.0_wp, 20.0_wp, 30.0_wp]                                         &
        )
        call add_variables_to_input(                                            &
            traj, 'Rz', 'km', 'J2000', 'EARTH',                                 &
            [100.0_wp, 200.0d0, 300.0_wp]                                       &
        )
        call add_variables_to_input(                                            &
            traj, 'Vx', 'km/s', 'J2000', 'EARTH',                               &
            [1.0e-3_wp, 2.0e-3_wp, 3.0e-3_wp]                                   &
        )
        call add_variables_to_input(                                            &
            traj, 'Vy', 'km/s', 'J2000', 'EARTH',                               &
            [2.0e-3_wp, 20.0e-3_wp, 3.0e-3_wp]                                  &
        )
        call add_variables_to_input(                                            &
            traj, 'Vz', 'km/s', 'J2000', 'EARTH',                               &
            [3.0e-3_wp, 30.0e-3_wp, 40.0e-3_wp]                                 &
        )
        nullify(traj)

        write(*,'(A)') ''
        write(*,'(A)') 'writing file ' // trim(dir//filename2) // '...'

        open(newunit=iunit, file=dir//filename2, status='REPLACE')
        call json_print(p,iunit)
        close(iunit)

        call json_destroy(p)
        write(*,'(A)') ''
    end subroutine test_2


    subroutine add_variables_to_input(this, variable, units, frame, center, rdata)
        implicit none
        character(len=*),intent(in) :: variable, units, frame, center
        real(wp),dimension(:),intent(in) :: rdata
        type(json_value),pointer :: this
        type(json_value),pointer :: var

        ! initialize:
        nullify(var)

        ! create the object before data can be added:
        call json_create_object(var,'')

        ! variable info:
        call json_add(var, 'VARIABLE',trim(variable))
        call json_add(var, 'UNITS', trim(units))
        call json_add(var, 'FRAME', trim(frame))
        call json_add(var, 'CENTER', trim(center))

        ! trajectory [vector of reals]:
        call json_add(var, 'DATA', rdata)

        ! add this variable to trajectory structure:
        call json_add(this, var)

        nullify(var)
    end subroutine add_variables_to_input


    subroutine test_1()
        implicit none
        type(json_file) :: json    !the JSON structure read from the file:
        type(json_value), pointer :: p
        character(len=:), allocatable :: cval
        integer  :: ival
        logical  :: found
        real(wp) :: rval

        call json_initialize()

        write(*,'(A)') ''
        write(*,'(A)') '================================='
        write(*,'(A)') '   EXAMPLE 1'
        write(*,'(A)') '================================='
        write(*,'(A)') ''

        ! parse the json file:
        write(*,'(A)') ''
        write(*,'(A)') 'parsing file...'

        call json%load_file(filename = dir//filename1)

        if (json_failed()) then    !if there was an error reading the file

            call print_error_message()

        else

            ! print the parsed data to the console
            write(*,'(A)') ''
            write(*,'(A)') 'printing the file...'
            call json%print_file()

            ! extract data from the parsed value
            write(*,'(A)') ''
            write(*,'(A)') 'get some data from the file...'

            write(*,'(A)') ''
            call json%get('version.svn', ival)
            if (json_failed()) then
                call print_error_message()
            else
                write(*,'(A,I5)') 'version.svn = ',ival
            end if

            write(*,'(A)') ''
            call json%get('data(1).array(2)', cval)
            if (json_failed()) then
                call print_error_message()
            else
                write(*,'(A)') 'data(1).array(2) = '//trim(cval)
            end if

            write(*,'(A)') ''
            call json%get('files(1)', cval)
            if (json_failed()) then
                call print_error_message()
            else
                write(*,'(A)') 'files(1) = '//trim(cval)
            end if

            write(*,'(A)') ''
            call json%get('files(2)', cval)
            if (json_failed()) then
                call print_error_message()
            else
                write(*,'(A)') 'files(2) = '//trim(cval)
            end if

            write(*,'(A)') ''
            call json%get('files(3)', cval)
            if (json_failed()) then
                call print_error_message()
            else
                write(*,'(A)') 'files(3) = '//trim(cval)
            end if

            write(*,'(A)') ''
            call json%get('data(2).real', rval)
            if (json_failed()) then
                call print_error_message()
            else
                write(*,'(A,E30.16)') 'data(2).real = ',rval
            end if

            write(*,'(A)') ''
            call json%get('files[4]', cval)                 ! has hex characters
            if (json_failed()) then
                call print_error_message()
            else
                write(*,'(A)') 'files[4] = '//trim(cval)
            end if

            write(*,'(A)') ''
            call json%get('files[5]', cval) ! string with spaces and no esc char
            if (json_failed()) then
                call print_error_message()
            else
                write(*,'(A)') 'files[5] = '//trim(cval)
            end if

            !
            ! Test of values that aren't there:
            ! Note: when using the "found" output, 
            !       the exceptions are cleared automatically.
            !
            
            write(*,'(A)') ''
            call json%get('files[10]', cval, found)      !value that isn't there
            if (.not. found) then
                write(*,'(A)') 'files[10] not in file.'
            else
                write(*,'(1x,A)') 'files[10] = '//trim(cval)
            end if

            write(*,'(A)') ''
            call json%get('version.blah', ival, found)   !value that isn't there
            if (.not. found) then
                write(*,'(A)') 'version.blah not in file.'
            else
                write(*,'(A)') 'version.blah = ',ival
            end if
            
            write(*,'(A)') ''
            write(*,'(A)') ' Test removing data from the json structure:'
            
            call json%get('files', p)           !in the middle of a list
            call json_remove(p)
            if (json_failed()) call print_error_message()
        
            call json%get('data(1).array', p)   !at the end of a list
            call json_remove(p)
            if (json_failed()) call print_error_message()
        
            call json%get('data(2).number', p)  !at the beginning of a list
            call json_remove(p)
            if (json_failed()) call print_error_message()
            
            write(*,'(A)') ''
            write(*,'(A)') 'printing the modified structure...'
            call json%print_file()       
            if (json_failed()) call print_error_message()

            write(*,'(A)') ''
            write(*,'(A)') ' Test replacing data from the json structure:'
            
            call json%get('data(1)', p)
            call json_update(p,'name','Cuthbert',found)
            if (json_failed()) call print_error_message()

            !call json%get('data(2)', p)
            !call json_update(p,'real',[1.0_wp, 2.0_wp, 3.0_wp],found)   !don't have one like this yet...
            
            !use the json_file procedure to update a variable:
            call json%update('version.svn',999,found)
            if (json_failed()) call print_error_message()

            write(*,'(A)') ''
            write(*,'(A)') 'printing the modified structure...'
            call json%print_file()       
            if (json_failed()) call print_error_message()
            
        end if

        write(*,'(A)') ''
        write(*,'(A)') 'destroy...'
        call json%destroy()
    end subroutine test_1


    subroutine print_error_message()
        implicit none

        character(len=:),allocatable :: error_msg
        logical :: status_ok
        
        call json_check_for_errors(status_ok, error_msg)

        if (.not. status_ok) then
            write(*,'(A)') error_msg
            deallocate(error_msg)
            call json_clear_exceptions()
        end if
    end subroutine print_error_message
    
end program json_test