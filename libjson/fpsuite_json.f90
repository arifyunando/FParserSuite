module fpsuite_json
    use, intrinsic :: iso_fortran_env
    implicit none

    private    

    integer, parameter :: RK = real64     
    integer, parameter :: IK = int32     
    integer, parameter :: CK = character_kinds(1) 
    integer, parameter :: LK = logical_kinds(min(3,size(logical_kinds)))

    character(kind=CK,len=*), parameter, public :: json_ext = '.json'   
    character(kind=CK,len=*), parameter :: space           = ' '
    character(kind=CK,len=*), parameter :: start_object    = '{'
    character(kind=CK,len=*), parameter :: end_object      = '}'
    character(kind=CK,len=*), parameter :: start_array     = '['
    character(kind=CK,len=*), parameter :: end_array       = ']'
    character(kind=CK,len=*), parameter :: delimiter       = ','
    character(kind=CK,len=*), parameter :: colon_char      = ':'
    character(kind=CK,len=*), parameter :: null_str        = 'null'
    character(kind=CK,len=*), parameter :: true_str        = 'true'
    character(kind=CK,len=*), parameter :: false_str       = 'false'
    character(kind=CK,len=*), parameter :: bspace          = achar(8)
    character(kind=CK,len=*), parameter :: horizontal_tab  = achar(9)
    character(kind=CK,len=*), parameter :: newline         = achar(10)
    character(kind=CK,len=*), parameter :: formfeed        = achar(12)
    character(kind=CK,len=*), parameter :: carriage_return = achar(13)
    character(kind=CK,len=*), parameter :: quotation_mark  = achar(34)
    character(kind=CK,len=*), parameter :: slash           = achar(47)
    character(kind=CK,len=*), parameter :: backslash       = achar(92)
    
    integer(IK), parameter :: spaces_per_tab = 4
    integer(IK), parameter :: max_numeric_str_len = 32

    character(kind=CK,len=*), parameter :: real_fmt = '(E30.16E3)'
    character(kind=CK,len=*), parameter :: int_fmt  = '(I10)'     
    character(kind=CK,len=*), parameter :: star     = '*'

    integer(IK), parameter, public :: json_unknown   = 0
    integer(IK), parameter, public :: json_null      = 1
    integer(IK), parameter, public :: json_object    = 2
    integer(IK), parameter, public :: json_array     = 3
    integer(IK), parameter, public :: json_logical   = 4
    integer(IK), parameter, public :: json_integer   = 5
    integer(IK), parameter, public :: json_double    = 6
    integer(IK), parameter, public :: json_string    = 7
    

    type,public :: json_value
        !  Purpose:
        !    Type used to construct the linked-list json structure
        !
        !  Example:
        !    type(json_value),pointer :: p
        !    call json_create_object(p) 
        !    call json_add(p,'year',1805)
        !    call json_add(p,'value',1.0d0)
        !    call json_print(p,'test.json')
        !    call json_destroy(p)
        sequence  

        ! linked list variables:
        type(json_value),pointer :: previous => null()
        type(json_value),pointer :: next     => null()
        type(json_value),pointer :: parent   => null()
        type(json_value),pointer :: children => null()
        type(json_value),pointer :: tail     => null()

        ! variable name:
        character(kind=CK,len=:),allocatable :: name

        ! the data for this variable:
        real(RK), allocatable                 :: dbl_value
        logical(LK), allocatable              :: log_value
        character(kind=CK,len=:), allocatable :: str_value
        integer(IK), allocatable              :: int_value

        integer(IK)          :: var_type    = json_unknown
        integer(IK), private :: n_children  = 0
    end type json_value


    type,public :: json_file
        !
        ! Purpose:
        !    The json_file is the main public class that is
        !    used to open a file and get data from it.
        !
        ! Example:
        !    type(json_file) :: json
        !    integer :: ival
        !    real(real64) :: rval
        !    character(len=:),allocatable :: cval
        !    logical :: found
        !    call json%load_file(filename='myfile.json')
        !    call json%print_file() !print to the console
        !    call json%get('var.i',ival,found)
        !    call json%get('var.r(3)',rval,found)
        !    call json%get('var.c',cval,found)
        !    call json%destroy()
        private

        !the JSON structure read from the file:
        type(json_value),pointer :: p => null()

        contains

        procedure,public :: load_file        => json_file_load
        procedure,public :: load_from_string => json_file_load_from_string
        
        procedure,public :: destroy     => json_file_destroy
        procedure,public :: move        => json_file_move_pointer
        procedure,public :: info        => json_file_variable_info
        
        procedure,public :: print_to_string => json_file_print_to_string

        generic,public :: print_file => json_file_print_to_console, &
                                        json_file_print_1, &
                                        json_file_print_2
        
        generic,public :: get => json_file_get_object,      &
                                 json_file_get_integer,     &
                                 json_file_get_double,      &
                                 json_file_get_logical,     &
                                 json_file_get_string,      &
                                 json_file_get_integer_vec, &
                                 json_file_get_double_vec,  &
                                 json_file_get_logical_vec, &
                                 json_file_get_string_vec
                                 
        generic,public :: update =>  json_file_update_integer,  &
                                     json_file_update_logical,  &
                                     json_file_update_real,     &
                                     json_file_update_string

        procedure :: json_file_get_object
        procedure :: json_file_get_integer
        procedure :: json_file_get_double
        procedure :: json_file_get_logical
        procedure :: json_file_get_string
        procedure :: json_file_get_integer_vec
        procedure :: json_file_get_double_vec
        procedure :: json_file_get_logical_vec
        procedure :: json_file_get_string_vec
        
        procedure :: json_file_update_integer
        procedure :: json_file_update_logical
        procedure :: json_file_update_real
        procedure :: json_file_update_string
        
        procedure :: json_file_print_to_console
        procedure :: json_file_print_1
        procedure :: json_file_print_2   
    end type json_file


    abstract interface
        subroutine array_callback_func(element, i, count)
            import :: json_value,IK
            implicit none
            type(json_value), pointer,intent(in) :: element
            integer(IK),intent(in) :: i        !index
            integer(IK),intent(in) :: count    !size of array
        end subroutine array_callback_func
    end interface

    interface json_get_child
        module procedure json_value_get_by_index
        module procedure json_value_get_by_name_chars
    end interface json_get_child

    interface json_add
        module procedure :: json_value_add_member
        module procedure :: json_value_add_integer, json_value_add_integer_vec
        module procedure :: json_value_add_double,  json_value_add_double_vec
        module procedure :: json_value_add_logical, json_value_add_logical_vec
        module procedure :: json_value_add_string,  json_value_add_string_vec
    end interface json_add

    interface json_update
        module procedure :: json_update_logical
        module procedure :: json_update_double
        module procedure :: json_update_integer
        module procedure :: json_update_string
    end interface json_update

    interface json_get
        module procedure :: json_get_by_path
        module procedure :: json_get_integer, json_get_integer_vec
        module procedure :: json_get_double,  json_get_double_vec
        module procedure :: json_get_logical, json_get_logical_vec
        module procedure :: json_get_string,  json_get_string_vec
        module procedure :: json_get_array
    end interface json_get

    interface json_print_to_string
        module procedure :: json_value_to_string
    end interface

    interface json_print
        module procedure :: json_print_1    !input is unit number
        module procedure :: json_print_2    !input is file name
    end interface
    
    interface json_destroy
        module procedure :: json_value_destroy
    end interface

    interface json_remove
        module procedure :: json_value_remove
    end interface

    interface json_remove_if_present
        module procedure :: json_value_remove_if_present
    end interface


    ! Public Subroutines:
    public :: json_add               ! add data to a JSON structure
    public :: json_check_for_errors  ! check for error and get error message
    public :: json_clear_exceptions  ! clear exceptions
    public :: json_count             ! count the number of children
    public :: json_create_array      ! allocate a json_value array
    public :: json_create_double     ! allocate a json_value double
    public :: json_create_integer    ! allocate a json_value integer
    public :: json_create_logical    ! allocate a json_value logical
    public :: json_create_null       ! allocate a json_value null
    public :: json_create_object     ! allocate a json_value object
    public :: json_create_string     ! allocate a json_value string
    public :: json_destroy           ! clear a JSON structure (destructor)
    public :: json_failed            ! check for error
    public :: json_get               ! get data from the JSON structure  
    public :: json_get_child         ! get a child of a json_value
    public :: json_info              ! get info about a json_value
    public :: json_initialize        ! to initialize the module
    public :: json_parse             ! read a JSON file and populate the structure
    public :: json_print             ! print the JSON structure to a file
    public :: json_print_to_string   ! write the JSON structure to a string
    public :: json_remove            ! remove from a JSON structure
    public :: json_remove_if_present ! remove from a JSON structure (if present)
    public :: json_update            ! update a value in a JSON structure
       
    ! exception handling [private variables]
    logical(LK) :: is_verbose = .false.                 ! print exception to console
    logical(LK) :: exception_thrown = .false.           ! the error flag
    character(kind=CK,len=:),allocatable :: err_message ! the error message

    ! temp vars used when parsing lines in file [private variables]
    integer(IK) :: char_count = 0    ! character position in the current line
    integer(IK) :: line_count = 1    ! lines read counter
    integer(IK) :: pushed_index = 0
    character(kind=CK,len=10) :: pushed_char

contains

! Private Utilities

    subroutine destroy_json_data(d)
        !
        ! Purpose:
        !   Destroy the json_data type
        ! 
        ! Usage:
        !   call destroy_json_data(d)
        !
        implicit none
        type(json_value), intent(inout) :: d

        d%var_type = json_unknown
        if (allocated(d%log_value)) deallocate(d%log_value)
        if (allocated(d%int_value)) deallocate(d%int_value)
        if (allocated(d%dbl_value)) deallocate(d%dbl_value)
        if (allocated(d%str_value)) deallocate(d%str_value)
    end subroutine destroy_json_data


    subroutine json_file_destroy(this)
        !
        ! Purpose:
        !   Methods for destroying the JSON file
        !
        ! Usage:
        !   call this%destroy()
        !
        implicit none
        class(json_file), intent(inout) :: this

        if (associated(this%p)) call json_value_destroy(this%p)
    end subroutine json_file_destroy


    subroutine json_file_move_pointer(to, from)
        !
        ! Purpose:
        !   Move the json_value pointer from one json_file to another.
        !   "from" is then nullified, but not destroyed.
        !   Note: if "from%p" is not associated, then an error is thrown.
        !  
        ! Usage:
        !   call to%move(from)
        !
        implicit none
        class(json_file), intent(inout) :: to
        class(json_file), intent(inout) :: from

        if (associated(from%p)) then
            to%p => from%p
            nullify(from%p)
        else
            call throw_exception('Error in json_file_move_pointer: '//&
                                'pointer is not associated.')
        end if
    end subroutine json_file_move_pointer
    

    subroutine json_file_load(this, filename, unit)
        ! 
        ! Purpose:
        !   Load a JSON File
        ! 
        ! Usage:
        !   type(json_file) :: f
        !   call f%load_file('my_file.json')
        !         
        implicit none
        class(json_file), intent(inout)      :: this
        character(kind=CK,len=*), intent(in) :: filename
        integer(IK), intent(in), optional    :: unit

        call json_parse(file=filename, p=this%p, unit=unit)
    end subroutine json_file_load


    subroutine json_file_load_from_string(this, str)
        !
        ! Purpose:
        !   Load the JSON data from a string.
        !
        ! Usage:
        !   type(json_file) :: f
        !   call f%load_from_string('{ "name": "Leonidas" }')
        !
        implicit none
        class(json_file), intent(inout)      :: this
        character(kind=CK,len=*), intent(in) :: str

        call json_parse(str=str, p=this%p)
    end subroutine json_file_load_from_string


    subroutine json_file_print_to_console(this)
        !
        ! Purpose:
        !   Print the JSON file to the console.
        !
        ! Usage:
        !   call this%print_file()
        !
        implicit none
        class(json_file), intent(inout)       :: this
        character(kind=CK,len=:), allocatable :: dummy

        call json_value_print(                                                  &
            this%p, iunit=output_unit, str=dummy, indent=1, colon=.true.        &
        )
    end subroutine json_file_print_to_console


    subroutine json_file_print_1(this, iunit)
        !  
        ! Purpose:
        !   Print the JSON file.
        !   Prints to the specified file unit (if not present, then to console)
        ! 
        ! Usage:
        !   call this%print_file(iunit)
        !
        implicit none

        class(json_file), intent(inout)  :: this
        integer(IK), intent(in)          :: iunit
        character(kind=CK, len=:), allocatable :: dummy
        integer(IK) :: i

        if (iunit /= 0) then
            i = iunit
        else
            call throw_exception(                                               &
                'Error in json_file_print_1: iunit must be nonzero.'            &
            )
            return
        end if

        call json_value_print(                                                  &
            this%p, iunit=i, str=dummy, indent=1, colon=.true.                  &
        )
    end subroutine json_file_print_1


    subroutine json_file_print_2(this, filename)
        !
        ! Purpose:
        !   Print the JSON file.
        !   Input is the filename.  The file is opened, printed, and then closed.
        !
        ! EXAMPLE
        !   type(json_file) :: f
        !   logical :: found
        !   call f%load_file('my_file.json')    !open the original file
        !   call f%update('version',4,found)    !change the value of a variable
        !   call f%print_file('my_file_2.json') !save file as new name
        !
        implicit none

        class(json_file), intent(inout)       :: this
        character(kind=CK, len=*), intent(in) :: filename
        integer(IK) :: iunit, istat

        open(newunit=iunit, file=filename, status='REPLACE', iostat=istat)
        if (istat==0) then
            call this%print_file(iunit)
            close(iunit, iostat=istat)
        else
            call throw_exception(                                               &
                'Error in json_file_print_2: could not open file: '             &
                // trim(filename)                                               &
            )
        end if
    end subroutine json_file_print_2


    subroutine json_file_print_to_string(this, str)
        !
        ! Purpose:
        !   Print the JSON file to a string.
        !
        ! Usage:
        !   type(json_file) :: f
        !   character(kind=CK,len=:),allocatable :: str
        !   call f%load_file('my_file.json')
        !   call f%print_file(str)
        !
        implicit none
        class(json_file), intent(inout)                    :: this
        character(kind=CK,len=:), allocatable, intent(out) :: str

        call json_value_to_string(this%p, str)
    end subroutine json_file_print_to_string


    subroutine json_file_variable_info(this, path, found, var_type, n_children)
        !
        ! Usage:
        !   call me%info(path,found,var_type,n_children)
        !
        ! Description:
        !   Returns information about a variable in a file.
        !
        implicit none
        class(json_file), intent(inout)      :: this
        character(kind=CK,len=*), intent(in) :: path
        logical(LK), intent(out)             :: found
        integer(IK), intent(out)             :: var_type
        integer(IK), intent(out)             :: n_children

        type(json_value),pointer :: p

        nullify(p)
        call this%get(path, p, found)
        if (found) then
            call json_info(p, var_type, n_children)
        else
            var_type   = json_unknown
            n_children = 0
        end if

        nullify(p)
    end subroutine json_file_variable_info


    subroutine json_info(p,var_type,n_children)
        !
        ! Purpose:
        !   Returns information about a json_value
        !
        ! Usage:
        !   call me%info(path,found,var_type,n_children)
        !
        implicit none

        type(json_value), pointer          :: p
        integer(IK), intent(out), optional :: var_type
        integer(IK), intent(out), optional :: n_children
            
        if (present(var_type))   var_type   = p%var_type  
        if (present(n_children)) n_children = json_count(p) 
    end subroutine json_info

    
    subroutine json_file_get_object(this, path, p, found)
        !
        ! Purpose:
        !   Get a pointer to an object from a JSON file.
        !
        ! Usage:
        !   call this%get(path,p,found)
        !
        implicit none
        class(json_file),intent(inout)       :: this
        character(kind=CK,len=*),intent(in)  :: path
        type(json_value),pointer,intent(out) :: p
        logical(LK),intent(out),optional     :: found

        call json_get_by_path(this%p, path=path, p=p, found=found)
    end subroutine json_file_get_object


    subroutine json_file_get_integer(this, path, val, found)
        ! 
        ! Purpose:
        !    Get an integer from a JSON file.
        !
        ! Usage:
        !    call this%get(path,val)
        !
        implicit none
        class(json_file), intent(inout)      :: this
        character(kind=CK,len=*), intent(in) :: path
        integer(IK), intent(out)             :: val
        logical(LK), intent(out), optional   :: found

        call json_get(this%p, path=path, value=val, found=found)
    end subroutine json_file_get_integer


    subroutine json_file_get_integer_vec(this, path, vec, found)
        !
        ! Purpose:
        !   Get an integer vector from a JSON file
        !
        ! Usage:
        !   call this%get(path,vec)
        !
        implicit none
        class(json_file),intent(inout)                   :: this
        character(kind=CK,len=*),intent(in)              :: path
        integer(IK),dimension(:),allocatable,intent(out) :: vec
        logical(LK),intent(out),optional                 :: found

        call json_get(this%p, path, vec, found)
    end subroutine json_file_get_integer_vec


    subroutine json_file_get_double(this, path, val, found)
        !
        ! Purpose:
        !   Get a double from a JSON file.
        !
        ! Usage:
        !   call this%get(path,val,found)
        !
        implicit none
        class(json_file),intent(inout)      :: this
        character(kind=CK,len=*),intent(in) :: path
        real(RK),intent(out)                :: val
        logical(LK),intent(out),optional    :: found

        call json_get(this%p, path=path, value=val, found=found)
    end subroutine json_file_get_double


    subroutine json_file_get_double_vec(this, path, vec, found)
        !
        ! Purpose:
        !   Get a double vector from a JSON file.
        !
        ! Usage:
        !   call me%get(path,vec,found)
        !
        implicit none
        class(json_file), intent(inout)      :: this
        character(kind=CK,len=*), intent(in) :: path
        real(RK), allocatable, intent(out)   :: vec(:)
        logical(LK), intent(out), optional   :: found

        call json_get(this%p, path, vec, found)
    end subroutine json_file_get_double_vec


    subroutine json_file_get_logical(this, path, val, found)
        !
        ! Purpose:
        !   Get a logical from a JSON file.
        !
        ! Usage:
        !   call this%get(path,val,found)
        !
        implicit none

        class(json_file),intent(inout)       :: this
        character(kind=CK,len=*),intent(in)  :: path
        logical(LK),intent(out)              :: val
        logical(LK),intent(out),optional     :: found

        call json_get(this%p, path=path, value=val, found=found)
    end subroutine json_file_get_logical


    subroutine json_file_get_logical_vec(this, path, vec, found)
        !
        ! Purpose:
        !   Get a logical vector from a JSON file.
        !
        ! Usage:
        !   call this%get(path,vec)
        !
        implicit none
        class(json_file),intent(inout)                   :: this
        character(kind=CK,len=*),intent(in)              :: path
        logical(LK),dimension(:),allocatable,intent(out) :: vec
        logical(LK),intent(out),optional                 :: found
        
        call json_get(this%p, path, vec, found)
    end subroutine json_file_get_logical_vec


    subroutine json_file_get_string(this, path, val, found)
        !
        ! Purpose:
        !   Get a character string from a json file.
        !   Note: val is an allocatable character string.
        !
        ! Usage:
        !   call this%get(path,val)
        !
        implicit none
        class(json_file),intent(inout)                   :: this
        character(kind=CK,len=*),intent(in)              :: path
        character(kind=CK,len=:),allocatable,intent(out) :: val
        logical(LK),intent(out),optional                 :: found

        call json_get(this%p, path=path, value=val, found=found)
    end subroutine json_file_get_string


    subroutine json_file_get_string_vec(me, path, vec, found)
        !
        ! Purpose:
        !   Get a string vector from a JSON file.
        !
        ! Usage:
        !   call me%get(path,vec)
        !
        implicit none
        class(json_file), intent(inout)                    :: me
        character(kind=CK,len=*), intent(in)               :: path
        character(kind=CK,len=*), allocatable, intent(out) :: vec(:)
        logical(LK), intent(out), optional                 :: found

        call json_get(me%p, path, vec, found)
    end subroutine json_file_get_string_vec


    subroutine json_initialize(verbose)
        !
        ! Purpose:
        !   Initialize the module.
        !   Should be called before the routines are used.
        !   It can also be called after using the module 
        !   and encountering exceptions.
        !
        implicit none
        logical(LK),intent(in),optional :: verbose 
        
        if (present(verbose)) is_verbose = verbose
        call json_clear_exceptions()

        pushed_index = 0
        pushed_char  = ''
        char_count   = 0
        line_count   = 1
    end subroutine json_initialize


    subroutine json_clear_exceptions()
        !
        ! Purpose:
        !   Clear exceptions in the JSON module.
        !
        implicit none

        exception_thrown = .false.
        err_message = ''
    end subroutine json_clear_exceptions

 
    subroutine throw_exception(msg)
        !
        ! Purpose:
        !   Throw an exception in the JSON module.
        !   This routine sets the error flag, and prevents any subsequent routine
        !   from doing anything, until json_clear_exceptions is called.
        !
        implicit none
        character(kind=CK,len=*),intent(in) :: msg    !the error message

        exception_thrown = .true.
        err_message = trim(msg)
        
        if (is_verbose) then
            write(*,'(A)') '***********************'
            write(*,'(A)') 'JSON-FORTRAN EXCEPTION: '//trim(msg)
            !call backtrace()     ! gfortran (use -fbacktrace -fall-intrinsics flags)
            !call tracebackqq(-1) ! intel (requires "use ifcore" in this routine)
            write(*,'(A)') '***********************'
        end if
    end subroutine throw_exception


    subroutine json_check_for_errors(status_ok, error_msg)
        !
        ! Purpose:
        !   Retrieve error code from the module.
        !   This should be called after json_parse to check for errors.
        !   If an error is thrown, before using the module again, json_initialize
        !   should be called to clean up before it is used again.
        !
        ! Usage:
        !   type(json_file) :: json
        !   logical :: status_ok
        !   character(len=:),allocatable :: error_msg
        !   call json%load_file(filename='myfile.json')
        !   call json_check_for_errors(status_ok, error_msg)
        !   if (.not. status_ok) then
        !       write(*,*) 'Error: '//error_msg
        !       call json_clear_exceptions()
        !       call json%destroy()
        !   end if
        !
        implicit none
        logical(LK), intent(out) :: status_ok
        character(kind=CK,len=:), allocatable, intent(out) :: error_msg

        status_ok = .not. exception_thrown

        if (.not. status_ok) then
            if (allocated(err_message)) then
                error_msg = err_message
            else
                error_msg = 'Unknown Error'
            end if
        else
            error_msg = ''
        end if
    end subroutine json_check_for_errors


    function json_failed() result(failed)
        !
        ! Purpose:
        !   Logical function to indicate if an exception has been thrown.
        !
        ! Usage:
        !   type(json_file) :: json
        !   logical :: status_ok
        !   character(len=:),allocatable :: error_msg
        !   call json%load_file(filename='myfile.json')
        !   if (json_failed()) then
        !       call json_check_for_errors(status_ok, error_msg)
        !       write(*,*) 'Error: '//error_msg
        !       call json_clear_exceptions()
        !       call json%destroy()
        !   end if
        !
        ! See also:
        !   json_check_for_errors
        !
        implicit none
        logical(LK) :: failed

        failed = exception_thrown
    end function json_failed


    subroutine json_value_create(p)
        !
        ! Purpose:
        !   Allocate a json_value pointer variable.
        !   This should be called before adding data to it.
        !
        ! Usage:
        !   type(json_value),pointer :: var
        !   call json_value_create(var)
        !   call to_double(var,1.0d0)
        !
        ! Notes:
        !   This routine does not check for exceptions.
        !   The pointer should not already be allocated.
        !
        implicit none
        type(json_value), pointer :: p

        nullify(p)
        allocate(p)
    end subroutine json_value_create


    recursive subroutine json_value_destroy(this)
        !
        ! DESCRIPTION
        !   Destroy a json_value linked-list structure.
        !
        implicit none
        type(json_value), pointer :: this

        if (associated(this)) then
            if (allocated(this%name)) deallocate(this%name)
            call destroy_json_data(this)
            if (associated(this%children)) call json_value_destroy(this%children)
            this%n_children = 0
            if (associated(this%next)) call json_value_destroy(this%next)
            if (associated(this%previous)) nullify(this%previous)
            if (associated(this%parent))   nullify(this%parent)
            if (associated(this%tail))     nullify(this%tail)
            deallocate(this)
            nullify(this)
        end if
    end subroutine json_value_destroy


    subroutine json_value_remove(this, destroy)
        !
        ! Purpose:
        !   Remove a json_value (and all its children) 
        !     from a linked-list structure, preserving the rest of the structure.
        !   If destroy is not present, it is also destroyed.
        !   If destroy is present and true, it is destroyed.
        !   If destroy is present and false, it is not destroyed.
        !
        ! Usage:
        !   !to extract an object from one json structure, and add it to another:
        !   type(json_value),pointer :: json1,json2,p
        !   logical :: found
        !   [create and populate json1 and json2]
        !   call json_get(json1,'name',p,found)  ! get pointer to name element of json1
        !   call json_remove(p,destroy=.false.)  ! remove it from json1 (don't destroy)
        !   call json_add(json2,p)         ! add it to json2
        !
        !   !to remove an object from a json structure (and destroy it)
        !   type(json_value),pointer :: json1,p
        !   logical :: found
        !   [create and populate json1]
        !   call json_get(json1,'name',p,found)  ! get pointer to name element of json1
        !   call json_remove(p)                  ! remove and destroy it
        !
        implicit none

        type(json_value), pointer         :: this
        logical(LK), intent(in), optional :: destroy
        
        type(json_value), pointer :: parent,previous,next
        logical(LK) :: destroy_it
            
        if (associated(this)) then
            !optional input argument:
            if (present(destroy)) then
                destroy_it = destroy
            else
                destroy_it = .true.
            end if
            if (associated(this%parent)) then
                parent => this%parent                    
                if (associated(this%next)) then
                    !there are later items in the list:
                    next => this%next
                    nullify(this%next)
                
                    if (associated(this%previous)) then   
                        !there are earlier items in the list
                        previous => this%previous                
                        previous%next => next
                        next%previous => previous
                    else
                        !this is the first item in the list
                        parent%children => next
                        nullify(next%previous)
                    end if
                else
                    if (associated(this%previous)) then
                        !there are earlier items in the list:
                        previous => this%previous 
                        nullify(previous%next)  
                        parent%tail => previous
                    else
                        !this is the only item in the list:
                        nullify(parent%children)
                        nullify(parent%tail)            
                    end if
                end if
                parent%n_children = parent%n_children - 1
            end if
            if (destroy_it) call json_value_destroy(this)
        end if

    end subroutine json_value_remove


    subroutine json_value_remove_if_present(p, name)
        !
        ! Purpose:
        !   Given the path string, remove the variable from 
        !   the json_value structure, if it exists.
        !
        implicit none
        type(json_value),pointer            :: p
        character(kind=CK,len=*),intent(in) :: name
        
        type(json_value),pointer :: p_var
        logical(LK) :: found
        
        call json_get(p,name,p_var,found)
        if (found) call json_remove(p_var)
    end subroutine json_value_remove_if_present


    subroutine json_file_update_integer(this, name, val, found)
        !
        ! Purpose:
        !   Given the path string, if the variable is present in the file, 
        !   and is a scalar, then update its value.
        !   If it is not present, then create it and set its value.
        !
        ! See also:
        !   json_update_integer
        !
        implicit none
        class(json_file),intent(inout)      :: this
        character(kind=CK,len=*),intent(in) :: name
        integer(IK),intent(in)              :: val
        logical(LK),intent(out)             :: found
        
        if (.not. exception_thrown) call json_update(this%p, name, val, found)
    end subroutine json_file_update_integer


    subroutine json_file_update_logical(me, name, val, found)
        !
        ! Purpose:
        !    Given the path string, if the variable is present in the file, 
        !    and is a scalar, then update its value.
        !    If it is not present, then create it and set its value.
        !
        ! See also:
        !    json_update_logical
        !
        implicit none
        class(json_file),intent(inout)      :: me
        character(kind=CK,len=*),intent(in) :: name
        logical(LK),intent(in)              :: val
        logical(LK),intent(out)             :: found

        if (.not. exception_thrown) call json_update(me%p,name,val,found)
    end subroutine json_file_update_logical


    subroutine json_file_update_real(this, name, val, found)
        !
        ! PurposeL
        !   Given the path string, if the variable is present in the file, 
        !   and is a scalar, then update its value.
        !   If it is not present, then create it and set its value.
        !
        ! See also:
        !   json_update_real
        !
        implicit none
        class(json_file), intent(inout)      :: this
        character(kind=CK,len=*), intent(in) :: name
        real(RK), intent(in)                 :: val
        logical(LK), intent(out)             :: found

        if (.not. exception_thrown) call json_update(this%p, name, val, found)
    end subroutine json_file_update_real


    subroutine json_file_update_string(me, name, val, found)
        !
        ! Purpose:
        !   Given the path string, if the variable is present in the file, 
        !   and is a scalar, then update its value.
        !   If it is not present, then create it and set its value.
        !
        ! See also:
        !   json_update_string
        !
        implicit none
        class(json_file), intent(inout)      :: me
        character(kind=CK,len=*), intent(in) :: name
        character(kind=CK,len=*), intent(in) :: val
        logical(LK), intent(out)             :: found

        if (.not. exception_thrown) call json_update(me%p,name,val,found)
    end subroutine json_file_update_string


    subroutine json_update_logical(p, name, val, found)
        !
        ! Purpose:
        !   Given the path string, if the variable is present, 
        !   and is a scalar, then update its value.
        !   If it is not present, then create it and set its value.
        !
        implicit none
        type(json_value),pointer            :: p
        character(kind=CK,len=*),intent(in) :: name
        logical(LK),intent(in)              :: val
        logical(LK),intent(out)             :: found
    
        type(json_value),pointer :: p_var
        integer(IK) :: var_type
 
        call json_get(p,name,p_var,found)
        if (found) then
            call json_info(p_var,var_type)
            select case (var_type)
            case (json_null,json_logical,json_integer,json_double,json_string)
                call to_logical(p_var,val)    !update the value
            case default
                found = .false.
                call throw_exception('Error in json_update_logical: '//&
                                    'the variable is not a scalar value')
            end select          
        else
            call json_add(p,name,val)   !add the new element
        end if
    end subroutine json_update_logical


    subroutine json_update_double(p, name, val, found)
        !
        ! Purpose:
        !   Given the path string, if the variable is present, 
        !   and is a scalar, then update its value.
        !   If it is not present, then create it and set its value.
        !
        implicit none
        type(json_value), pointer            :: p
        character(kind=CK,len=*), intent(in) :: name
        real(RK), intent(in)                 :: val
        logical(LK), intent(out)             :: found
        
        type(json_value),pointer :: p_var
        integer(IK) :: var_type
    
        call json_get(p,name,p_var,found)
        if (found) then
            call json_info(p_var,var_type)
            select case (var_type)
                case (json_null, json_logical, json_integer, json_double, json_string)
                    call to_double(p_var,val)
                case default
                    found = .false.
                    call throw_exception('Error in json_update_double: '//      &
                                         'the variable is not a scalar value')
            end select 
        else
            call json_add(p,name,val)
        end if
    end subroutine json_update_double


    subroutine json_update_integer(p, name, val, found)
        !
        ! Purpose:
        !   Given the path string, if the variable is present, 
        !   and is a scalar, then update its value.
        !   If it is not present, then create it and set its value.
        !
        implicit none
        
        type(json_value),pointer            :: p
        character(kind=CK,len=*),intent(in) :: name
        integer(IK),intent(in)              :: val
        logical(LK),intent(out)             :: found
        
        type(json_value),pointer :: p_var
        integer(IK) :: var_type
    
        call json_get(p,name,p_var,found)
        if (found) then
            call json_info(p_var,var_type)
            select case (var_type)
            case (json_null,json_logical,json_integer,json_double,json_string)
                call to_integer(p_var, val)
            case default
                found = .false.
                call throw_exception('Error in json_update_integer: '//&
                                    'the variable is not a scalar value')
            end select
        else
            call json_add(p,name, val)
        end if
    end subroutine json_update_integer


    subroutine json_update_string(p, name, val, found)
        !
        ! Purpose:
        !   Given the path string, if the variable is present, 
        !   and is a scalar, then update its value.
        !   If it is not present, then create it and set its value.
        !
        implicit none
    
        type(json_value),pointer            :: p
        character(kind=CK,len=*),intent(in) :: name
        character(kind=CK,len=*),intent(in) :: val
        logical(LK),intent(out)             :: found
        
        type(json_value),pointer :: p_var
        integer(IK) :: var_type
    
        call json_get(p,name,p_var,found)
        if (found) then
            call json_info(p_var,var_type)
            select case (var_type)
            case (json_null,json_logical,json_integer,json_double,json_string)
                call to_string(p_var,val)
            case default
                found = .false.
                call throw_exception('Error in json_update_string: '//&
                                    'the variable is not a scalar value')
            end select          
        else
            call json_add(p,name,val)
        end if
    end subroutine json_update_string


    subroutine json_value_add_member(this, member)
        !
        ! Purpose:
        !   Adds the member as a child of this.
        !
        implicit none
        type(json_value), pointer :: this, member

        if (.not. exception_thrown) then
            ! associate the parent
            member%parent => this
            ! add to linked list
            if (associated(this%children)) then
                this%tail%next => member
                member%previous => this%tail
            else
                this%children => member
                member%previous => null()  ! first in the list
            end if
            ! new member is now the last one in the list
            this%tail => member
            this%n_children = this%n_children + 1
        end if
    end subroutine json_value_add_member


    subroutine escape_string(str_in, str_out)
        !
        ! Purpose:
        !   Add the escape characters to a string for adding to JSON.
        !
        implicit none

        character(kind=CK,len=*), intent(in)              :: str_in
        character(kind=CK,len=:), allocatable, intent(out) :: str_out

        integer(IK) :: i
        character(kind=CK,len=1) :: c

        str_out = ''

        ! go through the string and look for special characters:
        do i=1,len(str_in)
            
            !get next character in the input string
            c = str_in(i:i)    
            select case(c)
                case(quotation_mark,backslash,slash)
                    str_out = str_out//backslash//c
                case(bspace)
                    str_out = str_out//'\b'
                case(formfeed)
                    str_out = str_out//'\f'
                case(newline)
                    str_out = str_out//'\n'
                case(carriage_return)
                    str_out = str_out//'\r'
                case(horizontal_tab)
                    str_out = str_out//'\t'
                case default
                    str_out = str_out//c
            end select
        end do
    end subroutine escape_string


! PUBLIC FUNCTIONS / SUBROUTINES
    
    function json_count(this) result(count)
        !
        ! Purpose:
        !   Count the number of children.
        !
        implicit none

        integer(IK)                         :: count
        type(json_value),pointer,intent(in) :: this
        
        count = this%n_children
    end function json_count


! PUBLIC INTERFACE : JSON_ADD

    subroutine json_value_add_double(this, name, val)
        !
        ! Purpose:
        !   Add a real value to the structure.
        !
        implicit none

        type(json_value),pointer            :: this
        character(kind=CK,len=*),intent(in) :: name
        real(RK),intent(in)                 :: val

        type(json_value),pointer :: var

        !create the variable:
        call json_value_create(var)
        call to_double(var,val,name)

        !add it:
        call json_add(this, var)

        !cleanup:
        nullify(var)

    end subroutine json_value_add_double


    subroutine json_value_add_double_vec(this, name, val)
        !
        ! Purpose:
        !   Add a real vector to the structure.
        !
        implicit none

        type(json_value),pointer            :: this
        character(kind=CK,len=*),intent(in) :: name
        real(RK), intent(in)                :: val(:)

        type(json_value),pointer :: var
        integer(IK) :: i

        ! create the variable as an array:
        call json_value_create(var)
        call to_array(var,name)

        ! populate the array:
        do i=1,size(val)
            call json_add(var, '', val(i))
        end do

        ! add it:
        call json_add(this, var)

        ! cleanup:
        nullify(var)
    end subroutine json_value_add_double_vec


    subroutine json_value_add_integer(me, name, val)
        !
        ! Purpose:
        !   Add an integer value to the structure.
        !
        implicit none
        type(json_value), pointer            :: me
        character(kind=CK,len=*), intent(in) :: name
        integer(IK), intent(in)              :: val

        type(json_value), pointer :: var

        !create the variable:
        call json_value_create(var)
        call to_integer(var,val,name)

        !add it:
        call json_add(me, var)

        !cleanup:
        nullify(var)
    end subroutine json_value_add_integer


    subroutine json_value_add_integer_vec(this, name, val)
        !
        ! Purpose:
        !   Add an integer vector to the structure.
        !
        implicit none

        type(json_value), pointer            :: this
        character(kind=CK,len=*), intent(in) :: name
        integer(IK), intent(in)              :: val(:)

        type(json_value), pointer :: var
        integer(IK) :: i    !counter

        !create the variable as an array:
        call json_value_create(var)
        call to_array(var,name)

        !populate the array:
        do i=1,size(val)
            call json_add(var, '', val(i))
        end do

        !add it:
        call json_add(this, var)

        !cleanup:
        nullify(var)
    end subroutine json_value_add_integer_vec


    subroutine json_value_add_logical(this, name, val)
        !
        ! Purpose:
        !   Add a logical value to the structure.
        !
        implicit none
        type(json_value), pointer            :: this
        character(kind=CK,len=*), intent(in) :: name
        logical(LK), intent(in)              :: val

        type(json_value),pointer :: var

        !create the variable:
        call json_value_create(var)
        call to_logical(var,val,name)

        !add it:
        call json_add(this, var)

        !cleanup:
        nullify(var)
    end subroutine json_value_add_logical


    subroutine json_value_add_logical_vec(this, name, val)
        !
        ! Purpose:
        !   Add a logical vector to the structure.
        !
        implicit none
        type(json_value), pointer            :: this
        character(kind=CK,len=*), intent(in) :: name
        logical(LK), intent(in)              :: val(:)

        type(json_value),pointer :: var
        integer(IK) :: i    !counter

        !create the variable as an array:
        call json_value_create(var)
        call to_array(var,name)

        !populate the array:
        do i=1,size(val)
            call json_add(var, '', val(i))
        end do

        !add it:
        call json_add(this, var)

        !cleanup:
        nullify(var)
    end subroutine json_value_add_logical_vec


    subroutine json_value_add_string(me, name, val)
        !
        ! Purpose:
        !   Add a character string the structure.
        !
        implicit none
        type(json_value),pointer            :: me
        character(kind=CK,len=*),intent(in) :: name
        character(kind=CK,len=*),intent(in) :: val

        type(json_value),pointer :: var
        character(kind=CK,len=:),allocatable :: str

        !add escape characters if necessary:
        call escape_string(val, str)

        !create the variable:
        call json_value_create(var)
        call to_string(var,str,name)

        !add it:
        call json_add(me, var)

        !cleanup:
        nullify(var)
    end subroutine json_value_add_string


    subroutine json_value_add_string_vec(me, name, val, trim_str, adjustl_str)
        !
        ! Purpose:
        !   Add an array of character strings to the structure.
        !
        implicit none

        type(json_value),pointer                         :: me
        character(kind=CK,len=*),intent(in)              :: name
        character(kind=CK,len=*),dimension(:),intent(in) :: val
        logical(LK),intent(in),optional                  :: trim_str
        logical(LK),intent(in),optional                  :: adjustl_str

        type(json_value),pointer :: var
        integer(IK) :: i
        logical(LK) :: trim_string, adjustl_string
        character(kind=CK,len=:),allocatable :: str

        !if the string is to be trimmed or not:
        if (present(trim_str)) then
            trim_string = trim_str
        else
            trim_string = .false.
        end if
        if (present(adjustl_str)) then
            adjustl_string = adjustl_str
        else
            adjustl_string = .false.
        end if

        !create the variable as an array:
        call json_value_create(var)
        call to_array(var,name)

        !populate the array:
        do i=1,size(val)

            !the string to write:
            str = val(i)
            if (adjustl_string) str = adjustl(str)
            if (trim_string)    str = trim(str)

            !write it:
            call json_add(var, '', str)

            !cleanup
            deallocate(str)

        end do

        !add it:
        call json_add(me, var)

        !cleanup:
        nullify(var)
    end subroutine json_value_add_string_vec


! PUBLIC INTERFACE : JSON_GET_CHILD

    subroutine json_value_get_by_index(this, idx, p)
        !
        ! Purpose:
        !    Returns a child in the object or array given the index.
        !
        implicit none
        type(json_value),pointer,intent(in) :: this
        integer(IK),intent(in)              :: idx
        type(json_value),pointer            :: p

        integer(IK) :: i

        nullify(p)
        if (.not. exception_thrown) then
            if (associated(this%children)) then
                p => this%children
                do i = 1, idx - 1
                    if (associated(p%next)) then
                        p => p%next
                    else
                        call throw_exception('Error in json_value_get_by_index:'//&
                                            ' p%next is not associated.')
                        nullify(p)
                        return
                    end if
                end do
            else
                call throw_exception('Error in json_value_get_by_index:'//&
                                    ' this%children is not associated.')
            end if
        end if
    end subroutine json_value_get_by_index


    subroutine json_value_get_by_name_chars(this, name, p)
        !
        ! Purpose:
        !   Returns a child in the object or array given the name string.
        !
        ! Notes:
        !   It is a case-sensitive search, and the name string is not trimmed,
        !       So, for example, 'a ' /= 'A ' /= 'a  '
        !   Note that the name is not parsed like it is in json_get_by_path.
        !
        implicit none
        type(json_value),pointer,intent(in) :: this
        character(kind=CK,len=*),intent(in) :: name
        type(json_value),pointer            :: p

        integer(IK) :: i,n_children
        
        nullify(p)
        if (.not. exception_thrown) then
            if (associated(this)) then
                if (this%var_type==json_object) then
                    n_children = json_count(this)
                    p => this%children    !start with first one
                    do i=1, n_children
                        if (allocated(p%name)) then
                            if (p%name == name) return
                        end if
                        p => p%next
                    end do
                end if
                !did not find anything:
                call throw_exception('Error in json_value_get_by_name_chars: '//&
                                    'child variable '//trim(name)//' was not found.')
                nullify(p)
            else
                call throw_exception('Error in json_value_get_by_name_chars: '//&
                                    'pointer is not associated.')
            end if
        end if
    end subroutine json_value_get_by_name_chars


    subroutine json_value_to_string(me,str)
        !
        ! Purpose:
        !   Print the JSON structure to an allocatable string.
        !
        implicit none
        type(json_value),pointer,intent(in)              :: me
        character(kind=CK,len=:),intent(out),allocatable :: str
        
        str = ''
        call json_value_print(me, iunit=0, str=str, indent=1, colon=.true.)
    end subroutine json_value_to_string


! PUBLIC INTERFACE : JSON_PRINT

    subroutine json_print_1(this, iunit)
        !
        ! Purpose:
        !   Print the JSON structure to a file
        !   Input is the nonzero file unit (the file must already have been opened).
        !
        implicit none
        type(json_value),pointer,intent(in) :: this
        integer(IK),intent(in)              :: iunit 
        
        character(kind=CK,len=:),allocatable :: dummy
        
        if (iunit/=0) then
            call json_value_print(this, iunit, str=dummy, indent=1, colon=.true.)
        else
            call throw_exception('Error in json_print: iunit must be nonzero.')
        end if
    end subroutine json_print_1


    subroutine json_print_2(me,filename)
        !
        ! Purpose:
        !    Print the JSON structure to a file.
        !    Input is the filename.
        !
        implicit none
        type(json_value),pointer,intent(in) :: me
        character(kind=CK,len=*),intent(in) :: filename
        
        integer(IK) :: iunit,istat
        
        open(newunit=iunit,file=filename,status='REPLACE',iostat=istat)
        if (istat==0) then
            call json_print(me,iunit)
            close(iunit,iostat=istat)
        else
            call throw_exception('Error in json_print: could not open file: '//&
                                trim(filename))
        end if
    end subroutine json_print_2
    

! INTERNAL PRIVATE SUBROUTINES

    recursive subroutine json_value_print(                                      &
            this, iunit, str, indent, need_comma, colon, is_array_element       &
        )
        !
        ! Purpose:
        !    Print the JSON structure to a string or a file.
        !
        ! Notes:
        !    This is an internal routine called by the wrapper routines
        !        json_print and json_value_to_string
        !    The reason the str argument is non-optional is because of a 
        !        bug in v4.9 of the gfortran compiler.  
        !
        implicit none
        type(json_value), pointer, intent(in) :: this
        integer(IK), intent(in)               :: iunit
        integer(IK), intent(in), optional     :: indent
        logical(LK), intent(in), optional     :: is_array_element
        logical(LK), intent(in), optional     :: need_comma
        logical(LK), intent(in), optional     :: colon

        character(kind=CK,len=:),intent(inout),allocatable :: str 
        ! if iunit==0, then the structure is 
        ! printed to this string rather than 
        ! a file. This mode is used by 
        ! json_value_to_string.

        character(kind=CK,len=max_numeric_str_len) :: tmp !for val to string conversions
        character(kind=CK,len=:),allocatable :: s
        type(json_value),pointer :: element
        integer(IK) :: tab, i, count, spaces
        logical(LK) :: print_comma
        logical(LK) :: write_file, write_string
        logical(LK) :: is_array
            
        if (.not. exception_thrown) then
                
            !whether to write a string or a file (one or the other):
            write_string = (iunit==0)
            write_file = .not. write_string
            
            !if the comma will be printed after the value
            ! [comma not printed for the last elements]
            if (present(need_comma)) then
                print_comma = need_comma
            else
                print_comma = .false.
            end if
            
            !number of "tabs" to indent:
            if (present(indent)) then
                tab = indent
            else
                tab = 0
            end if
            !convert to number of spaces:
            spaces = tab*spaces_per_tab
            
            !if this is an element in an array:
            if (present(is_array_element)) then
                is_array = is_array_element
            else
                is_array = .false.
            end if

            !if the colon was the last thing written
            if (present(colon)) then
                s = ''
            else
                s = repeat(space, spaces)
            end if
            
            select case (this%var_type)
                case (json_object)
                    
                    count = json_count(this)
                    
                    if (count==0) then    !special case for empty object
                    
                        call write_it( s//start_object//end_object, comma=print_comma )
                
                    else
                
                        call write_it( s//start_object )
                        
                        !if an object is in an array, there is an extra tab:
                        if (is_array) then
                            tab = tab+1
                            spaces = tab*spaces_per_tab                
                        end if
                        
                        nullify(element)
                        element => this%children
                        do i = 1, count
            
                            ! print the name
                            if (allocated(element%name)) then
                                call write_it(repeat(space, spaces)//quotation_mark//&
                                            element%name//quotation_mark//colon_char//space,&
                                            advance=.false.)
                            else
                                call throw_exception('Error in json_value_print:'//&
                                                    ' element%name not allocated')
                                nullify(element)
                                return
                            end if
                            
                            ! recursive print of the element
                            call json_value_print(element, iunit=iunit, indent=tab + 1, &
                                                need_comma=i<count, colon=.true., str=str)
                            
                            ! get the next child the list:
                            element => element%next
            
                        end do
                        
                        ! [one fewer tab if it isn't an array element]
                        if (.not. is_array) s = repeat(space, max(0,spaces-spaces_per_tab))
                        call write_it( s//end_object, comma=print_comma )
                        nullify(element)
                    
                    end if
                    
                case (json_array)

                    count = json_count(this)
                    
                    if (count==0) then    !special case for empty array

                        call write_it( s//start_array//end_array, comma=print_comma )
                        
                    else

                        call write_it( start_array )
                        
                        nullify(element)
                        element => this%children
                        do i = 1, count
                            
                            ! recursive print of the element
                            call json_value_print(element, iunit=iunit, indent=tab,&
                                                need_comma=i<count, is_array_element=.true., str=str)
            
                            ! get the next child the list:
                            element => element%next
                            
                        end do
                        
                        !indent the closing array character:        
                        call write_it( repeat(space, max(0,spaces-spaces_per_tab))//end_array,&
                                    comma=print_comma )
                        nullify(element)
                    
                    end if
                    
                case (json_null)

                    call write_it( s//null_str, comma=print_comma )
                    
                case (json_string)

                    if (allocated(this%str_value)) then
                        call write_it( s//quotation_mark// &
                                    trim(this%str_value)//quotation_mark, comma=print_comma )
                    else
                        call throw_exception('Error in json_value_print:'//&
                                            ' this%value_string not allocated')
                        return
                    end if

                case (json_logical)

                    if (this%log_value) then
                        call write_it( s//true_str, comma=print_comma )
                    else
                        call write_it( s//false_str, comma=print_comma )
                    end if

                case (json_integer)

                    call integer_to_string(this%int_value,tmp)

                    call write_it( s//trim(tmp), comma=print_comma )

                case (json_double)

                    call real_to_string(this%dbl_value,tmp)

                    call write_it( s//trim(tmp), comma=print_comma )

                case default

                call throw_exception('Error in json_value_print: unknown data type')

            end select

            !cleanup:
            if (allocated(s)) deallocate(s)
        end if

    contains

        subroutine write_it(string,advance,comma)
            !
            ! Purpose:
            !   write the string to the file (or the output string)
            !
            implicit none

            character(kind=CK,len=*), intent(in) :: string        !string to print
            logical(LK), intent(in), optional    :: advance  !to add line break or not
            logical(LK), intent(in), optional    :: comma    !print comma after the string
        
            logical(LK) :: add_line_break, add_comma
            character(kind=CK,len=:), allocatable :: string2
            
            if (present(comma)) then
                add_comma = comma
            else
                add_comma = .false. !default is not to add comma
            end if
            
            if (present(advance)) then
                add_line_break = advance
            else
                add_line_break = .true. !default is to advance
            end if
            
            !string to print:
            string2 = string
            if (add_comma) string2 = string2 // delimiter
            
            if (write_file) then
                if (add_line_break) then
                    write(iunit,fmt='(A)') string2
                else
                    write(iunit,fmt='(A)',advance='NO') string2
                end if
            else
                str = str // string2
                if (add_line_break) str = str // newline
            end if
            
            !cleanup:
            if (allocated(string2)) deallocate(string2)
        end subroutine write_it

    end subroutine json_value_print


! PUBLIC INTERFACE : JSON_GET

    subroutine json_get_by_path(this, path, p, found)
        !
        ! Purpose:
        !   Returns the json_value pointer given the path string.
        !
        ! Usage:
        !    type(json_value),pointer :: dat,p
        !    logical :: found
        !    ...
        !    call json_get(dat,'data(2).version',p,found)
        !
        ! Notes:
        !   The following special characters are used to denote paths:
        !    $         - root
        !    @         - this
        !    .         - child object member
        !    [] or ()  - child array element
        !
        implicit none
        type(json_value),pointer,intent(in)  :: this
        character(kind=CK,len=*),intent(in)  :: path
        type(json_value),pointer,intent(out) :: p
        logical(LK),intent(out),optional     :: found
        
        character(kind=CK,len=1),parameter :: start_array_alt = '('
        character(kind=CK,len=1),parameter :: end_array_alt   = ')'

        integer(IK) :: i, length, child_i
        character(kind=CK,len=1) :: c
        logical(LK) :: array
        type(json_value),pointer :: tmp

        if (.not. exception_thrown) then
            nullify(p)

            ! default to assuming relative to this
            p => this
            child_i = 1
            array = .false.
            length = len_trim(path)
            
            do i=1, length
                c = path(i:i)
                select case (c)
                    case ('$')
                        ! root
                        do while (associated (p%parent))
                            p => p%parent
                        end do
                        child_i = i + 1

                    case ('@')
                        ! this
                        p => this
                        child_i = i + 1

                    case ('.')
                        ! get child member from p
                        if (child_i < i) then
                            nullify(tmp)
                            call json_get_child(p, path(child_i:i-1), tmp)
                            p => tmp
                            nullify(tmp)
                        else
                            child_i = i + 1
                            cycle
                        end if

                        if (.not. associated(p)) then
                            call throw_exception('Error in json_get_by_path:'//&
                                                ' Error getting child member.')
                            exit
                        end if
                        child_i = i+1

                    case (start_array,start_array_alt)
                        ! start looking for the array element index
                        array = .true.

                        ! get child member from p
                        if (child_i < i) then
                            nullify(tmp)
                            call json_get_child(p, path(child_i:i-1), tmp)
                            p => tmp
                            nullify(tmp)
                        else
                            child_i = i + 1
                            cycle
                        end if
                        if (.not. associated(p)) then
                            call throw_exception('Error in json_get_by_path:'//&
                                                ' Error getting array element')
                            exit
                        end if
                        child_i = i + 1

                    case (end_array,end_array_alt)

                        if (.not.array) then
                            call throw_exception('Error in json_get_by_path: Unexpected ]')
                            exit
                        end if
                        array = .false.
                        child_i = string_to_integer(path(child_i:i-1))

                        nullify(tmp)
                        call json_get_child(p, child_i, tmp)
                        p => tmp
                        nullify(tmp)

                        child_i= i + 1

                end select
            end do

            if (exception_thrown) then

                if (present(found)) then
                    found = .false.
                    call json_clear_exceptions()
                end if

            else

                ! grab the last child if present in the path
                if (child_i <= length) then
                    nullify(tmp)
                    call json_get_child(p, path(child_i:i-1), tmp)
                    p => tmp
                    nullify(tmp)
                end if
                if (associated(p)) then
                    if (present(found)) found = .true.    !everything seems to be ok
                else
                    call throw_exception('Error in json_get_by_path:'//&
                                        ' variable not found: '//trim(path))
                    if (present(found)) then
                        found = .false.
                        call json_clear_exceptions()
                    end if
                end if
                
            end if
        else
            if (present(found)) found = .false.
        end if
    end subroutine json_get_by_path

    
    function string_to_integer(str) result(ival)
        !
        ! Purpose:
        !   Convert a string into an integer.
        !
        ! Notes:
        !   Replacement for the parse_integer function in the original code.
        !
        implicit none
        integer                              :: ival
        character(kind=CK,len=*), intent(in) :: str

        integer(IK) :: ierr

        if (.not. exception_thrown) then

            read(str,*,iostat=ierr) ival   !string to integer

            if (ierr/=0) then           !if there was an error
                ival = 0
                call throw_exception(                                           &
                    'Error in string_to_integer:'                   //          &
                    ' string cannot be converted to an integer: '   //          &
                    trim(str)                                                   &
                )
            end if

        end if

    end function string_to_integer


    function string_to_double(str) result(rval)
        !
        ! Purpose:
        !   Convert a string into a double.
        !
        implicit none

        real(RK)                             :: rval
        character(kind=CK,len=*), intent(in) :: str

        integer(IK) :: ierr

        if (.not. exception_thrown) then

            read(str,fmt=real_fmt,iostat=ierr) rval    !string to double

            if (ierr/=0) then    !if there was an error
                rval = 0.0_RK
                call throw_exception(                                           &
                    'Error in string_to_double:'                //              &
                    ' string cannot be converted to a double: ' //              &
                    trim(str)                                                   &
                )
            end if

        end if

    end function string_to_double


    subroutine json_get_integer(this, path, value, found)
        !
        ! Purpose:
        !   Get an integer value from a json_value.
        !
        implicit none

        type(json_value),pointer,intent(in) :: this
        character(kind=CK,len=*),optional   :: path
        integer(IK),intent(out)             :: value
        logical(LK),intent(out),optional    :: found

        type(json_value),pointer :: p

        if (.not. exception_thrown) then

            nullify(p)
            if (present(path)) then
                call json_get_by_path(this=this, path=path, p=p)
            else
                p => this
            end if

            if (.not. associated(p)) then

                call throw_exception(                                           &
                    'Error in json_get_integer:' //                             &
                    ' Unable to resolve path: '  //                             &
                    trim(path)                                                  &
                )

            else

                select case(p%var_type)
                    case (json_integer)
                        value = p%int_value
                    case (json_double)
                        value = int(p%dbl_value)
                    case (json_logical)
                        if (p%log_value) then
                            value = 1
                        else
                            value = 0
                        end if
                    case default
                        call throw_exception(                                   &
                            'Error in get_integer:'                 //          &
                            ' Unable to resolve value to integer: ' //          &
                            trim(path)                                          &
                        )
                end select

                nullify(p)

            end if

            if (exception_thrown) then
                if (present(found)) then
                    found = .false.
                    call json_clear_exceptions()
                end if
            else
                if (present(found)) found = .true.
            end if

        else

            value = 0
            if (present(found)) found = .false.

        end if

    end subroutine json_get_integer


    subroutine json_get_integer_vec(me, path, vec, found)
        !
        ! Purpose:
        !   Get an integer vector from a JSON value.
        !
        implicit none

        type(json_value),pointer              :: me
        character(kind=CK,len=*), intent(in)  :: path
        integer(IK), allocatable, intent(out) :: vec(:)
        logical(LK), intent(out), optional    :: found

        logical(LK) :: initialized

        initialized = .false.

        if (allocated(vec)) deallocate(vec)

        !the callback function is called for each element of the array:
        call json_get(me, path=path, array_callback=get_int_from_array, found=found)

    contains

        ! callback function for integer
        subroutine get_int_from_array(element, i, count)
            implicit none

            type(json_value),pointer,intent(in) :: element
            integer(IK),intent(in)              :: i        !index
            integer(IK),intent(in)              :: count    !size of array

            !size the output array:
            if (.not. initialized) then
                allocate(vec(count))
                initialized = .true.
            end if

            !populate the elements:
            call json_get(element, value=vec(i))

        end subroutine get_int_from_array

    end subroutine json_get_integer_vec


    subroutine json_get_double(this, path, value, found)
        !
        ! Purpose:
        !   Get a double value from a json_value.
        !
        implicit none

        type(json_value), pointer          :: this
        character(kind=CK,len=*), optional :: path
        real(RK), intent(out)              :: value
        logical(LK), intent(out), optional :: found

        type(json_value), pointer :: p

        if (.not. exception_thrown) then

            nullify(p)

            if (present(path)) then
                call json_get_by_path(this=this, path=path, p=p)
            else
                p => this
            end if

            if (.not. associated(p)) then
                call throw_exception(                                           &
                    'Error in json_get_double:' //                              &
                    ' Unable to resolve path: ' //                              &
                    trim(path)                                                  &
                )
            else
                select case (p%var_type)
                    case (json_integer)
                        value = p%int_value
                    case (json_double)
                        value = p%dbl_value
                    case (json_logical)
                        if (p%log_value) then
                            value = 1.0_RK
                        else
                            value = 0.0_RK
                        end if
                    case default
                        call throw_exception(                                   &
                            'Error in json_get_double:'            //           &
                            ' Unable to resolve value to double: ' //           &
                            trim(path)                                          &
                        )
                end select
                nullify(p)
            end if

            if (exception_thrown) then
                if (present(found)) then
                    found = .false.
                    call json_clear_exceptions()
                end if
            else
                if (present(found)) found = .true.
            end if
        else
            value = 0.0_RK
            if (present(found)) found = .false.
        end if

    end subroutine json_get_double


    subroutine json_get_double_vec(me, path, vec, found)
        !
        ! Purpose:
        !   Get a double vector from a JSON value.
        !
        implicit none
        type(json_value), pointer            :: me
        character(kind=CK,len=*), intent(in) :: path
        real(RK), allocatable, intent(out)   :: vec(:)
        logical(LK), intent(out), optional   :: found

        logical(LK) :: initialized

        initialized = .false.

        if (allocated(vec)) deallocate(vec)

        !the callback function is called for each element of the array:
        call json_get(me, path=path, array_callback=get_double_from_array, found=found)

    contains

        ! callback function for double
        subroutine get_double_from_array(element, i, count)
        implicit none

        type(json_value), pointer, intent(in) :: element
        integer(IK), intent(in)               :: i        !index
        integer(IK), intent(in)               :: count    !size of array

        !size the output array:
        if (.not. initialized) then
            allocate(vec(count))
            initialized = .true.
        end if

        !populate the elements:
        call json_get(element, value=vec(i))

        end subroutine get_double_from_array

    end subroutine json_get_double_vec

    
    subroutine json_get_logical(this, path, value, found)
        !
        ! Purpose:
        !   Get a logical value from a json_value.
        !
        implicit none
        type(json_value), pointer, intent(in) :: this
        character(kind=CK,len=*), optional    :: path
        logical(LK)                           :: value
        logical(LK), intent(out), optional    :: found

        type(json_value), pointer :: p

        if (.not. exception_thrown) then
            nullify(p)

            if (present(path)) then
                call json_get_by_path(this=this, path=path, p=p)
            else
                p => this
            end if

            if (.not. associated(p)) then
                call throw_exception(                                           &
                    'Error in json_get_logical:' //                             &
                    ' Unable to resolve path: '  //                             &
                    trim(path)                                                  &
                )
            else
                select case (p%var_type)
                    case (json_integer)
                        value = (p%int_value > 0)
                    case (json_logical)
                        value = p % log_value
                    case default
                        call throw_exception(                                   &
                            'Error in json_get_logical:'            //          &
                            ' Unable to resolve value to logical: ' //          &
                            trim(path)                                          &
                        )
                end select
                nullify(p)
            end if

            if (exception_thrown) then
                if (present(found)) then
                    found = .false.
                    call json_clear_exceptions()
                end if
            else
                if (present(found)) found = .true.
            end if
        else
            value = .false.
            if (present(found)) found = .false.
        end if

    end subroutine json_get_logical


    subroutine json_get_logical_vec(me, path, vec, found)
        !
        ! Purpose:
        !   Get a logical vector from a JSON value.
        !
        implicit none
        type(json_value), pointer, intent(in) :: me
        character(kind=CK,len=*), intent(in)  :: path
        logical(LK), allocatable, intent(out) :: vec(:)
        logical(LK), intent(out), optional    :: found

        logical(LK) :: initialized

        initialized = .false.

        if (allocated(vec)) deallocate(vec)

        !the callback function is called for each element of the array:
        call json_get(me, path=path, array_callback=get_logical_from_array, found=found)

    contains

        ! callback function for logical
        subroutine get_logical_from_array(element, i, count)
            implicit none

            type(json_value),pointer,intent(in) :: element
            integer(IK),intent(in)              :: i        !index
            integer(IK),intent(in)              :: count    !size of array

            !size the output array:
            if (.not. initialized) then
                allocate(vec(count))
                initialized = .true.
            end if

            !populate the elements:
            call json_get(element, value=vec(i))

        end subroutine get_logical_from_array

    end subroutine json_get_logical_vec


    subroutine json_get_string(this, path, value, found)
        !
        ! Purpose:
        !   Get a character string from a json_value.
        !
        implicit none
        type(json_value), pointer, intent(in)              :: this
        character(kind=CK,len=*), intent(in), optional     :: path
        character(kind=CK,len=:), allocatable, intent(out) :: value
        logical(LK), intent(out), optional                 :: found

        type(json_value),pointer :: p
        character(kind=CK,len=:),allocatable :: s,pre,post
        integer(IK) :: j,jprev,n
        character(kind=CK,len=1) :: c

        if (.not. exception_thrown) then

            nullify(p)

            if (present(path)) then
                call json_get_by_path(this=this, path=path, p=p)
            else
                p => this
            end if

            if (.not. associated(p)) then

                call throw_exception(                                           &
                    'Error in json_get_string:' //                              &
                    ' Unable to resolve path: ' //                              &
                    trim(path)                                                  &
                )

            else

                select case (p%var_type)
                
                case (json_string)
                
                    if (allocated(p%str_value)) then

                        !get the value as is:
                        s = p%str_value

                        !initialize:
                        n = len(s)
                        j = 1

                        do
                            jprev = j

                            ! look for an escape character    
                            j = index(s(j:n),backslash)    

                            ! if an escape character was found
                            if (j>0) then            
                            
                                !index in full string of the escape character:
                                j = j + (jprev-1)   

                                if (j<n) then

                                    !save the bit before the escape character:
                                    if (j>1) then
                                        pre = s( 1 : j-1 )
                                    else
                                        pre = ''
                                    end if

                                    !character after the escape character:
                                    c = s( j+1 : j+1 )

                                    select case (c)
                                        case(quotation_mark,backslash,slash,&
                                            'b','f','n','r','t')

                                            !save the bit after the escape characters:
                                            if (j+2<n) then
                                                post = s(j+2:n)
                                            else
                                                post = ''
                                            end if

                                            select case(c)
                                                case(quotation_mark,backslash,slash)
                                                    !use c as is
                                                case('b')
                                                    c = bspace
                                                case('f')
                                                    c = formfeed
                                                case('n')
                                                    c = newline
                                                case('r')
                                                    c = carriage_return
                                                case('t')
                                                    c = horizontal_tab
                                            end select

                                            s = pre//c//post
                                            n = n-1 

                                        case('u')

                                            if (j+5<=n) then
                                                j=j+4
                                            else
                                                call throw_exception(           &
                                                    'Error in json_get_string:'     //&
                                                    ' Invalid hexadecimal sequence' //&
                                                    ' in string: '//            &
                                                    trim(c)                     &
                                                )
                                                exit
                                            end if

                                        case default
                                            call throw_exception(               &
                                                'Error in json_get_string:' //  &
                                                ' unknown escape sequence in string "' //&
                                                trim(s) //                      &
                                                '" [' // backslash // c // ']'  &
                                            )
                                            exit
                                    end select

                                    j = j+1
                                else
                                    ! finished
                                    exit
                                end if

                            else
                                ! no more escape characters in the string
                                exit    
                            end if

                        end do

                        if (exception_thrown) then
                            if (allocated(value)) deallocate(value)
                        else
                            value = s
                        end if

                    else
                        call throw_exception(                                   &
                            'Error in json_get_string:' //                      &
                            ' p%value not allocated'                            &
                        )
                    end if

                case default

                    call throw_exception(                                       &
                        'Error in json_get_string:'                 //          &
                        ' Unable to resolve value to characters: '  //          &
                        trim(path)                                              &
                    )

                end select

            end if

            if (allocated(value) .and. .not. exception_thrown) then
                if (present(found)) found = .true.
            else
                if (present(found)) then
                    found = .false.
                    call json_clear_exceptions()
                end if
            end if

            !cleanup:
            if (associated(p)) nullify(p)
            if (allocated(s)) deallocate(s)
            if (allocated(pre)) deallocate(pre)
            if (allocated(post)) deallocate(post)

        else

            value = ''
            found = .false.

        end if

    end subroutine json_get_string


    subroutine json_get_string_vec(me, path, vec, found)
        !
        ! Purpose:
        !   Get a string vector from a JSON file.
        !
        implicit none
        type(json_value), pointer, intent(in)              :: me
        character(kind=CK,len=*), intent(in)               :: path
        character(kind=CK,len=*), allocatable, intent(out) :: vec(:)
        logical(LK), intent(out), optional                 :: found

        logical(LK) :: initialized

        initialized = .false.

        if (allocated(vec)) deallocate(vec)

        !the callback function is called for each element of the array:
        call json_get(me, path=path, array_callback=get_chars_from_array, found=found)

    contains

        ! callback function for chars
        subroutine get_chars_from_array(element, i, count)
            implicit none

            type(json_value),pointer,intent(in) :: element
            integer(IK),intent(in)              :: i        !index
            integer(IK),intent(in)              :: count    !size of array

            character(kind=CK,len=:),allocatable :: cval

            !size the output array:
            if (.not. initialized) then
                allocate(vec(count))
                initialized = .true.
            end if

            !populate the elements:
            call json_get(element, value=cval)
            if (allocated(cval)) then
                vec(i) = cval
                deallocate(cval)
            else
                vec(i) = ''
            end if
        end subroutine get_chars_from_array

    end subroutine json_get_string_vec


    subroutine json_get_array(this, path, array_callback, found)
        !
        ! Purpose:
        !   This routine calls the user-supplied array_callback subroutine
        !   for each element in the array.
        ! Note: 
        !   for integer, double, logical, and character arrays,
        !   a higher-level routine is provided (see json_get), so
        !   this routine does not have to be used for those cases.
        !

        implicit none
            type(json_value), pointer, intent(in)          :: this
            character(kind=CK,len=*), intent(in), optional :: path
            procedure(array_callback_func)                 :: array_callback
            logical(LK), intent(out), optional             :: found

            type(json_value),pointer :: element,p
            integer(IK) :: i, count

            if (.not. exception_thrown) then

                nullify(p)

                ! resolve the path to the value
                if (present(path)) then
                    call json_get_by_path(this=this, path=path, p=p)
                else
                    p => this
                end if

                if (.not. associated(p)) then

                    call throw_exception(                                       &
                        'Error in json_get_array:'  //                          &
                        ' Unable to resolve path: ' //                          &
                        trim(path)                                              &
                    )

                else

                    select case (p%var_type)
                    case (json_array)
                        count = json_count(p)
                        element => p%children
                        do i = 1, count ! callback for each child
                            call array_callback(element, i, count)
                            element => element%next
                        end do
                    case default
                        call throw_exception(                                   &
                            'Error in json_get_array:'          //              &
                            ' Resolved value is not an array. ' //              &
                            trim(path)                                          &
                        )
                    end select

                    !cleanup:
                    if (associated(p))       nullify(p)
                    if (associated(element)) nullify(element)

                end if

                if (exception_thrown) then
                    if (present(found)) then
                        found = .false.
                        call json_clear_exceptions()
                    end if
                else
                    if (present(found)) found = .true.
                end if

            else
                if (present(found)) found = .false.
            end if

    end subroutine json_get_array

 
    subroutine json_parse(file, p, unit, str)
        !
        ! Purpose:
        !   Parse the JSON file and populate the json_value tree.
        !
        !   Inputs can be:
        !     file and unit : the specified unit is used to read JSON from file
        !                     [note if unit is already open, then the filename
        !                     is ignored]
        !     file          : JSON is read from file using internal unit number
        !     str           : JSON data is read from the string instead
        !
        ! Usage:
        !   type(json_value),pointer :: p
        !   call json_parse(file='myfile.json', p=p)
        !  
        ! Notes:
        !   When calling this routine, any exceptions thrown from previous
        !   calls will automatically be cleared.
        !
        implicit none

        character(kind=CK,len=*), intent(in), optional :: file  !JSON file name
        type(json_value), pointer                      :: p     !output structure
        integer(IK), intent(in), optional              :: unit  !file unit number (/= 0)
        character(kind=CK,len=*), intent(in), optional :: str   !string with JSON data

        integer(IK)                           :: iunit, istat
        integer(IK)                           :: i, i_nl_prev, i_nl
        character(kind=CK,len=:), allocatable :: line, arrow_str
        character(kind=CK,len=10)             :: line_str, char_str
        logical(LK)                           :: is_open
        character(len=:),allocatable          :: buffer

        !clear any exceptions and initialize:
        call json_initialize()

        if (present(unit) .and. present(file) .and. .not. present(str)) then
            
            if (unit==0) then
                call throw_exception(                                           &
                    'Error in json_parse: unit number must not be 0.'           &
                )
                return      
            end if
            
            iunit = unit 
            
            ! check to see if the file is already open
            ! if it is, then use it, otherwise open the file with the name given.
            inquire(unit=iunit, opened=is_open, iostat=istat)
            if (istat==0 .and. .not. is_open) then
            ! open the file
                open (                                                          &
                    unit        = iunit,                                        &
                    file        = file,                                         &
                    status      = 'OLD',                                        &
                    action      = 'READ',                                       &
                    form        = 'FORMATTED',                                  &
                    position    = 'REWIND',                                     &
                    iostat      = istat                                         &
                )
            end if
            
        elseif (.not. present(unit) .and. present(file) .and. .not. present(str)) then
        
            ! open the file with a new unit number:
            open (                                                              &
                newunit     = iunit,                                            &
                file        = file,                                             &
                status      = 'OLD',                                            &
                action      = 'READ',                                           &
                form        = 'FORMATTED',                                      &
                position    = 'REWIND',                                         &
                iostat      = istat                                             &
            )
        
        elseif (.not. present(unit) .and. .not. present(file) .and. present(str)) then
        
            buffer = str
            iunit  = 0    !indicates that json data will be read from buffer
            istat  = 0
            
        else
            call throw_exception('Error in json_parse: Invalid inputs')
            return
        end if

        if (istat==0) then

            ! create the value and associate the pointer
            call json_value_create(p)
            
            ! Note: the name of the root json_value doesn't really matter,
            !  but we'll allocate something here just in case.
            if (present(file)) then
                p%name = trim(file)  !use the file name
            else
                p%name = ''          !if reading it from the string
            end if
            
            ! parse as a value
            call parse_value(unit=iunit, str=buffer, value=p)
            
            ! cleanup:
            if (allocated(buffer)) deallocate(buffer)

            !
            !  If there was an error reading the file, then
            !   print the line where the error occurred:
            !
            if (exception_thrown) then
                
                !the counters for the current line and the last character read:
                call integer_to_string(line_count, line_str)
                call integer_to_string(char_count, char_str)
                
                !draw the arrow string that points to the current character:
                arrow_str = repeat('-',max( 0, char_count - 1) )//'^'
            
                if (iunit/=0) then
                
                    call get_current_line_from_file(iunit,line)
                    
                else
                
                    !get the current line from the string:
                    ! [this is done by counting the newline characters]
                    i_nl_prev = 0  !index of previous newline character
                    do i=1,line_count
                        i_nl = index(str(i_nl_prev+1:),newline)
                        if (i_nl==0) then   !last line - no newline character
                            i_nl = len(str)+1
                            exit
                        end if
                        i_nl = i_nl + i_nl_prev   !index of current newline character
                        i_nl_prev = i_nl          !update for next iteration
                    end do
                    line = str(i_nl_prev+1 : i_nl-1)  !extract current line
                    
                end if
                
                !create the error message:
                err_message = err_message//newline//&
                            'line: '//trim(adjustl(line_str))//', '//&
                            'character: '//trim(adjustl(char_str))//newline//&
                            trim(line)//newline//arrow_str
                                            
                if (allocated(line)) deallocate(line)
                            
            end if

            ! close the file if necessary
            if (iunit/=0) close(unit=iunit, iostat=istat)

        else

            call throw_exception('Error in json_parse: Error opening file: '//trim(file))
            nullify(p)

        end if

    end subroutine json_parse


    subroutine get_current_line_from_file(iunit, line)
        !
        ! Purpose:
        !   Rewind the file to the beginning of the current line, and return this line.
        !   The file is assumed to be opened.
        !
        implicit none
    
        integer(IK), intent(in)                            :: iunit
        character(kind=CK,len=:), allocatable, intent(out) :: line
        
        integer(IK), parameter              :: n_chunk = 256   ! chunk size [arbitrary]
        character(kind=CK,len=*), parameter :: nfmt = '(A256)' ! corresponding format statement
        
        character(kind=CK,len=n_chunk) :: chunk
        integer(IK) :: istat, isize
    
        ! initialize:
        line = ''

        ! rewind to beginning of the current record:
        backspace(iunit, iostat=istat)

        ! loop to read in all the characters in the current record.
        ! [the line is read in chunks until the end of the line is reached]
        if (istat==0) then
            do
                read(iunit, fmt=nfmt, advance='NO', size=isize, iostat=istat) chunk
                if (istat==0) then            
                    line = line // chunk
                else
                    if (isize>0) line = line // chunk(1:isize)
                    exit
                end if
            end do
        end if
    
    end subroutine get_current_line_from_file

    
    recursive subroutine parse_value(unit, str, value)
        !
        ! Purpose:
        !   Core parsing routine.
        !
        implicit none

        integer(IK), intent(in)                              :: unit
        character(kind=CK,len=:), allocatable, intent(inout) :: str
        type(json_value), pointer                            :: value

        logical(LK) :: eof
        character(kind=CK,len=1) :: c
        character(kind=CK,len=:), allocatable :: tmp

        if (.not. exception_thrown) then

            !the routine is being called incorrectly.
            if (.not. associated(value)) then
                call throw_exception(                                           &
                    'Error in parse_value: value pointer not associated.'       &
                )
            end if

            ! pop the next non whitespace character off the file
            c = pop_char(unit, str=str, eof = eof, skip_ws = .true.)

            if (eof) then
                return
            else
                select case (c)
                    case (start_object)

                        call to_object(value)
                        call parse_object(unit, str, value)

                    case (start_array)

                        call to_array(value)
                        call parse_array(unit, str, value)

                    case (end_array)

                        call push_char(c)
                        nullify(value)

                    case (quotation_mark)

                        call to_string(value)

                        select case (value%var_type)
                            case (json_string)
                                call parse_string(unit, str, tmp)
                                value%str_value = tmp
                                deallocate(tmp)
                        end select

                    case (true_str(1:1))

                        call parse_for_chars(unit, str, true_str(2:))
                        if (.not. exception_thrown) call to_logical(value,.true.)    

                    case (false_str(1:1))

                        call parse_for_chars(unit, str, false_str(2:))
                        if (.not. exception_thrown) call to_logical(value,.false.)

                    case (null_str(1:1))

                        call parse_for_chars(unit, str, null_str(2:))
                        if (.not. exception_thrown) call to_null(value)

                    case('-', '0': '9')

                        call push_char(c)
                        call parse_number(unit, str, value)

                    case default

                        call throw_exception(                                   &
                            'Error in parse_value:'                        //   &
                            ' Unexpected character while parsing value. "' //   &
                            c // '"'                                            &
                        )

                end select
            end if
        end if
    end subroutine parse_value


    subroutine json_create_logical(me, val, name)
        !
        ! Purpose:
        !   Allocate a json_value pointer and make it a logical variable.
        !   The pointer should not already be allocated.
        !
        ! Usage:
        !   type(json_value),pointer :: p
        !   call json_create(p,'value',.true.)
        !
        implicit none
        
        type(json_value), pointer            :: me
        character(kind=CK,len=*), intent(in) :: name
        logical(LK), intent(in)              :: val
        
        call json_value_create(me)
        call to_logical(me,val,name)   

    end subroutine json_create_logical


    subroutine json_create_integer(me, val, name)
        !
        ! Purpose:
        !   Allocate a json_value pointer and make it an integer variable.
        !   The pointer should not already be allocated.
        !
        ! Usage:
        !   type(json_value),pointer :: p
        !   call json_create(p,'value',1)
        !
        implicit none
        
        type(json_value),pointer            :: me
        character(kind=CK,len=*),intent(in) :: name
        integer(IK),intent(in)              :: val
        
        call json_value_create(me)
        call to_integer(me,val,name)    

    end subroutine json_create_integer


    subroutine json_create_double(me, val, name)
        !
        ! Purpose:
        !   Allocate a json_value pointer and make it a double variable.
        !   The pointer should not already be allocated.
        !
        ! Usage:
        !   type(json_value),pointer :: p
        !   call json_create(p,'value',1.0d0)
        !
        implicit none
        type(json_value), pointer            :: me
        character(kind=CK,len=*), intent(in) :: name
        real(RK), intent(in)                 :: val
        
        call json_value_create(me)
        call to_double(me,val,name)    
    
    end subroutine json_create_double

    
    subroutine json_create_string(me, val, name)
        !
        ! Purpose:
        !   Allocate a json_value pointer and make it a string variable.
        !   The pointer should not already be allocated.
        !
        ! Usage:
        !   type(json_value),pointer :: p
        !   call json_create(p,'value','hello')
        !
        implicit none
        
        type(json_value),pointer            :: me
        character(kind=CK,len=*),intent(in) :: name
        character(kind=CK,len=*),intent(in) :: val
        
        call json_value_create(me)
        call to_string(me,val,name)    
    
    end subroutine json_create_string


    subroutine json_create_null(me, name)
        !
        ! Purpose:
        !   Allocate a json_value pointer and make it a null variable.
        !   The pointer should not already be allocated.
        !
        ! Usage:
        !   type(json_value),pointer :: p
        !   call json_create(p,'value')
        !
        implicit none
        
        type(json_value), pointer            :: me
        character(kind=CK,len=*), intent(in) :: name
        
        call json_value_create(me)
        call to_null(me,name)    
    
    end subroutine json_create_null


    subroutine json_create_object(me, name)
        implicit none
        
        type(json_value), pointer            :: me
        character(kind=CK,len=*), intent(in) :: name
        
        call json_value_create(me)
        call to_object(me,name)    
    
    end subroutine json_create_object


    subroutine json_create_array(me, name)
        !
        ! Purpose:
        !   Allocate a json_value pointer and make it an array variable.
        !   The pointer should not already be allocated.
        !
        ! Usage:
        !   type(json_value),pointer :: p
        !   call json_create(p,'arrayname')
        !
        implicit none
        type(json_value),pointer            :: me
        character(kind=CK,len=*),intent(in) :: name
        
        call json_value_create(me)
        call to_array(me,name)    
    
    end subroutine json_create_array


    subroutine to_logical(me, val, name)
        !
        ! Purpose:
        !   Change the variable to a logical.
        !
        implicit none
        type(json_value), intent(inout)                :: me
        logical(LK), intent(in), optional              :: val
        character(kind=CK,len=*), intent(in), optional :: name

        !set type and value:
        call destroy_json_data(me)
        me%var_type = json_logical
        allocate(me%log_value)

        if (present(val)) then
            me%log_value = val
        else
            me%log_value = .false.    !default value
        end if

        !name:
        if (present(name)) me%name = trim(name)

    end subroutine to_logical


    subroutine to_integer(me, val, name)
        !
        ! Purpose:
        !   Change the variable to an integer.
        !
        implicit none

        type(json_value),intent(inout)               :: me
        integer(IK),intent(in),optional              :: val
        character(kind=CK,len=*),intent(in),optional :: name

        !set type and value:
        call destroy_json_data(me)
        me%var_type = json_integer
        allocate(me%int_value)
        if (present(val)) then
            me%int_value = val
        else
            me%int_value = 0    !default value
        end if

        !name:
        if (present(name)) me%name = trim(name)

    end subroutine to_integer


    subroutine to_double(me, val, name)
        !
        ! Purpose:
        !    Change the variable to a double.
        !
        implicit none
        type(json_value), intent(inout)                :: me
        real(RK), intent(in), optional                 :: val
        character(kind=CK,len=*), intent(in), optional :: name

        !set type and value:
        call destroy_json_data(me)
        me%var_type = json_double
        allocate(me%dbl_value)
        if (present(val)) then
            me%dbl_value = val
        else
            me%dbl_value = 0.0_RK    !default value
        end if

        !name:
        if (present(name)) me%name = trim(name)

    end subroutine to_double


    subroutine to_string(me, val, name)
        !
        ! Purpose:
        !   Change the variable to a string.
        !
        implicit none

        type(json_value), intent(inout)                :: me
        character(kind=CK,len=*), intent(in), optional :: val
        character(kind=CK,len=*), intent(in), optional :: name

        !set type and value:
        call destroy_json_data(me)
        me%var_type = json_string
        if (present(val)) then
            me%str_value = val
        else
            me%str_value = ''    !default value
        end if

        !name:
        if (present(name)) me%name = trim(name)

    end subroutine to_string


    subroutine to_null(me,name)
        !
        ! Purpose:
        !   Change the variable to a null.
        !
        implicit none
        type(json_value),intent(inout)               :: me
        character(kind=CK,len=*),intent(in),optional :: name

        !set type and value:
        call destroy_json_data(me)
        me%var_type = json_null

        !name:
        if (present(name)) me%name = trim(name)

    end subroutine to_null


    subroutine to_object(me,name)
        !
        ! Purpose:
        !   Change the variable to an object.
        !
        implicit none

        type(json_value),intent(inout)               :: me
        character(kind=CK,len=*),intent(in),optional :: name
        
        !set type and value:
        call destroy_json_data(me)
        me%var_type = json_object

        !name:
        if (present(name)) me%name = trim(name)

    end subroutine to_object


    subroutine to_array(me, name)
        !
        !  DESCRIPTION
        !    Change the variable to an array.
        !
        implicit none

        type(json_value), intent(inout)                :: me
        character(kind=CK,len=*), intent(in), optional :: name

        !set type and value:
        call destroy_json_data(me)
        me%var_type = json_array

        !name:
        if (present(name)) me%name = trim(name)

    end subroutine to_array

! CORE PARSING SUBROUTINES

    recursive subroutine parse_object(unit, str, parent)
        !
        ! Purpose:
        !   Core parsing routine.
        !
        implicit none

        integer(IK), intent(in)  :: unit
        character(kind=CK,len=:), allocatable, intent(inout) :: str
        type(json_value),pointer :: parent

        type(json_value),pointer :: pair
        logical(LK)              :: eof
        character(kind=CK,len=1) :: c
        character(kind=CK,len=:),allocatable :: tmp 

        if (.not. exception_thrown) then

            !the routine is being called incorrectly.
            if (.not. associated(parent)) then
                call throw_exception(                                           &
                    'Error in parse_object: parent pointer not associated.'     &
                )
            end if

            nullify(pair)    !probably not necessary

            ! pair name
            c = pop_char(unit, str=str, eof = eof, skip_ws = .true.)
            if (eof) then
                call throw_exception(                                           &
                    'Error in parse_object:' //                                 &
                    ' Unexpected end of file while parsing start of object.'    &
                )
                return
            else if (end_object == c) then
                ! end of an empty object
                return
            else if (quotation_mark == c) then
                call json_value_create(pair)
                call parse_string(unit, str, tmp)
                pair % name = tmp
                deallocate(tmp)
                if (exception_thrown) then
                    call json_destroy(pair)
                    return
                end if
            else
                call throw_exception(                                           &
                    'Error in parse_object: Expecting string: "'//c//'"'        &
                )
                return
            end if

            ! pair value
            c = pop_char(unit, str=str, eof = eof, skip_ws = .true.)
            if (eof) then
                call throw_exception(                                           &
                    'Error in parse_object:' //                                 &
                    ' Unexpected end of file while parsing object member.'      &
                )
                return
            else if (colon_char == c) then
                ! parse the value
                call parse_value(unit, str, pair)
                if (exception_thrown) then
                    call json_destroy(pair)
                    return
                else
                    call json_add(parent, pair)
                end if
            else
                call throw_exception(                                           &
                    'Error in parse_object:' //                                 &
                    ' Expecting : and then a value: '// c                       &
                )
                return
            end if

            ! another possible pair
            c = pop_char(unit, str=str, eof = eof, skip_ws = .true.)
            if (eof) then
                call throw_exception(                                           &
                    'Error in parse_object: ' //                                &
                    'End of file encountered when parsing an object'            &
                )            
                return
            else if (delimiter == c) then
                ! read the next member
                call parse_object(unit = unit, str=str, parent = parent)
            else if (end_object == c) then
                ! end of object
                return
            else
                call throw_exception(                                           &
                    'Error in parse_object: Expecting end of object: ' // c     &
                )
                return
            end if

        end if

    end subroutine parse_object


    recursive subroutine parse_array(unit, str, array)
        !
        ! Purpose:
        !   Core parsing routine.
        !
        implicit none

        integer(IK), intent(in)   :: unit
        character(kind=CK,len=:), allocatable, intent(inout) :: str
        type(json_value) ,pointer :: array

        type(json_value), pointer :: element
        logical(LK)               :: eof
        character(kind=CK,len=1)  :: c
        
        do
            if (exception_thrown) exit

            ! try to parse an element value
            nullify(element)
            call json_value_create(element)
            call parse_value(unit, str, element)
            if (exception_thrown) then
                if (associated(element)) call json_destroy(element)
                exit
            end if
            
            ! parse value will disassociate an empty array value
            if (associated(element)) call json_add(array, element)

            ! popped the next character
            c = pop_char(unit, str=str, eof = eof, skip_ws = .true.)

            if (eof) then
                ! The file ended before array was finished:
                call throw_exception(                                           &
                    'Error in parse_array: ' //                                 &
                    'End of file encountered when parsing an array.'            &
                )
                exit
            else if (delimiter == c) then
                ! parse the next element
                cycle
            else if (end_array == c) then
                ! end of array
                exit
            else
                call throw_exception(                                           &
                    'Error in parse_array: ' //                                 &
                    'Unexpected character encountered when parsing array.'      &
                )
                exit
            end if
        end do

    end subroutine parse_array

    
    subroutine parse_string(unit, str, string)
        !
        ! Purpose:
        !   Parses a string while reading a json file
        !
        implicit none

        integer(IK), intent(in)                              :: unit
        character(kind=CK,len=:), allocatable, intent(inout) :: str
        character(kind=CK,len=:), allocatable, intent(out)   :: string

        logical(LK) :: eof, is_hex, escape
        character(kind=CK,len=1) :: c, last
        character(kind=CK,len=4) :: hex
        integer(IK) :: i
        
        !at least return a blank string if there is a problem:
        string = '' 

        if (.not. exception_thrown) then

            !initialize:
            last = space
            is_hex = .false.
            escape = .false.
            i = 0
            
            do
            
                !get the next character from the file:
                c = pop_char(unit, str=str, eof = eof, skip_ws = .false.)
            
                if (eof) then
                
                    call throw_exception(                                       &
                        'Error in parse_string: Expecting end of string'        &
                    )                                                           &
                    return
                    
                else if (quotation_mark == c .and. last /= backslash) then
                
                    if (is_hex) call throw_exception(                           &
                        'Error in parse_string:'     //                         &
                        ' incomplete hex string: \u' // trim(hex)               &
                    )
                    exit
                        
                else
                
                    !append to string:
                    string = string // c    
                    
                    !hex validation:
                    if (is_hex) then  !accumulate the four characters after '\u'
                        
                        i = i+1
                        hex(i:i) = c
                        if (i==4) then
                            if (valid_json_hex(hex)) then
                                i = 0
                                hex = ''
                                is_hex = .false.                            
                            else
                                call throw_exception(                           &
                                    'Error in parse_string:'  //                &
                                    ' invalid hex string: \u' // trim(hex)      &
                                )
                                exit
                            end if
                        end if
                        
                    else
                        
                        if (escape) then
                            escape = .false.
                            is_hex = (c=='u')
                        else
                            escape = (c==backslash)
                        end if
                        
                    end if
                    
                    !update for next char:
                    last = c
                    
                end if
                
            end do

        end if

    end subroutine parse_string


    subroutine parse_for_chars(unit, str, chars)
        !
        ! Purpose:
        !    Core parsing routine.
        !
        implicit none

        integer(IK), intent(in)                              :: unit
        character(kind=CK,len=:), allocatable, intent(inout) :: str
        character(kind=CK,len = *), intent(in)               :: chars

        integer(IK) :: i, length
        logical(LK) :: eof
        character(kind=CK,len=1) :: c

        if (.not. exception_thrown) then

            length = len_trim(chars)

            do i = 1, length
                c = pop_char(unit, str=str, eof = eof, skip_ws = .true.)
                if (eof) then
                    call throw_exception(                                       &
                        'Error in parse_for_chars:' //                          &
                        ' Unexpected end of file while parsing array.'          &
                    )
                    return
                else if (c /= chars(i:i)) then
                    call throw_exception(                                       &
                        'Error in parse_for_chars:' //                          &
                        ' Unexpected character.: "' // c // '" ' // chars(i:i)  &
                    )
                    return
                end if
            end do
        end if
    end subroutine parse_for_chars


    subroutine parse_number(unit, str, value)
        !
        ! Purpose:
        !   Read a numerical value from the file.
        !   The routine will determine if it is an integer or a double, and
        !   allocate the type accordingly.
        !
        ! Notes:
        !   Complete rewrite of the original FSON routine, which had 
        !   some problems.
        !
        implicit none

        integer(IK), intent(in)                              :: unit
        character(kind=CK,len=:), allocatable, intent(inout) :: str
        type(json_value), pointer                            :: value

        character(kind=CK,len=:), allocatable :: tmp
        character(kind=CK,len=1) :: c
        logical(LK) :: eof
        real(RK)    :: rval
        integer(IK) :: ival
        logical(LK) :: first
        logical(LK) :: is_integer

        if (.not. exception_thrown) then

            tmp = ''
            first = .true.
            is_integer = .true.

            do
                !get the next character:
                c = pop_char(unit, str=str, eof = eof, skip_ws = .true.)

                if (eof) then
                    call throw_exception('Error in parse_number:'//&
                                        ' Unexpected end of file while parsing number.')
                    return
                else

                    select case (c)
                    case('-','+')    !note: allowing a '+' as the first character here.

                        if (is_integer .and. (.not. first)) is_integer = .false.

                        !add it to the string:
                        tmp = tmp // c

                    case('.','E','e')    !can be present in real numbers

                        if (is_integer) is_integer = .false.

                        !add it to the string:
                        tmp = tmp // c

                    case('0':'9')    !valid characters for numbers

                        !add it to the string:
                        tmp = tmp // c

                    case default

                        !push back the last character read:
                        call push_char(c)

                        !string to value:
                        if (is_integer) then
                            ival = string_to_integer(tmp)
                            call to_integer(value,ival)
                        else
                            rval = string_to_double(tmp)
                            call to_double(value,rval)
                        end if

                        exit    !finished

                    end select

                end if
                if (first) first = .false.

            end do

            !cleanup:
            if (allocated(tmp)) deallocate(tmp)

        end if

    end subroutine parse_number


    recursive function pop_char(unit, str, eof, skip_ws) result(popped)
        !
        ! Purpose:
        !   Get the next character from the file (or string).
        !
        ! Notes:
        !   This routine ignores non-printing ascii characters (iachar<=31) that
        !   are in strings.
        !
        implicit none

        character(kind=CK,len=1)                             :: popped
        integer(IK), intent(in)                              :: unit
        character(kind=CK,len=:), allocatable, intent(inout) :: str
        logical(LK), intent(out)                             :: eof
        logical(LK), intent(in), optional                    :: skip_ws

        integer(IK) :: ios
        character(kind=CK,len=1) :: c
        logical(LK) :: ignore
        integer(IK) :: str_len

        if (.not. exception_thrown) then

            eof = .false.
            if (.not.present(skip_ws)) then
                ignore = .false.
            else
                ignore = skip_ws
            end if

            do

                if (pushed_index > 0) then

                    ! there is a character pushed back on, most likely from the number parsing
                    c = pushed_char(pushed_index:pushed_index)
                    pushed_index = pushed_index - 1

                else

                    if (unit/=0) then    !read from the file
                        read (unit = unit, fmt = '(A1)', advance = 'NO', iostat = ios) c
                    else    !read from the string
                        str_len = len(str)   !length of the string
                        if (str_len>0) then
                            c = str(1:1)
                            if (str_len>1) then
                                str = str(2:str_len)  !remove the character that was read
                            else
                                str = ''    !that was the last one
                            end if
                            ios = 0
                        else
                            ios = IOSTAT_END  !end of the string
                        end if
                    end if
                    
                    char_count = char_count + 1    !character count in the current line
                                        
                    if (IS_IOSTAT_EOR(ios) .or. c==newline) then    !end of record
            
                        char_count = 0
                        line_count = line_count + 1
                        cycle
        
                    else if (IS_IOSTAT_END(ios)) then  !end of file
                    
                        char_count = 0
                        eof = .true.
                        exit
        
                    end if
                        
                end if
                
                if (iachar(c) <= 31) then         !JW : fixed so it will read spaces 
                                                !      in the string (was 32)

                    ! non printing ascii characters
                    cycle

                else if (ignore .and. c == space) then

                    cycle

                else

                    popped = c
                    exit

                end if

            end do

        end if

    end function pop_char


    subroutine push_char(c)
        implicit none
        character(kind=CK,len=1), intent(in) :: c

        character(kind=CK,len=max_numeric_str_len) :: istr

        if (.not. exception_thrown) then

            pushed_index = pushed_index + 1

            if (pushed_index>0 .and. pushed_index<=len(pushed_char)) then
                pushed_char(pushed_index:pushed_index) = c
            else
                call integer_to_string(pushed_index,istr)
                call throw_exception(                                           &
                    'Error in push_char:' //                                    &
                    ' invalid valid of pushed_index: ' // trim(istr)            &
                )
            end if

        end if

    end subroutine push_char


    subroutine integer_to_string(ival, str)
        !
        ! Purpose:
        !   Convert an integer to a string.
        !
        implicit none

        integer(IK), intent(in)               :: ival
        character(kind=CK,len=*), intent(out) :: str

        integer(IK) :: istat

        write(str,fmt=int_fmt,iostat=istat) ival

        if (istat==0) then
            str = adjustl(str)
        else
            str = repeat(star,len(str))
        end if

    end subroutine integer_to_string


    subroutine real_to_string(rval,str)
        !
        !  DESCRIPTION
        !    Convert a real value to a string.
        !
        implicit none

        real(RK),intent(in)                  :: rval
        character(kind=CK,len=*),intent(out) :: str

        integer(IK) :: istat

        write(str,fmt=real_fmt,iostat=istat) rval

        if (istat==0) then
            str = adjustl(str)
        else
            str = repeat(star,len(str))
        end if

    end subroutine real_to_string

    
    function valid_json_hex(str) result(valid)
        !
        ! Purpose:
        !    Returns true if the string is a valid 4-digit hex string.
        !
        ! Example:
        !    valid_json_hex('0000')  !returns true
        !    valid_json_hex('ABC4')  !returns true    
        !    valid_json_hex('AB')    !returns false (< 4 characters)
        !    valid_json_hex('WXYZ')  !returns false (invalid characters)
        !
        implicit none

        logical(LK)                         :: valid
        character(kind=CK,len=*),intent(in) :: str
        
        integer(IK) :: n,i
        
        !an array of the valid hex characters:
        character(kind=CK,len=1),dimension(16),parameter :: valid_chars =       &
            ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']

        !initialize
        valid = .false.   
        
        !check all the characters in the string:
        n = len(str)
        if (n==4) then
            do i=1,n
                if (.not. any(str(i:i)==valid_chars)) return
            end do
            valid = .true.    !all are in the set, so it is OK
        end if

    end function valid_json_hex
    
end module fpsuite_json
