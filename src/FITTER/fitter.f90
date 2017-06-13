!< FITTER, Fortran tIc Toc TimER

module fitter_snippet
!-----------------------------------------------------------------------------------------------------------------------------------
!< FITTER **snippet** class.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: snippet
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: snippet
  !< **Snippet** class.
  character(len=:), allocatable      :: name              !< Name of the snippet.
  real(R8P)                          :: time=0._R8P       !< Elapsed time in the snippet.
  integer(I8P), private, allocatable :: tic_toc_(:,:)     !< Tic toc storage.
  integer(I8P), private              :: count_rate_=0     !< Count rate.
  integer(I8P), private              :: tic_toc_number_=0 !< Tic toc pairs number.
  contains
    ! public methods
    procedure, pass(self) :: add_timing     => snippet_add_timing     !< Add timing data to the snippet.
    procedure, pass(self) :: clean          => snippet_clean          !< Clean snippet.
    procedure, pass(self) :: print          => snippet_print          !< Print time of the snippet.
    procedure, pass(self) :: statistics     => snippet_statistics     !< Return snippet statistics.
    procedure, pass(self) :: tic_toc_number => snippet_tic_toc_number !< Return tic toc pairs number.
endtype snippet

character(1), parameter :: NL=new_line('a') !< New line character.
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public methods
  subroutine snippet_add_timing(self, tic_toc, count_rate)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add timing data to the snippet.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(snippet), intent(inout) :: self          !< The snippet.
  integer(I8P),   intent(in)    :: tic_toc(1:)   !< Snippet timing.
  integer(I8P),   intent(in)    :: count_rate    !< Snippet count rate.
  integer(I8P), allocatable     :: tic_toc_(:,:) !< Tic toc storage.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%tic_toc_number_>0) then
    allocate(tic_toc_(1:2, 1:self%tic_toc_number_+1))
    tic_toc_(:, 1:self%tic_toc_number_) = self%tic_toc_
    self%tic_toc_number_ = self%tic_toc_number_ + 1
    call move_alloc(from=tic_toc_, to=self%tic_toc_)
  else
    self%tic_toc_number_ = 1
    allocate(self%tic_toc_(1:2, 1:self%tic_toc_number_))
  endif
  self%tic_toc_(1:2, self%tic_toc_number_) = tic_toc
  self%count_rate_ = count_rate
  self%time = self%time + &
              real(self%tic_toc_(2, self%tic_toc_number_) - self%tic_toc_(1, self%tic_toc_number_), kind=R8P)/self%count_rate_
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine snippet_add_timing

  elemental subroutine snippet_clean(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Clean snippet.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(snippet), intent(inout) :: self !< The snippet.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%name)) deallocate(self%name)
  self%time = 0._R8P
  if (allocated(self%tic_toc_)) deallocate(self%tic_toc_)
  self%count_rate_ = 0
  self%tic_toc_number_ = 0
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine snippet_clean

  subroutine snippet_print(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Print time of the snippet.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(snippet), intent(in) :: self !< The snippet.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  print '(A)', 'Elapsed time into "'//self%name//'": '//trim(str(self%time, .true.))//' [s]'
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine snippet_print

  pure function snippet_statistics(self, prefix, zpad) result(statistics)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return snippet statistics.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(snippet), intent(in)           :: self       !< The timer.
  character(*),   intent(in), optional :: prefix     !< Prefixing string.
  integer(I4P),   intent(in), optional :: zpad       !< Zero padding of hits number counter.
  character(len=:), allocatable        :: statistics !< Timer statistics.
  character(len=:), allocatable        :: prefix_    !< Prefixing string, local variable.
  real(R8P)                            :: time       !< Snippets whole time.
  integer(I4P)                         :: zpad_      !< Zero padding of hits number counter, local variable.
  integer(I4P)                         :: h          !< Counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefix_ = '' ; if (present(prefix)) prefix_ = prefix
  zpad_ = 3 ; if (present(zpad)) zpad_ = zpad
  statistics = ''
  if (self%tic_toc_number_>0) then
    statistics = prefix_//'Number of snippet hits: '//trim(str(self%tic_toc_number_, .true.))
    statistics = statistics//NL//prefix_//'Total elapsed time: '//trim(str(self%time, .true.))//' [s]'
    statistics = statistics//NL//prefix_//'Average elapsed time: '//trim(str(self%time/self%tic_toc_number_, .true.))//' [s]'
    statistics = statistics//NL//prefix_//'Relative elapsed time into each hit:'
    do h=1, self%tic_toc_number_
      time = real(self%tic_toc_(2, h) - self%tic_toc_(1, h), kind=R8P)/self%count_rate_
      statistics = statistics//NL//prefix_//'  + '//trim(strz(h, zpad_))//': '//trim(str('(F6.3)', time/self%time*100))//'%'
    enddo
    statistics = statistics//NL
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction snippet_statistics

  elemental function snippet_tic_toc_number(self) result(tic_toc_number)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return snippet statistics.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(snippet), intent(in) :: self           !< The timer.
  integer(I4P)               :: tic_toc_number !< Tic toc pairs number.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tic_toc_number = self%tic_toc_number_
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction snippet_tic_toc_number
endmodule fitter_snippet

module fitter
!-----------------------------------------------------------------------------------------------------------------------------------
!< FITTER, Fortran tIc Toc TimER
!-----------------------------------------------------------------------------------------------------------------------------------
use fitter_snippet
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: timer
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: timer
  !< **Timer** class.
  private
  ! data for the current snippet
  character(len=:), allocatable :: snippet_name            !< Name of the current snippet.
  integer(I8P)                  :: tic_toc(1:2)=[0,0]      !< Tic toc storage.
  integer(I8P)                  :: count_rate=0            !< Count rate.
  integer(I4P)                  :: snippets_counter=0      !< Snippets number counter for automic naming.
  integer(I4P)                  :: scratch_unit=0          !< Scratch file unit.
  logical                       :: is_scratch_open=.false. !< Sentinel of scratch file status.
  logical                       :: is_tracking=.false.     !< Sentinel of tracking status.
  ! data for global statistics, allocated only on parsing scratch-saved tracking
  type(snippet), allocatable :: snippets(:)       !< Snippets tracked.
  integer(I4P)               :: snippets_number=0 !< Snippets number.
  contains
    ! public methods
    procedure, pass(self) :: clean      => timer_clean      !< Clean timer.
    procedure, pass(self) :: print      => timer_print      !< Print time of a snippet or of all ones.
    procedure, pass(self) :: statistics => timer_statistics !< Return timer statistics.
    procedure, pass(self) :: tic        => timer_tic        !< Start a new snippet tracking.
    procedure, pass(self) :: time       => timer_time       !< Get time of a snippet or whole time.
    procedure, pass(self) :: times      => timer_times      !< Get time-array of all snippets.
    procedure, pass(self) :: toc        => timer_toc        !< Stop current tracking.
    ! private methods
    procedure, pass(self), private :: add_snippet        => timer_add_snippet        !< Add new snippet.
    procedure, pass(self), private :: clean_stats        => timer_clean_stats        !< Clean timer statistics data.
    procedure, pass(self), private :: parse_scratch_file => timer_parse_scratch_file !< Parse_scratch file.
    procedure, pass(self), private :: snippet_index      => timer_snippet_index      !< Return the snippet index given the name.
endtype timer

character(1), parameter :: NL=new_line('a') !< New line character.
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public methods
  subroutine timer_clean(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Clean timer.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(inout) :: self !< The timer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%snippet_name)) deallocate(self%snippet_name)
  self%tic_toc(1:2) = [0,0]
  self%count_rate = 0
  self%snippets_counter = 0
  self%scratch_unit = 0
  self%is_tracking = .false.
  if (self%is_scratch_open) close(unit=self%scratch_unit)
  self%is_scratch_open = .false.

  call self%clean_stats
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_clean

  subroutine timer_print(self, name, statistics, zpad)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Print time of a snippet or of all ones.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(inout)        :: self       !< The timer.
  character(*), intent(in), optional :: name       !< Snippet name.
  logical,      intent(in), optional :: statistics !< Print statistics.
  integer(I4P), intent(in), optional :: zpad       !< Zero padding for snippet sequential naming.
  integer(I4P)                       :: s          !< Counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%parse_scratch_file
  if (self%snippets_number>0) then
    if (present(name)) then
      do s=1, self%snippets_number
        if (self%snippets(s)%name==name) then
          call self%snippets(s)%print
          exit
        endif
      enddo
    else
      do s=1, self%snippets_number
        call self%snippets(s)%print
      enddo
    endif
    if (present(statistics)) then
      if (statistics) print '(A)', self%statistics(zpad=zpad)
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_print

  function timer_statistics(self, zpad) result(statistics)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return timer statistics.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(inout)        :: self       !< The timer.
  integer(I4P), intent(in), optional :: zpad       !< Zero padding for snippet sequential naming.
  character(len=:), allocatable      :: statistics !< Timer statistics.
  real(R8P)                          :: time       !< Snippets whole time.
  integer(I4P)                       :: s          !< Counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_tracking) error stop 'error: cannot get timer statistics during traking'
  statistics = ''
  if (self%snippets_number>0) then
    time = self%time()
    statistics = 'Number of snippets tracked: '//trim(str(self%snippets_number, .true.))//NL
    statistics = statistics//'Total elapsed time: '//trim(str(time, .true.))//' [s]'//NL
    statistics = statistics//'Average (snippet) elapsed time: '//trim(str(time/self%snippets_number, .true.))//' [s]'//NL
    statistics = statistics//'Relative elapsed time into each snippet:'//NL
    do s=1, self%snippets_number
      statistics = statistics//'  + '//self%snippets(s)%name//': '//trim(str('(F6.3)', self%snippets(s)%time/time*100))//'%'//NL
      if (self%snippets(s)%tic_toc_number()>1) statistics = statistics//self%snippets(s)%statistics(prefix='    ', zpad=zpad)
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction timer_statistics

  subroutine timer_tic(self, name, zpad)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Start a new snippet tracking.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(inout)        :: self  !< The timer.
  character(*), intent(in), optional :: name  !< Snippet name.
  integer(I4P), intent(in), optional :: zpad  !< Zero padding for snippet sequential naming.
  integer(I4P)                       :: zpad_ !< Zero padding for snippet sequential naming, local variable.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_tracking) error stop 'error: cannot start a new snippet tracking before stop the current'
  if (.not.self%is_scratch_open) open(newunit=self%scratch_unit, status='scratch', form='formatted')
  self%is_scratch_open = .true.
  zpad_ = 3 ; if (present(zpad)) zpad_ = zpad
  if (present(name)) then
    self%snippet_name = name
  else
    self%snippets_counter = self%snippets_counter + 1
    self%snippet_name = 'snippet-'//trim(strz(self%snippets_counter, zpad_))
  endif
  self%is_tracking = .true.
  call system_clock(self%tic_toc(1), self%count_rate)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_tic

  function timer_time(self, name) result(time)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get time of a snippet or whole time.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(inout)        :: self !< The timer.
  character(*), intent(in), optional :: name !< Snippet name.
  real(R8P)                          :: time !< Snippet(s) time.
  integer(I4P)                       :: s    !< Counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_tracking) error stop 'error: cannot get elapsed time during traking'
  time = 0._R8P
  call self%parse_scratch_file
  if (self%snippets_number>0) then
    if (present(name)) then
      do s=1, self%snippets_number
        if (self%snippets(s)%name==name) then
          time = self%snippets(s)%time
          exit
        endif
      enddo
    else
      do s=1, self%snippets_number
        time = time + self%snippets(s)%time
      enddo
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction timer_time

  function timer_times(self) result(times)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get time-array of all snippets.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(inout) :: self     !< The timer.
  real(R8P), allocatable      :: times(:) !< Snippets time.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_tracking) error stop 'error: cannot get elapsed time during traking'
  call self%parse_scratch_file
  if (self%snippets_number>0) then
    allocate(times(1:self%snippets_number))
    times = self%snippets(:)%time
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction timer_times

  subroutine timer_toc(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Stop current tracking.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(inout) :: self !< The timer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call system_clock(self%tic_toc(2), self%count_rate)
  if (.not.self%is_tracking) error stop 'error: there is not a snippet tracking to stop'
  self%is_tracking = .false.
  write(unit=self%scratch_unit, fmt=*) self%snippet_name, self%tic_toc(1), self%tic_toc(2), self%count_rate
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_toc

  ! private methods
  subroutine timer_add_snippet(self, name, tic_toc, count_rate)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add new snippet.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(inout) :: self        !< The timer.
  character(*), intent(in)    :: name        !< Snippet name.
  integer(I8P), intent(in)    :: tic_toc(1:) !< Snippet timing.
  integer(I8P), intent(in)    :: count_rate  !< Snippet count rate.
  type(snippet), allocatable  :: snippets(:) !< The new snippet.
  integer(I4P)                :: s           !< Counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%snippets_number>0) then
    s = self%snippet_index(name=name)
    if (s==0) then
      ! add new snippet
      allocate(snippets(1:self%snippets_number+1))
      snippets(1:self%snippets_number) = self%snippets
      snippets(self%snippets_number+1) = snippet(name=name)
      call snippets(self%snippets_number+1)%add_timing(tic_toc=tic_toc, count_rate=count_rate)
      call move_alloc(from=snippets, to=self%snippets)
      self%snippets_number = self%snippets_number + 1
    else
      ! snippet already exist
      call self%snippets(s)%add_timing(tic_toc=tic_toc, count_rate=count_rate)
    endif
  else
    ! there are not snippets at all, add the first one
    allocate(self%snippets(1:1))
    self%snippets(1) = snippet(name=name)
    call self%snippets(1)%add_timing(tic_toc=tic_toc, count_rate=count_rate)
    self%snippets_number = self%snippets_number + 1
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_add_snippet

  subroutine timer_clean_stats(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Clean timer statistics data.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(inout) :: self !< The timer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%snippets)) then
    call self%snippets%clean
    deallocate(self%snippets)
  endif
  self%snippets_number = 0
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_clean_stats

  subroutine timer_parse_scratch_file(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse scratch file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(inout) :: self                 !< The timer.
  character(99)               :: snippet_name         !< Snippet name.
  integer(I8P)                :: snippet_tic_toc(1:2) !< Snippet timing.
  integer(I8P)                :: snippet_count_rate   !< Snippet count rate.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_scratch_open) then
    call self%clean_stats
    rewind(unit=self%scratch_unit)
    do
      read(unit=self%scratch_unit, fmt=*, end=10, err=10) snippet_name, snippet_tic_toc(1), snippet_tic_toc(2), snippet_count_rate
      call self%add_snippet(name=trim(snippet_name), tic_toc=snippet_tic_toc, count_rate=snippet_count_rate)
    enddo
    10 continue
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_parse_scratch_file

  elemental function timer_snippet_index(self, name) result(snippet_index)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the snippet index given the name.
  !<
  !< Return 0 is not present
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(in) :: self          !< The timer.
  character(*), intent(in) :: name          !< Snippet name.
  integer(I4P)             :: snippet_index !< Snippet index, 0 is not present.
  integer(I4P)             :: s             !< Counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  snippet_index = 0
  if (self%snippets_number>0) then
    do s=1, self%snippets_number
      if (self%snippets(s)%name==name) then
        snippet_index = s
        exit
      endif
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction timer_snippet_index
endmodule fitter
