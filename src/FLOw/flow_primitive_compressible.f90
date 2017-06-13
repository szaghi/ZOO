!< FLOw **primitive** compressible object.

module flow_primitive_compressible
!< FLOw **primitive** compressible object.
!<
!< [[primitive_compressible]] is a class that handles compressible primitive fluid dynamic variables.

use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
use flow_eos_object, only : eos_object
use flow_field_object, only : field_object
use flow_primitive_object, only : primitive_object
use penf, only : I_P, R_P, str
use vecfor, only : vector

implicit none
private
public :: primitive_compressible
public :: primitive_compressible_pointer

type, extends(primitive_object) :: primitive_compressible
   !< **Primitive** compressible multispecie object.
   real(R_P)    :: density=0._R_P  !< Density, `rho`.
   type(vector) :: velocity        !< Velocity, `v`.
   real(R_P)    :: pressure=0._R_P !< Pressure, `p`.
   contains
      ! public methods
      procedure, pass(self), public :: left_eigenvectors  !< Return the left eigenvectors matrix `L` as `dF/dP = A = R ^ L`.
      procedure, pass(self), public :: right_eigenvectors !< Return the right eigenvectors matrix `R` as `dF/dP = A = R ^ L`.
      ! deferred methods
      procedure, pass(self), public :: array       !< Return serialized array of field.
      procedure, pass(self), public :: description !< Return pretty-printed object description.
      procedure, pass(self), public :: destroy     !< Destroy primitive.
      procedure, pass(self), public :: energy      !< Return energy value.
      procedure, pass(self), public :: initialize  !< Initialize primitive.
      procedure, pass(self), public :: momentum    !< Return momentum vector.
      procedure, pass(self), public :: normalize   !< Normalize primitive with respect a given normal vector.
      ! deferred operators
      ! +
      procedure, pass(lhs),  public :: field_add_field !< `+` operator.
      procedure, pass(lhs),  public :: field_add_real  !< `+ real` operator.
      procedure, pass(rhs),  public :: real_add_field  !< `real +` operator.
      procedure, pass(self), public :: positive        !< Unary operator `+ field`.
      ! /
      procedure, pass(lhs), public :: field_div_field          !< `/` operator.
      procedure, pass(lhs), public :: field_div_integer        !< `/ integer` operator.
      procedure, pass(rhs), public :: integer_div_field        !< `integer /` operator.
      procedure, pass(lhs), public :: field_div_integer_scalar !< `/ integer_scalar` operator.
      procedure, pass(rhs), public :: integer_scalar_div_field !< `integer_scalar /` operator.
      procedure, pass(lhs), public :: field_div_real           !< `/ real` operator.
      procedure, pass(rhs), public :: real_div_field           !< `real /` operator.
      procedure, pass(lhs), public :: field_div_real_scalar    !< `/ real_scalar` operator.
      procedure, pass(rhs), public :: real_scalar_div_field    !< `real_scalar /` operator.
      ! *
      procedure, pass(lhs), public :: field_mul_field          !< `*` operator.
      procedure, pass(lhs), public :: field_mul_integer        !< `* integer` operator.
      procedure, pass(rhs), public :: integer_mul_field        !< `integer *` operator.
      procedure, pass(lhs), public :: field_mul_integer_scalar !< `* integer_scalar` operator.
      procedure, pass(rhs), public :: integer_scalar_mul_field !< `integer_scalar *` operator.
      procedure, pass(lhs), public :: field_mul_real           !< `* real` operator.
      procedure, pass(rhs), public :: real_mul_field           !< `real *` operator.
      procedure, pass(lhs), public :: field_mul_real_scalar    !< `* real_scalar` operator.
      procedure, pass(rhs), public :: real_scalar_mul_field    !< `real_scalar *` operator.
      ! -
      procedure, pass(lhs),  public :: field_sub_field !< `-` operator.
      procedure, pass(lhs),  public :: field_sub_real  !< `- real` operator.
      procedure, pass(rhs),  public :: real_sub_field  !< `real -` operator.
      procedure, pass(self), public :: negative        !< Unary operator `- field`.
      ! **
      procedure, pass(lhs), public :: field_pow_integer !< `** integer` operator.
      procedure, pass(lhs), public :: field_pow_real    !< `** real` operator.
      ! =
      procedure, pass(lhs), public :: assign_field !< `=` operator.
      procedure, pass(lhs), public :: assign_real  !< `= real` operator.
      ! ==
      procedure, pass(lhs), public :: eq !< `==' operator.
      ! /=
      procedure, pass(lhs), public :: not_eq !< `/=' operator.
endtype primitive_compressible

interface primitive_compressible
   !< Overload [[primitive_compressible]] name with its constructor.
   module procedure primitive_compressible_instance
endinterface

contains
   ! public non TBP
   function primitive_compressible_pointer(to, error_message) result(pointer_)
   !< Return [[primitive_compressible]] pointer associated to [[primitive_object]] or its extensions until
   !< [[primitive_compressible]] included.
   !<
   !< @note A type-guard check is performed and error stop is raised if necessary.
   class(primitive_object), intent(in), target   :: to            !< Target of associate.
   character(*),            intent(in), optional :: error_message !< Auxiliary error message.
   class(primitive_compressible), pointer        :: pointer_      !< Associated pointer.

   select type(to)
   type is(primitive_compressible)
      pointer_ => to
   class default
      write(stderr, '(A)') 'error: cast primitive_object to primitive_compressible failed!'
      if (present(error_message)) write(stderr, '(A)') error_message
      stop
   endselect
   endfunction primitive_compressible_pointer

   ! public methods
   pure function left_eigenvectors(self, eos) result(eig)
   !< Return the left eigenvectors matrix `L` as `dF/dP = A = R ^ L`.
   class(primitive_compressible), intent(in) :: self          !< Primitive.
   class(eos_object),             intent(in) :: eos           !< Equation of state.
   real(R_P)                                 :: eig(1:3, 1:3) !< Eigenvectors.
   real(R_P)                                 :: gp            !< `g*p`.
   real(R_P)                                 :: gp_a          !< `g*p/a`.

   gp = eos%g() * self%pressure
   gp_a = gp / eos%speed_of_sound(density=self%density, pressure=self%pressure)
   eig(1, 1) = 0._R_P            ; eig(1, 2) = -gp_a  ; eig(1, 3) =  1._R_P
   eig(2, 1) = gp / self%density ; eig(2, 2) = 0._R_P ; eig(2, 3) = -1._R_P
   eig(3, 1) = 0._R_P            ; eig(3, 2) =  gp_a  ; eig(3, 3) =  1._R_P
   endfunction left_eigenvectors

   pure function right_eigenvectors(self, eos) result(eig)
   !< Return the right eigenvectors matrix `R` as `dF/dP = A = R ^ L`.
   class(primitive_compressible), intent(in) :: self          !< Primitive.
   class(eos_object),             intent(in) :: eos           !< Equation of state.
   real(R_P)                                 :: eig(1:3, 1:3) !< Eigenvectors.
   real(R_P)                                 :: gp            !< `g*p`.
   real(R_P)                                 :: gp_inv        !< `1/(g*p)`.
   real(R_P)                                 :: a             !< Speed of sound, `sqrt(g*p/r)`.

   gp = eos%g() * self%pressure
   gp_inv = 1._R_P / gp
   a = eos%speed_of_sound(density=self%density, pressure=self%pressure)
   eig(1, 1) =  0.5_R_P * self%density * gp_inv ; eig(1, 2) = self%density * gp_inv  ; eig(1, 3) =  eig(1, 1)
   eig(2, 1) = -0.5_R_P * a * gp_inv            ; eig(2, 2) = 0._R_P                 ; eig(2, 3) = -eig(2, 1)
   eig(3, 1) =  0.5_R_P                         ; eig(3, 2) = 0._R_P                 ; eig(3, 3) =  eig(3, 1)
   endfunction right_eigenvectors

   ! deferred methods
   pure function array(self) result(array_)
   !< Return serialized array of field.
   class(primitive_compressible), intent(in) :: self      !< Primitive.
   real(R_P), allocatable                    :: array_(:) !< Serialized array of field.

   allocate(array_(1:5))
   array_(1) = self%density
   array_(2) = self%velocity%x
   array_(3) = self%velocity%y
   array_(4) = self%velocity%z
   array_(5) = self%pressure
   endfunction array

   pure function description(self, prefix) result(desc)
   !< Return a pretty-formatted object description.
   class(primitive_compressible), intent(in)           :: self             !< Primitive.
   character(*),                  intent(in), optional :: prefix           !< Prefixing string.
   character(len=:), allocatable                       :: prefix_          !< Prefixing string, local variable.
   character(len=:), allocatable                       :: desc             !< Description.
   character(len=1), parameter                         :: NL=new_line('a') !< New line character.

   prefix_ = '' ; if (present(prefix)) prefix_ = prefix
   desc = ''
   desc = desc//prefix_//'density  = '//trim(str(n=self%density))//NL
   desc = desc//prefix_//'velocity = '//trim(str(n=[self%velocity%x, self%velocity%y, self%velocity%z]))//NL
   desc = desc//prefix_//'pressure = '//trim(str(n=self%pressure))
   endfunction description

   elemental subroutine destroy(self)
   !< Destroy primitive.
   class(primitive_compressible), intent(inout) :: self  !< Primitive.
   type(primitive_compressible)                 :: fresh !< Fresh instance of primitive object.

   self = fresh
   endsubroutine destroy

   elemental function energy(self, eos) result(energy_)
   !< Return energy value.
   class(primitive_compressible), intent(in) :: self    !< Primitive.
   class(eos_object),             intent(in) :: eos     !< Equation of state.
   real(R_P)                                 :: energy_ !< Energy value.

   energy_ = self%pressure / (eos%g() - 1._R_P) + 0.5_R_P * self%density * self%velocity%sq_norm()
   endfunction energy

   subroutine initialize(self, initial_state)
   !< Initialize primitive.
   class(primitive_compressible), intent(inout)        :: self          !< Primitive.
   class(primitive_object),       intent(in), optional :: initial_state !< Initial state.

   call self%destroy
   if (present(initial_state)) then
      select type(initial_state)
      class is(primitive_compressible)
         self = initial_state
      endselect
   endif
   endsubroutine initialize

   elemental function momentum(self) result(momentum_)
   !< Return momentum vector.
   class(primitive_compressible), intent(in) :: self      !< Primitive.
   type(vector)                              :: momentum_ !< Momentum vector.

   momentum_ = self%density * self%velocity
   endfunction momentum

   elemental subroutine normalize(self, normal)
   !< *Normalize* primitive with respect a given normal vector.
   class(primitive_compressible), intent(inout) :: self   !< Primitive.
   type(vector),                  intent(in)    :: normal !< Normal vector.

   self%velocity = self%velocity .paral. normal
   endsubroutine normalize

   ! deferred oprators
   ! +
   pure function field_add_field(lhs, rhs) result(opr)
   !< `+` operator.
   class(primitive_compressible), intent(in) :: lhs    !< Left hand side.
   class(field_object),           intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                    :: opr(:) !< Operator result.

   select type(rhs)
   class is(primitive_compressible)
      allocate(opr(1:5))
      opr(1) = lhs%density    + rhs%density
      opr(2) = lhs%velocity%x + rhs%velocity%x
      opr(3) = lhs%velocity%y + rhs%velocity%y
      opr(4) = lhs%velocity%z + rhs%velocity%z
      opr(5) = lhs%pressure   + rhs%pressure
   endselect
   endfunction field_add_field

   pure function field_add_real(lhs, rhs) result(opr)
   !< `+ real` operator.
   class(primitive_compressible), intent(in) :: lhs     !< Left hand side.
   real(R_P),                     intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                    :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    + rhs(1)
   opr(2) = lhs%velocity%x + rhs(2)
   opr(3) = lhs%velocity%y + rhs(3)
   opr(4) = lhs%velocity%z + rhs(4)
   opr(5) = lhs%pressure   + rhs(5)
   endfunction field_add_real

   pure function real_add_field(lhs, rhs) result(opr)
   !< `real +` operator.
   real(R_P),                     intent(in) :: lhs(1:) !< Left hand side.
   class(primitive_compressible), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                    :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs(1) + rhs%density
   opr(2) = lhs(2) + rhs%velocity%x
   opr(3) = lhs(3) + rhs%velocity%y
   opr(4) = lhs(4) + rhs%velocity%z
   opr(5) = lhs(5) + rhs%pressure
   endfunction real_add_field

   pure function positive(self) result(opr)
   !< Unary operator `+ field`.
   class(primitive_compressible), intent(in) :: self   !< Primitive.
   real(R_P), allocatable                    :: opr(:) !< Operator result.

   opr = self%array()
   endfunction positive

   ! /
   pure function field_div_field(lhs, rhs) result(opr)
   !< `/` operator.
   class(primitive_compressible), intent(in) :: lhs    !< Left hand side.
   class(field_object),           intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                    :: opr(:) !< Operator result.

   select type(rhs)
   class is(primitive_compressible)
      allocate(opr(1:5))
      opr(1) = lhs%density    / rhs%density
      opr(2) = lhs%velocity%x / rhs%velocity%x
      opr(3) = lhs%velocity%y / rhs%velocity%y
      opr(4) = lhs%velocity%z / rhs%velocity%z
      opr(5) = lhs%pressure   / rhs%pressure
   endselect
   endfunction field_div_field

   pure function field_div_integer(lhs, rhs) result(opr)
   !< `/ integer` operator.
   class(primitive_compressible), intent(in) :: lhs     !< Left hand side.
   integer(I_P),                  intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                    :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    / rhs(1)
   opr(2) = lhs%velocity%x / rhs(2)
   opr(3) = lhs%velocity%y / rhs(3)
   opr(4) = lhs%velocity%z / rhs(4)
   opr(5) = lhs%pressure   / rhs(5)
   endfunction field_div_integer

   pure function integer_div_field(lhs, rhs) result(opr)
   !< `integer /` operator.
   integer(I_P),                  intent(in) :: lhs(1:) !< Left hand side.
   class(primitive_compressible), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                    :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs(1) / rhs%density
   opr(2) = lhs(2) / rhs%velocity%x
   opr(3) = lhs(3) / rhs%velocity%y
   opr(4) = lhs(4) / rhs%velocity%z
   opr(5) = lhs(5) / rhs%pressure
   endfunction integer_div_field

   pure function field_div_integer_scalar(lhs, rhs) result(opr)
   !< `/ integer_scalar` operator.
   class(primitive_compressible), intent(in) :: lhs    !< Left hand side.
   integer(I_P),                  intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                    :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    / rhs
   opr(2) = lhs%velocity%x / rhs
   opr(3) = lhs%velocity%y / rhs
   opr(4) = lhs%velocity%z / rhs
   opr(5) = lhs%pressure   / rhs
   endfunction field_div_integer_scalar

   pure function integer_scalar_div_field(lhs, rhs) result(opr)
   !< `integer_scalar /` operator.
   integer(I_P),                  intent(in) :: lhs    !< Left hand side.
   class(primitive_compressible), intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                    :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs / rhs%density
   opr(2) = lhs / rhs%velocity%x
   opr(3) = lhs / rhs%velocity%y
   opr(4) = lhs / rhs%velocity%z
   opr(5) = lhs / rhs%pressure
   endfunction integer_scalar_div_field

   pure function field_div_real(lhs, rhs) result(opr)
   !< `/ real` operator.
   class(primitive_compressible), intent(in) :: lhs     !< Left hand side.
   real(R_P),                     intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                    :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    / rhs(1)
   opr(2) = lhs%velocity%x / rhs(2)
   opr(3) = lhs%velocity%y / rhs(3)
   opr(4) = lhs%velocity%z / rhs(4)
   opr(5) = lhs%pressure   / rhs(5)
   endfunction field_div_real

   pure function real_div_field(lhs, rhs) result(opr)
   !< `real /` operator.
   real(R_P),                     intent(in) :: lhs(1:) !< Left hand side.
   class(primitive_compressible), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                    :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs(1) / rhs%density
   opr(2) = lhs(2) / rhs%velocity%x
   opr(3) = lhs(3) / rhs%velocity%y
   opr(4) = lhs(4) / rhs%velocity%z
   opr(5) = lhs(5) / rhs%pressure
   endfunction real_div_field

   pure function field_div_real_scalar(lhs, rhs) result(opr)
   !< `/ real_scalar` operator.
   class(primitive_compressible), intent(in) :: lhs    !< Left hand side.
   real(R_P),                     intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                    :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    / rhs
   opr(2) = lhs%velocity%x / rhs
   opr(3) = lhs%velocity%y / rhs
   opr(4) = lhs%velocity%z / rhs
   opr(5) = lhs%pressure   / rhs
   endfunction field_div_real_scalar

   pure function real_scalar_div_field(lhs, rhs) result(opr)
   !< `real_scalar /` operator.
   real(R_P),                     intent(in) :: lhs    !< Left hand side.
   class(primitive_compressible), intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                    :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs / rhs%density
   opr(2) = lhs / rhs%velocity%x
   opr(3) = lhs / rhs%velocity%y
   opr(4) = lhs / rhs%velocity%z
   opr(5) = lhs / rhs%pressure
   endfunction real_scalar_div_field

   ! *
   pure function field_mul_field(lhs, rhs) result(opr)
   !< `*` operator.
   class(primitive_compressible), intent(in) :: lhs    !< Left hand side.
   class(field_object),           intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                    :: opr(:) !< Operator result.

   select type(rhs)
   class is(primitive_compressible)
      allocate(opr(1:5))
      opr(1) = lhs%density    * rhs%density
      opr(2) = lhs%velocity%x * rhs%velocity%x
      opr(3) = lhs%velocity%y * rhs%velocity%y
      opr(4) = lhs%velocity%z * rhs%velocity%z
      opr(5) = lhs%pressure   * rhs%pressure
   endselect
   endfunction field_mul_field

   pure function field_mul_integer(lhs, rhs) result(opr)
   !< `* integer` operator.
   class(primitive_compressible), intent(in) :: lhs     !< Left hand side.
   integer(I_P),                  intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                    :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    * rhs(1)
   opr(2) = lhs%velocity%x * rhs(2)
   opr(3) = lhs%velocity%y * rhs(3)
   opr(4) = lhs%velocity%z * rhs(4)
   opr(5) = lhs%pressure   * rhs(5)
   endfunction field_mul_integer

   pure function integer_mul_field(lhs, rhs) result(opr)
   !< `integer *` operator.
   integer(I_P),                  intent(in) :: lhs(1:) !< Left hand side.
   class(primitive_compressible), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                    :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs(1) * rhs%density
   opr(2) = lhs(2) * rhs%velocity%x
   opr(3) = lhs(3) * rhs%velocity%y
   opr(4) = lhs(4) * rhs%velocity%z
   opr(5) = lhs(5) * rhs%pressure
   endfunction integer_mul_field

   pure function field_mul_integer_scalar(lhs, rhs) result(opr)
   !< `* integer_scalar` operator.
   class(primitive_compressible), intent(in) :: lhs    !< Left hand side.
   integer(I_P),                  intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                    :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    * rhs
   opr(2) = lhs%velocity%x * rhs
   opr(3) = lhs%velocity%y * rhs
   opr(4) = lhs%velocity%z * rhs
   opr(5) = lhs%pressure   * rhs
   endfunction field_mul_integer_scalar

   pure function integer_scalar_mul_field(lhs, rhs) result(opr)
   !< `integer_scalar *` operator.
   integer(I_P),                  intent(in) :: lhs    !< Left hand side.
   class(primitive_compressible), intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                    :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs * rhs%density
   opr(2) = lhs * rhs%velocity%x
   opr(3) = lhs * rhs%velocity%y
   opr(4) = lhs * rhs%velocity%z
   opr(5) = lhs * rhs%pressure
   endfunction integer_scalar_mul_field

   pure function field_mul_real(lhs, rhs) result(opr)
   !< `* real` operator.
   class(primitive_compressible), intent(in) :: lhs     !< Left hand side.
   real(R_P),                     intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                    :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    * rhs(1)
   opr(2) = lhs%velocity%x * rhs(2)
   opr(3) = lhs%velocity%y * rhs(3)
   opr(4) = lhs%velocity%z * rhs(4)
   opr(5) = lhs%pressure   * rhs(5)
   endfunction field_mul_real

   pure function real_mul_field(lhs, rhs) result(opr)
   !< `real *` operator.
   real(R_P),                     intent(in) :: lhs(1:) !< Left hand side.
   class(primitive_compressible), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                    :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs(1) * rhs%density
   opr(2) = lhs(2) * rhs%velocity%x
   opr(3) = lhs(3) * rhs%velocity%y
   opr(4) = lhs(4) * rhs%velocity%z
   opr(5) = lhs(5) * rhs%pressure
   endfunction real_mul_field

   pure function field_mul_real_scalar(lhs, rhs) result(opr)
   !< `* real_scalar` operator.
   class(primitive_compressible), intent(in) :: lhs    !< Left hand side.
   real(R_P),                     intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                    :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    * rhs
   opr(2) = lhs%velocity%x * rhs
   opr(3) = lhs%velocity%y * rhs
   opr(4) = lhs%velocity%z * rhs
   opr(5) = lhs%pressure   * rhs
   endfunction field_mul_real_scalar

   pure function real_scalar_mul_field(lhs, rhs) result(opr)
   !< `real_scalar *` operator.
   real(R_P),                     intent(in) :: lhs    !< Left hand side.
   class(primitive_compressible), intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                    :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs * rhs%density
   opr(2) = lhs * rhs%velocity%x
   opr(3) = lhs * rhs%velocity%y
   opr(4) = lhs * rhs%velocity%z
   opr(5) = lhs * rhs%pressure
   endfunction real_scalar_mul_field

   ! -
   pure function field_sub_field(lhs, rhs) result(opr)
   !< `-` operator.
   class(primitive_compressible), intent(in) :: lhs    !< Left hand side.
   class(field_object),           intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                    :: opr(:) !< Operator result.

   select type(rhs)
   class is(primitive_compressible)
      allocate(opr(1:5))
      opr(1) = lhs%density    - rhs%density
      opr(2) = lhs%velocity%x - rhs%velocity%x
      opr(3) = lhs%velocity%y - rhs%velocity%y
      opr(4) = lhs%velocity%z - rhs%velocity%z
      opr(5) = lhs%pressure   - rhs%pressure
   endselect
   endfunction field_sub_field

   pure function field_sub_real(lhs, rhs) result(opr)
   !< `- real` operator.
   class(primitive_compressible), intent(in) :: lhs     !< Left hand side.
   real(R_P),                     intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                    :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    - rhs(1)
   opr(2) = lhs%velocity%x - rhs(2)
   opr(3) = lhs%velocity%y - rhs(3)
   opr(4) = lhs%velocity%z - rhs(4)
   opr(5) = lhs%pressure   - rhs(5)
   endfunction field_sub_real

   pure function real_sub_field(lhs, rhs) result(opr)
   !< `real -` operator.
   real(R_P),                     intent(in) :: lhs(1:) !< Left hand side.
   class(primitive_compressible), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                    :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs(1) - rhs%density
   opr(2) = lhs(2) - rhs%velocity%x
   opr(3) = lhs(3) - rhs%velocity%y
   opr(4) = lhs(4) - rhs%velocity%z
   opr(5) = lhs(5) - rhs%pressure
   endfunction real_sub_field

   pure function negative(self) result(opr)
   !< Unary operator `- field`.
   class(primitive_compressible), intent(in) :: self   !< Primitive.
   real(R_P), allocatable                    :: opr(:) !< Operator result.

   opr = - self%array()
   endfunction negative

   ! **
   pure function field_pow_integer(lhs, rhs) result(opr)
   !< `** integer` operator.
   class(primitive_compressible), intent(in) :: lhs    !< Left hand side.
   integer(I_P),                  intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                    :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    ** rhs
   opr(2) = lhs%velocity%x ** rhs
   opr(3) = lhs%velocity%y ** rhs
   opr(4) = lhs%velocity%z ** rhs
   opr(5) = lhs%pressure   ** rhs
   endfunction field_pow_integer

   pure function field_pow_real(lhs, rhs) result(opr)
   !< `** real` operator.
   class(primitive_compressible), intent(in) :: lhs    !< Left hand side.
   real(R_P),                     intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                    :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    ** rhs
   opr(2) = lhs%velocity%x ** rhs
   opr(3) = lhs%velocity%y ** rhs
   opr(4) = lhs%velocity%z ** rhs
   opr(5) = lhs%pressure   ** rhs
   endfunction field_pow_real

   ! =
   pure subroutine assign_field(lhs, rhs)
   !< Operator `=`.
   class(primitive_compressible), intent(inout) :: lhs !< Left hand side.
   class(field_object),           intent(in)    :: rhs !< Right hand side.

   select type(rhs)
   class is(primitive_compressible)
      lhs%density  = rhs%density
      lhs%velocity = rhs%velocity
      lhs%pressure = rhs%pressure
   endselect
   endsubroutine assign_field

   pure subroutine assign_real(lhs, rhs)
   !< Operator `field = real`.
   class(primitive_compressible), intent(inout) :: lhs     !< Left hand side.
   real(R_P),                     intent(in)    :: rhs(1:) !< Right hand side.

   lhs%density    = rhs(1)
   lhs%velocity%x = rhs(2)
   lhs%velocity%y = rhs(3)
   lhs%velocity%z = rhs(4)
   lhs%pressure   = rhs(5)
   endsubroutine assign_real

   ! ==
   elemental function eq(lhs, rhs) result(opr)
   !< Operator `=='.
   class(primitive_compressible), intent(in) :: lhs !< Left hand side.
   class(field_object),           intent(in) :: rhs !< Right hand side.
   logical                                   :: opr !< Operator result.

   select type(rhs)
   class is(primitive_compressible)
               opr = lhs%density  == rhs%density
      if (opr) opr = lhs%velocity == rhs%velocity
      if (opr) opr = lhs%pressure == rhs%pressure
   endselect
   endfunction eq

   ! /=
   elemental function not_eq(lhs, rhs) result(opr)
   !< Operator `/='.
   class(primitive_compressible), intent(in) :: lhs !< Left hand side.
   class(field_object),           intent(in) :: rhs !< Right hand side.
   logical                                   :: opr !< Operator result.

   opr = .not.(lhs%eq(rhs=rhs))
   endfunction not_eq

   ! private non TBP
   pure function primitive_compressible_instance(density, velocity, pressure) result(instance)
   !< Return and instance of [[primitive_compressible]].
   !<
   !< @note This procedure is used for overloading [[primitive_compressible]] name.
   real(R_P),    intent(in), optional :: density  !< Density field.
   type(vector), intent(in), optional :: velocity !< Velocity field.
   real(R_P),    intent(in), optional :: pressure !< Pressure field.
   type(primitive_compressible)       :: instance !< Instance of [[primitive_compressible]].

   if (present(density )) instance%density  = density
   if (present(velocity)) instance%velocity = velocity
   if (present(pressure)) instance%pressure = pressure
   endfunction primitive_compressible_instance
endmodule flow_primitive_compressible
