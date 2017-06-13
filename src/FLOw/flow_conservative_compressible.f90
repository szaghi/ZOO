!< FLOw **conservative** compressible object.

module flow_conservative_compressible
!< FLOw **conservative** compressible object.
!<
!< [[conservative_compressible]] is a class that handles compressible conservative fluid dynamic variables.

use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
use flow_eos_object, only : eos_object
use flow_field_object, only : field_object
use flow_conservative_object, only : conservative_object
use penf, only : I1P, I_P, R_P, str
use vecfor, only : vector

implicit none
private
public :: conservative_compressible
public :: conservative_compressible_pointer

type, extends(conservative_object) :: conservative_compressible
   !< **Conservative** compressible multispecie object.
   real(R_P)    :: density=0._R_P !< Density, `rho`.
   type(vector) :: momentum       !< Momentum, `rho * v`, `rho` being the density and `v` the velocity vector.
   real(R_P)    :: energy=0._R_P  !< Energy, `rho * E`, `rho` being the density and `E` the specific energy.
   contains
      ! public methods
      procedure, pass(self), public :: compute_fluxes_from_primitive !< Compute conservative fluxes from primitives at interface.
      ! deferred methods
      procedure, pass(self), public :: array          !< Return serialized array of field.
      procedure, pass(self), public :: compute_fluxes !< Compute conservative fluxes.
      procedure, pass(self), public :: description    !< Return pretty-printed object description.
      procedure, pass(self), public :: destroy        !< Destroy conservative.
      procedure, pass(self), public :: initialize     !< Initialize conservative.
      procedure, pass(self), public :: normalize      !< Normalize conservative with respect a given normal vector.
      procedure, pass(self), public :: pressure       !< Return pressure value.
      procedure, pass(self), public :: velocity       !< Return velocity vector.
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
endtype conservative_compressible

interface conservative_compressible
   !< Overload [[conservative_compressible]] name with its constructor.
   module procedure conservative_compressible_instance
endinterface

contains
   ! public non TBP
   function conservative_compressible_pointer(to, error_message) result(pointer_)
   !< Return [[conservative_compressible]] pointer associated to [[conservative_object]] or its extensions until
   !< [[conservative_compressible]] included.
   !<
   !< @note A type-guard check is performed and error stop is raised if necessary.
   class(conservative_object), intent(in), target   :: to            !< Target of associate.
   character(*),               intent(in), optional :: error_message !< Auxiliary error message.
   class(conservative_compressible), pointer        :: pointer_      !< Associated pointer.

   select type(to)
   type is(conservative_compressible)
      pointer_ => to
   class default
      write(stderr, '(A)') 'error: cast conservative_object to conservative_compressible failed!'
      if (present(error_message)) write(stderr, '(A)') error_message
      stop
   endselect
   endfunction conservative_compressible_pointer

   ! public methods
   elemental subroutine compute_fluxes_from_primitive(self, eos, p, r, u, normal)
   !< Compute conservative fluxes from primitives at interface.
   class(conservative_compressible), intent(inout) :: self   !< Conservative.
   class(eos_object),                intent(in)    :: eos    !< Equation of state.
   real(R_P),                        intent(in)    :: p      !< Pressure at interface.
   real(R_P),                        intent(in)    :: r      !< Density at interface.
   real(R_P),                        intent(in)    :: u      !< Velocity (normal component) at interface.
   type(vector),                     intent(in)    :: normal !< Normal (versor) of face where fluxes are given.

   self%density = r * u
   self%momentum = (r * u * u + p) * normal
   self%energy = (r * eos%internal_energy(density=r, pressure=p) + r * u * u * 0.5_R_P + p) * u
   endsubroutine compute_fluxes_from_primitive

   ! deferred methods
   pure function array(self) result(array_)
   !< Return serialized array of field.
   class(conservative_compressible), intent(in) :: self      !< Conservative.
   real(R_P), allocatable                       :: array_(:) !< Serialized array of field.

   allocate(array_(1:5))
   array_(1) = self%density
   array_(2) = self%momentum%x
   array_(3) = self%momentum%y
   array_(4) = self%momentum%z
   array_(5) = self%energy
   endfunction array

   subroutine compute_fluxes(self, eos, normal, fluxes)
   !< Compute conservative fluxes.
   class(conservative_compressible), intent(in)  :: self             !< Conservative.
   class(eos_object),                intent(in)  :: eos              !< Equation of state.
   type(vector),                     intent(in)  :: normal           !< Normal (versor) of face where fluxes are given.
   class(conservative_object),       intent(out) :: fluxes           !< Conservative fluxes.
   real(R_P)                                     :: pressure_        !< Pressure value.
   type(vector)                                  :: velocity_        !< Velocity vector.
   real(R_P)                                     :: velocity_normal_ !< Velocity component parallel to given normal.

   select type(fluxes)
   class is(conservative_compressible)
      pressure_ = self%pressure(eos=eos)
      velocity_ = self%velocity()
      velocity_normal_ = velocity_.dot.normal
      fluxes%density = self%momentum.dot.normal
      fluxes%momentum = self%density * velocity_ * velocity_normal_ + pressure_ * normal
      fluxes%energy = (self%energy + pressure_) * velocity_normal_
   endselect
   endsubroutine compute_fluxes

   pure function description(self, prefix) result(desc)
   !< Return a pretty-formatted object description.
   class(conservative_compressible), intent(in)           :: self             !< Conservative.
   character(*),                     intent(in), optional :: prefix           !< Prefixing string.
   character(len=:), allocatable                          :: prefix_          !< Prefixing string, local variable.
   character(len=:), allocatable                          :: desc             !< Description.
   character(len=1), parameter                            :: NL=new_line('a') !< New line character.

   prefix_ = '' ; if (present(prefix)) prefix_ = prefix
   desc = ''
   desc = desc//prefix_//'density  = '//trim(str(n=self%density))//NL
   desc = desc//prefix_//'momentum = '//trim(str(n=[self%momentum%x, self%momentum%y, self%momentum%z]))//NL
   desc = desc//prefix_//'energy   = '//trim(str(n=self%energy))
   endfunction description

   elemental subroutine destroy(self)
   !< Destroy conservative.
   class(conservative_compressible), intent(inout) :: self  !< Conservative.
   type(conservative_compressible)                 :: fresh !< Fresh instance of conservative object.

   self = fresh
   endsubroutine destroy

   subroutine initialize(self, initial_state)
   !< Initialize conservative.
   class(conservative_compressible), intent(inout)        :: self          !< conservative.
   class(conservative_object),       intent(in), optional :: initial_state !< Initial state.

   call self%destroy
   if (present(initial_state)) then
      select type(initial_state)
      class is(conservative_compressible)
         self = initial_state
      endselect
   endif
   endsubroutine initialize

   elemental subroutine normalize(self, eos, normal)
   !< *Normalize* conservative with respect a given normal vector.
   class(conservative_compressible), intent(inout) :: self      !< Conservative.
   class(eos_object),                intent(in)    :: eos       !< Equation of state.
   type(vector),                     intent(in)    :: normal    !< Normal vector.
   real(R_P)                                       :: pressure_ !< Pressure value.
   type(vector)                                    :: velocity_ !< Velocity vector.

   velocity_ = self%velocity() .paral. normal
   pressure_ = self%pressure(eos=eos)
   self%momentum = self%density * velocity_
   self%energy = pressure_ / eos%gm1() + 0.5_R_P * self%density * velocity_%sq_norm()
   endsubroutine normalize

   elemental function pressure(self, eos) result(pressure_)
   !< Return pressure value.
   class(conservative_compressible), intent(in) :: self      !< Conservative.
   class(eos_object),                intent(in) :: eos       !< Equation of state.
   real(R_P)                                    :: pressure_ !< Pressure value.
   type(vector)                                 :: velocity_ !< Velocity vector.

   velocity_ = self%velocity()
   pressure_ = eos%gm1() * (self%energy - 0.5_R_P * self%density * velocity_%sq_norm())
   endfunction pressure

   elemental function velocity(self) result(velocity_)
   !< Return velocity vector.
   class(conservative_compressible), intent(in) :: self      !< Conservative.
   type(vector)                                 :: velocity_ !< Velocity vector.

   velocity_ = self%momentum / self%density
   endfunction velocity

   ! deferred oprators
   ! +
   pure function field_add_field(lhs, rhs) result(opr)
   !< `+` operator.
   class(conservative_compressible), intent(in) :: lhs    !< Left hand side.
   class(field_object),              intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                       :: opr(:) !< Operator result.

   select type(rhs)
   class is(conservative_compressible)
      allocate(opr(1:5))
      opr(1) = lhs%density    + rhs%density
      opr(2) = lhs%momentum%x + rhs%momentum%x
      opr(3) = lhs%momentum%y + rhs%momentum%y
      opr(4) = lhs%momentum%z + rhs%momentum%z
      opr(5) = lhs%energy     + rhs%energy
   endselect
   endfunction field_add_field

   pure function field_add_real(lhs, rhs) result(opr)
   !< `+ real` operator.
   class(conservative_compressible), intent(in) :: lhs     !< Left hand side.
   real(R_P),                        intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                       :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    + rhs(1)
   opr(2) = lhs%momentum%x + rhs(2)
   opr(3) = lhs%momentum%y + rhs(3)
   opr(4) = lhs%momentum%z + rhs(4)
   opr(5) = lhs%energy     + rhs(5)
   endfunction field_add_real

   pure function real_add_field(lhs, rhs) result(opr)
   !< `real +` operator.
   real(R_P),                        intent(in) :: lhs(1:) !< Left hand side.
   class(conservative_compressible), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                       :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs(1) + rhs%density
   opr(2) = lhs(2) + rhs%momentum%x
   opr(3) = lhs(3) + rhs%momentum%y
   opr(4) = lhs(4) + rhs%momentum%z
   opr(5) = lhs(5) + rhs%energy
   endfunction real_add_field

   pure function positive(self) result(opr)
   !< Unary operator `+ field`.
   class(conservative_compressible), intent(in) :: self   !< conservative.
   real(R_P), allocatable                       :: opr(:) !< Operator result.

   opr = self%array()
   endfunction positive

   ! /
   pure function field_div_field(lhs, rhs) result(opr)
   !< `/` operator.
   class(conservative_compressible), intent(in) :: lhs    !< Left hand side.
   class(field_object),              intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                       :: opr(:) !< Operator result.

   select type(rhs)
   class is(conservative_compressible)
      allocate(opr(1:5))
      opr(1) = lhs%density    / rhs%density
      opr(2) = lhs%momentum%x / rhs%momentum%x
      opr(3) = lhs%momentum%y / rhs%momentum%y
      opr(4) = lhs%momentum%z / rhs%momentum%z
      opr(5) = lhs%energy     / rhs%energy
   endselect
   endfunction field_div_field

   pure function field_div_integer(lhs, rhs) result(opr)
   !< `/ integer` operator.
   class(conservative_compressible), intent(in) :: lhs     !< Left hand side.
   integer(I_P),                     intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                       :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    / rhs(1)
   opr(2) = lhs%momentum%x / rhs(2)
   opr(3) = lhs%momentum%y / rhs(3)
   opr(4) = lhs%momentum%z / rhs(4)
   opr(5) = lhs%energy     / rhs(5)
   endfunction field_div_integer

   pure function integer_div_field(lhs, rhs) result(opr)
   !< `integer /` operator.
   integer(I_P),                     intent(in) :: lhs(1:) !< Left hand side.
   class(conservative_compressible), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                       :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs(1) / rhs%density
   opr(2) = lhs(2) / rhs%momentum%x
   opr(3) = lhs(3) / rhs%momentum%y
   opr(4) = lhs(4) / rhs%momentum%z
   opr(5) = lhs(5) / rhs%energy
   endfunction integer_div_field

   pure function field_div_integer_scalar(lhs, rhs) result(opr)
   !< `/ integer_scalar` operator.
   class(conservative_compressible), intent(in) :: lhs    !< Left hand side.
   integer(I_P),                     intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                       :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    / rhs
   opr(2) = lhs%momentum%x / rhs
   opr(3) = lhs%momentum%y / rhs
   opr(4) = lhs%momentum%z / rhs
   opr(5) = lhs%energy     / rhs
   endfunction field_div_integer_scalar

   pure function integer_scalar_div_field(lhs, rhs) result(opr)
   !< `integer_scalar /` operator.
   integer(I_P),                     intent(in) :: lhs    !< Left hand side.
   class(conservative_compressible), intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                       :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs / rhs%density
   opr(2) = lhs / rhs%momentum%x
   opr(3) = lhs / rhs%momentum%y
   opr(4) = lhs / rhs%momentum%z
   opr(5) = lhs / rhs%energy
   endfunction integer_scalar_div_field

   pure function field_div_real(lhs, rhs) result(opr)
   !< `/ real` operator.
   class(conservative_compressible), intent(in) :: lhs     !< Left hand side.
   real(R_P),                        intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                       :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    / rhs(1)
   opr(2) = lhs%momentum%x / rhs(2)
   opr(3) = lhs%momentum%y / rhs(3)
   opr(4) = lhs%momentum%z / rhs(4)
   opr(5) = lhs%energy     / rhs(5)
   endfunction field_div_real

   pure function real_div_field(lhs, rhs) result(opr)
   !< `real /` operator.
   real(R_P),                        intent(in) :: lhs(1:) !< Left hand side.
   class(conservative_compressible), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                       :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs(1) / rhs%density
   opr(2) = lhs(2) / rhs%momentum%x
   opr(3) = lhs(3) / rhs%momentum%y
   opr(4) = lhs(4) / rhs%momentum%z
   opr(5) = lhs(5) / rhs%energy
   endfunction real_div_field

   pure function field_div_real_scalar(lhs, rhs) result(opr)
   !< `/ real_scalar` operator.
   class(conservative_compressible), intent(in) :: lhs    !< Left hand side.
   real(R_P),                        intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                       :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    / rhs
   opr(2) = lhs%momentum%x / rhs
   opr(3) = lhs%momentum%y / rhs
   opr(4) = lhs%momentum%z / rhs
   opr(5) = lhs%energy     / rhs
   endfunction field_div_real_scalar

   pure function real_scalar_div_field(lhs, rhs) result(opr)
   !< `real_scalar /` operator.
   real(R_P),                        intent(in) :: lhs    !< Left hand side.
   class(conservative_compressible), intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                       :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs / rhs%density
   opr(2) = lhs / rhs%momentum%x
   opr(3) = lhs / rhs%momentum%y
   opr(4) = lhs / rhs%momentum%z
   opr(5) = lhs / rhs%energy
   endfunction real_scalar_div_field

   ! *
   pure function field_mul_field(lhs, rhs) result(opr)
   !< `*` operator.
   class(conservative_compressible), intent(in) :: lhs    !< Left hand side.
   class(field_object),              intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                       :: opr(:) !< Operator result.

   select type(rhs)
   class is(conservative_compressible)
      allocate(opr(1:5))
      opr(1) = lhs%density    * rhs%density
      opr(2) = lhs%momentum%x * rhs%momentum%x
      opr(3) = lhs%momentum%y * rhs%momentum%y
      opr(4) = lhs%momentum%z * rhs%momentum%z
      opr(5) = lhs%energy     * rhs%energy
   endselect
   endfunction field_mul_field

   pure function field_mul_integer(lhs, rhs) result(opr)
   !< `* integer` operator.
   class(conservative_compressible), intent(in) :: lhs     !< Left hand side.
   integer(I_P),                     intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                       :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    * rhs(1)
   opr(2) = lhs%momentum%x * rhs(2)
   opr(3) = lhs%momentum%y * rhs(3)
   opr(4) = lhs%momentum%z * rhs(4)
   opr(5) = lhs%energy     * rhs(5)
   endfunction field_mul_integer

   pure function integer_mul_field(lhs, rhs) result(opr)
   !< `integer *` operator.
   integer(I_P),                     intent(in) :: lhs(1:) !< Left hand side.
   class(conservative_compressible), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                       :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs(1) * rhs%density
   opr(2) = lhs(2) * rhs%momentum%x
   opr(3) = lhs(3) * rhs%momentum%y
   opr(4) = lhs(4) * rhs%momentum%z
   opr(5) = lhs(5) * rhs%energy
   endfunction integer_mul_field

   pure function field_mul_integer_scalar(lhs, rhs) result(opr)
   !< `* integer_scalar` operator.
   class(conservative_compressible), intent(in) :: lhs    !< Left hand side.
   integer(I_P),                     intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                       :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    * rhs
   opr(2) = lhs%momentum%x * rhs
   opr(3) = lhs%momentum%y * rhs
   opr(4) = lhs%momentum%z * rhs
   opr(5) = lhs%energy     * rhs
   endfunction field_mul_integer_scalar

   pure function integer_scalar_mul_field(lhs, rhs) result(opr)
   !< `integer_scalar *` operator.
   integer(I_P),                     intent(in) :: lhs    !< Left hand side.
   class(conservative_compressible), intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                       :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs * rhs%density
   opr(2) = lhs * rhs%momentum%x
   opr(3) = lhs * rhs%momentum%y
   opr(4) = lhs * rhs%momentum%z
   opr(5) = lhs * rhs%energy
   endfunction integer_scalar_mul_field

   pure function field_mul_real(lhs, rhs) result(opr)
   !< `* real` operator.
   class(conservative_compressible), intent(in) :: lhs     !< Left hand side.
   real(R_P),                        intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                       :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    * rhs(1)
   opr(2) = lhs%momentum%x * rhs(2)
   opr(3) = lhs%momentum%y * rhs(3)
   opr(4) = lhs%momentum%z * rhs(4)
   opr(5) = lhs%energy     * rhs(5)
   endfunction field_mul_real

   pure function real_mul_field(lhs, rhs) result(opr)
   !< `real *` operator.
   real(R_P),                        intent(in) :: lhs(1:) !< Left hand side.
   class(conservative_compressible), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                       :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs(1) * rhs%density
   opr(2) = lhs(2) * rhs%momentum%x
   opr(3) = lhs(3) * rhs%momentum%y
   opr(4) = lhs(4) * rhs%momentum%z
   opr(5) = lhs(5) * rhs%energy
   endfunction real_mul_field

   pure function field_mul_real_scalar(lhs, rhs) result(opr)
   !< `* real_scalar` operator.
   class(conservative_compressible), intent(in) :: lhs    !< Left hand side.
   real(R_P),                        intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                       :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    * rhs
   opr(2) = lhs%momentum%x * rhs
   opr(3) = lhs%momentum%y * rhs
   opr(4) = lhs%momentum%z * rhs
   opr(5) = lhs%energy     * rhs
   endfunction field_mul_real_scalar

   pure function real_scalar_mul_field(lhs, rhs) result(opr)
   !< `real_scalar *` operator.
   real(R_P),                        intent(in) :: lhs    !< Left hand side.
   class(conservative_compressible), intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                       :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs * rhs%density
   opr(2) = lhs * rhs%momentum%x
   opr(3) = lhs * rhs%momentum%y
   opr(4) = lhs * rhs%momentum%z
   opr(5) = lhs * rhs%energy
   endfunction real_scalar_mul_field

   ! -
   pure function field_sub_field(lhs, rhs) result(opr)
   !< `-` operator.
   class(conservative_compressible), intent(in) :: lhs    !< Left hand side.
   class(field_object),              intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                       :: opr(:) !< Operator result.

   select type(rhs)
   class is(conservative_compressible)
      allocate(opr(1:5))
      opr(1) = lhs%density    - rhs%density
      opr(2) = lhs%momentum%x - rhs%momentum%x
      opr(3) = lhs%momentum%y - rhs%momentum%y
      opr(4) = lhs%momentum%z - rhs%momentum%z
      opr(5) = lhs%energy     - rhs%energy
   endselect
   endfunction field_sub_field

   pure function field_sub_real(lhs, rhs) result(opr)
   !< `- real` operator.
   class(conservative_compressible), intent(in) :: lhs     !< Left hand side.
   real(R_P),                        intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                       :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    - rhs(1)
   opr(2) = lhs%momentum%x - rhs(2)
   opr(3) = lhs%momentum%y - rhs(3)
   opr(4) = lhs%momentum%z - rhs(4)
   opr(5) = lhs%energy     - rhs(5)
   endfunction field_sub_real

   pure function real_sub_field(lhs, rhs) result(opr)
   !< `real -` operator.
   real(R_P),                        intent(in) :: lhs(1:) !< Left hand side.
   class(conservative_compressible), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                       :: opr(:)  !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs(1) - rhs%density
   opr(2) = lhs(2) - rhs%momentum%x
   opr(3) = lhs(3) - rhs%momentum%y
   opr(4) = lhs(4) - rhs%momentum%z
   opr(5) = lhs(5) - rhs%energy
   endfunction real_sub_field

   pure function negative(self) result(opr)
   !< Unary operator `- field`.
   class(conservative_compressible), intent(in) :: self   !< conservative.
   real(R_P), allocatable                       :: opr(:) !< Operator result.

   opr = - self%array()
   endfunction negative

   ! **
   pure function field_pow_integer(lhs, rhs) result(opr)
   !< `** integer` operator.
   class(conservative_compressible), intent(in) :: lhs    !< Left hand side.
   integer(I_P),                     intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                       :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    ** rhs
   opr(2) = lhs%momentum%x ** rhs
   opr(3) = lhs%momentum%y ** rhs
   opr(4) = lhs%momentum%z ** rhs
   opr(5) = lhs%energy     ** rhs
   endfunction field_pow_integer

   pure function field_pow_real(lhs, rhs) result(opr)
   !< `** real` operator.
   class(conservative_compressible), intent(in) :: lhs    !< Left hand side.
   real(R_P),                        intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                       :: opr(:) !< Operator result.

   allocate(opr(1:5))
   opr(1) = lhs%density    ** rhs
   opr(2) = lhs%momentum%x ** rhs
   opr(3) = lhs%momentum%y ** rhs
   opr(4) = lhs%momentum%z ** rhs
   opr(5) = lhs%energy     ** rhs
   endfunction field_pow_real

   ! =
   pure subroutine assign_field(lhs, rhs)
   !< Operator `=`.
   class(conservative_compressible), intent(inout) :: lhs !< Left hand side.
   class(field_object),              intent(in)    :: rhs !< Right hand side.

   select type(rhs)
   class is(conservative_compressible)
      lhs%density  = rhs%density
      lhs%momentum = rhs%momentum
      lhs%energy   = rhs%energy
   endselect
   endsubroutine assign_field

   pure subroutine assign_real(lhs, rhs)
   !< Operator `field = real`.
   class(conservative_compressible), intent(inout) :: lhs     !< Left hand side.
   real(R_P),                        intent(in)    :: rhs(1:) !< Right hand side.

   lhs%density    = rhs(1)
   lhs%momentum%x = rhs(2)
   lhs%momentum%y = rhs(3)
   lhs%momentum%z = rhs(4)
   lhs%energy     = rhs(5)
   endsubroutine assign_real

   ! ==
   elemental function eq(lhs, rhs) result(opr)
   !< Operator `=='.
   class(conservative_compressible), intent(in) :: lhs !< Left hand side.
   class(field_object),              intent(in) :: rhs !< Right hand side.
   logical                                      :: opr !< Operator result.

   select type(rhs)
   class is(conservative_compressible)
               opr = lhs%density  == rhs%density
      if (opr) opr = lhs%momentum == rhs%momentum
      if (opr) opr = lhs%energy   == rhs%energy
   endselect
   endfunction eq

   ! /=
   elemental function not_eq(lhs, rhs) result(opr)
   !< Operator `/='.
   class(conservative_compressible), intent(in) :: lhs !< Left hand side.
   class(field_object),              intent(in) :: rhs !< Right hand side.
   logical                                      :: opr !< Operator result.

   opr = .not.(lhs%eq(rhs=rhs))
   endfunction not_eq

   ! private non TBP
   pure function conservative_compressible_instance(density, velocity, pressure) result(instance)
   !< Return and instance of [[conservative_compressible]].
   !<
   !< @note This procedure is used for overloading [[conservative_compressible]] name.
   real(R_P),    intent(in), optional :: density  !< Density field.
   type(vector), intent(in), optional :: velocity !< Velocity field.
   real(R_P),    intent(in), optional :: pressure !< Pressure field.
   type(conservative_compressible)    :: instance !< Instance of [[conservative_compressible]].

   if (present(density )) instance%density  = density
   if (present(velocity)) instance%momentum = velocity
   if (present(pressure)) instance%energy   = pressure
   endfunction conservative_compressible_instance
endmodule flow_conservative_compressible
