!< FLOw **primitive** compressible mutlispecie object.

module flow_primitive_compressible_multispecie
!< FLOw **primitive** compressible mutlispecie object.
!<
!< [[primitive_compressible_multispecie]] is a class that handles compressible multispecie primitive fluid dynamic variables.

use flow_eos_object, only : eos_object
use flow_field_object, only : field_object
use flow_primitive_object, only : primitive_object
use penf, only : I_P, R_P, str
use vecfor, only : vector

implicit none
private
public :: primitive_compressible_multispecie

type, extends(primitive_object) :: primitive_compressible_multispecie
   !< **Primitive** compressible multispecie object.
   real(R_P)              :: density=0._R_P       !< Density, `rho`.
   type(vector)           :: velocity             !< Velocity, `v`.
   real(R_P)              :: pressure=0._R_P      !< Pressure, `p`.
   real(R_P), allocatable :: partial_densities(:) !< Partial densities `rho(s), rho = sum(rho(s))`.
   contains
      ! public operators
      procedure, pass(lhs), public :: field_compatible_field   !< Operator `.compatible.`.
      procedure, pass(lhs), public :: field_compatible_integer !< Operator `.compatible.integer`.
      procedure, pass(rhs), public :: integer_compatible_field !< Operator `integer.compatible.`.
      procedure, pass(lhs), public :: field_compatible_real    !< Operator `.compatible.real`.
      procedure, pass(rhs), public :: real_compatible_field    !< Operator `real.compatible.`.
      generic, public :: operator(.compatible.) => field_compatible_field,   &
                                                   field_compatible_integer, &
                                                   integer_compatible_field, &
                                                   field_compatible_real,    &
                                                   real_compatible_field !< Operator `.compatible.` overloading.
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
endtype primitive_compressible_multispecie

interface primitive_compressible_multispecie
   !< Overload [[primitive_compressible_multispecie]] name with its constructor.
   module procedure primitive_compressible_multispecie_instance
endinterface

contains
   ! public methods
   pure function field_compatible_field(lhs, rhs) result(opr)
   !< Operator `.compatible.`.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   type(primitive_compressible_multispecie),  intent(in) :: rhs !< Right hand side.
   logical                                               :: opr !< Operator result.

   opr = allocated(lhs%partial_densities).and.allocated(rhs%partial_densities)
   if (opr) opr = size(lhs%partial_densities, dim=1) == size(rhs%partial_densities, dim=1)
   endfunction field_compatible_field

   pure function field_compatible_integer(lhs, rhs) result(opr)
   !< Operator `.compatible.integer`.
   class(primitive_compressible_multispecie), intent(in) :: lhs     !< Left hand side.
   integer(I_P),                              intent(in) :: rhs(1:) !< Right hand side.
   logical                                               :: opr     !< Operator result.

   opr = allocated(lhs%partial_densities)
   if (opr) opr = size(lhs%partial_densities, dim=1) == size(rhs, dim=1)
   endfunction field_compatible_integer

   pure function integer_compatible_field(lhs, rhs) result(opr)
   !< Operator `integer.compatible.`.
   integer(I_P),                              intent(in) :: lhs(1:) !< Left hand side.
   class(primitive_compressible_multispecie), intent(in) :: rhs     !< Right hand side.
   logical                                               :: opr     !< Operator result.

   opr = allocated(rhs%partial_densities)
   if (opr) opr = size(lhs, dim=1) == size(rhs%partial_densities, dim=1)
   endfunction integer_compatible_field

   pure function field_compatible_real(lhs, rhs) result(opr)
   !< Operator `.compatible.real`.
   class(primitive_compressible_multispecie), intent(in) :: lhs     !< Left hand side.
   real(R_P),                                 intent(in) :: rhs(1:) !< Right hand side.
   logical                                               :: opr     !< Operator result.

   opr = allocated(lhs%partial_densities)
   if (opr) opr = size(lhs%partial_densities, dim=1) == size(rhs, dim=1)
   endfunction field_compatible_real

   pure function real_compatible_field(lhs, rhs) result(opr)
   !< Operator `real.compatible.`.
   real(R_P),                                 intent(in) :: lhs(1:) !< Left hand side.
   class(primitive_compressible_multispecie), intent(in) :: rhs     !< Right hand side.
   logical                                               :: opr     !< Operator result.

   opr = allocated(rhs%partial_densities)
   if (opr) opr = size(lhs, dim=1) == size(rhs%partial_densities, dim=1)
   endfunction real_compatible_field

   ! deferred methods
   pure function array(self) result(array_)
   !< Return serialized array of field.
   class(primitive_compressible_multispecie), intent(in) :: self      !< Primitive.
   real(R_P), allocatable                                :: array_(:) !< Serialized array of field.

   if (allocated(self%partial_densities)) then
      allocate(array_(1:5+size(self%partial_densities, dim=1)))
      array_(1)  = self%density
      array_(2)  = self%velocity%x
      array_(3)  = self%velocity%y
      array_(4)  = self%velocity%z
      array_(5)  = self%pressure
      array_(6:) = self%partial_densities(:)
   else
      allocate(array_(1:5))
      array_(1) = self%density
      array_(2) = self%velocity%x
      array_(3) = self%velocity%y
      array_(4) = self%velocity%z
      array_(5) = self%pressure
   endif
   endfunction array

   pure function description(self, prefix) result(desc)
   !< Return a pretty-formatted object description.
   class(primitive_compressible_multispecie), intent(in)           :: self             !< Primitive.
   character(*),                              intent(in), optional :: prefix           !< Prefixing string.
   character(len=:), allocatable                                   :: prefix_          !< Prefixing string, local variable.
   character(len=:), allocatable                                   :: desc             !< Description.
   character(len=1), parameter                                     :: NL=new_line('a') !< New line character.

   if (allocated(self%partial_densities)) then
      prefix_ = '' ; if (present(prefix)) prefix_ = prefix
      desc = ''
      desc = desc//prefix_//'density           = '//trim(str(n=self%density))//NL
      desc = desc//prefix_//'velocity          = '//trim(str(n=[self%velocity%x, self%velocity%y, self%velocity%z]))//NL
      desc = desc//prefix_//'pressure          = '//trim(str(n=self%pressure))//NL
      desc = desc//prefix_//'partial densities = '//trim(str(n=self%partial_densities))
   else
      prefix_ = '' ; if (present(prefix)) prefix_ = prefix
      desc = ''
      desc = desc//prefix_//'density  = '//trim(str(n=self%density))//NL
      desc = desc//prefix_//'velocity = '//trim(str(n=[self%velocity%x, self%velocity%y, self%velocity%z]))//NL
      desc = desc//prefix_//'pressure = '//trim(str(n=self%pressure))
   endif
   endfunction description

   elemental subroutine destroy(self)
   !< Destroy primitive.
   class(primitive_compressible_multispecie), intent(inout) :: self  !< Primitive.
   type(primitive_compressible_multispecie)                 :: fresh !< Fresh instance of primitive object.

   self = fresh
   endsubroutine destroy

   elemental function energy(self, eos) result(energy_)
   !< Return energy value.
   class(primitive_compressible_multispecie), intent(in) :: self    !< Primitive.
   class(eos_object),                         intent(in) :: eos     !< Equation of state.
   real(R_P)                                             :: energy_ !< Energy value.

   energy_ = self%pressure / (eos%g() - 1._R_P) + 0.5_R_P * self%density * self%velocity%sq_norm()
   endfunction energy

   subroutine initialize(self, initial_state)
   !< Initialize primitive.
   class(primitive_compressible_multispecie), intent(inout)         :: self          !< Primitive.
   class(primitive_object),                    intent(in), optional :: initial_state !< Initial state.

   call self%destroy
   if (present(initial_state)) then
      select type(initial_state)
      class is(primitive_compressible_multispecie)
         self = initial_state
      endselect
   endif
   endsubroutine initialize

   elemental function momentum(self) result(momentum_)
   !< Return momentum vector.
   class(primitive_compressible_multispecie), intent(in) :: self      !< Primitive.
   type(vector)                                          :: momentum_ !< Momentum vector.

   momentum_ = self%density * self%velocity
   endfunction momentum

   elemental subroutine normalize(self, normal)
   !< *Normalize* primitive with respect a given normal vector.
   class(primitive_compressible_multispecie), intent(inout) :: self   !< Primitive.
   type(vector),                              intent(in)    :: normal !< Normal vector.

   self%velocity = self%velocity .paral. normal
   endsubroutine normalize

   ! deferred oprators
   ! +
   pure function field_add_field(lhs, rhs) result(opr)
   !< `+` operator.
   class(primitive_compressible_multispecie), intent(in) :: lhs    !< Left hand side.
   class(field_object),                       intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                                :: opr(:) !< Operator result.

   select type(rhs)
   class is(primitive_compressible_multispecie)
      if (lhs.compatible.rhs) then
         allocate(opr(1:5+size(lhs%partial_densities, dim=1)))
         opr(1)  = lhs%density           + rhs%density
         opr(2)  = lhs%velocity%x        + rhs%velocity%x
         opr(3)  = lhs%velocity%y        + rhs%velocity%y
         opr(4)  = lhs%velocity%z        + rhs%velocity%z
         opr(5)  = lhs%pressure          + rhs%pressure
         opr(6:) = lhs%partial_densities + rhs%partial_densities
      else
         allocate(opr(1:5))
         opr(1) = lhs%density    + rhs%density
         opr(2) = lhs%velocity%x + rhs%velocity%x
         opr(3) = lhs%velocity%y + rhs%velocity%y
         opr(4) = lhs%velocity%z + rhs%velocity%z
         opr(5) = lhs%pressure   + rhs%pressure
      endif
   endselect
   endfunction field_add_field

   pure function field_add_real(lhs, rhs) result(opr)
   !< `+ real` operator.
   class(primitive_compressible_multispecie), intent(in) :: lhs     !< Left hand side.
   real(R_P),                                 intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                                :: opr(:)  !< Operator result.

   if (lhs.compatible.rhs) then
      allocate(opr(1:5+size(lhs%partial_densities, dim=1)))
      opr(1)  = lhs%density           + rhs(1)
      opr(2)  = lhs%velocity%x        + rhs(2)
      opr(3)  = lhs%velocity%y        + rhs(3)
      opr(4)  = lhs%velocity%z        + rhs(4)
      opr(5)  = lhs%pressure          + rhs(5)
      opr(6:) = lhs%partial_densities + rhs(6:)
   else
      allocate(opr(1:5))
      opr(1) = lhs%density    + rhs(1)
      opr(2) = lhs%velocity%x + rhs(2)
      opr(3) = lhs%velocity%y + rhs(3)
      opr(4) = lhs%velocity%z + rhs(4)
      opr(5) = lhs%pressure   + rhs(5)
   endif
   endfunction field_add_real

   pure function real_add_field(lhs, rhs) result(opr)
   !< `real +` operator.
   real(R_P),                                 intent(in) :: lhs(1:) !< Left hand side.
   class(primitive_compressible_multispecie), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                                :: opr(:)  !< Operator result.

   if (lhs.compatible.rhs) then
      allocate(opr(1:5+size(rhs%partial_densities, dim=1)))
      opr(1)  = lhs(1)  + rhs%density
      opr(2)  = lhs(2)  + rhs%velocity%x
      opr(3)  = lhs(3)  + rhs%velocity%y
      opr(4)  = lhs(4)  + rhs%velocity%z
      opr(5)  = lhs(5)  + rhs%pressure
      opr(6:) = lhs(6:) + rhs%partial_densities
   else
      allocate(opr(1:5))
      opr(1) = lhs(1) + rhs%density
      opr(2) = lhs(2) + rhs%velocity%x
      opr(3) = lhs(3) + rhs%velocity%y
      opr(4) = lhs(4) + rhs%velocity%z
      opr(5) = lhs(5) + rhs%pressure
   endif
   endfunction real_add_field

   pure function positive(self) result(opr)
   !< Unary operator `+ field`.
   class(primitive_compressible_multispecie), intent(in) :: self   !< Primitive.
   real(R_P), allocatable                                :: opr(:) !< Operator result.

   opr = self%array()
   endfunction positive

   ! /
   pure function field_div_field(lhs, rhs) result(opr)
   !< `/` operator.
   class(primitive_compressible_multispecie), intent(in) :: lhs    !< Left hand side.
   class(field_object),                       intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                                :: opr(:) !< Operator result.

   select type(rhs)
   class is(primitive_compressible_multispecie)
      if (lhs.compatible.rhs) then
         allocate(opr(1:5+size(lhs%partial_densities, dim=1)))
         opr(1)  = lhs%density           / rhs%density
         opr(2)  = lhs%velocity%x        / rhs%velocity%x
         opr(3)  = lhs%velocity%y        / rhs%velocity%y
         opr(4)  = lhs%velocity%z        / rhs%velocity%z
         opr(5)  = lhs%pressure          / rhs%pressure
         opr(6:) = lhs%partial_densities / rhs%partial_densities
      else
         allocate(opr(1:5))
         opr(1) = lhs%density    / rhs%density
         opr(2) = lhs%velocity%x / rhs%velocity%x
         opr(3) = lhs%velocity%y / rhs%velocity%y
         opr(4) = lhs%velocity%z / rhs%velocity%z
         opr(5) = lhs%pressure   / rhs%pressure
      endif
   endselect
   endfunction field_div_field

   pure function field_div_integer(lhs, rhs) result(opr)
   !< `/ integer` operator.
   class(primitive_compressible_multispecie), intent(in) :: lhs     !< Left hand side.
   integer(I_P),                              intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                                :: opr(:)  !< Operator result.

   if (lhs.compatible.rhs) then
      allocate(opr(1:5+size(lhs%partial_densities, dim=1)))
      opr(1)  = lhs%density           / rhs(1)
      opr(2)  = lhs%velocity%x        / rhs(2)
      opr(3)  = lhs%velocity%y        / rhs(3)
      opr(4)  = lhs%velocity%z        / rhs(4)
      opr(5)  = lhs%pressure          / rhs(5)
      opr(6:) = lhs%partial_densities / rhs(6:)
   else
      allocate(opr(1:5))
      opr(1) = lhs%density    / rhs(1)
      opr(2) = lhs%velocity%x / rhs(2)
      opr(3) = lhs%velocity%y / rhs(3)
      opr(4) = lhs%velocity%z / rhs(4)
      opr(5) = lhs%pressure   / rhs(5)
   endif
   endfunction field_div_integer

   pure function integer_div_field(lhs, rhs) result(opr)
   !< `integer /` operator.
   integer(I_P),                              intent(in) :: lhs(1:) !< Left hand side.
   class(primitive_compressible_multispecie), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                                :: opr(:)  !< Operator result.

   if (lhs.compatible.rhs) then
      allocate(opr(1:5+size(rhs%partial_densities, dim=1)))
      opr(1)  = lhs(1)  / rhs%density
      opr(2)  = lhs(2)  / rhs%velocity%x
      opr(3)  = lhs(3)  / rhs%velocity%y
      opr(4)  = lhs(4)  / rhs%velocity%z
      opr(5)  = lhs(5)  / rhs%pressure
      opr(6:) = lhs(6:) / rhs%partial_densities
   else
      allocate(opr(1:5))
      opr(1) = lhs(1) / rhs%density
      opr(2) = lhs(2) / rhs%velocity%x
      opr(3) = lhs(3) / rhs%velocity%y
      opr(4) = lhs(4) / rhs%velocity%z
      opr(5) = lhs(5) / rhs%pressure
   endif
   endfunction integer_div_field

   pure function field_div_integer_scalar(lhs, rhs) result(opr)
   !< `/ integer_scalar` operator.
   class(primitive_compressible_multispecie), intent(in) :: lhs    !< Left hand side.
   integer(I_P),                              intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                                :: opr(:) !< Operator result.

   if (allocated(lhs%partial_densities)) then
      allocate(opr(1:5+size(lhs%partial_densities, dim=1)))
      opr(1)  = lhs%density           / rhs
      opr(2)  = lhs%velocity%x        / rhs
      opr(3)  = lhs%velocity%y        / rhs
      opr(4)  = lhs%velocity%z        / rhs
      opr(5)  = lhs%pressure          / rhs
      opr(6:) = lhs%partial_densities / rhs
   else
      allocate(opr(1:5))
      opr(1) = lhs%density    / rhs
      opr(2) = lhs%velocity%x / rhs
      opr(3) = lhs%velocity%y / rhs
      opr(4) = lhs%velocity%z / rhs
      opr(5) = lhs%pressure   / rhs
   endif
   endfunction field_div_integer_scalar

   pure function integer_scalar_div_field(lhs, rhs) result(opr)
   !< `integer_scalar /` operator.
   integer(I_P),                              intent(in) :: lhs    !< Left hand side.
   class(primitive_compressible_multispecie), intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                                :: opr(:) !< Operator result.

   if (allocated(rhs%partial_densities)) then
      allocate(opr(1:5+size(rhs%partial_densities, dim=1)))
      opr(1)  = lhs / rhs%density
      opr(2)  = lhs / rhs%velocity%x
      opr(3)  = lhs / rhs%velocity%y
      opr(4)  = lhs / rhs%velocity%z
      opr(5)  = lhs / rhs%pressure
      opr(6:) = lhs / rhs%partial_densities
   else
      allocate(opr(1:5))
      opr(1) = lhs / rhs%density
      opr(2) = lhs / rhs%velocity%x
      opr(3) = lhs / rhs%velocity%y
      opr(4) = lhs / rhs%velocity%z
      opr(5) = lhs / rhs%pressure
   endif
   endfunction integer_scalar_div_field

   pure function field_div_real(lhs, rhs) result(opr)
   !< `/ real` operator.
   class(primitive_compressible_multispecie), intent(in) :: lhs     !< Left hand side.
   real(R_P),                                 intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                                :: opr(:)  !< Operator result.

   if (lhs.compatible.rhs) then
      allocate(opr(1:5+size(lhs%partial_densities, dim=1)))
      opr(1)  = lhs%density           / rhs(1)
      opr(2)  = lhs%velocity%x        / rhs(2)
      opr(3)  = lhs%velocity%y        / rhs(3)
      opr(4)  = lhs%velocity%z        / rhs(4)
      opr(5)  = lhs%pressure          / rhs(5)
      opr(6:) = lhs%partial_densities / rhs(6:)
   else
      allocate(opr(1:5))
      opr(1) = lhs%density    / rhs(1)
      opr(2) = lhs%velocity%x / rhs(2)
      opr(3) = lhs%velocity%y / rhs(3)
      opr(4) = lhs%velocity%z / rhs(4)
      opr(5) = lhs%pressure   / rhs(5)
   endif
   endfunction field_div_real

   pure function real_div_field(lhs, rhs) result(opr)
   !< `real /` operator.
   real(R_P),                                 intent(in) :: lhs(1:) !< Left hand side.
   class(primitive_compressible_multispecie), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                                :: opr(:)  !< Operator result.

   if (lhs.compatible.rhs) then
      allocate(opr(1:5+size(rhs%partial_densities, dim=1)))
      opr(1)  = lhs(1)  / rhs%density
      opr(2)  = lhs(2)  / rhs%velocity%x
      opr(3)  = lhs(3)  / rhs%velocity%y
      opr(4)  = lhs(4)  / rhs%velocity%z
      opr(5)  = lhs(5)  / rhs%pressure
      opr(6:) = lhs(6:) / rhs%partial_densities
   else
      allocate(opr(1:5))
      opr(1) = lhs(1) / rhs%density
      opr(2) = lhs(2) / rhs%velocity%x
      opr(3) = lhs(3) / rhs%velocity%y
      opr(4) = lhs(4) / rhs%velocity%z
      opr(5) = lhs(5) / rhs%pressure
   endif
   endfunction real_div_field

   pure function field_div_real_scalar(lhs, rhs) result(opr)
   !< `/ real_scalar` operator.
   class(primitive_compressible_multispecie), intent(in) :: lhs    !< Left hand side.
   real(R_P),                                 intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                                :: opr(:) !< Operator result.

   if (allocated(lhs%partial_densities)) then
      allocate(opr(1:5+size(lhs%partial_densities, dim=1)))
      opr(1)  = lhs%density           / rhs
      opr(2)  = lhs%velocity%x        / rhs
      opr(3)  = lhs%velocity%y        / rhs
      opr(4)  = lhs%velocity%z        / rhs
      opr(5)  = lhs%pressure          / rhs
      opr(6:) = lhs%partial_densities / rhs
   else
      allocate(opr(1:5))
      opr(1) = lhs%density    / rhs
      opr(2) = lhs%velocity%x / rhs
      opr(3) = lhs%velocity%y / rhs
      opr(4) = lhs%velocity%z / rhs
      opr(5) = lhs%pressure   / rhs
   endif
   endfunction field_div_real_scalar

   pure function real_scalar_div_field(lhs, rhs) result(opr)
   !< `real_scalar /` operator.
   real(R_P),                                 intent(in) :: lhs    !< Left hand side.
   class(primitive_compressible_multispecie), intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                                :: opr(:) !< Operator result.

   if (allocated(rhs%partial_densities)) then
      allocate(opr(1:5+size(rhs%partial_densities, dim=1)))
      opr(1)  = lhs / rhs%density
      opr(2)  = lhs / rhs%velocity%x
      opr(3)  = lhs / rhs%velocity%y
      opr(4)  = lhs / rhs%velocity%z
      opr(5)  = lhs / rhs%pressure
      opr(6:) = lhs / rhs%partial_densities
   else
      allocate(opr(1:5))
      opr(1) = lhs / rhs%density
      opr(2) = lhs / rhs%velocity%x
      opr(3) = lhs / rhs%velocity%y
      opr(4) = lhs / rhs%velocity%z
      opr(5) = lhs / rhs%pressure
   endif
   endfunction real_scalar_div_field

   ! *
   pure function field_mul_field(lhs, rhs) result(opr)
   !< `*` operator.
   class(primitive_compressible_multispecie), intent(in) :: lhs    !< Left hand side.
   class(field_object),                       intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                                :: opr(:) !< Operator result.

   select type(rhs)
   class is(primitive_compressible_multispecie)
      if (lhs.compatible.rhs) then
         allocate(opr(1:5+size(lhs%partial_densities, dim=1)))
         opr(1)  = lhs%density           * rhs%density
         opr(2)  = lhs%velocity%x        * rhs%velocity%x
         opr(3)  = lhs%velocity%y        * rhs%velocity%y
         opr(4)  = lhs%velocity%z        * rhs%velocity%z
         opr(5)  = lhs%pressure          * rhs%pressure
         opr(6:) = lhs%partial_densities * rhs%partial_densities
      else
         allocate(opr(1:5))
         opr(1) = lhs%density    * rhs%density
         opr(2) = lhs%velocity%x * rhs%velocity%x
         opr(3) = lhs%velocity%y * rhs%velocity%y
         opr(4) = lhs%velocity%z * rhs%velocity%z
         opr(5) = lhs%pressure   * rhs%pressure
      endif
   endselect
   endfunction field_mul_field

   pure function field_mul_integer(lhs, rhs) result(opr)
   !< `* integer` operator.
   class(primitive_compressible_multispecie), intent(in) :: lhs     !< Left hand side.
   integer(I_P),                              intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                                :: opr(:)  !< Operator result.

   if (lhs.compatible.rhs) then
      allocate(opr(1:5+size(lhs%partial_densities, dim=1)))
      opr(1)  = lhs%density           * rhs(1)
      opr(2)  = lhs%velocity%x        * rhs(2)
      opr(3)  = lhs%velocity%y        * rhs(3)
      opr(4)  = lhs%velocity%z        * rhs(4)
      opr(5)  = lhs%pressure          * rhs(5)
      opr(6:) = lhs%partial_densities * rhs(6:)
   else
      allocate(opr(1:5))
      opr(1) = lhs%density    * rhs(1)
      opr(2) = lhs%velocity%x * rhs(2)
      opr(3) = lhs%velocity%y * rhs(3)
      opr(4) = lhs%velocity%z * rhs(4)
      opr(5) = lhs%pressure   * rhs(5)
   endif
   endfunction field_mul_integer

   pure function integer_mul_field(lhs, rhs) result(opr)
   !< `integer *` operator.
   integer(I_P),                              intent(in) :: lhs(1:) !< Left hand side.
   class(primitive_compressible_multispecie), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                                :: opr(:)  !< Operator result.

   if (lhs.compatible.rhs) then
      allocate(opr(1:5+size(rhs%partial_densities, dim=1)))
      opr(1)  = lhs(1)  * rhs%density
      opr(2)  = lhs(2)  * rhs%velocity%x
      opr(3)  = lhs(3)  * rhs%velocity%y
      opr(4)  = lhs(4)  * rhs%velocity%z
      opr(5)  = lhs(5)  * rhs%pressure
      opr(6:) = lhs(6:) * rhs%partial_densities
   else
      allocate(opr(1:5))
      opr(1) = lhs(1) * rhs%density
      opr(2) = lhs(2) * rhs%velocity%x
      opr(3) = lhs(3) * rhs%velocity%y
      opr(4) = lhs(4) * rhs%velocity%z
      opr(5) = lhs(5) * rhs%pressure
   endif
   endfunction integer_mul_field

   pure function field_mul_integer_scalar(lhs, rhs) result(opr)
   !< `* integer_scalar` operator.
   class(primitive_compressible_multispecie), intent(in) :: lhs    !< Left hand side.
   integer(I_P),                              intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                                :: opr(:) !< Operator result.

   if (allocated(lhs%partial_densities)) then
      allocate(opr(1:5+size(lhs%partial_densities, dim=1)))
      opr(1)  = lhs%density           * rhs
      opr(2)  = lhs%velocity%x        * rhs
      opr(3)  = lhs%velocity%y        * rhs
      opr(4)  = lhs%velocity%z        * rhs
      opr(5)  = lhs%pressure          * rhs
      opr(6:) = lhs%partial_densities * rhs
   else
      allocate(opr(1:5))
      opr(1) = lhs%density    * rhs
      opr(2) = lhs%velocity%x * rhs
      opr(3) = lhs%velocity%y * rhs
      opr(4) = lhs%velocity%z * rhs
      opr(5) = lhs%pressure   * rhs
   endif
   endfunction field_mul_integer_scalar

   pure function integer_scalar_mul_field(lhs, rhs) result(opr)
   !< `integer_scalar *` operator.
   integer(I_P),                              intent(in) :: lhs    !< Left hand side.
   class(primitive_compressible_multispecie), intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                                :: opr(:) !< Operator result.

   if (allocated(rhs%partial_densities)) then
      allocate(opr(1:5+size(rhs%partial_densities, dim=1)))
      opr(1)  = lhs * rhs%density
      opr(2)  = lhs * rhs%velocity%x
      opr(3)  = lhs * rhs%velocity%y
      opr(4)  = lhs * rhs%velocity%z
      opr(5)  = lhs * rhs%pressure
      opr(6:) = lhs * rhs%partial_densities
   else
      allocate(opr(1:5))
      opr(1) = lhs * rhs%density
      opr(2) = lhs * rhs%velocity%x
      opr(3) = lhs * rhs%velocity%y
      opr(4) = lhs * rhs%velocity%z
      opr(5) = lhs * rhs%pressure
   endif
   endfunction integer_scalar_mul_field

   pure function field_mul_real(lhs, rhs) result(opr)
   !< `* real` operator.
   class(primitive_compressible_multispecie), intent(in) :: lhs     !< Left hand side.
   real(R_P),                                 intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                                :: opr(:)  !< Operator result.

   if (lhs.compatible.rhs) then
      allocate(opr(1:5+size(lhs%partial_densities, dim=1)))
      opr(1)  = lhs%density           * rhs(1)
      opr(2)  = lhs%velocity%x        * rhs(2)
      opr(3)  = lhs%velocity%y        * rhs(3)
      opr(4)  = lhs%velocity%z        * rhs(4)
      opr(5)  = lhs%pressure          * rhs(5)
      opr(6:) = lhs%partial_densities * rhs(6:)
   else
      allocate(opr(1:5))
      opr(1) = lhs%density    * rhs(1)
      opr(2) = lhs%velocity%x * rhs(2)
      opr(3) = lhs%velocity%y * rhs(3)
      opr(4) = lhs%velocity%z * rhs(4)
      opr(5) = lhs%pressure   * rhs(5)
   endif
   endfunction field_mul_real

   pure function real_mul_field(lhs, rhs) result(opr)
   !< `real *` operator.
   real(R_P),                                 intent(in) :: lhs(1:) !< Left hand side.
   class(primitive_compressible_multispecie), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                                :: opr(:)  !< Operator result.

   if (lhs.compatible.rhs) then
      allocate(opr(1:5+size(rhs%partial_densities, dim=1)))
      opr(1)  = lhs(1)  * rhs%density
      opr(2)  = lhs(2)  * rhs%velocity%x
      opr(3)  = lhs(3)  * rhs%velocity%y
      opr(4)  = lhs(4)  * rhs%velocity%z
      opr(5)  = lhs(5)  * rhs%pressure
      opr(6:) = lhs(6:) * rhs%partial_densities
   else
      allocate(opr(1:5))
      opr(1) = lhs(1) * rhs%density
      opr(2) = lhs(2) * rhs%velocity%x
      opr(3) = lhs(3) * rhs%velocity%y
      opr(4) = lhs(4) * rhs%velocity%z
      opr(5) = lhs(5) * rhs%pressure
   endif
   endfunction real_mul_field

   pure function field_mul_real_scalar(lhs, rhs) result(opr)
   !< `* real_scalar` operator.
   class(primitive_compressible_multispecie), intent(in) :: lhs    !< Left hand side.
   real(R_P),                                 intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                                :: opr(:) !< Operator result.

   if (allocated(lhs%partial_densities)) then
      allocate(opr(1:5+size(lhs%partial_densities, dim=1)))
      opr(1)  = lhs%density           * rhs
      opr(2)  = lhs%velocity%x        * rhs
      opr(3)  = lhs%velocity%y        * rhs
      opr(4)  = lhs%velocity%z        * rhs
      opr(5)  = lhs%pressure          * rhs
      opr(6:) = lhs%partial_densities * rhs
   else
      allocate(opr(1:5))
      opr(1) = lhs%density    * rhs
      opr(2) = lhs%velocity%x * rhs
      opr(3) = lhs%velocity%y * rhs
      opr(4) = lhs%velocity%z * rhs
      opr(5) = lhs%pressure   * rhs
   endif
   endfunction field_mul_real_scalar

   pure function real_scalar_mul_field(lhs, rhs) result(opr)
   !< `real_scalar *` operator.
   real(R_P),                                 intent(in) :: lhs    !< Left hand side.
   class(primitive_compressible_multispecie), intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                                :: opr(:) !< Operator result.

   if (allocated(rhs%partial_densities)) then
      allocate(opr(1:5+size(rhs%partial_densities, dim=1)))
      opr(1)  = lhs * rhs%density
      opr(2)  = lhs * rhs%velocity%x
      opr(3)  = lhs * rhs%velocity%y
      opr(4)  = lhs * rhs%velocity%z
      opr(5)  = lhs * rhs%pressure
      opr(6:) = lhs * rhs%partial_densities
   else
      allocate(opr(1:5))
      opr(1) = lhs * rhs%density
      opr(2) = lhs * rhs%velocity%x
      opr(3) = lhs * rhs%velocity%y
      opr(4) = lhs * rhs%velocity%z
      opr(5) = lhs * rhs%pressure
   endif
   endfunction real_scalar_mul_field

   ! -
   pure function field_sub_field(lhs, rhs) result(opr)
   !< `-` operator.
   class(primitive_compressible_multispecie), intent(in) :: lhs    !< Left hand side.
   class(field_object),                       intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                                :: opr(:) !< Operator result.

   select type(rhs)
   class is(primitive_compressible_multispecie)
      if (lhs.compatible.rhs) then
         allocate(opr(1:5+size(lhs%partial_densities, dim=1)))
         opr(1)  = lhs%density           - rhs%density
         opr(2)  = lhs%velocity%x        - rhs%velocity%x
         opr(3)  = lhs%velocity%y        - rhs%velocity%y
         opr(4)  = lhs%velocity%z        - rhs%velocity%z
         opr(5)  = lhs%pressure          - rhs%pressure
         opr(6:) = lhs%partial_densities - rhs%partial_densities
      else
         allocate(opr(1:5))
         opr(1) = lhs%density    - rhs%density
         opr(2) = lhs%velocity%x - rhs%velocity%x
         opr(3) = lhs%velocity%y - rhs%velocity%y
         opr(4) = lhs%velocity%z - rhs%velocity%z
         opr(5) = lhs%pressure   - rhs%pressure
      endif
   endselect
   endfunction field_sub_field

   pure function field_sub_real(lhs, rhs) result(opr)
   !< `- real` operator.
   class(primitive_compressible_multispecie), intent(in) :: lhs     !< Left hand side.
   real(R_P),                                 intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable                                :: opr(:)  !< Operator result.

   if (lhs.compatible.rhs) then
      allocate(opr(1:5+size(lhs%partial_densities, dim=1)))
      opr(1)  = lhs%density           - rhs(1)
      opr(2)  = lhs%velocity%x        - rhs(2)
      opr(3)  = lhs%velocity%y        - rhs(3)
      opr(4)  = lhs%velocity%z        - rhs(4)
      opr(5)  = lhs%pressure          - rhs(5)
      opr(6:) = lhs%partial_densities - rhs(6:)
   else
      allocate(opr(1:5))
      opr(1) = lhs%density    - rhs(1)
      opr(2) = lhs%velocity%x - rhs(2)
      opr(3) = lhs%velocity%y - rhs(3)
      opr(4) = lhs%velocity%z - rhs(4)
      opr(5) = lhs%pressure   - rhs(5)
   endif
   endfunction field_sub_real

   pure function real_sub_field(lhs, rhs) result(opr)
   !< `real -` operator.
   real(R_P),                                 intent(in) :: lhs(1:) !< Left hand side.
   class(primitive_compressible_multispecie), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable                                :: opr(:)  !< Operator result.

   if (lhs.compatible.rhs) then
      allocate(opr(1:5+size(rhs%partial_densities, dim=1)))
      opr(1)  = lhs(1)  - rhs%density
      opr(2)  = lhs(2)  - rhs%velocity%x
      opr(3)  = lhs(3)  - rhs%velocity%y
      opr(4)  = lhs(4)  - rhs%velocity%z
      opr(5)  = lhs(5)  - rhs%pressure
      opr(6:) = lhs(6:) - rhs%partial_densities
   else
      allocate(opr(1:5))
      opr(1) = lhs(1) - rhs%density
      opr(2) = lhs(2) - rhs%velocity%x
      opr(3) = lhs(3) - rhs%velocity%y
      opr(4) = lhs(4) - rhs%velocity%z
      opr(5) = lhs(5) - rhs%pressure
   endif
   endfunction real_sub_field

   pure function negative(self) result(opr)
   !< Unary operator `- field`.
   class(primitive_compressible_multispecie), intent(in) :: self   !< Primitive.
   real(R_P), allocatable                                :: opr(:) !< Operator result.

   opr = - self%array()
   endfunction negative

   ! **
   pure function field_pow_integer(lhs, rhs) result(opr)
   !< `** integer` operator.
   class(primitive_compressible_multispecie), intent(in) :: lhs    !< Left hand side.
   integer(I_P),                              intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable                                :: opr(:) !< Operator result.

   if (allocated(lhs%partial_densities)) then
      allocate(opr(1:5+size(lhs%partial_densities, dim=1)))
      opr(1)  = lhs%density           ** rhs
      opr(2)  = lhs%velocity%x        ** rhs
      opr(3)  = lhs%velocity%y        ** rhs
      opr(4)  = lhs%velocity%z        ** rhs
      opr(5)  = lhs%pressure          ** rhs
      opr(6:) = lhs%partial_densities ** rhs
   else
      allocate(opr(1:5))
      opr(1) = lhs%density    ** rhs
      opr(2) = lhs%velocity%x ** rhs
      opr(3) = lhs%velocity%y ** rhs
      opr(4) = lhs%velocity%z ** rhs
      opr(5) = lhs%pressure   ** rhs
   endif
   endfunction field_pow_integer

   pure function field_pow_real(lhs, rhs) result(opr)
   !< `** real` operator.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   real(R_P),                                 intent(in) :: rhs !< Right hand side.
   real(R_P), allocatable                                :: opr(:) !< Operator result.

   if (allocated(lhs%partial_densities)) then
      allocate(opr(1:5+size(lhs%partial_densities, dim=1)))
      opr(1)  = lhs%density           ** rhs
      opr(2)  = lhs%velocity%x        ** rhs
      opr(3)  = lhs%velocity%y        ** rhs
      opr(4)  = lhs%velocity%z        ** rhs
      opr(5)  = lhs%pressure          ** rhs
      opr(6:) = lhs%partial_densities ** rhs
   else
      allocate(opr(1:5))
      opr(1) = lhs%density    ** rhs
      opr(2) = lhs%velocity%x ** rhs
      opr(3) = lhs%velocity%y ** rhs
      opr(4) = lhs%velocity%z ** rhs
      opr(5) = lhs%pressure   ** rhs
   endif
   endfunction field_pow_real

   ! =
   pure subroutine assign_field(lhs, rhs)
   !< Operator `=`.
   class(primitive_compressible_multispecie), intent(inout) :: lhs !< Left hand side.
   class(field_object),                       intent(in)    :: rhs !< Right hand side.

   select type(rhs)
   class is(primitive_compressible_multispecie)
                                            lhs%density           = rhs%density
                                            lhs%velocity          = rhs%velocity
                                            lhs%pressure          = rhs%pressure
      if (allocated(rhs%partial_densities)) lhs%partial_densities = rhs%partial_densities
   endselect
   endsubroutine assign_field

   pure subroutine assign_real(lhs, rhs)
   !< Operator `field = real`.
   class(primitive_compressible_multispecie), intent(inout) :: lhs     !< Left hand side.
   real(R_P),                                 intent(in)    :: rhs(1:) !< Right hand side.

                              lhs%density           = rhs(1)
                              lhs%velocity%x        = rhs(2)
                              lhs%velocity%y        = rhs(3)
                              lhs%velocity%z        = rhs(4)
                              lhs%pressure          = rhs(5)
   if (size(rhs, dim=1) >= 6) lhs%partial_densities = rhs(6:)
   endsubroutine assign_real

   ! ==
   elemental function eq(lhs, rhs) result(opr)
   !< Operator `=='.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   class(field_object),                       intent(in) :: rhs !< Right hand side.
   logical                                               :: opr !< Operator result.
   integer(I_P)                                          :: d   !< Counter.

   select type(rhs)
   class is(primitive_compressible_multispecie)
               opr = lhs%density  == rhs%density
      if (opr) opr = lhs%velocity == rhs%velocity
      if (opr) opr = lhs%pressure == rhs%pressure
      if (opr.and.allocated(lhs%partial_densities).and.allocated(rhs%partial_densities)) then
         if (opr) opr = lhs.compatible.rhs
         if (opr) then
            do d=1, size(lhs%partial_densities, dim=1)
               opr = lhs%partial_densities(d) == rhs%partial_densities(d)
               if (.not.opr) exit
            enddo
         endif
      elseif (opr.and.allocated(lhs%partial_densities).and.(.not.allocated(rhs%partial_densities))) then
         opr = .false.
      elseif (opr.and.(.not.allocated(lhs%partial_densities)).and.allocated(rhs%partial_densities)) then
         opr = .false.
      endif
   endselect
   endfunction eq

   ! /=
   elemental function not_eq(lhs, rhs) result(opr)
   !< Operator `/='.
   class(primitive_compressible_multispecie), intent(in) :: lhs !< Left hand side.
   class(field_object),                       intent(in) :: rhs !< Right hand side.
   logical                                               :: opr !< Operator result.

   opr = .not.(lhs%eq(rhs=rhs))
   endfunction not_eq

   ! private non TBP
   pure function primitive_compressible_multispecie_instance(density, velocity, pressure, partial_densities) result(instance)
   !< Return and instance of [[primitive_compressible_multispecie]].
   !<
   !< @note This procedure is used for overloading [[primitive_compressible_multispecie]] name.
   real(R_P),    intent(in), optional       :: density               !< Density field.
   type(vector), intent(in), optional       :: velocity              !< Velocity field.
   real(R_P),    intent(in), optional       :: pressure              !< Pressure field.
   real(R_P),    intent(in), optional       :: partial_densities(1:) !< Partial densities field.
   type(primitive_compressible_multispecie) :: instance !< Instance of [[primitive_compressible_multispecie]].

   if (present(density          )) instance%density           = density
   if (present(velocity         )) instance%velocity          = velocity
   if (present(pressure         )) instance%pressure          = pressure
   if (present(partial_densities)) instance%partial_densities = partial_densities
   endfunction primitive_compressible_multispecie_instance
endmodule flow_primitive_compressible_multispecie
