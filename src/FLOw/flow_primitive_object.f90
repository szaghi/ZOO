!< FLOw **primitive** abstract object.

module flow_primitive_object
!< FLOw **primitive** abstract object.
!<
!< [[primitive_object]] is a class that handles primitive fluid dynamic variables.

use flow_eos_object, only : eos_object
use flow_field_object, only : field_object
use penf, only : R_P
use vecfor, only : vector

implicit none
private
public :: primitive_object

type, extends(field_object), abstract :: primitive_object
  !< **Primitive** object.
  contains
      ! deferred methods
      procedure(destroy_interface),    pass(self), deferred :: destroy    !< Destroy primitive.
      procedure(energy_interface),     pass(self), deferred :: energy     !< Return energy value.
      procedure(initialize_interface), pass(self), deferred :: initialize !< Initialize primitive.
      procedure(momentum_interface),   pass(self), deferred :: momentum   !< Return momentum vector.
      procedure(normalize_interface),  pass(self), deferred :: normalize  !< Normalize with respect a normal vector.
endtype primitive_object

abstract interface
   !< Abstract interfaces of deferred methods of [[primitive_object]].
   elemental subroutine destroy_interface(self)
   !< Destroy primitive.
   import :: primitive_object
   class(primitive_object), intent(inout) :: self !< Primitive.
   endsubroutine destroy_interface

   elemental function energy_interface(self, eos) result(energy_)
   !< Return energy value.
   import :: primitive_object, eos_object, R_P
   class(primitive_object), intent(in) :: self    !< Primitive.
   class(eos_object),       intent(in) :: eos     !< Equation of state.
   real(R_P)                           :: energy_ !< Energy value.
   endfunction energy_interface

   subroutine initialize_interface(self, initial_state)
   !< Initialize primitive.
   import :: primitive_object
   class(primitive_object),           intent(inout) :: self          !< Primitive.
   class(primitive_object), optional, intent(in)    :: initial_state !< Initial state.
   endsubroutine initialize_interface

   elemental function momentum_interface(self) result(momentum_)
   !< Return momentum vector.
   import :: primitive_object, vector
   class(primitive_object), intent(in) :: self      !< Primitive.
   type(vector)                        :: momentum_ !< Momentum vector.
   endfunction momentum_interface

   elemental subroutine normalize_interface(self, normal)
   !< *Normalize* primitive with respect a given normal vector.
   import :: primitive_object, vector
   class(primitive_object), intent(inout) :: self   !< Primitive.
   type(vector),            intent(in)    :: normal !< Normal vector.
   endsubroutine normalize_interface
endinterface
endmodule flow_primitive_object
