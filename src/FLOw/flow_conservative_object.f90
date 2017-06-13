!< FLOw **conservative** abstract object.

module flow_conservative_object
!< FLOw **conservative** abstract object.
!<
!< [[conservative_object]] is a class that handles conservative fluid dynamic variables.

use flow_eos_object, only : eos_object
use flow_field_object, only : field_object
use penf, only : R_P
use vecfor, only : vector

implicit none
private
public :: conservative_object

type, extends(field_object), abstract :: conservative_object
  !< **conservative** object.
  contains
      ! deferred methods
      procedure(compute_fluxes_interface), pass(self), deferred :: compute_fluxes !< Compute conservative fluxes.
      procedure(destroy_interface),        pass(self), deferred :: destroy        !< Destroy conservative.
      procedure(initialize_interface),     pass(self), deferred :: initialize     !< Initialize conservative.
      procedure(normalize_interface),      pass(self), deferred :: normalize      !< Normalize with respect a normal vector.
      procedure(pressure_interface),       pass(self), deferred :: pressure       !< Return pressure value.
      procedure(velocity_interface),       pass(self), deferred :: velocity       !< Return velocity vector.
endtype conservative_object

abstract interface
   !< Abstract interfaces of deferred methods of [[conservative_object]].
   subroutine compute_fluxes_interface(self, eos, normal, fluxes)
   !< Compute conservative fluxes.
   import :: conservative_object, eos_object, vector
   class(conservative_object), intent(in)  :: self   !< Conservative.
   class(eos_object),          intent(in)  :: eos    !< Equation of state.
   type(vector),               intent(in)  :: normal !< Normal (versor) of face where fluxes are given.
   class(conservative_object), intent(out) :: fluxes !< Conservative fluxes.
   endsubroutine compute_fluxes_interface

   elemental subroutine destroy_interface(self)
   !< Destroy conservative.
   import :: conservative_object
   class(conservative_object), intent(inout) :: self !< conservative.
   endsubroutine destroy_interface

   subroutine initialize_interface(self, initial_state)
   !< Initialize conservative.
   import :: conservative_object
   class(conservative_object),           intent(inout) :: self          !< conservative.
   class(conservative_object), optional, intent(in)    :: initial_state !< Initial state.
   endsubroutine initialize_interface

   elemental subroutine normalize_interface(self, eos, normal)
   !< *Normalize* conservative with respect a given normal vector.
   import :: conservative_object, eos_object, vector
   class(conservative_object), intent(inout) :: self   !< Conservative.
   class(eos_object),          intent(in)    :: eos    !< Equation of state.
   type(vector),               intent(in)    :: normal !< Normal vector.
   endsubroutine normalize_interface

   elemental function pressure_interface(self, eos) result(pressure_)
   !< Return pressure value.
   import :: conservative_object, eos_object, R_P
   class(conservative_object), intent(in) :: self      !< Conservative.
   class(eos_object),          intent(in) :: eos       !< Equation of state.
   real(R_P)                              :: pressure_ !< Pressure value.
   endfunction pressure_interface

   elemental function velocity_interface(self) result(velocity_)
   !< Return velocity vector.
   import :: conservative_object, vector
   class(conservative_object), intent(in) :: self      !< Conservative.
   type(vector)                           :: velocity_ !< Velocity vector.
   endfunction velocity_interface
endinterface
endmodule flow_conservative_object
