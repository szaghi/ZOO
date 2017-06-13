!< Define the abstract Riemann (states) pattern for FORESEER library.

module foreseer_riemann_pattern_object
!< Define the abstract Riemann (states) pattern for FORESEER library.

use flow_conservative_object, only : conservative_object
use flow_eos_object, only : eos_object
use vecfor, only : vector

implicit none
private
public :: riemann_pattern_object

type, abstract :: riemann_pattern_object
   !< Riemann (states) pattern object class.
   !<
   !< This pattern is generated after the breaking of the initial discontinuity of the Riemann Problem.
   contains
      ! deferred methods
      procedure(compute_interface),        pass(self), deferred :: compute               !< Compute whole pattern.
      procedure(compute_fluxes_interface), pass(self), deferred :: compute_fluxes        !< Compute fluxes at interface.
      procedure(compute_interface),        pass(self), deferred :: compute_waves_extrema !< Compute waves speed extrema.
      procedure(description_interface),    pass(self), deferred :: description           !< Return pretty-printed description.
      procedure(initialize_interface),     pass(self), deferred :: initialize            !< Initialize pattern.
      procedure(assignment_interface),     pass(lhs),  deferred :: rpat_assign_rpat      !< Operator `=`.
      ! operators
      generic :: assignment(=) => rpat_assign_rpat !< Overload `=`.
endtype riemann_pattern_object

abstract interface
   !< Abstract interfaces of deferred methods of [[riemann_pattern_object]].
   pure subroutine assignment_interface(lhs, rhs)
   !< Operator `=`.
   import :: riemann_pattern_object
   class(riemann_pattern_object), intent(inout) :: lhs !< Left hand side.
   class(riemann_pattern_object), intent(in)    :: rhs !< Right hand side.
   endsubroutine assignment_interface

   elemental subroutine compute_interface(self)
   !< Compute (something) by self.
   import :: riemann_pattern_object
   class(riemann_pattern_object), intent(inout) :: self !< Riemann (states) pattern solution.
   endsubroutine compute_interface

   elemental subroutine compute_fluxes_interface(self, normal, fluxes)
   !< Compute fluxes at initial discontinuity interface.
   import :: conservative_object, riemann_pattern_object, vector
   class(riemann_pattern_object), intent(in)    :: self   !< Riemann (states) pattern solution.
   type(vector),                  intent(in)    :: normal !< Normal (versor) of face where fluxes are given.
   class(conservative_object),    intent(inout) :: fluxes !< Fluxes at initial discontinuity interface.
   endsubroutine compute_fluxes_interface

   pure function description_interface(self, prefix) result(desc)
   !< Return a pretty-formatted object description.
   import :: riemann_pattern_object
   class(riemann_pattern_object), intent(in)           :: self   !< Riemann pattern.
   character(*),                  intent(in), optional :: prefix !< Prefixing string.
   character(len=:), allocatable                       :: desc   !< Description.
   endfunction description_interface

   elemental subroutine initialize_interface(self, eos_left, state_left, eos_right, state_right, normal)
   !< Initialize pattern with left and right states.
   import :: conservative_object, eos_object, riemann_pattern_object, vector
   class(riemann_pattern_object), intent(inout) :: self        !< Riemann (states) pattern solution.
   class(eos_object),             intent(in)    :: eos_left    !< Equation of state for left state.
   class(conservative_object),    intent(in)    :: state_left  !< Left Riemann state.
   class(eos_object),             intent(in)    :: eos_right   !< Equation of state for right state.
   class(conservative_object),    intent(in)    :: state_right !< Right Riemann state.
   type(vector),                  intent(in)    :: normal      !< Normal (versor) of face where fluxes are given.
   endsubroutine initialize_interface
endinterface
endmodule foreseer_riemann_pattern_object
