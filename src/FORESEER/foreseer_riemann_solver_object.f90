!< Define the abstract Riemann solver of FORESEER library.

module foreseer_riemann_solver_object
!< Define the abstract Riemann solver of FORESEER library.

use flow_conservative_object, only : conservative_object
use flow_eos_object, only : eos_object
use penf, only : R8P
use vecfor, only : vector

implicit none
private
public :: riemann_solver_object

type, abstract :: riemann_solver_object
   !< Abstract Riemann Solver.
   contains
      ! public operators
      generic :: assignment(=) => riem_assign_riem !< `=` overloading.
      ! public deferred methods
      procedure(initialize_interface),       pass(self), deferred :: initialize       !< Initialize solver.
      ! procedure(description_interface),      pass(self), deferred :: description      !< Return pretty-printed object description.
      procedure(riem_assign_riem_interface), pass(lhs),  deferred :: riem_assign_riem !< `=` operator.
      procedure(solve_interface),            pass(self), deferred :: solve            !< Solve Riemann Problem.
endtype riemann_solver_object

abstract interface
   !< Abstract interfaces of [[riemann_solver_object]] deferred methods.
   subroutine initialize_interface(self, config)
   !< Initialize solver.
   import :: riemann_solver_object
   class(riemann_solver_object), intent(inout)        :: self   !< Solver.
   character(len=*),             intent(in), optional :: config !< Configuration for solver algorithm.
   endsubroutine initialize_interface

   pure function description_interface(self, prefix) result(desc)
   !< Return a pretty-formatted object description.
   import :: riemann_solver_object
   class(riemann_solver_object), intent(in)           :: self   !< Solver.
   character(*),                 intent(in), optional :: prefix !< Prefixing string.
   character(len=:), allocatable                      :: desc   !< Description.
   endfunction description_interface

   pure subroutine riem_assign_riem_interface(lhs, rhs)
   !< `=` operator.
   import :: riemann_solver_object
   class(riemann_solver_object), intent(inout) :: lhs !< Left hand side.
   class(riemann_solver_object), intent(in)    :: rhs !< Right hand side.
   endsubroutine riem_assign_riem_interface

   subroutine solve_interface(self, eos_left, state_left, eos_right, state_right, normal, fluxes)
   !< Solve Riemann Problem.
   import :: conservative_object, eos_object, riemann_solver_object, vector
   class(riemann_solver_object),  intent(in)    :: self        !< Solver.
   class(eos_object),             intent(in)    :: eos_left    !< Equation of state for left state.
   class(conservative_object),    intent(in)    :: state_left  !< Left Riemann state.
   class(eos_object),             intent(in)    :: eos_right   !< Equation of state for right state.
   class(conservative_object),    intent(in)    :: state_right !< Right Riemann state.
   type(vector),                  intent(in)    :: normal      !< Normal (versor) of face where fluxes are given.
   class(conservative_object),    intent(inout) :: fluxes      !< Fluxes of the Riemann Problem solution.
   endsubroutine solve_interface
endinterface
endmodule foreseer_riemann_solver_object
