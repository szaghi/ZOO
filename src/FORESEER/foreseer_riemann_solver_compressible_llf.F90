!< Define the Local Lax-Friedrichs (known also as Rusanov) Riemann solver of FORESEER library.

module foreseer_riemann_solver_compressible_llf
!< Define the Local Lax-Friedrichs (known also as Rusanov) Riemann solver of FORESEER library.

use flow_conservative_compressible, only : conservative_compressible, conservative_compressible_pointer
use flow_conservative_object, only : conservative_object
use flow_eos_object, only : eos_object
use foreseer_riemann_pattern_compressible_pvl, only : riemann_pattern_compressible_pvl
use foreseer_riemann_solver_object, only : riemann_solver_object
use penf, only : R8P
use vecfor, only : vector

implicit none
private
public :: riemann_solver_compressible_llf

type, extends(riemann_solver_object) :: riemann_solver_compressible_llf
   !< Local Lax-Friedrichs (known also as Rusanov) Riemann Solver.
   !<
   !< @note This is the implemention for [[conservative_compressible]] Riemann states.
   contains
      ! public deferred methods
      procedure, pass(self) :: initialize       !< Initialize solver.
      procedure, pass(lhs)  :: riem_assign_riem !< `=` operator.
      procedure, pass(self) :: solve            !< Solve Riemann Problem.
endtype riemann_solver_compressible_llf

contains
   ! public deferred methods
   subroutine initialize(self, config)
   !< Initialize solver.
   class(riemann_solver_compressible_llf), intent(inout)        :: self    !< Solver.
   character(len=*),                       intent(in), optional :: config  !< Configuration for solver algorithm.
   character(len=:), allocatable                                :: config_ !< Configuration for solver algorithm, local variable.

   config_ = 'up23' ; if (present(config)) config_ = config
   ! call self%solver_pvl%initialize(config=config_)
   endsubroutine initialize

   pure subroutine riem_assign_riem(lhs, rhs)
   !< `=` operator.
   !<
   !< @TODO Update this if solver is updated.
   class(riemann_solver_compressible_llf), intent(inout) :: lhs !< Left hand side.
   class(riemann_solver_object),           intent(in)    :: rhs !< Right hand side.
   endsubroutine riem_assign_riem

   subroutine solve(self, eos_left, state_left, eos_right, state_right, normal, fluxes)
   !< Solve Riemann Problem.
   !<
   !< Approximate Riemann Solver based on (local) Lax-Friedrichs (known also as Rusanov) algorithm.
   class(riemann_solver_compressible_llf), intent(in)    :: self         !< Solver.
   class(eos_object),                      intent(in)    :: eos_left     !< Equation of state for left state.
   class(conservative_object),             intent(in)    :: state_left   !< Left Riemann state.
   class(eos_object),                      intent(in)    :: eos_right    !< Equation of state for right state.
   class(conservative_object),             intent(in)    :: state_right  !< Right Riemann state.
   type(vector),                           intent(in)    :: normal       !< Normal (versor) of face where fluxes are given.
   class(conservative_object),             intent(inout) :: fluxes       !< Fluxes of the Riemann Problem solution.
   type(riemann_pattern_compressible_pvl)                :: pattern      !< Riemann (states) PVL pattern solution.
   type(conservative_compressible)                       :: state_left_  !< Left Riemann state, local variable.
   type(conservative_compressible)                       :: state_right_ !< Right Riemann state, local variable.
   type(conservative_compressible)                       :: fluxes_left  !< Fluxes of left state.
   type(conservative_compressible)                       :: fluxes_right !< Fluxes of right state.
   real(R8P)                                             :: lmax         !< Maximum wave speed estimation.

   state_left_ = state_left ; call state_left_%normalize(eos=eos_left, normal=normal)
   state_right_ = state_right ; call state_right_%normalize(eos=eos_right, normal=normal)
   call pattern%initialize(eos_left=eos_left, state_left=state_left_, eos_right=eos_right, state_right=state_right_, normal=normal)
   call pattern%compute_waves_extrema
   lmax = max(abs(pattern%s_1), abs(pattern%s_4))
   call state_left_%compute_fluxes(eos=eos_left, normal=normal, fluxes=fluxes_left)
   call state_right_%compute_fluxes(eos=eos_right, normal=normal, fluxes=fluxes_right)
   select type(fluxes)
   type is(conservative_compressible)
#ifdef __GFORTRAN__
      fluxes = 0.5_R8P * (fluxes_left + fluxes_right - (lmax * (state_right_ - state_left_)))
#else
      error stop 'error: Intel fortran still does not support abstract math!'
#endif
   endselect
   endsubroutine solve
endmodule foreseer_riemann_solver_compressible_llf
