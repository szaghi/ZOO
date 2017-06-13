!< Define the HLLC Riemann solver of FORESEER library.

module foreseer_riemann_solver_compressible_hllc
!< Define the HLLC Riemann solver of FORESEER library.

use flow_conservative_compressible, only : conservative_compressible, conservative_compressible_pointer
use flow_conservative_object, only : conservative_object
use flow_eos_object, only : eos_object
use foreseer_riemann_pattern_compressible_pvl, only : riemann_pattern_compressible_pvl
use foreseer_riemann_solver_object, only : riemann_solver_object
use penf, only : R8P
use vecfor, only : vector

implicit none
private
public :: riemann_solver_compressible_hllc

type, extends(riemann_solver_object) :: riemann_solver_compressible_hllc
   !< HLLC (Harten, Lax, Van Leer, Toro) Riemann Solver.
   !<
   !< @note This is the implemention for [[conservative_compressible]] Riemann states.
   contains
      ! public deferred methods
      procedure, pass(self) :: initialize       !< Initialize solver.
      procedure, pass(lhs)  :: riem_assign_riem !< `=` operator.
      procedure, pass(self) :: solve            !< Solve Riemann Problem.
endtype riemann_solver_compressible_hllc

contains
   ! public deferred methods
   subroutine initialize(self, config)
   !< Initialize solver.
   class(riemann_solver_compressible_hllc), intent(inout)        :: self    !< Solver.
   character(len=*),                        intent(in), optional :: config  !< Configuration for solver algorithm.
   character(len=:), allocatable                                 :: config_ !< Configuration for solver algorithm, local variable.

   config_ = 'up23' ; if (present(config)) config_ = config
   ! call self%solver_pvl%initialize(config=config_)
   endsubroutine initialize

   pure subroutine riem_assign_riem(lhs, rhs)
   !< `=` operator.
   !<
   !< @TODO Update this if solver is updated.
   class(riemann_solver_compressible_hllc), intent(inout) :: lhs !< Left hand side.
   class(riemann_solver_object),            intent(in)    :: rhs !< Right hand side.
   endsubroutine riem_assign_riem

   subroutine solve(self, eos_left, state_left, eos_right, state_right, normal, fluxes)
   !< Solve Riemann Problem.
   !<
   !< Approximate Riemann Solver based on (local) Lax-Friedrichs (known also as Rusanov) algorithm.
   class(riemann_solver_compressible_hllc), intent(in)    :: self         !< Solver.
   class(eos_object),                       intent(in)    :: eos_left     !< Equation of state for left state.
   class(conservative_object),              intent(in)    :: state_left   !< Left Riemann state.
   class(eos_object),                       intent(in)    :: eos_right    !< Equation of state for right state.
   class(conservative_object),              intent(in)    :: state_right  !< Right Riemann state.
   type(vector),                            intent(in)    :: normal       !< Normal (versor) of face where fluxes are given.
   class(conservative_object),              intent(inout) :: fluxes       !< Fluxes of the Riemann Problem solution.
   type(conservative_compressible)                        :: state23      !< Intermediate states.
   type(conservative_compressible)                        :: state_left_  !< Left Riemann state, local variable.
   type(conservative_compressible)                        :: state_right_ !< Right Riemann state, local variable.
   type(riemann_pattern_compressible_pvl)                 :: pattern      !< Riemann (states) PVL pattern solution.
   real(R8P)                                              :: u23          !< Maximum wave speed estimation.

   state_left_ = state_left ; call state_left_%normalize(eos=eos_left, normal=normal)
   state_right_ = state_right ; call state_right_%normalize(eos=eos_right, normal=normal)
   call pattern%initialize(eos_left=eos_left, state_left=state_left_, eos_right=eos_right, state_right=state_right_, normal=normal)
   call pattern%compute_waves_extrema
   associate(r_1=>pattern%r_1, u_1=>pattern%u_1, p_1=>pattern%p_1, g_1=>pattern%eos_1%g(), &
             r_4=>pattern%r_4, u_4=>pattern%u_4, p_4=>pattern%p_4, g_4=>pattern%eos_4%g(), &
             s_1=>pattern%s_1, s_4=>pattern%s_4,                                           &
             E_1=>state_left_%energy/state_left_%density, E_4=>state_right_%energy/state_right_%density)
      u23 = (r_4 * u_4 * (s_4 - u_4) - r_1 * u_1 * (s_1 - u_1) + p_1 - p_4) / &
            (r_4 * (s_4 - u_4) - r_1 * (s_1 - u_1))
      select case(minloc([-s_1, s_1 * u23, u23 * s_4, s_4], dim=1))
      case(1)
         call state_left_%compute_fluxes(eos=eos_left, normal=normal, fluxes=fluxes)
      case(2)
         call state_left_%compute_fluxes(eos=eos_left, normal=normal, fluxes=fluxes)
         state23%density  = r_1 * (s_1 - u_1) / (s_1 - u23)
         state23%momentum = state23%density * u23 * normal
         state23%energy   = state23%density * (E_1 + (u23 - u_1) * (u23 + p_1 / (r_1 * (s_1 - u_1))))
         select type(fluxes)
         type is(conservative_compressible)
#ifdef __GFORTRAN__
            fluxes = fluxes + (s_1 * (state23 - state_left_))
#else
            error stop 'error: Intel fortran still does not support abstract math!'
#endif
         endselect
      case(3)
         call state_right_%compute_fluxes(eos=eos_right, normal=normal, fluxes=fluxes)
         state23%density  = r_4 * (s_4 - u_4) / (s_4 - u23)
         state23%momentum = state23%density * u23 * normal
         state23%energy   = state23%density * (E_4 + (u23 - u_4) * (u23 + p_4 / (r_4 * (s_4 - u_4))))
         select type(fluxes)
         type is(conservative_compressible)
#ifdef __GFORTRAN__
            fluxes = fluxes + (s_4 * (state23 - state_right_))
#else
            error stop 'error: Intel fortran still does not support abstract math!'
#endif
         endselect
      case(4)
         call state_right_%compute_fluxes(eos=eos_right, normal=normal, fluxes=fluxes)
      endselect
   endassociate
   endsubroutine solve
endmodule foreseer_riemann_solver_compressible_hllc
