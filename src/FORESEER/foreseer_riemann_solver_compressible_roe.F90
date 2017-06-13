!< Define the Roe (with the Harten-Hyman entropy fix) Riemann solver of FORESEER library.

module foreseer_riemann_solver_compressible_roe
!< Define the Roe (with the Harten-Hyman entropy fix) Riemann solver of FORESEER library.

use flow_conservative_compressible, only : conservative_compressible, conservative_compressible_pointer
use flow_conservative_object, only : conservative_object
use flow_eos_compressible, only : eos_compressible
use flow_eos_object, only : eos_object
use foreseer_riemann_pattern_compressible_pvl, only : riemann_pattern_compressible_pvl
use foreseer_riemann_pattern_compressible_roe, only : riemann_pattern_compressible_roe
use foreseer_riemann_solver_object, only : riemann_solver_object
use penf, only : R8P
use vecfor, only : vector

implicit none
private
public :: riemann_solver_compressible_roe

type, extends(riemann_solver_object) :: riemann_solver_compressible_roe
   !< Roe (with the Harten-Hyman entropy fix) Riemann Solver.
   !<
   !< @note This is the implemention for [[conservative_compressible]] Riemann states.
   contains
      ! public deferred methods
      procedure, pass(self) :: initialize       !< Initialize solver.
      procedure, pass(lhs)  :: riem_assign_riem !< `=` operator.
      procedure, pass(self) :: solve            !< Solve Riemann Problem.
endtype riemann_solver_compressible_roe

contains
   ! public deferred methods
   subroutine initialize(self, config)
   !< Initialize solver.
   class(riemann_solver_compressible_roe), intent(inout)        :: self    !< Solver.
   character(len=*),                       intent(in), optional :: config  !< Configuration for solver algorithm.
   character(len=:), allocatable                                :: config_ !< Configuration for solver algorithm, local variable.

   config_ = 'up23' ; if (present(config)) config_ = config
   ! call self%solver_pvl%initialize(config=config_)
   endsubroutine initialize

   pure subroutine riem_assign_riem(lhs, rhs)
   !< `=` operator.
   !<
   !< @TODO Update this if solver is updated.
   class(riemann_solver_compressible_roe), intent(inout) :: lhs !< Left hand side.
   class(riemann_solver_object),           intent(in)    :: rhs !< Right hand side.
   endsubroutine riem_assign_riem

   subroutine solve(self, eos_left, state_left, eos_right, state_right, normal, fluxes)
   !< Solve Riemann Problem.
   !<
   !< Approximate Riemann Solver based on Roe (with Harten-Hyman entropy fix) algorithm.
   class(riemann_solver_compressible_roe), intent(in)    :: self          !< Solver.
   class(eos_object),                      intent(in)    :: eos_left      !< Equation of state for left state.
   class(conservative_object),             intent(in)    :: state_left    !< Left Riemann state.
   class(eos_object),                      intent(in)    :: eos_right     !< Equation of state for right state.
   class(conservative_object),             intent(in)    :: state_right   !< Right Riemann state.
   type(vector),                           intent(in)    :: normal        !< Normal (versor) of face where fluxes are given.
   class(conservative_object),             intent(inout) :: fluxes        !< Fluxes of the Riemann Problem solution.
   type(conservative_compressible)                       :: state_left_   !< Left Riemann state, local variable.
   type(conservative_compressible)                       :: state_right_  !< Right Riemann state, local variable.
   type(conservative_compressible)                       :: fluxes_left   !< Fluxes of left state.
   type(conservative_compressible)                       :: fluxes_right  !< Fluxes of right state.
   type(riemann_pattern_compressible_pvl)                :: pattern_pvl   !< Riemann (states) PVL pattern solution.
   type(riemann_pattern_compressible_roe)                :: pattern_roe   !< Riemann (states) Roe pattern solution.
   real(R8P)                                             :: r_roe         !< Density of Roe average state.
   real(R8P)                                             :: u_roe         !< Velocity of Roe average state.
   real(R8P)                                             :: a_roe         !< Speed of sound of Roe average state.
   real(R8P)                                             :: H_roe         !< Total specific entalpy of Roe average state
   real(R8P)                                             :: Dr            !< Density difference `Dr = r_4-r_1`.
   real(R8P)                                             :: Du            !< Velocity difference `Du = u_4-u_1`.
   real(R8P)                                             :: Dp            !< Pressure difference `Dp = p_4-p_1`.
   real(R8P)                                             :: aa1, aa2, aa3 !< Wawes amplitudes Roe's estimation.
   real(R8P)                                             :: ll1, ll2, ll3 !< Wawes speeds Roe's estimation.
   real(R8P)                                             :: ls1,      ls3 !< Wawes speeds Roe's estimation
                                                                          !< with entropy fix of Harten-Hyman.

   state_left_ = state_left ; call state_left_%normalize(eos=eos_left, normal=normal)
   state_right_ = state_right ; call state_right_%normalize(eos=eos_right, normal=normal)
   call pattern_pvl%initialize(eos_left=eos_left, state_left=state_left_, &
                               eos_right=eos_right, state_right=state_right_, normal=normal)
   call pattern_pvl%compute
   associate(r_1=>pattern_pvl%r_1, u_1=>pattern_pvl%u_1, p_1=>pattern_pvl%p_1, s_1=>pattern_pvl%s_1, &
             r_4=>pattern_pvl%r_4, u_4=>pattern_pvl%u_4, p_4=>pattern_pvl%p_4, s_4=>pattern_pvl%s_4, &
             s_2=>pattern_pvl%s_2, s_3=>pattern_pvl%s_3, u23=>pattern_pvl%u23)
      select case(minloc([-s_1, s_1 * s_2, s_2 * u23, u23 * s_3, s_3 * s_4, s_4], dim=1))
      case(1)
         call state_left_%compute_fluxes(eos=eos_left, normal=normal, fluxes=fluxes)
      case(2)
         call pattern_roe%initialize(eos_left=eos_left, state_left=state_left_, &
                                     eos_right=eos_right, state_right=state_right_, normal=normal)
         call pattern_roe%compute_roe_state(r_roe=r_roe, u_roe=u_roe, a_roe=a_roe, H_roe=H_roe)
         Du  = u_4 - u_1
         Dp  = p_4 - p_1
         aa1 = 0.5_R8P * (Dp - r_roe * a_roe * Du) / (a_roe * a_roe)
         ll1 = u_roe - a_roe
         ls1 = s_1 * (s_2 - ll1) / (s_2 - s_1)
         call state_left_%compute_fluxes(eos=eos_left, normal=normal, fluxes=fluxes_left)
         select type(fluxes)
         type is(conservative_compressible)
            fluxes%density  = fluxes_left%density  + aa1 * ls1
            fluxes%momentum = fluxes_left%momentum + aa1 * ls1 * ll1 * normal
            fluxes%energy   = fluxes_left%energy   + aa1 * ls1 * (H_roe - u_roe * a_roe)
         endselect
      case(3, 4)
         call pattern_roe%initialize(eos_left=eos_left, state_left=state_left_, &
                                     eos_right=eos_right, state_right=state_right_, normal=normal)
         call pattern_roe%compute_roe_state(r_roe=r_roe, u_roe=u_roe, a_roe=a_roe, H_roe=H_roe)
         Dr  = r_4 - r_1
         Du  = u_4 - u_1
         Dp  = p_4 - p_1
         aa1 = 0.5_R8P * (Dp - r_roe * a_roe * Du) / (a_roe * a_roe)
         aa2 = Dr - Dp / (a_roe * a_roe)
         aa3 = 0.5_R8P * (Dp + r_roe * a_roe * Du) / (a_roe * a_roe)
         ll1 = u_roe - a_roe
         ll2 = u_roe
         ll3 = u_roe + a_roe
         call state_left_%compute_fluxes (eos=eos_left,  normal=normal, fluxes=fluxes_left)
         call state_right_%compute_fluxes(eos=eos_right, normal=normal, fluxes=fluxes_right)
         select type(fluxes)
         type is(conservative_compressible)
            fluxes%density  = 0.5_R8P * (fluxes_left%density  + fluxes_right%density  - &
                                         (aa1 * abs(ll1) +                              &
                                          aa2 * abs(ll2) +                              &
                                          aa3 * abs(ll3)))
            fluxes%momentum = 0.5_R8P * (fluxes_left%momentum + fluxes_right%momentum - &
                                         (aa1 * abs(ll1) * ll1 +                        &
                                          aa2 * abs(ll2) * ll2 +                        &
                                          aa3 * abs(ll3) * ll3))
            fluxes%energy   = 0.5_R8P * (fluxes_left%energy   + fluxes_right%energy   - &
                                         (aa1 * abs(ll1) * (H_roe - u_roe * a_roe) +    &
                                          aa2 * abs(ll2) * ll2 * ll2 * 0.5_R8P +        &
                                          aa3 * abs(ll3) * (H_roe + u_roe * a_roe)))
         endselect
      case(5)
         call pattern_roe%initialize(eos_left=eos_left, state_left=state_left_, &
                                     eos_right=eos_right, state_right=state_right_, normal=normal)
         call pattern_roe%compute_roe_state(r_roe=r_roe, u_roe=u_roe, a_roe=a_roe, H_roe=H_roe)
         Du  = u_4 - u_1
         Dp  = p_4 - p_1
         aa3 = 0.5_R8P * (Dp + r_roe * a_roe * Du) / (a_roe * a_roe)
         ll3 = u_roe + a_roe
         ls3 = s_4 * (ll3 - s_3) / (s_4 - s_3)
         call state_right_%compute_fluxes(eos=eos_right, normal=normal, fluxes=fluxes_right)
         select type(fluxes)
         type is(conservative_compressible)
            fluxes%density  = fluxes_right%density  + aa3 * ls3
            fluxes%momentum = fluxes_right%momentum + aa3 * ls3 * ll3 * normal
            fluxes%energy   = fluxes_right%energy   + aa3 * ls3 * (H_roe + u_roe * a_roe)
         endselect
      case(6)
         call state_right_%compute_fluxes(eos=eos_right, normal=normal, fluxes=fluxes)
      endselect
   endassociate
   endsubroutine solve
endmodule foreseer_riemann_solver_compressible_roe
