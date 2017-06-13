!< Define the exact (Newton-iterative) Riemann solver of FORESEER library.

module foreseer_riemann_solver_compressible_exact
!< Define the exact (Newton-iterative) Riemann solver of FORESEER library.

use flow_conservative_compressible, only : conservative_compressible, conservative_compressible_pointer
use flow_conservative_object, only : conservative_object
use flow_eos_object, only : eos_object
use foreseer_riemann_pattern_compressible_pvl, only : riemann_pattern_compressible_pvl
use foreseer_riemann_solver_object, only : riemann_solver_object
use penf, only : cton, R8P
use vecfor, only : vector

implicit none
private
public :: riemann_solver_compressible_exact

type, extends(riemann_solver_object) :: riemann_solver_compressible_exact
   !< Exact (Newton-iterative) Riemann Solver.
   !<
   !< @note This is the implemention for [[conservative_compressible]] Riemann states.
   real(R8P) :: tolerance=1.e-10_R8P !< Tolerance on Newton convergence.
   contains
      ! public deferred methods
      procedure, pass(self) :: initialize       !< Initialize solver.
      procedure, pass(lhs)  :: riem_assign_riem !< `=` operator.
      procedure, pass(self) :: solve            !< Solve Riemann Problem.
endtype riemann_solver_compressible_exact

contains
   ! public deferred methods
   subroutine initialize(self, config)
   !< Initialize solver.
   class(riemann_solver_compressible_exact), intent(inout)        :: self    !< Solver.
   character(len=*),                         intent(in), optional :: config  !< Configuration for solver algorithm.
   character(len=:), allocatable                                  :: config_ !< Configuration for solver algorithm, local variable.

   config_ = '1.e-10' ; if (present(config)) config_ = config
   self%tolerance = cton(config_, knd=1._R8P)
   endsubroutine initialize

   pure subroutine riem_assign_riem(lhs, rhs)
   !< `=` operator.
   class(riemann_solver_compressible_exact), intent(inout) :: lhs !< Left hand side.
   class(riemann_solver_object),             intent(in)    :: rhs !< Right hand side.

   select type(rhs)
   type is(riemann_solver_compressible_exact)
      lhs%tolerance = rhs%tolerance
   endselect
   endsubroutine riem_assign_riem

   pure subroutine solve(self, eos_left, state_left, eos_right, state_right, normal, fluxes)
   !< Solve Riemann Problem.
   !<
   !< Approximate Riemann Solver based on (local) Lax-Friedrichs (known also as Rusanov) algorithm.
   class(riemann_solver_compressible_exact), intent(in)    :: self            !< Solver.
   class(eos_object),                        intent(in)    :: eos_left        !< Equation of state for left state.
   class(conservative_object),               intent(in)    :: state_left      !< Left Riemann state.
   class(eos_object),                        intent(in)    :: eos_right       !< Equation of state for right state.
   class(conservative_object),               intent(in)    :: state_right     !< Right Riemann state.
   type(vector),                             intent(in)    :: normal          !< Normal (versor) of face where fluxes are given.
   class(conservative_object),               intent(inout) :: fluxes          !< Fluxes of the Riemann Problem solution.
   type(conservative_compressible)                         :: state_left_  !< Left Riemann state, local variable.
   type(conservative_compressible)                         :: state_right_ !< Right Riemann state, local variable.
   type(riemann_pattern_compressible_pvl)                  :: pattern         !< Riemann (states) pattern solution.
   real(R8P)                                               :: dum, alfa, beta !< Dummies coefficients.
   real(R8P)                                               :: p_2, p_3        !< Pessure of state 2 and 3.
   real(R8P)                                               :: dp2, dp3        !< Derivate of pessure (dp/du) of state 2 and 3.

   state_left_ = state_left ; call state_left_%normalize(eos=eos_left, normal=normal)
   state_right_ = state_right ; call state_right_%normalize(eos=eos_right, normal=normal)
   call pattern%initialize(eos_left=eos_left, state_left=state_left_, eos_right=eos_right, state_right=state_right_, normal=normal)

   ! initiale u23 speed
   if (pattern%p_1 < pattern%p_4) then
     dum  = 0.5_R8P * pattern%eos_4%gm1() / pattern%eos_4%g() ! (gamma - 1) / (gamma * 2)
   else
     dum  = 0.5_R8P * pattern%eos_1%gm1() / pattern%eos_1%g() ! (gamma - 1) / (gamma * 2)
   endif
   alfa = (pattern%p_1 / pattern%p_4) ** dum
   beta = alfa * pattern%eos_1%delta() / pattern%a_1 + pattern%eos_4%delta()/ pattern%a_4
   pattern%u23 = (alfa - 1.0_R8P) / beta +               &
                 0.5_R8P * (pattern%u_1 + pattern%u_4) + &
                 0.5_R8P * (pattern%u_1 - pattern%u_4) * &
                 (alfa * pattern%eos_1%delta() / pattern%a_1 - pattern%eos_4%delta()/ pattern%a_4) / beta

   Newton: do
      call pattern%compute_states23_from_u23(p_2=p_2, p_3=p_3)
      ! evaluate the Newton-Rapson convergence
      if (abs(1.0_R8P - (p_2 / p_3)) >= self%tolerance) then
         dp2 = -1._R8P * pattern%eos_1%g() * p_2 / pattern%a_2
         dp3 =  1._R8P * pattern%eos_4%g() * p_3 / pattern%a_3
         pattern%u23 = pattern%u23 - ((p_2 - p_3) / (dp2-dp3))
      else
        pattern%p23 = p_2 ! p_2 ~= p_3
        exit Newton
      endif
   enddo Newton

   call pattern%compute_fluxes(normal=normal, fluxes=fluxes)
   endsubroutine solve
endmodule foreseer_riemann_solver_compressible_exact
