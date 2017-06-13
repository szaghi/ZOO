!< Define the compressible Riemann (states) Roe pattern for FORESEER library.

module foreseer_riemann_pattern_compressible_roe
!< Define the compressible Riemann (states) Roe pattern for FORESEER library.

use flow_conservative_object, only : conservative_object
use flow_eos_object, only : eos_object
use foreseer_riemann_pattern_compressible_object, only : riemann_pattern_compressible_object
use penf, only : R8P
use vecfor, only : vector

implicit none
private
public :: riemann_pattern_compressible_roe

type, extends(riemann_pattern_compressible_object) :: riemann_pattern_compressible_roe
   !< Compressible Riemann (states) PVL pattern object class.
   contains
      ! deferred methods
      procedure, pass(self) :: compute               !< Compute whole pattern.
      procedure, pass(self) :: compute_waves_extrema !< Compute waves speed extrema.
      ! public methods
      procedure, pass(self) :: compute_roe_state !< Compute Roe state linearization.
      ! private methods
      procedure, pass(self), private :: compute_up23 !< Compute interstates velocity and pressure.
endtype riemann_pattern_compressible_roe

abstract interface
   pure subroutine compute_waves_interface(self)
   !< Compute waves pattern.
   import :: riemann_pattern_compressible_roe
   class(riemann_pattern_compressible_roe), intent(inout) :: self !< Riemann (states) pattern solution.
   endsubroutine compute_waves_interface
endinterface

contains
   ! deferred methods
   elemental subroutine compute(self)
   !< Compute whole pattern.
   class(riemann_pattern_compressible_roe), intent(inout) :: self !< Riemann (states) pattern solution.

   endsubroutine compute

   elemental subroutine compute_waves_extrema(self)
   !< Compute waves speed extrema.
   class(riemann_pattern_compressible_roe), intent(inout) :: self !< Riemann (states) pattern solution.

   call self%compute_up23

   ! compute left state
   if (self%u23 < self%u_1) then ! shock
      self%s_1 = self%u_1 - self%a_1 * sqrt(1._R8P + 0.5_R8P * self%eos_1%gp1() / self%eos_1%g() * (self%p23 / self%p_1 - 1._R8P))
   else ! rarefaction
      self%s_1 = self%u_1 - self%a_1
   endif
   ! compute right state
   if (self%u23 > self%u_4) then ! shock
      self%s_4 = self%u_4 + self%a_4 * sqrt(1._R8P + 0.5_R8P * self%eos_4%gp1() / self%eos_4%g() * (self%p23 / self%p_4 - 1._R8P))
   else ! rarefaction
      self%s_4 = self%u_4  + self%a_4
   endif
   endsubroutine compute_waves_extrema

   ! public methods
   elemental subroutine compute_roe_state(self, r_roe, u_roe, a_roe, H_roe)
   !< Compute Roe state linearization.
   class(riemann_pattern_compressible_roe), intent(inout) :: self   !< Riemann (states) pattern solution.
   real(R8P),                               intent(out)   :: r_roe  !< Density of Roe average state.
   real(R8P),                               intent(out)   :: u_roe  !< Velocity of Roe average state.
   real(R8P),                               intent(out)   :: a_roe  !< Speed of sound of Roe average state.
   real(R8P),                               intent(out)   :: H_roe  !< Total specific entalpy of Roe average state
   real(R8P)                                              :: x      !< Densities mean `sqrt(r1)/(sqrt(r1)+sqrt(r4))`.
   real(R8P)                                              :: omx    !< Densities mean complement `1-x=sqrt(r4)/(sqrt(r1)+sqrt(r4))`.
   real(R8P)                                              :: cp_roe !< Specific heats cp of Roe average state
   real(R8P)                                              :: cv_roe !< Specific heats cv of Roe average state
   real(R8P)                                              :: g_roe  !< Specific heats ratio of Roe average state.

   associate(r_1=>self%r_1, r_4=>self%r_4, u_1=>self%u_1, u_4=>self%u_4, p_1=>self%p_1, p_4=>self%p_4,               &
             H_1=>self%eos_1%total_entalpy(density=self%r_1, pressure=self%p_1, velocity_sq_norm=self%u_1*self%u_1), &
             H_4=>self%eos_4%total_entalpy(density=self%r_4, pressure=self%p_4, velocity_sq_norm=self%u_4*self%u_4), &
             cp_1=>self%eos_1%cp(), cv_1=>self%eos_1%cv(), cp_4=>self%eos_4%cp(), cv_4=>self%eos_4%cv())
      x      = sqrt(r_1) / (sqrt(r_1) + sqrt(r_4))
      omx    = 1._R8P - x
      r_roe  = sqrt(r_1 * r_4)
      u_roe  = u_1  * x + u_4  * omx
      H_roe  = H_1  * x + H_4  * omx
      cp_roe = cp_1 * x + cp_4 * omx
      cv_roe = cv_1 * x + cv_4 * omx
      g_roe  = cp_roe / cv_roe
      a_roe  = sqrt((g_roe - 1._R8P) * (H_roe - 0.5_R8P * u_roe * u_roe))
   endassociate
   endsubroutine compute_roe_state

   ! public methods
   elemental subroutine compute_up23(self)
   !< Compute interstates velocity and pressure.
   class(riemann_pattern_compressible_roe), intent(inout) :: self   !< Riemann (states) pattern solution.
   real(R8P)                                              :: x      !< Densities mean `sqrt(r1)/(sqrt(r1)+sqrt(r4))`.
   real(R8P)                                              :: omx    !< Densities mean complement `1-x=sqrt(r4)/(sqrt(r1)+sqrt(r4))`.
   real(R8P)                                              :: r_roe  !< Density of Roe average state.
   real(R8P)                                              :: cp_roe !< Specific heats cp of Roe average state
   real(R8P)                                              :: cv_roe !< Specific heats cv of Roe average state
   real(R8P)                                              :: g_roe  !< Specific heats ratio of Roe average state.
   real(R8P)                                              :: H_roe  !< Total specific entalpy of Roe average state

   associate(r_1=>self%r_1, r_4=>self%r_4, u_1=>self%u_1, u_4=>self%u_4, p_1=>self%p_1, p_4=>self%p_4,               &
             u23=>self%u23, p23=>self%p23,                                                                           &
             H_1=>self%eos_1%total_entalpy(density=self%r_1, pressure=self%p_1, velocity_sq_norm=self%u_1*self%u_1), &
             H_4=>self%eos_4%total_entalpy(density=self%r_4, pressure=self%p_4, velocity_sq_norm=self%u_4*self%u_4), &
             cp_1=>self%eos_1%cp(), cv_1=>self%eos_1%cv(), cp_4=>self%eos_4%cp(), cv_4=>self%eos_4%cv())
      x      = sqrt(r_1) / (sqrt(r_1) + sqrt(r_4))
      omx    = 1._R8P - x
      r_roe  = sqrt(r_1 * r_4)
      u23    = u_1  * x + u_4  * omx
      H_roe  = H_1  * x + H_4  * omx
      cp_roe = cp_1 * x + cp_4 * omx
      cv_roe = cv_1 * x + cv_4 * omx
      g_roe  = cp_roe / cv_roe
      p23    = (H_roe - 0.5_R8P * u23 * u23) * (g_roe - 1._R8P) / g_roe * r_roe
   endassociate
   endsubroutine compute_up23
endmodule foreseer_riemann_pattern_compressible_roe
