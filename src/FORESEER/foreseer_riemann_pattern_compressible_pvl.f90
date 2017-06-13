!< Define the compressible Riemann (states) PVL pattern for FORESEER library.

module foreseer_riemann_pattern_compressible_pvl
!< Define the compressible Riemann (states) PVL pattern for FORESEER library.

use flow_conservative_object, only : conservative_object
use flow_eos_object, only : eos_object
use foreseer_riemann_pattern_compressible_object, only : riemann_pattern_compressible_object
use penf, only : R8P
use vecfor, only : vector

implicit none
private
public :: riemann_pattern_compressible_pvl

type, extends(riemann_pattern_compressible_object) :: riemann_pattern_compressible_pvl
   !< Compressible Riemann (states) PVL pattern object class.
   contains
      ! deferred methods
      procedure, pass(self) :: compute               !< Compute whole pattern.
      procedure, pass(self) :: compute_waves_extrema !< Compute waves speed extrema.
      ! private methods
      procedure, pass(self), private :: compute_up23 !< Compute interstates velocity and pressure.
endtype riemann_pattern_compressible_pvl

abstract interface
   pure subroutine compute_waves_interface(self)
   !< Compute waves pattern.
   import :: riemann_pattern_compressible_pvl
   class(riemann_pattern_compressible_pvl), intent(inout) :: self !< Riemann (states) pattern solution.
   endsubroutine compute_waves_interface
endinterface

contains
   ! deferred methods
   elemental subroutine compute(self)
   !< Compute whole pattern.
   class(riemann_pattern_compressible_pvl), intent(inout) :: self !< Riemann (states) pattern solution.

   call self%compute_up23
   call self%compute_states23_from_up23
   endsubroutine compute

   elemental subroutine compute_waves_extrema(self)
   !< Compute waves speed extrema.
   class(riemann_pattern_compressible_pvl), intent(inout) :: self !< Riemann (states) pattern solution.

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

   ! private methods
   elemental subroutine compute_up23(self)
   !< Compute interstates velocity and pressure.
   class(riemann_pattern_compressible_pvl), intent(inout) :: self !< Riemann (states) pattern solution.
   real(R8P)                                              :: ram  !< Mean value of `r * a`.

   ram = 0.25_R8P * (self%r_1 + self%r_4) * (self%a_1 + self%a_4)
   self%u23 = 0.5_R8P * ((self%u_1 + self%u_4) - (self%p_4 - self%p_1) / ram)
   self%p23 = 0.5_R8P * ((self%p_1 + self%p_4) - (self%u_4 - self%u_1) * ram)
   endsubroutine compute_up23
endmodule foreseer_riemann_pattern_compressible_pvl
