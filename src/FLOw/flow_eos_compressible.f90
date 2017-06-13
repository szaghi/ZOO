!< FLOw **EOS** (Equation of State) of ideal compressible fluid object.

module flow_eos_compressible
!< FLOw **EOS** (Equation of State) of ideal compressible fluid object.

use flow_eos_object, only : eos_object
use penf, only : R_P, str

implicit none
private
public :: eos_compressible

type, extends(eos_object) :: eos_compressible
   !< Equation of state (EOS) of ideal compressible object class.
   private
   real(R_P) :: cp_=0._R_P    !< Specific heat at constant pressure `cp`.
   real(R_P) :: cv_=0._R_P    !< Specific heat at constant volume `cv`.
   real(R_P) :: g_=0._R_P     !< Specific heats ratio `gamma = cp / cv`.
   real(R_P) :: R_=0._R_P     !< Fluid constant `R = cp - cv`.
   real(R_P) :: gm1_=0._R_P   !< `gamma - 1`.
   real(R_P) :: gp1_=0._R_P   !< `gamma + 1`.
   real(R_P) :: delta_=0._R_P !< `(gamma - 1) / 2`.
   real(R_P) :: eta_=0._R_P   !< `2 * gamma / (gamma - 1)`.
   contains
      ! public methods
      procedure, pass(self) :: compute_derivate !< Compute derivate quantities (from `cp` and `cv`).
      ! deferred methods
      procedure, pass(self) :: cp              !< Return specific heat at constant pressure.
      procedure, pass(self) :: cv              !< Return specific heat at constant volume.
      procedure, pass(self) :: delta           !< Return `(gamma - 1) / 2`.
      procedure, pass(self) :: density         !< Return density.
      procedure, pass(self) :: description     !< Return pretty-printed object description.
      procedure, pass(lhs)  :: eos_assign_eos  !< Operator `=`.
      procedure, pass(self) :: eta             !< Return `2 * gamma / (gamma - 1)`.
      procedure, pass(self) :: g               !< Return specific heats ratio `gamma=cp/cv`.
      procedure, pass(self) :: gm1             !< Return `gamma - 1`.
      procedure, pass(self) :: gp1             !< Return `gamma + 1`.
      procedure, pass(self) :: internal_energy !< Return specific internal energy.
      procedure, pass(self) :: pressure        !< Return pressure.
      procedure, pass(self) :: R               !< Return fluid constant `R=cp-cv`.
      procedure, pass(self) :: speed_of_sound  !< Return speed of sound.
      procedure, pass(self) :: temperature     !< Return temperature.
      procedure, pass(self) :: total_entalpy   !< Return total specific entalpy.
endtype eos_compressible

interface eos_compressible
   !< Overload [[eos_compressible]] name with its constructor.
   module procedure eos_compressible_instance
endinterface

contains
   ! public methods
   elemental subroutine compute_derivate(self)
   !< Compute derivate quantities (from `cp` and `cv`).
   class(eos_compressible), intent(inout) :: self !< Equation of state.

   self%g_     = self%cp_ / self%cv_
   self%R_     = self%cp_ - self%cv_
   self%gm1_   = self%g_ - 1._R_P
   self%gp1_   = self%g_ + 1._R_P
   self%delta_ = (self%g_ - 1._R_P) * 0.5_R_P
   self%eta_   = 2._R_P * self%g_ / (self%g_ - 1._R_P)
   endsubroutine compute_derivate

   ! deferred methods
   elemental function cp(self) result(cp_)
   !< Return specific heat at constant pressure.
   class(eos_compressible), intent(in) :: self !< Equation of state.
   real(R_P)                           :: cp_  !< `cp` value.

   cp_ = self%cp_
   endfunction cp

   elemental function cv(self) result(cv_)
   !< Return specific heat at constant volume.
   class(eos_compressible), intent(in) :: self !< Equation of state.
   real(R_P)                           :: cv_  !< `cv` value.

   cv_ = self%cv_
   endfunction cv

   elemental function delta(self) result(delta_)
   !< Return `(gamma - 1) / 2`.
   class(eos_compressible), intent(in) :: self   !< Equation of state.
   real(R_P)                           :: delta_ !< `(gamma - 1) / 2` value.

   delta_ = self%delta_
   endfunction delta

   elemental function density(self, pressure, speed_of_sound) result(density_)
   !< Return density.
   class(eos_compressible), intent(in) :: self           !< Equation of state.
   real(R_P),               intent(in) :: pressure       !< Pressure value.
   real(R_P),               intent(in) :: speed_of_sound !< Speed of sound value.
   real(R_P)                           :: density_       !< Density value.

   density_ = self%g_ * pressure / (speed_of_sound * speed_of_sound)
   endfunction density

   pure function description(self, prefix) result(desc)
   !< Return a pretty-formatted object description.
   class(eos_compressible), intent(in)           :: self             !< Equation of state.
   character(*),            intent(in), optional :: prefix           !< Prefixing string.
   character(len=:), allocatable                 :: desc             !< Description.
   character(len=:), allocatable                 :: prefix_          !< Prefixing string, local variable.
   character(len=1), parameter                   :: NL=new_line('a') !< New line character.

   prefix_ = '' ; if (present(prefix)) prefix_ = prefix
   desc = ''
   desc = desc//prefix_//'cp  = '//trim(str(n=self%cp_))//NL
   desc = desc//prefix_//'cv  = '//trim(str(n=self%cv_))
   endfunction description

   elemental function eta(self) result(eta_)
   !< Return `2 * gamma / (gamma - 1)`.
   class(eos_compressible), intent(in) :: self !< Equation of state.
   real(R_P)                           :: eta_ !< `2 * gamma / (gamma - 1)` value.

   eta_ = self%eta_
   endfunction eta

   elemental function g(self) result(g_)
   !< Return specific heats ratio `gamma=cp/cv`.
   class(eos_compressible), intent(in) :: self !< Equation of state.
   real(R_P)                           :: g_   !< Specific heats ratio value.

   g_ = self%g_
   endfunction g

   elemental function gm1(self) result(gm1_)
   !< Return `gamma - 1`.
   class(eos_compressible), intent(in) :: self !< Equation of state.
   real(R_P)                           :: gm1_ !< `gamma - 1` value.

   gm1_ = self%gm1_
   endfunction gm1

   elemental function gp1(self) result(gp1_)
   !< Return `gamma + 1`.
   class(eos_compressible), intent(in) :: self !< Equation of state.
   real(R_P)                           :: gp1_ !< `gamma + 1` value.

   gp1_ = self%gp1_
   endfunction gp1

   elemental function internal_energy(self, density, pressure, temperature) result(energy_)
   !< Return specific internal energy.
   class(eos_compressible), intent(in)           :: self        !< Equation of state.
   real(R_P),               intent(in), optional :: density     !< Density value.
   real(R_P),               intent(in), optional :: pressure    !< Pressure value.
   real(R_P),               intent(in), optional :: temperature !< Temperature value.
   real(R_P)                                     :: energy_     !< Energy value.

   energy_ = 0._R_P
   if (present(density).and.present(pressure)) then
      energy_ = pressure / ((self%g_ - 1._R_P) * density)
   elseif (present(temperature)) then
      energy_ = self%cv() * temperature
   endif
   endfunction internal_energy

   elemental function pressure(self, density, energy, temperature) result(pressure_)
   !< Return pressure.
   class(eos_compressible), intent(in)           :: self        !< Equation of state.
   real(R_P),               intent(in), optional :: density     !< Density value.
   real(R_P),               intent(in), optional :: energy      !< Specific internal energy value.
   real(R_P),               intent(in), optional :: temperature !< Temperature value.
   real(R_P)                                     :: pressure_   !< Pressure value.

   pressure_ = 0._R_P
   if (present(density).and.present(energy)) then
      pressure_ = density * (self%g_ - 1._R_P) * energy
   elseif (present(density).and.present(temperature)) then
      pressure_ = density * self%R_ * temperature
   endif
   endfunction pressure

   elemental function R(self) result(R_)
   !< Return fluid constant `R=cp-cv`.
   class(eos_compressible), intent(in) :: self !< Equation of state.
   real(R_P)                           :: R_   !< Fluid constant value.

   R_ = self%R_
   endfunction R

   elemental function speed_of_sound(self, density, pressure) result(speed_of_sound_)
   !< Return speed of sound.
   class(eos_compressible), intent(in) :: self            !< Equation of state.
   real(R_P),               intent(in) :: density         !< Density value.
   real(R_P),               intent(in) :: pressure        !< Pressure value.
   real(R_P)                           :: speed_of_sound_ !< Speed of sound value.

   speed_of_sound_ = sqrt(self%g_ * pressure / density)
   endfunction speed_of_sound

   elemental function temperature(self, density, energy, pressure) result(temperature_)
   !< Return temperature.
   class(eos_compressible), intent(in)           :: self         !< Equation of state.
   real(R_P),               intent(in), optional :: density      !< Density value.
   real(R_P),               intent(in), optional :: energy       !< Specific internal energy value.
   real(R_P),               intent(in), optional :: pressure     !< Pressure value.
   real(R_P)                                     :: temperature_ !< Temperature value.

   temperature_ = 0._R_P
   if (present(density).and.present(pressure)) then
      temperature_ = pressure / (self%R_ * density)
   elseif (present(energy)) then
      temperature_ = energy / self%cv()
   endif
   endfunction temperature

   elemental function total_entalpy(self, density, pressure, velocity_sq_norm) result(entalpy_)
   !< Return total specific entalpy.
   class(eos_compressible), intent(in) :: self             !< Equation of state.
   real(R_P),               intent(in) :: density          !< Density value.
   real(R_P),               intent(in) :: pressure         !< Pressure value.
   real(R_P),               intent(in) :: velocity_sq_norm !< Velocity vector square norm `||velocity||^2`.
   real(R_P)                           :: entalpy_         !< Total specific entalpy (per unit of mass).

   entalpy_ = self%g_ * pressure/(self%gm1_ * density) + 0.5_R_P * velocity_sq_norm
   endfunction total_entalpy

   ! operators
   pure subroutine eos_assign_eos(lhs, rhs)
   !< Operator `=`.
   class(eos_compressible), intent(inout) :: lhs !< Left hand side.
   class(eos_object),       intent(in)    :: rhs !< Right hand side.

   select type(rhs)
   class is (eos_compressible)
      lhs%cp_    = rhs%cp_
      lhs%cv_    = rhs%cv_
      lhs%g_     = rhs%g_
      lhs%R_     = rhs%R_
      lhs%delta_ = rhs%delta_
      lhs%eta_   = rhs%eta_
      lhs%gm1_   = rhs%gm1_
      lhs%gp1_   = rhs%gp1_
   endselect
   endsubroutine eos_assign_eos

   ! private non TBP
   elemental function eos_compressible_instance(cp, cv, gam, R) result(instance)
   !< Return and instance of [[eos_compressible]].
   !<
   !< @note This procedure is used for overloading [[eos_compressible]] name.
   real(R_P), intent(in), optional :: cp       !< Specific heat at constant pressure `cp` value.
   real(R_P), intent(in), optional :: cv       !< Specific heat at constant volume `cv` value.
   real(R_P), intent(in), optional :: gam      !< Specific heats ratio `gamma=cp/cv` value.
   real(R_P), intent(in), optional :: R        !< Fluid constant `R=cp-cv` value.
   type(eos_compressible)          :: instance !< Instance of [[eos_compressible]].

   if (present(cp).and.present(cv)) then
      instance%cp_ = cp
      instance%cv_ = cv
   elseif (present(gam).and.present(R)) then
      instance%cv_ = R/(gam - 1._R_P)
      instance%cp_ = gam * instance%cv_
   elseif (present(gam).and.present(cp)) then
      instance%cp_ = cp
      instance%cv_ = cp / gam
   elseif (present(gam).and.present(cv)) then
      instance%cp_ = gam * cv
      instance%cv_ = cv
   elseif (present(R).and.present(cp)) then
      instance%cp_ = cp
      instance%cv_ = cp - R
   elseif (present(R).and.present(cv)) then
      instance%cp_ = cv + R
      instance%cv_ = cv
   endif
   call instance%compute_derivate
   endfunction eos_compressible_instance
endmodule flow_eos_compressible
