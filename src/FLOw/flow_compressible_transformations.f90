!< FLOw compressible variables transformations.

module flow_compressible_transformations
!< FLOw compressible variables transformations.

use flow_conservative_compressible, only : conservative_compressible
use flow_eos_object, only : eos_object
use flow_primitive_compressible, only : primitive_compressible

implicit none
private
public :: conservative_to_primitive_compressible
public :: primitive_to_conservative_compressible

contains
   ! public procedures
   elemental function conservative_to_primitive_compressible(conservative, eos) result(primitive_)
   !< Return a [[primitive_compressible]] state transforming a given [[conservative_compressible]] state.
   type(conservative_compressible), intent(in) :: conservative !< Conservative state.
   class(eos_object),               intent(in) :: eos          !< Equation of state.
   type(primitive_compressible)                :: primitive_   !< Primitive state.

   primitive_%density  = conservative%density
   primitive_%velocity = conservative%velocity()
   primitive_%pressure = conservative%pressure(eos=eos)
   endfunction conservative_to_primitive_compressible

   elemental function primitive_to_conservative_compressible(primitive, eos) result(conservative_)
   !< Return a [[conservative_compressible]] state transforming a given [[primitive_compressible]] state.
   type(primitive_compressible), intent(in) :: primitive     !< Primitive state.
   class(eos_object),            intent(in) :: eos           !< Equation of state.
   type(conservative_compressible)          :: conservative_ !< Conservative state.

   conservative_%density  = primitive%density
   conservative_%momentum = primitive%momentum()
   conservative_%energy   = primitive%energy(eos=eos)
   endfunction primitive_to_conservative_compressible
endmodule flow_compressible_transformations
