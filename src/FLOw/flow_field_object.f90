!< FLOw **field** abstract object.

module flow_field_object
!< FLOw **field** abstract object.

use penf, only : I_P, R_P

implicit none
private
public :: field_object

type, abstract :: field_object
   !< **Field** abstract object.
   contains
      ! deferred methods
      procedure(array_interface),       pass(self), deferred :: array        !< Return serialized array of field.
      procedure(description_interface), pass(self), deferred :: description  !< Return pretty-printed object description.
      ! deferred operators
      ! +
      procedure(symmetric_operator), pass(lhs),  deferred, public :: field_add_field !< `+` operator.
      procedure(field_op_real),      pass(lhs),  deferred, public :: field_add_real  !< `+ real` operator.
      procedure(real_op_field),      pass(rhs),  deferred, public :: real_add_field  !< `real +` operator.
      procedure(unary_operator),     pass(self), deferred, public :: positive        !< Unary operator `+ field`.
      generic, public :: operator(+) => field_add_field, &
                                        field_add_real,  &
                                        real_add_field,  &
                                        positive !< Overloading `+` operator.
      ! /
      procedure(symmetric_operator),      pass(lhs), deferred, public :: field_div_field          !< `/` operator.
      procedure(field_op_integer),        pass(lhs), deferred, public :: field_div_integer        !< `/ integer` operator.
      procedure(integer_op_field),        pass(rhs), deferred, public :: integer_div_field        !< `integer /` operator.
      procedure(field_op_integer_scalar), pass(lhs), deferred, public :: field_div_integer_scalar !< `/ integer_scalar` operator.
      procedure(integer_scalar_op_field), pass(rhs), deferred, public :: integer_scalar_div_field !< `integer_scalar /` operator.
      procedure(field_op_real),           pass(lhs), deferred, public :: field_div_real           !< `/ real` operator.
      procedure(real_op_field),           pass(rhs), deferred, public :: real_div_field           !< `real /` operator.
      procedure(field_op_real_scalar),    pass(lhs), deferred, public :: field_div_real_scalar    !< `/ real_scalar` operator.
      procedure(real_scalar_op_field),    pass(rhs), deferred, public :: real_scalar_div_field    !< `real_scalar /` operator.
      generic, public :: operator(/) => field_div_field,          &
                                        field_div_integer,        &
                                        integer_div_field,        &
                                        field_div_integer_scalar, &
                                        integer_scalar_div_field, &
                                        field_div_real,           &
                                        real_div_field,           &
                                        field_div_real_scalar,    &
                                        real_scalar_div_field !< Overloading `/` operator.
      ! *
      procedure(symmetric_operator),      pass(lhs), deferred, public :: field_mul_field          !< `*` operator.
      procedure(field_op_integer),        pass(lhs), deferred, public :: field_mul_integer        !< `* integer` operator.
      procedure(integer_op_field),        pass(rhs), deferred, public :: integer_mul_field        !< `integer *` operator.
      procedure(field_op_integer_scalar), pass(lhs), deferred, public :: field_mul_integer_scalar !< `* integer_scalar` operator.
      procedure(integer_scalar_op_field), pass(rhs), deferred, public :: integer_scalar_mul_field !< `integer_scalar *` operator.
      procedure(field_op_real),           pass(lhs), deferred, public :: field_mul_real           !< `* real` operator.
      procedure(real_op_field),           pass(rhs), deferred, public :: real_mul_field           !< `real *` operator.
      procedure(field_op_real_scalar),    pass(lhs), deferred, public :: field_mul_real_scalar    !< `* real_scalar` operator.
      procedure(real_scalar_op_field),    pass(rhs), deferred, public :: real_scalar_mul_field    !< `real_scalar *` operator.
      generic, public :: operator(*) => field_mul_field,          &
                                        field_mul_integer,        &
                                        integer_mul_field,        &
                                        field_mul_integer_scalar, &
                                        integer_scalar_mul_field, &
                                        field_mul_real,           &
                                        real_mul_field,           &
                                        field_mul_real_scalar,    &
                                        real_scalar_mul_field !< Overloading `*` operator.
      ! -
      procedure(symmetric_operator), pass(lhs),  deferred, public :: field_sub_field !< `-` operator.
      procedure(field_op_real),      pass(lhs),  deferred, public :: field_sub_real  !< `- real` operator.
      procedure(real_op_field),      pass(rhs),  deferred, public :: real_sub_field  !< `real -` operator.
      procedure(unary_operator),     pass(self), deferred, public :: negative        !< Unary operator `- field`.
      generic, public :: operator(-) => field_sub_field, &
                                        field_sub_real,  &
                                        real_sub_field,  &
                                        negative !< Overloading `-` operator.
      ! **
      procedure(field_op_integer_scalar), pass(lhs), deferred, public :: field_pow_integer !< `** integer` operator.
      procedure(field_op_real_scalar),    pass(lhs), deferred, public :: field_pow_real    !< `** real` operator.
      generic, public :: operator(**) => field_pow_integer, field_pow_real !< Overloading `**` operator.
      ! =
      procedure(assign_field_interface), pass(lhs), deferred, public :: assign_field !< `=` operator.
      procedure(assign_real_interface),  pass(lhs), deferred, public :: assign_real  !< `= real` operator.
      generic, public :: assignment(=) => assign_field, assign_real !< Overloading `=` assignament.
      ! ==
      procedure(compare_interface), pass(lhs), deferred, public :: eq !< `==' operator.
      generic, public :: operator(==) => eq !< Overloading `==` operator.
      ! /=
      procedure(compare_interface), pass(lhs), deferred, public :: not_eq !< `/=' operator.
      generic :: operator(/=) => not_eq !< Overloading `/=` operator.
endtype field_object

abstract interface
   !< Abstract interfaces of deferred methods of [[field_object]].

   pure function array_interface(self) result(array_)
   !< Return serialized array of field.
   import :: field_object, R_P
   class(field_object), intent(in) :: self      !< Field.
   real(R_P), allocatable          :: array_(:) !< Serialized array of field.
   endfunction array_interface

   pure function description_interface(self, prefix) result(desc)
   !< Return a pretty-formatted object description.
   import :: field_object
   class(field_object), intent(in)           :: self   !< Field.
   character(*),        intent(in), optional :: prefix !< Prefixing string.
   character(len=:), allocatable             :: desc   !< Description.
   endfunction description_interface

   ! operators
   pure function symmetric_operator(lhs, rhs) result(opr)
   !< Operator `field.op.field`.
   import :: field_object, R_P
   class(field_object), intent(in) :: lhs    !< Left hand side.
   class(field_object), intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable          :: opr(:) !< Operator result.
   endfunction symmetric_operator

   pure function field_op_integer(lhs, rhs) result(opr)
   !< Operator `field.op.integer`.
   import :: field_object, I_P, R_P
   class(field_object), intent(in) :: lhs     !< Left hand side.
   integer(I_P),        intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable          :: opr(:)  !< Operator result.
   endfunction field_op_integer

   pure function integer_op_field(lhs, rhs) result(opr)
   !< Operator `integer.op.field`.
   import :: field_object, I_P, R_P
   integer(I_P),        intent(in) :: lhs(1:) !< Left hand side.
   class(field_object), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable          :: opr(:)  !< Operator result.
   endfunction integer_op_field

   pure function field_op_integer_scalar(lhs, rhs) result(opr)
   !< Operator `field.op.integer_scalar`.
   import :: field_object, I_P, R_P
   class(field_object), intent(in) :: lhs    !< Left hand side.
   integer(I_P),        intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable          :: opr(:) !< Operator result.
   endfunction field_op_integer_scalar

   pure function integer_scalar_op_field(lhs, rhs) result(opr)
   !< Operator `integer_scalar.op.field`.
   import :: field_object, I_P, R_P
   integer(I_P),        intent(in) :: lhs    !< Left hand side.
   class(field_object), intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable          :: opr(:) !< Operator result.
   endfunction integer_scalar_op_field

   pure function field_op_real(lhs, rhs) result(opr)
   !< Operator `field.op.real`.
   import :: field_object, R_P
   class(field_object), intent(in) :: lhs     !< Left hand side.
   real(R_P),           intent(in) :: rhs(1:) !< Right hand side.
   real(R_P), allocatable          :: opr(:)  !< Operator result.
   endfunction field_op_real

   pure function real_op_field(lhs, rhs) result(opr)
   !< Operator `real.op.field`.
   import :: field_object, R_P
   real(R_P),           intent(in) :: lhs(1:) !< Left hand side.
   class(field_object), intent(in) :: rhs     !< Right hand side.
   real(R_P), allocatable          :: opr(:)  !< Operator result.
   endfunction real_op_field

   pure function field_op_real_scalar(lhs, rhs) result(opr)
   !< Operator `field.op.real_scalar`.
   import :: field_object, R_P
   class(field_object), intent(in) :: lhs    !< Left hand side.
   real(R_P),           intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable          :: opr(:) !< Operator result.
   endfunction field_op_real_scalar

   pure function real_scalar_op_field(lhs, rhs) result(opr)
   !< Operator `real_scalar.op.field`.
   import :: field_object, R_P
   real(R_P),           intent(in) :: lhs    !< Left hand side.
   class(field_object), intent(in) :: rhs    !< Right hand side.
   real(R_P), allocatable          :: opr(:) !< Operator result.
   endfunction real_scalar_op_field

   pure function unary_operator(self) result(opr)
   !< Unary operator `.op.field`.
   import :: field_object, R_P
   class(field_object), intent(in) :: self   !< Field.
   real(R_P), allocatable          :: opr(:) !< Operator result.
   endfunction unary_operator

   pure subroutine assign_field_interface(lhs, rhs)
   !< Operator `=`.
   import :: field_object
   class(field_object), intent(inout) :: lhs !< Left hand side.
   class(field_object), intent(in)    :: rhs !< Right hand side.
   endsubroutine assign_field_interface

   pure subroutine assign_real_interface(lhs, rhs)
   !< Operator `field = real`.
   import :: field_object, R_P
   class(field_object), intent(inout) :: lhs     !< Left hand side.
   real(R_P),           intent(in)    :: rhs(1:) !< Right hand side.
   endsubroutine assign_real_interface

   elemental function compare_interface(lhs, rhs) result(opr)
   !< Operator `field.compare.field'.
   import :: field_object
   class(field_object), intent(in) :: lhs !< Left hand side.
   class(field_object), intent(in) :: rhs !< Right hand side.
   logical                         :: opr !< Operator result.
   endfunction compare_interface
endinterface
endmodule flow_field_object
