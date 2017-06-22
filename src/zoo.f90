!< ZOO, Zaghi fOrtran cOllection, where my wild Fortran pets will survive

module zoo
!< ZOO, Zaghi fOrtran cOllection, where my wild Fortran pets will survive
use befor64, only : is_b64_initialized, b64_init, &
                    b64_encode, b64_encode_up,    &
                    b64_decode, b64_decode_up,    &
                    pack_data,                    &
                    autotest
use face, only : colorize,          &
                 colors_samples,    &
                 styles_samples,    &
                 face_ASCII=>ASCII, &
                 face_UCS4=>UCS4
use flap, only : command_line_argument,        &
                 command_line_arguments_group, &
                 command_line_interface
use flow, only : conservative_to_primitive_compressible, primitive_to_conservative_compressible, &
                 conservative_compressible, conservative_compressible_pointer,                   &
                 conservative_object,                                                            &
                 eos_compressible,                                                               &
                 eos_object,                                                                     &
                 field_object,                                                                   &
                 primitive_compressible, primitive_compressible_pointer,                         &
                 primitive_compressible_multispecie,                                             &
                 primitive_object
use finer, only : err_option_name,     &
                  err_option_vals,     &
                  err_option,          &
                  err_section_name,    &
                  err_section_options, &
                  err_section,         &
                  err_source_missing,  &
                  file_ini,            &
                  file_ini_autotest
use fitter, only : timer
use forbear, only : bar_object,           &
                    forbear_ASCII=>ASCII, &
                    forbear_UCS4=>UCS4
use foreseer, only : riemann_pattern_compressible_object, &
                     riemann_pattern_compressible_pvl,    &
                     riemann_pattern_object,              &
                     riemann_solver_compressible_exact,   &
                     riemann_solver_compressible_hllc,    &
                     riemann_solver_compressible_llf,     &
                     riemann_solver_compressible_pvl,     &
                     riemann_solver_compressible_roe,     &
                     riemann_solver_object
! use fury, only : assignment(=),                          &
!                  operator(+),                            &
!                  operator(/),                            &
!                  operator(*),                            &
!                  operator(-),                            &
!                  operator(==),                           &
!                  operator(/=),                           &
!                  qreal32, qreal64, qreal128,             &
!                  system_si32, system_si64, system_si128, &
!                  uom32, uom64, uom128,                   &
!                  uom_converter,                          &
!                  uom_reference32, uom_reference64, uom_reference128
! use mortif, only : morton2D, demorton2D, morton3D, demorton3D
use penf, only : endianL, endianB, endian, is_initialized,                                 &
                 R16P, FR16P, DR16P, MinR16P, MaxR16P, BIR16P, BYR16P, smallR16P, ZeroR16, &
                 R8P,  FR8P,  DR8P,  MinR8P,  MaxR8P,  BIR8P,  BYR8P,  smallR8P,  ZeroR8,  &
                 R4P,  FR4P,  DR4P,  MinR4P,  MaxR4P,  BIR4P,  BYR4P,  smallR4P,  ZeroR4,  &
                 R_P,  FR_P,  DR_P,  MinR_P,  MaxR_P,  BIR_P,  BYR_P,  smallR_P,  Zero,    &
                 I8P,  FI8P,  DI8P,  MinI8P,  MaxI8P,  BII8P,  BYI8P,                      &
                 I4P,  FI4P,  DI4P,  MinI4P,  MaxI4P,  BII4P,  BYI4P,                      &
                 I2P,  FI2P,  DI2P,  MinI2P,  MaxI2P,  BII2P,  BYI2P,                      &
                 I1P,  FI1P,  DI1P,  MinI1P,  MaxI1P,  BII1P,  BYI1P,                      &
                 I_P,  FI_P,  DI_P,  MinI_P,  MaxI_P,  BII_P,  BYI_P,                      &
                 REAL_KINDS_LIST, REAL_FORMATS_LIST,                                       &
                 INTEGER_KINDS_LIST, INTEGER_FORMATS_LIST,                                 &
                 bit_size, byte_size,                                                      &
                 str, strz, cton,                                                          &
                 bstr, bcton,                                                              &
                 check_endian,                                                             &
                 digit,                                                                    &
                 penf_Init,                                                                &
                 penf_print
use stringifor, only : CK,                                                                &
                       string,                                                            &
                       adjustl, adjustr, count, index, len, len_trim, repeat, scan, trim, &
                       read_file, read_lines, write_file, write_lines
use vecfor, only : distance_to_line,            &
                   distance_to_plane,           &
                   distance_vectorial_to_plane, &
                   ex, ey, ez,                  &
                   face_normal3, face_normal4,  &
                   iolen,                       &
                   is_collinear,                &
                   is_concyclic,                &
                   normalized,                  &
                   normL2,                      &
                   projection_onto_plane,       &
                   sq_norm,                     &
                   vector
use vtk_fortran, only : pvtk_file, vtk_file, vtm_file
use foodie, only : foodie_integrator_class_names,          &
                   foodie_integrator_factory,              &
                   foodie_integrator_schemes,              &
                   is_available,                           &
                   is_class_available,                     &
                   is_scheme_available,                    &
                   integrand_object,                       &
                   integrator_object,                      &
                   integrator_multistage_object,           &
                   integrator_multistage_multistep_object, &
                   integrator_multistep_object,            &
                   integrator_adams_bashforth,             &
                   integrator_adams_bashforth_moulton,     &
                   integrator_adams_moulton,               &
                   integrator_back_df,                     &
                   integrator_euler_explicit,              &
                   integrator_leapfrog,                    &
                   integrator_lmm_ssp,                     &
                   integrator_lmm_ssp_vss,                 &
                   integrator_ms_runge_kutta_ssp,          &
                   integrator_runge_kutta_emd,             &
                   integrator_runge_kutta_ls,              &
                   integrator_runge_kutta_lssp,            &
                   integrator_runge_kutta_ssp
use foxy, only : xml_file, xml_tag
use wenoof, only : interpolator_object, wenoof_create

implicit none
private
! BeFoR64
public :: is_b64_initialized, b64_init
public :: b64_encode, b64_encode_up
public :: b64_decode, b64_decode_up
public :: pack_data
public :: autotest
! FACE
public :: colorize
public :: colors_samples
public :: styles_samples
public :: face_ASCII
public :: face_UCS4
! FLAP
public :: command_line_argument
public :: command_line_arguments_group
public :: command_line_interface
! FLOw
public :: conservative_to_primitive_compressible, primitive_to_conservative_compressible
public :: conservative_compressible, conservative_compressible_pointer
public :: conservative_object
public :: eos_compressible
public :: eos_object
public :: field_object
public :: primitive_compressible, primitive_compressible_pointer
public :: primitive_compressible_multispecie
public :: primitive_object
! FiNeR
public :: err_option_name
public :: err_option_vals
public :: err_option
public :: err_section_name
public :: err_section_options
public :: err_section
public :: err_source_missing
public :: file_ini
public :: file_ini_autotest
! FITTER
public :: timer
! forbear
public :: bar_object
public :: forbear_ASCII
public :: forbear_UCS4
! FORESEER
public :: riemann_pattern_compressible_object
public :: riemann_pattern_compressible_pvl
public :: riemann_pattern_object
public :: riemann_solver_compressible_exact
public :: riemann_solver_compressible_hllc
public :: riemann_solver_compressible_llf
public :: riemann_solver_compressible_pvl
public :: riemann_solver_compressible_roe
public :: riemann_solver_object
! FURY
! public :: assignment(=)
! public :: operator(+)
! public :: operator(/)
! public :: operator(*)
! public :: operator(-)
! public :: operator(==)
! public :: operator(/=)
! public :: qreal32, qreal64, qreal128
! public :: system_si32, system_si64, system_si128
! public :: uom32, uom64, uom128
! public :: uom_converter
! public :: uom_reference32, uom_reference64, uom_reference128
! MORTIF
! public :: morton2D, demorton2D, morton3D, demorton3D
! PENF
public :: endianL, endianB, endian, is_initialized
public :: R16P, FR16P, DR16P, MinR16P, MaxR16P, BIR16P, BYR16P, smallR16P, ZeroR16
public :: R8P,  FR8P,  DR8P,  MinR8P,  MaxR8P,  BIR8P,  BYR8P,  smallR8P,  ZeroR8
public :: R4P,  FR4P,  DR4P,  MinR4P,  MaxR4P,  BIR4P,  BYR4P,  smallR4P,  ZeroR4
public :: R_P,  FR_P,  DR_P,  MinR_P,  MaxR_P,  BIR_P,  BYR_P,  smallR_P,  Zero
public :: I8P,  FI8P,  DI8P,  MinI8P,  MaxI8P,  BII8P,  BYI8P
public :: I4P,  FI4P,  DI4P,  MinI4P,  MaxI4P,  BII4P,  BYI4P
public :: I2P,  FI2P,  DI2P,  MinI2P,  MaxI2P,  BII2P,  BYI2P
public :: I1P,  FI1P,  DI1P,  MinI1P,  MaxI1P,  BII1P,  BYI1P
public :: I_P,  FI_P,  DI_P,  MinI_P,  MaxI_P,  BII_P,  BYI_P
public :: REAL_KINDS_LIST, REAL_FORMATS_LIST
public :: INTEGER_KINDS_LIST, INTEGER_FORMATS_LIST
public :: bit_size, byte_size
public :: str, strz, cton
public :: bstr, bcton
public :: check_endian
public :: digit
public :: penf_Init
public :: penf_print
! StringiFor
public :: CK
public :: string
public :: adjustl, adjustr, count, index, len, len_trim, repeat, scan, trim
public :: read_file, read_lines, write_file, write_lines
! VecFor
public :: distance_to_line
public :: distance_to_plane
public :: distance_vectorial_to_plane
public :: ex, ey, ez
public :: face_normal3, face_normal4
public :: iolen
public :: is_collinear
public :: is_concyclic
public :: normalized
public :: normL2
public :: projection_onto_plane
public :: sq_norm
public :: vector
! VTKFortran
public :: pvtk_file
public :: vtk_file
public :: vtm_file
! FOODIE
public :: foodie_integrator_class_names
public :: foodie_integrator_factory
public :: foodie_integrator_schemes
public :: is_available
public :: is_class_available
public :: is_scheme_available
public :: integrand_object
public :: integrator_object
public :: integrator_multistage_object
public :: integrator_multistage_multistep_object
public :: integrator_multistep_object
public :: integrator_adams_bashforth
public :: integrator_adams_bashforth_moulton
public :: integrator_adams_moulton
public :: integrator_back_df
public :: integrator_euler_explicit
public :: integrator_leapfrog
public :: integrator_lmm_ssp
public :: integrator_lmm_ssp_vss
public :: integrator_ms_runge_kutta_ssp
public :: integrator_runge_kutta_emd
public :: integrator_runge_kutta_ls
public :: integrator_runge_kutta_lssp
public :: integrator_runge_kutta_ssp
! FoXy
public :: xml_file
public :: xml_tag
! WenoOOF
public :: interpolator_object
public :: wenoof_create
endmodule zoo
