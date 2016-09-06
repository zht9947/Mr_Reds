!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE)
!
! PURPOSE:
!
!   TO
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_DEF_FIELD_VARS_DSET_NAMES

    USE MR_KINDS

    USE MR_DEF_FIELD_VARS_UNITS

    IMPLICIT NONE

    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_ZB      = "Bed Elevation "                !// "(" // UNIT_ZB      // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_ZS      = "Free Surface Elevation "       !// "(" // UNIT_ZS      // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_H       = "Depth "                        !// "(" // UNIT_H       // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_R       = "Density "                      !// "(" // UNIT_R       // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_UVA     = "Depth-Averaged Velosity "      !// "(" // UNIT_UVA     // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_UV      = "Horizontal Velosity "          !// "(" // UNIT_UV      // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_W       = "Vertical Velosity "            !// "(" // UNIT_W       // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_TBUV    = "Bed Shear Stress "             !// "(" // UNIT_TBUV    // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_TBFUV   = "Bed Shear Stress by Friction " !// "(" // UNIT_TBFUV   // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_QSBUV   = ""                              !// "(" // UNIT_QSBUV   // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_BBS     = ""                              !// "(" // UNIT_BBS     // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_CSS     = ""                              !// "(" // UNIT_CSS     // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_VXYUV   = "Horizontal Eddy Viscosity "    !// "(" // UNIT_VXYUV   // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_VZW     = "Vertical Eddy Viscosity "      !// "(" // UNIT_VZW     // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_DXYUV   = "Horizontal Eddy Diffusivity "  !// "(" // UNIT_DXYUV   // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_DZW     = "Vertical Eddy Diffusivity "    !// "(" // UNIT_DZW     // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_KI      = "Turbulence Kinetic Energy "    !// "(" // UNIT_KI      // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_DI      = "Turbulence Dissipation "       !// "(" // UNIT_DI      // ")"

  END MODULE MR_DEF_FIELD_VARS_DSET_NAMES