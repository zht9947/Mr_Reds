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

    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_TBFUV   = "Frictional Bed Shear Stress"   !// " (" // UNIT_TBFUV   // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_TBUV    = "Total Bed Shear Stress"        !// " (" // UNIT_TBUV    // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_CSS     = "NONE"                          !// " (" // UNIT_CSS     // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_QSBUV   = "NONE"                          !// " (" // UNIT_QSBUV   // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_KI      = "Turbulence Kinetic Energy"     !// " (" // UNIT_KI      // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_DI      = "Turbulence Dissipation"        !// " (" // UNIT_DI      // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_ZB      = "Bed Elevation"                 !// " (" // UNIT_ZB      // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_ZS      = "Free Surface Elevation"        !// " (" // UNIT_ZS      // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_H       = "Depth"                         !// " (" // UNIT_H       // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_UVA     = "Depth-Averaged Velocity"       !// " (" // UNIT_UVA     // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_UV      = "Horizontal Velocity"           !// " (" // UNIT_UV      // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_WW      = "Vertical Velocity"             !// " (" // UNIT_WW      // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_R       = "Density"                       !// " (" // UNIT_R       // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_VZWW    = "Eddy Viscosity"                !// " (" // UNIT_VZWW    // ")"
    CHARACTER( 2**06 ) , PARAMETER :: DSET_NAME_DZWW    = "Eddy Diffusivity"              !// " (" // UNIT_DZWW    // ")"

  END MODULE MR_DEF_FIELD_VARS_DSET_NAMES