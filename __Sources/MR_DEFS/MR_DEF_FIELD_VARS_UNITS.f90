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
  MODULE MR_DEF_FIELD_VARS_UNITS

    USE MR_KINDS

    IMPLICIT NONE

    CHARACTER( 2**04 ) , PARAMETER :: UNIT_TBFUV   = "Pa"
    CHARACTER( 2**04 ) , PARAMETER :: UNIT_TBUV    = "Pa"
    CHARACTER( 2**04 ) , PARAMETER :: UNIT_KI      = "m^2/s^2"
    CHARACTER( 2**04 ) , PARAMETER :: UNIT_DI      = "m^2/s^3"
    CHARACTER( 2**04 ) , PARAMETER :: UNIT_CSS     = "kg/m^3"
    CHARACTER( 2**04 ) , PARAMETER :: UNIT_QSBUV   = "NONE"
    CHARACTER( 2**04 ) , PARAMETER :: UNIT_ZB      = "m"
    CHARACTER( 2**04 ) , PARAMETER :: UNIT_ZS      = "m"
    CHARACTER( 2**04 ) , PARAMETER :: UNIT_H       = "m"
    CHARACTER( 2**04 ) , PARAMETER :: UNIT_UVA     = "m/s"
    CHARACTER( 2**04 ) , PARAMETER :: UNIT_UV      = "m/s"
    CHARACTER( 2**04 ) , PARAMETER :: UNIT_WW      = "m/s"
    CHARACTER( 2**04 ) , PARAMETER :: UNIT_R       = "kg/m^3"
    CHARACTER( 2**04 ) , PARAMETER :: UNIT_VZWW    = "m^2/s"
    CHARACTER( 2**04 ) , PARAMETER :: UNIT_DZWW    = "m^2/s"

  END MODULE MR_DEF_FIELD_VARS_UNITS