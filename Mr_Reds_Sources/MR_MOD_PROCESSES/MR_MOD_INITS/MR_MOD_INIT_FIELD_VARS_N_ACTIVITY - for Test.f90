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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_INIT_FIELD_VARS_N_ACTIVITY

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_ACTIVITY

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_INIT_FIELD_VARS_N_ACTIVITY

!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_INIT_FIELD_VARS_N_ACTIVITY

    IMPLICIT NONE

    ZB    = - 1.000
    ZS    =   0.000      ;  ZSU =   0.000      ;  ZSV =   0.000
    H     =   1.000      ;   HU =   1.000      ;   HV =   1.000
    R     =   0.000
    RI    =   0.000
    UVA   =   0.000      ;   UA =   0.000      ;   VA =   0.000
    UV    =   0.000      ;    U =   0.000      ;    V =   0.000
    WW    =   0.000
    W     =   0.000
    TBUV  =   0.000
    TBFUV =   0.000
    QSBUV =   0.000      ; QSBU =   0.000      ; QSBV =   0.000
    KI    =   0.000
    KIB   =   0.000
    DI    =   0.000
    DIB   =   0.000
    DIS   =   0.000
    VXYUV =   1.000e+0   ; VXYU =   1.000e+0   ; VXYV =   1.000e+0
    VZWW  =   1.000e-6
    VZW   =   1.000e-6
    DXYUV =   1.000e+0   ; DXYU =   1.000e+0   ; DXYV =   1.000e+0
    DZWW  =   1.000e-6
    DZW   =   1.000e-6


    ACTIVITY = BEACTIVE


    WHERE( ACTIVITY == NOACTIVE )
      ZB = HUGE(ZB)
       H = EPSILON(H)
    END WHERE

  END SUBROUTINE MR_INIT_FIELD_VARS_N_ACTIVITY

  END MODULE MR_MOD_INIT_FIELD_VARS_N_ACTIVITY