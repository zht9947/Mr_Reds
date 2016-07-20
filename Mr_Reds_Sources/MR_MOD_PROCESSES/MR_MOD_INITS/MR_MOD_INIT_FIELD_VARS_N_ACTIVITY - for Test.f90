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
    
    ZB    = - 0.026
    ZS    =   0.0        ;  ZSU =   0.0        ;  ZSV =   0.0
    H     =   0.026      ;   HU =   0.026      ;   HV =   0.026
    R     =   0.0
    RI    =   0.0
    UVA   =   0.0000     ;   UA =   0.0000     ;   VA =   0.0000
    UV    =   0.0000     ;    U =   0.0000     ;    V =   0.0000
    WW    =   0.000
    W     =   0.000
    TBUV  =   0.0
    TBFUV =   0.0        ; TBFU =   0.0        ; TBFV =   0.0
    QSBUV =   0.0        ; QSBU =   0.0        ; QSBV =   0.0
    VXYUV =   1.000e+0   ; VXYU =   1.000e+0   ; VXYV =   1.000e+0
    VZWW  =   1.000e-4
    VZW   =   1.000e-4
    DXYUV =   1.000e+0   ; DXYU =   1.000e+0   ; DXYV =   1.000e+0
    DZWW  =   1.000e-4
    DZW   =   1.000e-4
    KI    =   0.0
    DI    =   0.0
    
    
    ACTIVITY(1:NI,1:NJ) = BEACTIVE
    
    
    WHERE( ACTIVITY == NOACTIVE )
      ZB = HUGE(ZB)
       H = EPSILON(H)
    END WHERE

  END SUBROUTINE MR_INIT_FIELD_VARS_N_ACTIVITY
  
  END MODULE MR_MOD_INIT_FIELD_VARS_N_ACTIVITY