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

    PUBLIC :: MR_INIT_FIELD_VARS_N_ACTIVITY_COLD_MODE
    PUBLIC :: MR_INIT_FIELD_VARS_N_ACTIVITY_HOT_MODE

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
  SUBROUTINE MR_INIT_FIELD_VARS_N_ACTIVITY_COLD_MODE

    IMPLICIT NONE

    ZB       = - 1.000e+0
    ZS       =   0.000e+0   ;  ZSU    =   0.000e+0   ;  ZSV    =   0.000e+0
    H        =   1.000e+0   ;   HU    =   1.000e+0   ;   HV    =   1.000e+0
    R        =   0.000e+0
    RI       =   0.000e+0
    UVA      =   0.000e+0   ;   UA    =   0.000e+0   ;   VA    =   0.000e+0
    UV       =   0.000e+0   ;    U    =   0.000e+0   ;    V    =   0.000e+0
    WW       =   0.000e+0
    W        =   0.000e+0
    TBUV     =   0.000e+0
    TBFUV    =   0.000e+0
    QSBUV    =   0.000e+0   ; QSBU    =   0.000e+0   ; QSBV    =   0.000e+0
    KI       =   0.000e+0
    KIB      =   0.000e+0
    DI       =   0.000e+0
    DIB      =   0.000e+0
    DIS      =   0.000e+0
    VXYUV    =   1.000e+0   ; VXYU    =   1.000e+0   ; VXYV    =   1.000e+0
    VZWW     =   1.000e-6
    VZW      =   1.000e-6
    DXYUV    =   1.000e+0   ; DXYU    =   1.000e+0   ; DXYV    =   1.000e+0
    DZWW     =   1.000e-6
    DZW      =   1.000e-6

    ACTIVITY =   BEACTIVE

  END SUBROUTINE MR_INIT_FIELD_VARS_N_ACTIVITY_COLD_MODE

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
  SUBROUTINE MR_INIT_FIELD_VARS_N_ACTIVITY_HOT_MODE( FILE_XMDF_NAME , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MULTI_DSETS

    USE MR_MOD_READ_FIELD_VARS_N_ACTIVITY
  
    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME

    INTEGER                          :: FILE_XMDF_ID , MULTI_DSETS_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY

  END SUBROUTINE MR_INIT_FIELD_VARS_N_ACTIVITY_HOT_MODE

  END MODULE MR_MOD_INIT_FIELD_VARS_N_ACTIVITY