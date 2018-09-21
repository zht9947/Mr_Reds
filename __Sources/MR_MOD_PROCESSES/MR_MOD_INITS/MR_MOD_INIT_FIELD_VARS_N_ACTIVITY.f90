#INCLUDE 'MR_H_ALIGN_PADDING.H'
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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_INIT_FIELD_VARS_N_ACTIVITY

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_FIELD_VARS_DSET_NAMES
    USE MR_DEF_ACTIVITY

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_INIT_FIELD_VARS_N_ACTIVITY_COLD
    PUBLIC :: MR_INIT_FIELD_VARS_N_ACTIVITY_HOT

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_INIT_FIELD_VARS_N_ACTIVITY_COLD( HTH , T )

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN ) :: HTH

    REAL   (TMRD_KIND) , INTENT(OUT) :: T

    TBFUV    =   0.000E+0
    TBUV     =   0.000E+0
    KI       =   0.000E+0
    DI       =   0.000E+0
    CSS      =   0.000E+0
    QSBUV    =   0.000E+0   ; QSBU    =   0.000E+0   ; QSBV    =   0.000E+0
    R        =   0.000E+0
    RI       =   0.000E+0
    ZB       = - HTH
    ZS       =   0.000E+0   ; ZSU     =   0.000E+0   ; ZSV     =   0.000E+0
    H        =   HTH        ; HU      =   HTH        ; HV      =   HTH
    UVA      =   0.000E+0   ; UA      =   0.000E+0   ; VA      =   0.000E+0
    UV       =   0.000E+0   ; U       =   0.000E+0   ; V       =   0.000E+0
    W        =   0.000E+0
    VZWW     =   V0 / VZR   ; VXYU    =   V0 / VZR   ; VXYV    =   V0 / VZR
    VZW      =   V0 / VZR
    DZWW     =   V0 / DZR   ; DXYU    =   V0 / DZR   ; DXYV    =   V0 / DZR
    DZW      =   V0 / DZR

    ACTIVITY =   BEACTIVE

   !BLOCK
  ! ALL ABOVE FIELD VARIABLES AND ACTIVITY 
  ! ARE ASSOCIATED WITH THE FOLLOWING THEORETICAL ZERO TIME
    T = 0.00
   !END BLOCK

  END SUBROUTINE MR_INIT_FIELD_VARS_N_ACTIVITY_COLD

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_INIT_FIELD_VARS_N_ACTIVITY_HOT( FILE_XMDF_NAME , T , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MULTI_DSETS

    USE MR_MOD_GET_TIMES

    USE MR_MOD_INPUT

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME

    INTEGER                          :: FILE_XMDF_ID , MULTI_DSETS_ID

    INTEGER(TSID_KIND)               :: NTSS

    REAL   (TMRD_KIND) , INTENT(OUT) :: T

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    CALL MR_GET_NTSS_N_T_NTSS( FILE_XMDF_NAME , NTSS , T , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing field variables and activity with hot mode"
      RETURN
    END IF

    CALL MR_INPUT( FILE_XMDF_NAME , NTSS , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing field variables and activity with hot mode"
      RETURN
    END IF

  END SUBROUTINE MR_INIT_FIELD_VARS_N_ACTIVITY_HOT

  END MODULE MR_MOD_INIT_FIELD_VARS_N_ACTIVITY