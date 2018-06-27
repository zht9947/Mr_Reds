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
  MODULE MR_MOD_CALC_EQD_ADV_N_DIF_Z

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_ACTIVITY
    USE MR_DEF_TIMING

    USE MR_MOC_K_EPS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_EQD_ADV_N_DIF_Z

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
  SUBROUTINE MR_CALC_EQD_ADV_N_DIF_Z( NI , NJ , NK , EQD_ADV_Z , EQD_DIF_Z )

    USE MR_MOD_OPERATOR_UV

    USE MR_MOD_CALC_QADV_N_QDIF_Z
    USE MR_MOD_CALC_GRAD_Z

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) :: EQD_ADV_Z , EQD_DIF_Z

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,0:NK) :: EQD_QADV_Z_W , EQD_QDIF_Z_W

    INTEGER(IJID_KIND) :: I , J

    CALL MR_CALC_QADV_N_QDIF_Z_SS_W( NI , NJ , NK , DI , RB , W , EQD_QADV_Z_W , EKZ/SID , VZW , EQD_QDIF_Z_W )

  ! CALCULATE ADVECTION
    CALL MR_CALC_GRAD_Z_SS( NI , NJ , NK , EQD_QADV_Z_W , EQD_ADV_Z )

  ! CALCULATE DIFFUSION
    CALL MR_CALC_GRAD_Z_SS( NI , NJ , NK , EQD_QDIF_Z_W , EQD_DIF_Z )

  END SUBROUTINE MR_CALC_EQD_ADV_N_DIF_Z

  END MODULE MR_MOD_CALC_EQD_ADV_N_DIF_Z