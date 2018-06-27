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
  MODULE MR_MOD_CALC_EQK_ADV_N_DIF_XY

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_ACTIVITY
    USE MR_DEF_TIMING

    USE MR_MOC_K_EPS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_EQK_ADV_N_DIF_XY

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:    ) :: KIU , KIV

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
  SUBROUTINE MR_CALC_EQK_ADV_N_DIF_XY( NI , NJ , K , EQK_ADV_XY , EQK_DIF_XY )

    USE MR_MOD_INTERP_XY

    USE MR_MOD_CALC_QADV_N_QDIF_XY
    USE MR_MOD_CALC_GRAD_XY

    USE MR_MOD_OPERATOR_SS

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(KKID_KIND) , INTENT(IN ) :: K

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: EQK_ADV_XY , EQK_DIF_XY

    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: EQK_QADV_XY_U , EQK_QDIF_XY_U
    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: EQK_QADV_XY_V , EQK_QDIF_XY_V

    ALLOCATE( KIV(1:NI1(NI,FDRD_KIND),0:NJ) )
      CALL MR_INTERP_XY_SS_V( NI , NJ , KI(:,:, K ) , KIV )
      CALL MR_CALC_QADV_N_QDIF_XY_SS_U( NI , NJ ,   &
      & KI(:,:, K ) , KIV , RB , U(:,:, K ) , EQK_QADV_XY_U , EKXY/SIK , VXYU(:,:, K ) , EQK_QDIF_XY_U )
    DEALLOCATE( KIV )

    ALLOCATE( KIU(0:NI0(NI,FDRD_KIND),1:NJ) )
      CALL MR_INTERP_XY_SS_U( NI , NJ , KI(:,:, K ) , KIU )
      CALL MR_CALC_QADV_N_QDIF_XY_SS_V( NI , NJ ,   &
      & KI(:,:, K ) , KIU , RB , V(:,:, K ) , EQK_QADV_XY_V , EKXY/SIK , VXYV(:,:, K ) , EQK_QDIF_XY_V )
    DEALLOCATE( KIU )

  ! CALCULATE ADVECTION
  ! REVISE ADVECTION BY THE DEPTH H
    EQK_QADV_XY_U = EQK_QADV_XY_U .MRSSMTP. HU
    EQK_QADV_XY_V = EQK_QADV_XY_V .MRSSMTP. HV
    CALL MR_CALC_REDC_GRAD_XY_SS( NI , NJ , EQK_QADV_XY_U , EQK_QADV_XY_V , EQK_ADV_XY )
  ! CONTRAVARIANTLY TRANSFORM
    EQK_ADV_XY = EQK_ADV_XY .MRSSDIV. H

  ! CALCULATE DIFFUSION
    CALL MR_CALC_REDC_GRAD_XY_SS( NI , NJ , EQK_QDIF_XY_U , EQK_QDIF_XY_V , EQK_DIF_XY )

  END SUBROUTINE MR_CALC_EQK_ADV_N_DIF_XY

  END MODULE MR_MOD_CALC_EQK_ADV_N_DIF_XY