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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_CALC_EQD_ADV_N_DIF_XY

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_ACTIVITY
    USE MR_DEF_TIMING

    USE MR_MOC_K_EPS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_EQD_ADV_N_DIF_XY

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:    ) :: DIU , DIV

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
!   2015-06-10    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_CALC_EQD_ADV_N_DIF_XY( NI , NJ , K , EQD_ADV_XY , EQD_DIF_XY )

    USE MR_MOD_INTERP_XY

    USE MR_MOD_CALC_QADV_N_QDIF_XY
    USE MR_MOD_CALC_GRAD_XY

    USE MR_MOD_OPERATOR_SS

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(KKID_KIND) , INTENT(IN ) :: K

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: EQD_ADV_XY , EQD_DIF_XY

    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: EQD_QADV_XY_U , EQD_QDIF_XY_U
    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: EQD_QADV_XY_V , EQD_QDIF_XY_V

    ALLOCATE( DIV(1:NI1(NI,FDRD_KIND),0:NJ) )
      CALL MR_INTERP_XY_SS_V( NI , NJ , DI(:,:, K ) , DIV )
      CALL MR_CALC_QADV_N_QDIF_XY_SS_U( NI , NJ ,   &
      & DI(:,:, K ) , DIV , RB , U(:,:, K ) , EQD_QADV_XY_U , EKXY/SID , VXYU(:,:, K ) , EQD_QDIF_XY_U )
    DEALLOCATE( DIV )

    ALLOCATE( DIU(0:NI0(NI,FDRD_KIND),1:NJ) )
      CALL MR_INTERP_XY_SS_U( NI , NJ , DI(:,:, K ) , DIU )
      CALL MR_CALC_QADV_N_QDIF_XY_SS_V( NI , NJ ,   &
      & DI(:,:, K ) , DIU , RB , V(:,:, K ) , EQD_QADV_XY_V , EKXY/SID , VXYV(:,:, K ) , EQD_QDIF_XY_V )
    DEALLOCATE( DIU )

  ! CALCULATE ADVECTION
  ! REVISE ADVECTION BY THE DEPTH H
    EQD_QADV_XY_U = EQD_QADV_XY_U .MRSSMTP. HU
    EQD_QADV_XY_V = EQD_QADV_XY_V .MRSSMTP. HV
    CALL MR_CALC_REDC_GRAD_XY_SS( NI , NJ , EQD_QADV_XY_U , EQD_QADV_XY_V , EQD_ADV_XY )
  ! CONTRAVARIANTLY TRANSFORM
    EQD_ADV_XY = EQD_ADV_XY .MRSSDIV. H

  ! CALCULATE DIFFUSION
    CALL MR_CALC_REDC_GRAD_XY_SS( NI , NJ , EQD_QDIF_XY_U , EQD_QDIF_XY_V , EQD_DIF_XY )

  END SUBROUTINE MR_CALC_EQD_ADV_N_DIF_XY

  END MODULE MR_MOD_CALC_EQD_ADV_N_DIF_XY