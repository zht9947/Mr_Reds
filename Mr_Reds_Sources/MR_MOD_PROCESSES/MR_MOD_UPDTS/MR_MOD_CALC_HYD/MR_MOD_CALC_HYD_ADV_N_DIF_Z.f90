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
  MODULE MR_MOD_CALC_HYD_ADV_N_DIF_Z

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_ACTIVITY
    USE MR_DEF_TIMING

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_HYD_ADV_N_DIF_Z

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:    ) :: TSUV

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
  SUBROUTINE MR_CALC_HYD_ADV_N_DIF_Z( NI , NJ , NK , HYD_ADV_Z , HYD_DIF_Z )

    USE MR_MOD_CALC_QADV_N_QDIF_Z
    USE MR_MOD_CALC_GRAD_Z

    USE MR_MOD_OPERATOR_UV

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI,1:NJ,1:2,1:NK) :: HYD_ADV_Z , HYD_DIF_Z

    REAL   (FDRD_KIND) , DIMENSION(1:NI,1:NJ,1:2,0:NK) :: HYD_QADV_Z_W , HYD_QDIF_Z_W

    INTEGER(KKID_KIND) :: K

    ALLOCATE( TSUV(1:NI,1:NJ,1:2) ) ; TSUV = 0.0
      CALL MR_CALC_QADV_N_QDIF_Z_UV_W( NI , NJ , NK , UV , RB , W , HYD_QADV_Z_W , EKZ , VZW , HYD_QDIF_Z_W , TBUV , TSUV )
    DEALLOCATE( TSUV )

  ! CALCULATE ADVECTION
  ! REVISE ADVECTION BY DEPTH
    DO K = 0 , NK
      HYD_QADV_Z_W(:,:,1:2, K ) = HYD_QADV_Z_W(:,:,1:2, K )
    END DO
    CALL MR_CALC_GRAD_Z_UV( NI , NJ , NK , HYD_QADV_Z_W , HYD_ADV_Z )

  ! CALCULATE DIFFUSION
    CALL MR_CALC_GRAD_Z_UV( NI , NJ , NK , HYD_QDIF_Z_W , HYD_DIF_Z )
  ! REVISE DIFFUSION BY DEPTH
    DO K = 1 , NK
      HYD_DIF_Z(:,:,1:2, K ) = HYD_DIF_Z(:,:,1:2, K )
    END DO

  END SUBROUTINE MR_CALC_HYD_ADV_N_DIF_Z

  END MODULE MR_MOD_CALC_HYD_ADV_N_DIF_Z