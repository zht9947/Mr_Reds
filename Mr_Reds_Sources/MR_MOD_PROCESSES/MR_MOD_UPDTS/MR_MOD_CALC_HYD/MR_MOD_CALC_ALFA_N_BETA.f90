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
  MODULE MR_MOD_CALC_ALFA_N_BETA

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_SLOPE
    USE MR_DEF_ACTIVITY
    USE MR_DEF_TIMING

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_ALFA_N_BETA

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
  SUBROUTINE MR_CALC_ALFA_N_BETA( NI , NJ , NK , HYD_D3 , ALFA , BETA )

    USE MR_MOD_OPERATOR_UV
    USE MR_MOD_CALC_GRAD_XY

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (CARD_KIND) , INTENT(INOUT) , DIMENSION(1:NI,1:NJ,1:2,1:NK) :: HYD_D3

    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI,1:NJ,1:2) :: ALFA , BETA

    REAL   (FDRD_KIND) , DIMENSION(1:NI,1:NJ,1:2) :: GRAD_XY_ZS

    INTEGER(KKID_KIND) :: K

  ! INITIALIZE BETA
    BETA(:,:,1:2) = 0.0
    DO K = 1 , NK
      BETA(:,:,1:2) = BETA(:,:,1:2) + DSIGMA * HYD_D3(:,:,1:2, K )
    END DO

  ! REVISE HYD_D3
    DO K = 1 , NK
      HYD_D3(:,:,1:2, K ) = HYD_D3(:,:,1:2, K ) - BETA(:,:,1:2)
    END DO

    BETA = BETA .MRUVMTP. H

  ! CALCULATE ALFA AND BETA WITH THE AID OF GRAD_XY_ZS
    CALL MR_CALC_GRAD_XY_SS( NI , NJ , ZSU , ZSV , GRAD_XY_ZS )

    ALFA(:,:,1) =   &
    -   DT * MW * H *   &
    &               PHI  * FUV(:,:,1,1)
    WHERE( ACTIVITY == BEACTIVE )
      BETA(:,:,1) = BETA(:,:,1)   &
      + DT * MW * H *   &
        ( XYR/ZR / ALPAR * IUV(:,:,1,1) * SLOPE   &   ! ADDITIONAL SLOPE TERM
        - (    (1.0-PHI) * FUV(:,:,1,1) * GRAD_XY_ZS(:,:,1)   &
          +                FUV(:,:,1,2) * GRAD_XY_ZS(:,:,2)   &
          )   &
      & )
    END WHERE

    ALFA(:,:,2) =   &
    -   DT * MW * H *   &
    &               PHI  * FUV(:,:,2,2)
    WHERE( ACTIVITY == BEACTIVE )
      BETA(:,:,2) = BETA(:,:,2)   &
      + DT * MW * H *   &
        ( XYR/ZR / ALPAR * IUV(:,:,2,1) * SLOPE   &   ! ADDITIONAL SLOPE TERM
        - (                FUV(:,:,2,1) * GRAD_XY_ZS(:,:,1)   &
          +    (1.0-PHI) * FUV(:,:,2,2) * GRAD_XY_ZS(:,:,2)   &
          )   &
      & )
    END WHERE

  END SUBROUTINE MR_CALC_ALFA_N_BETA

  END MODULE MR_MOD_CALC_ALFA_N_BETA