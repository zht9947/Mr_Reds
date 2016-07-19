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
  MODULE MR_MOD_CALC_HYD_A3_B3_C3_D3

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_ACTIVITY
    USE MR_DEF_TIMING

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_HYD_A3_B3_C3 , MR_CALC_RAW_HYD_D3

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
  SUBROUTINE MR_CALC_HYD_A3_B3_C3( NI , NJ , NK , HYD_A3 , HYD_B3 , HYD_C3 )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI,1:NJ,    2:NK  ) :: HYD_A3
    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI,1:NJ,    1:NK  ) :: HYD_B3
    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI,1:NJ,    1:NK-1) :: HYD_C3

    INTEGER(KKID_KIND) :: K

    DO K = 1 , NK
      !BLOCK
        HYD_B3(:,:, K ) = MW
      !END BLOCK
    END DO

    DO K = 2 , NK
      WHERE( ACTIVITY == BEACTIVE )
        HYD_A3(:,:, K ) =-DT * PHI * EKZ * MW * VZW(:,:,K-1) / ( H * DSIGMA )**2
        HYD_B3(:,:, K ) = HYD_B3(:,:, K ) - HYD_A3(:,:, K )
      ELSE WHERE
        HYD_A3(:,:, K ) = 0.0
      END WHERE
    END DO

    DO K = 1 , NK-1
      WHERE( ACTIVITY == BEACTIVE )
        HYD_C3(:,:, K ) = HYD_A3(:,:,K+1)
        HYD_B3(:,:, K ) = HYD_B3(:,:, K ) - HYD_C3(:,:, K )
      ELSE WHERE
        HYD_C3(:,:, K ) = 0.0
      END WHERE
    END DO

  END SUBROUTINE MR_CALC_HYD_A3_B3_C3

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
  SUBROUTINE MR_CALC_RAW_HYD_D3( NI , NJ , NK , HYD_D3 )

    USE MR_MOD_CALC_HYD_ADV_N_DIF_Z
    USE MR_MOD_CALC_HYD_ADV_N_DIF_XY
    USE MR_MOD_CALC_HYD_BAR_XY
    USE MR_MOD_CALC_HYD_COR_XY

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI,1:NJ,1:2,1:NK  ) :: HYD_D3

    REAL   (FDRD_KIND) , DIMENSION(1:NI,1:NJ,1:2,1:NK) :: HYD_ADV_Z , HYD_DIF_Z
    REAL   (FDRD_KIND) , DIMENSION(1:NI,1:NJ,1:2) :: HYD_ADV_XY , HYD_DIF_XY , HYD_BAR_XY , HYD_COR_XY

    INTEGER(KKID_KIND) :: K

    INTEGER :: DIM

    CALL MR_CALC_HYD_ADV_N_DIF_Z( NI , NJ , NK , HYD_ADV_Z , HYD_DIF_Z )

    DO K = 1 , NK

      CALL MR_CALC_HYD_ADV_N_DIF_XY( NI , NJ , K , HYD_ADV_XY , HYD_DIF_XY )
      CALL MR_CALC_HYD_BAR_XY( NI , NJ , K , HYD_BAR_XY )
      CALL MR_CALC_HYD_COR_XY( NI , NJ , K , HYD_COR_XY )

      DO DIM = 1 , 2

        HYD_D3(:,:,DIM, K ) = MW * UV(:,:,DIM, K )

        WHERE( ACTIVITY == BEACTIVE )
          HYD_D3(:,:,DIM, K ) = HYD_D3(:,:,DIM, K ) +  &
          & HYD_ADV_XY(:,:,DIM) + HYD_ADV_Z(:,:,DIM, K ) +   &
          & HYD_DIF_XY(:,:,DIM) + HYD_DIF_Z(:,:,DIM, K ) !+   &
        ! & HYD_BAR_XY(:,:,DIM) +   &
        ! & HYD_COR_XY(:,:,DIM)
        END WHERE

      END DO

    END DO

  END SUBROUTINE MR_CALC_RAW_HYD_D3

  END MODULE MR_MOD_CALC_HYD_A3_B3_C3_D3