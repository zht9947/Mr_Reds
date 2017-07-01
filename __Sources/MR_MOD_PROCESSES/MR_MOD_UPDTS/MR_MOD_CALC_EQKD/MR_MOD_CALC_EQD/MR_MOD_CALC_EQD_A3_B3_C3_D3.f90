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
  MODULE MR_MOD_CALC_EQD_A3_B3_C3_D3

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_ACTIVITY
    USE MR_DEF_TIMING
    
    USE MR_MCS_K_EPS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_EQD_A3_B3_C3 , MR_CALC_EQD_D3

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
  SUBROUTINE MR_CALC_EQD_A3_B3_C3( NI , NJ , NK , EQD_A3 , EQD_B3 , EQD_C3 )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,2:NK  ) :: EQD_A3
    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,1:NK  ) :: EQD_B3
    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,1:NK-1) :: EQD_C3

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

    DO K = 1 , NK
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          !BLOCK
            EQD_B3( I , J , K ) = MW( I , J )
          !END BLOCK
        END DO
      END DO
    END DO

    DO K = 2 , NK-1
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            EQD_A3( I , J , K ) =-DT * PHI * EKZ/SID * MW( I , J ) * VZW( I , J ,K-1) / ( H( I , J ) * DSIGMA )**2
            EQD_B3( I , J , K ) = EQD_B3( I , J , K ) - EQD_A3( I , J , K )
          ELSE
            EQD_A3( I , J , K ) = 0.0
          END IF
        END DO
      END DO
    END DO
    K = NK
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          !BLOCK
            EQD_A3( I , J , K ) = 0.0
          !END BLOCK
        END DO
      END DO
    !END K = NK

    K = 1
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          !BLOCK
            EQD_C3( I , J , K ) = 0.0
          !END BLOCK
        END DO
      END DO
    !END K = 1
    DO K = 2 , NK-1
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            EQD_C3( I , J , K ) = EQD_A3( I , J ,K+1)
            EQD_B3( I , J , K ) = EQD_B3( I , J , K ) - EQD_C3( I , J , K )
          ELSE
            EQD_C3( I , J , K ) = 0.0
          END IF
        END DO
      END DO
    END DO

  END SUBROUTINE MR_CALC_EQD_A3_B3_C3

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
  SUBROUTINE MR_CALC_EQD_D3( NI , NJ , NK , EQKD_PRO_XY , EQKD_PRO_Z , EQD_D3 )

    USE MR_MOD_CALC_EQD_ADV_N_DIF_Z
    USE MR_MOD_CALC_EQD_ADV_N_DIF_XY
    USE MR_MOD_CALC_EQD_SRC_XYZ

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:NK  ) :: EQKD_PRO_XY , EQKD_PRO_Z
    
    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,1:NK  ) :: EQD_D3

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) :: EQD_ADV_Z  , EQD_DIF_Z
    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ     ) :: EQD_ADV_XY , EQD_DIF_XY
    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ     ) :: EQD_SRC_XYZ

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

    DO K = 1 , NK

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          !BLOCK
            EQD_D3( I , J , K ) = MW( I , J ) * DI( I , J , K )
          !END BLOCK
        END DO
      END DO

    END DO

  ! ASSIGN NONEQUILIBRIUM TERMS
    CALL MR_CALC_EQD_ADV_N_DIF_Z( NI , NJ , NK , EQD_ADV_Z , EQD_DIF_Z )

    DO K = 2 , NK-1

      CALL MR_CALC_EQD_ADV_N_DIF_XY( NI , NJ , K , EQD_ADV_XY , EQD_DIF_XY )
      CALL MR_CALC_EQD_SRC_XYZ( NI , NJ , K , EQKD_PRO_XY(:,:, K ) , EQKD_PRO_Z(:,:, K ) , EQD_SRC_XYZ )

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            EQD_D3( I , J , K ) = EQD_D3( I , J , K ) +  &
            & EQD_ADV_XY( I , J ) + EQD_ADV_Z( I , J , K ) +   &
            & EQD_DIF_XY( I , J ) + EQD_DIF_Z( I , J , K ) +   &
            & EQD_SRC_XYZ( I , J )
          END IF
        END DO
      END DO

    END DO

  END SUBROUTINE MR_CALC_EQD_D3

  END MODULE MR_MOD_CALC_EQD_A3_B3_C3_D3