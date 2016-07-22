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

    REAL   (CARD_KIND) , INTENT(INOUT) , DIMENSION(1:NI1(CARD_KIND),1:NJ,1:2,1:NK) :: HYD_D3

    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(CARD_KIND),1:NJ,1:2) :: ALFA , BETA

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2) :: GRAD_XY_ZS

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    INTEGER(KKID_KIND) :: K

  ! INITIALIZE BETA
    DO DIM = 1 , 2
      !BLOCK
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            BETA( I , J ,DIM) = 0.0
          END DO
        END DO
      !END BLOCK
      DO K = 1 , NK
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            BETA( I , J ,DIM) = BETA( I , J ,DIM) + DSIGMA * HYD_D3( I , J ,DIM, K )
          END DO
        END DO
      END DO
    END DO

  ! REVISE HYD_D3
    DO DIM = 1 , 2
      DO K = 1 , NK
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            HYD_D3( I , J ,DIM, K ) = HYD_D3( I , J ,DIM, K ) - BETA( I , J ,DIM)
          END DO
        END DO
      END DO
    END DO
    
    BETA = BETA .MRUVMTP. H

  ! CALCULATE ALFA AND BETA WITH THE AID OF GRAD_XY_ZS
    CALL MR_CALC_GRAD_XY_SS( NI , NJ , ZSU , ZSV , GRAD_XY_ZS )

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        ALFA( I , J ,1) =   &
        -   DT * MW( I , J ) * H( I , J ) *   &
        &               PHI  * FUV( I , J ,1,1)
      END DO
    END DO
    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
          BETA( I , J ,1) = BETA( I , J ,1)   &
          + DT * MW( I , J ) * H( I , J ) *   &
            ( XYR/ZR / ALPAR * IUV( I , J ,1,1) * SLOPE   &   ! ADDITIONAL SLOPE TERM
            - (    (1.0-PHI) * FUV( I , J ,1,1) * GRAD_XY_ZS( I , J ,1)   &
              +                FUV( I , J ,1,2) * GRAD_XY_ZS( I , J ,2)   &
              )   &
          & )
        END IF
      END DO
    END DO

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        ALFA( I , J ,2) =   &
        -   DT * MW(I , J ) * H( I , J ) *   &
        &              PHI  * FUV( I , J ,2,2)
      END DO
    END DO
    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
          BETA( I , J ,2) = BETA( I , J ,2)   &
          + DT * MW( I , J ) * H( I , J ) *   &
            ( XYR/ZR / ALPAR * IUV( I , J ,2,1) * SLOPE   &   ! ADDITIONAL SLOPE TERM
            - (                FUV( I , J ,2,1) * GRAD_XY_ZS( I , J ,1)   &
              +    (1.0-PHI) * FUV( I , J ,2,2) * GRAD_XY_ZS( I , J ,2)   &
              )   &
          & )
        END IF
      END DO
    END DO

  END SUBROUTINE MR_CALC_ALFA_N_BETA

  END MODULE MR_MOD_CALC_ALFA_N_BETA