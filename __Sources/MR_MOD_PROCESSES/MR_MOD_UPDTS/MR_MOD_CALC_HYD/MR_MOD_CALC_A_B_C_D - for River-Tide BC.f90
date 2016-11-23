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
  MODULE MR_MOD_CALC_A_B_C_D

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_ACTIVITY
    USE MR_DEF_TIMING

    IMPLICIT NONE

    PRIVATE

    PUBLIC ::

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
  SUBROUTINE MR_CALC_A1_B1_C1( NI , NJ , ALFA , A1 , B1 , C1 )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI  ,1:NJ,1:2) :: ALFA

    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(2:NI  ,1:NJ) :: A1
    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI  ,1:NJ) :: B1
    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI-1,1:NJ) :: C1

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
      DO I = 1 , NI
        B1( I , J ) = MUV( I , J )
      END DO
    END DO

    DO J = 1 , NJ
      DO I = 2 , NI-1
        IF( ACTIVITY(I-1, J ) == BEACTIVE ) THEN
          A1( I , J ) = DT * PHI * BPAR * ( ALFA( I , J ,1) + ALFA(I-1, J ,1) ) / 2.0
          B1( I , J ) = B1( I , J ) - A1( I , J )
        ELSE
          A1( I , J ) = 0.0
        END IF
      END DO
      I = NI
      IF( ACTIVITY(I-1, J ) == BEACTIVE ) THEN
        A1( I , J ) = DT * PHI * BPAR * ALFA(I-1, J ,1) / 2.0
        B1( I , J ) = B1( I , J ) - A1( I , J )
      ELSE
        A1( I , J ) = 0.0
      END IF
    END DO

    DO J = 1 , NJ
      DO I = 1 , NI-1
        IF( ACTIVITY(I+1, J ) == BEACTIVE ) THEN
          C1( I , J ) = DT * PHI * BPAR * ( ALFA(I+1, J ,1) + ALFA( I , J ,1) ) / 2.0
          B1( I , J ) = B1( I , J ) - C1( I , J )
        ELSE
          C1( I , J ) = 0.0
        END IF
      END DO
    END DO

  END SUBROUTINE MR_CALC_A1_B1_C1

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
  SUBROUTINE MR_CALC_D1( NI , NJ , ZS , BETA , D1 , QU_AT_I0 )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI  ,1:NJ) :: ZS
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI  ,1:NJ,1:2) :: BETA

    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI  ,1:NJ) :: D1

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(       1:NJ) :: QU_AT_I0

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
      DO I = 1 , NI
        D1( I , J ) =   &
        MUV( I , J ) * ZS( I , J )   &
        - DT * (1.0-PHI) * BPAR *   &
          ( MU( I , J ) * HU( I , J ) * UA( I , J )   &
          - MU(I-1, J ) * HU(I-1, J ) * UA(I-1, J )   &
        & )   &
        - DT * BPAR *   &
          ( MV( I , J ) * HV( I , J ) * VA( I , J )   &
          - MV( I ,J-1) * HV( I ,J-1) * VA( I ,J-1)   &
        & )
      END DO
    END DO

    DO J = 1 , NJ
      I = 1
      D1( I , J ) = D1( I , J ) + DT * PHI * BPAR * MU(I-1, J ) * QU_AT_I0( J )
      DO I = 2 , NI-1
        IF( ACTIVITY(I-1, J ) == BEACTIVE ) THEN
          D1( I , J ) = D1( I , J ) + DT * PHI * BPAR * ( BETA( I , J ,1) + BETA(I-1, J ,1) ) / 2.0
        END IF
      END DO
      I = NI
      IF( ACTIVITY(I-1, J ) == BEACTIVE ) THE
        D1( I , J ) = D1( I , J ) - DT * PHI * BPAR * ( BETA( I , J ,1) - BETA(I-1, J ,1) ) / 2.0
      END IF
    END DO

    DO J = 1 , NJ
      DO I = 1 , NI-1
        IF( ACTIVITY(I+1, J ) == BEACTIVE ) THEN
          D1( I , J ) = D1( I , J ) - DT * PHI * BPAR * ( BETA(I+1, J ,1) + BETA( I , J ,1) ) / 2.0
        END IF
      END DO
    END DO

  END SUBROUTINE MR_CALC_D1

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
  SUBROUTINE MR_CALC_A2_B2_C2( NI , NJ , ALFA , A2 , B2 , C2 )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ  ,1:2) :: ALFA

    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI,2:NJ  ) :: A2
    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI,1:NJ  ) :: B2
    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI,1:NJ-1) :: C2

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
      DO I = 1 , NI
        B2( I , J ) = MUV( I , J )
      END DO
    END DO

    DO J = 2 , NJ
      DO I = 1 , NI
        IF( ACTIVITY( I ,J-1) == BEACTIVE ) THEN
          A2( I , J ) = DT * PHI * BPAR * ( ALFA( I , J ,2) + ALFA( I ,J-1,2) ) / 2.0
          B2( I , J ) = B2( I , J ) - A2( I , J )
        ELSE
          A2( I , J ) = 0.0
        END IF
      END DO
    END DO

    DO J = 1 , NJ-1
      DO I = 1 , NI
        IF( ACTIVITY( I ,J+1) == BEACTIVE ) THEN
          C2( I , J ) = DT * PHI * BPAR * ( ALFA( I ,J+1,2) + ALFA( I , J ,2) ) / 2.0
          B2( I , J ) = B2( I , J ) - C2( I , J )
        ELSE
          C2( I , J ) = 0.0
        END IF
      END DO
    END DO

  END SUBROUTINE MR_CALC_A2_B2_C2

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
  SUBROUTINE MR_CALC_D2( NI , NJ , ZT , BETA , D2 )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ  ) :: ZT
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ  ,1:2) :: BETA

    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI,1:NJ  ) :: D2

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
      DO I = 1 , NI
        D2( I , J ) =   &
        MUV( I , J ) * ZT( I , J )   &
        + DT * BPAR *   &
          ( PHI * ( MV( I , J ) * HV( I , J ) * VA( I , J )   &
                  - MV( I ,J-1) * HV( I ,J-1) * VA( I ,J-1)   &
                  )   &
        & )
      END DO
    END DO

    DO J = 2 , NJ
      DO I = 1 , NI
        IF( ACTIVITY( I ,J-1) == BEACTIVE ) THEN
          D2( I , J ) = D2( I , J ) + DT * PHI * BPAR * ( BETA( I , J ,2) + BETA( I ,J-1,2) ) / 2.0
        END IF
      END DO
    END DO

    DO J = 1 , NJ-1
      DO I = 1 , NI
        IF( ACTIVITY( I ,J+1) == BEACTIVE ) THEN
          D2( I , J ) = D2( I , J ) - DT * PHI * BPAR * ( BETA( I ,J+1,2) + BETA( I , J ,2) ) / 2.0
        END IF
      END DO
    END DO

  END SUBROUTINE MR_CALC_D2

  END MODULE MR_MOD_CALC_A_B_C_D