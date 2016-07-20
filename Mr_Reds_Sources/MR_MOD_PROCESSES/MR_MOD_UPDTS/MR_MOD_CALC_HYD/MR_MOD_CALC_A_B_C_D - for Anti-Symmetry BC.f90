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
  MODULE MR_MOD_CALC_A_B_C_D

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_ACTIVITY
    USE MR_DEF_TIMING

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_A1_B1_C1 , MR_CALC_D1
    PUBLIC :: MR_CALC_A2_B2_C2 , MR_CALC_D2

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

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ  ,1:2) :: ALFA

    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(2:NI2(CARD_KIND)  ,1:NJ      ) :: A1
    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ      ) :: B1
    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI2(CARD_KIND)-1,1:NJ      ) :: C1

    INTEGER(IJID_KIND) :: I , J
    INTEGER(IJID_KIND) :: P , Q

    DO J = 1 , NJ
      DO I = 1 , NI
        !BLOCK
          B1( I , J ) = MW( I , J )
        !END BLOCK
      END DO
    END DO

    DO J = 1 , NJ
      I = 1
        P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1
        IF( ACTIVITY( I , J ) == BEACTIVE .AND. ACTIVITY(P-1, Q ) == BEACTIVE ) THEN
          B1( I , J ) = B1( I , J ) - DT * PHI * BPAR * ( ALFA( I , J ,1) + ALFA(P-1, Q ,1) ) / 2.0
        END IF
      !END I = 1
      DO I = 2 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE .AND. ACTIVITY(I-1, J ) == BEACTIVE ) THEN
          A1( I , J ) = DT * PHI * BPAR * ( ALFA( I , J ,1) + ALFA(I-1, J ,1) ) / 2.0
          B1( I , J ) = B1( I , J ) - A1( I , J )
        ELSE
          A1( I , J ) = 0.0
        END IF
      END DO
    END DO

    DO J = 1 , NJ
      DO I = 1 , NI-1
        IF( ACTIVITY( I , J ) == BEACTIVE .AND. ACTIVITY(I+1, J ) == BEACTIVE ) THEN
          C1( I , J ) = DT * PHI * BPAR * ( ALFA(I+1, J ,1) + ALFA( I , J ,1) ) / 2.0
          B1( I , J ) = B1( I , J ) - C1( I , J )
        ELSE
          C1( I , J ) = 0.0
        END IF
      END DO
      I = NI
        P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1
        IF( ACTIVITY( I , J ) == BEACTIVE .AND. ACTIVITY(P+1, Q ) == BEACTIVE ) THEN
          B1( I , J ) = B1( I , J ) - DT * PHI * BPAR * ( ALFA(P+1, Q ,1) + ALFA( I , J ,1) ) / 2.0
        END IF
      !END I = NI
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
  SUBROUTINE MR_CALC_D1( NI , NJ , ZS , ALFA , BETA , D1 )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND)  ,1:NJ      ) :: ZS
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ  ,1:2) :: ALFA , BETA

    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ      ) :: D1

    INTEGER(IJID_KIND) :: I , J
    INTEGER(IJID_KIND) :: P , Q

    DO J = 1 , NJ
      DO I = 1 , NI
        !BLOCK
          D1( I , J ) = MW( I , J ) * ZS( I , J )
        !END BLOCK
      END DO
    END DO

    DO J = 1 , NJ
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
          D1( I , J ) = D1( I , J )   &
          - DT * (1.0-PHI) * BPAR *   &
            ( MU( I , J ) * HU( I , J ) * UA( I , J )   &
            - MU(I-1, J ) * HU(I-1, J ) * UA(I-1, J )   &
          & )   &
          - DT * BPAR *   &
            ( MV( I , J ) * HV( I , J ) * VA( I , J )   &
            - MV( I ,J-1) * HV( I ,J-1) * VA( I ,J-1)   &
          & )
        END IF
      END DO
    END DO

    DO J = 1 , NJ
      I = 1
        P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1
        IF( ACTIVITY( I , J ) == BEACTIVE .AND. ACTIVITY(P-1, Q ) == BEACTIVE ) THEN
          D1( I , J ) = D1( I , J )   &
          + DT * PHI * BPAR *   &
            ( ( ALFA( I , J ,1) + ALFA(P-1, Q ,1) ) * (             - ZS(P-1, Q ) )   &
            + ( BETA( I , J ,1) + BETA(P-1, Q ,1) )   &
          & ) / 2.0
        END IF
      !END I = 1
      DO I = 2 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE .AND. ACTIVITY(I-1, J ) == BEACTIVE ) THEN
          D1( I , J ) = D1( I , J ) + DT * PHI * BPAR * ( BETA( I , J ,1) + BETA(I-1, J ,1) ) / 2.0
        END IF
      END DO
    END DO

    DO J = 1 , NJ
      DO I = 1 , NI-1
        IF( ACTIVITY( I , J ) == BEACTIVE .AND. ACTIVITY(I+1, J ) == BEACTIVE ) THEN
          D1( I , J ) = D1( I , J ) - DT * PHI * BPAR * ( BETA(I+1, J ,1) + BETA( I , J ,1) ) / 2.0
        END IF
      END DO
      I = NI
        P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1
        IF( ACTIVITY( I , J ) == BEACTIVE .AND. ACTIVITY(P+1, Q ) == BEACTIVE ) THEN
          D1( I , J ) = D1( I , J )   &
          - DT * PHI * BPAR *   &
            ( ( ALFA(P+1, Q ,1) + ALFA( I , J ,1) ) * ( ZS(P+1, Q )               )   &
            + ( BETA(P+1, Q ,1) + BETA( I , J ,1) )   &
          & ) / 2.0
        END IF
      !END I = NI
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

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ  ,1:2) :: ALFA

    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(CARD_KIND)  ,2:NJ      ) :: A2
    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ      ) :: B2
    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ-1    ) :: C2

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
      DO I = 1 , NI
        !BLOCK
          B2( I , J ) = MW( I , J )
        !END BLOCK
      END DO
    END DO

    DO J = 2 , NJ
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE .AND. ACTIVITY( I ,J-1) == BEACTIVE ) THEN
          A2( I , J ) = DT * PHI * BPAR * ( ALFA( I , J ,2) + ALFA( I ,J-1,2) ) / 2.0
          B2( I , J ) = B2( I , J ) - A2( I , J )
        ELSE
          A2( I , J ) = 0.0
        END IF
      END DO
    END DO

    DO J = 1 , NJ-1
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE .AND. ACTIVITY( I ,J+1) == BEACTIVE ) THEN
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

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND)  ,1:NJ      ) :: ZT
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ  ,1:2) :: BETA

    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ      ) :: D2

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
      DO I = 1 , NI
        !BLOCK
          D2( I , J ) = MW( I , J ) * ZT( I , J )
        !END BLOCK
      END DO
    END DO

    DO J = 1 , NJ
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
          D2( I , J ) = D2( I , J )   &
          + DT * PHI * BPAR *   &
            ( MV( I , J ) * HV( I , J ) * VA( I , J )   &
            - MV( I ,J-1) * HV( I ,J-1) * VA( I ,J-1)   &
          & )
        END IF
      END DO
    END DO

    DO J = 2 , NJ
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE .AND. ACTIVITY( I ,J-1) == BEACTIVE ) THEN
          D2( I , J ) = D2( I , J ) + DT * PHI * BPAR * ( BETA( I , J ,2) + BETA( I ,J-1,2) ) / 2.0
        END IF
      END DO
    END DO

    DO J = 1 , NJ-1
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE .AND. ACTIVITY( I ,J+1) == BEACTIVE ) THEN
          D2( I , J ) = D2( I , J ) - DT * PHI * BPAR * ( BETA( I ,J+1,2) + BETA( I , J ,2) ) / 2.0
        END IF
      END DO
    END DO

  END SUBROUTINE MR_CALC_D2

  END MODULE MR_MOD_CALC_A_B_C_D