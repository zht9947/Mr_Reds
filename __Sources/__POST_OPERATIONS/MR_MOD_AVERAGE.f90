#INCLUDE 'MR_H_ALIGN_PADDING.H'
!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MOD_INTERP_XY
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
  MODULE MR_MOD_AVERAGE

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_ACTIVITY

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_AVERAGE_UV
    PUBLIC :: MR_AVERAGE_SS

    PUBLIC :: MR_AVERAGE_QUV

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
  SUBROUTINE MR_AVERAGE_QUV( NI , NJ , QUV , QUV_AVERAGE )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2) :: QUV
    REAL   (FDRD_KIND) , INTENT(OUT) :: QUV_AVERAGE(1:2)

    REAL   (CARD_KIND) :: NUMER
    REAL   (CARD_KIND) :: DENOR

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      NUMER = 0.0
      DENOR = EPSILON(DENOR)
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            NUMER = NUMER + SQRT( GUV( I , J ,1,1) ) * SQRT( GUV( I , J ,2,2) ) * QUV( I , J ,DIM)
            DENOR = DENOR + SQRT( GUV( I , J ,MOD(DIM,2)+1,MOD(DIM,2)+1) )
          END IF
        END DO
      END DO

      QUV_AVERAGE(DIM) = NUMER / DENOR

    END DO

  END SUBROUTINE MR_AVERAGE_QUV

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
  SUBROUTINE MR_AVERAGE_UV( NI , NJ , UV , UV_AVERAGE )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(OUT) :: UV_AVERAGE(1:2)

    REAL   (CARD_KIND) :: NUMER
    REAL   (CARD_KIND) :: DENOR

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      NUMER = 0.0
      DENOR = EPSILON(DENOR)
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            NUMER = NUMER + MW( I , J ) * SQRT( GUV( I , J ,DIM,DIM) ) * UV( I , J ,DIM)
            DENOR = DENOR + MW( I , J )
          END IF
        END DO
      END DO

      UV_AVERAGE(DIM) = NUMER / DENOR

    END DO

  END SUBROUTINE MR_AVERAGE_UV

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
  SUBROUTINE MR_AVERAGE_SS( NI , NJ , SS , SS_AVERAGE )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ) :: SS
    REAL   (FDRD_KIND) , INTENT(OUT) :: SS_AVERAGE

    REAL   (CARD_KIND) :: NUMER
    REAL   (CARD_KIND) :: DENOR

    INTEGER(IJID_KIND) :: I , J

    NUMER = 0.0
    DENOR = EPSILON(DENOR)
    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
          NUMER = NUMER + MW( I , J ) * SS( I , J )
          DENOR = DENOR + MW( I , J )
        END IF
      END DO
    END DO

    SS_AVERAGE = NUMER / DENOR

  END SUBROUTINE MR_AVERAGE_SS

  END MODULE MR_MOD_AVERAGE