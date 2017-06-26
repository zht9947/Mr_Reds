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

    PUBLIC :: MR_AVERAGE_UV , MR_AVERAGE_VERT_UV
    PUBLIC :: MR_AVERAGE_SS , MR_AVERAGE_VERT_SS

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
  SUBROUTINE MR_AVERAGE_QUV( NI , NJ , QUV , QUV_AVERAGE_PLAN , QUV_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: QUV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND)         ) :: QUV_AVERAGE_XSEC
    REAL   (FDRD_KIND) , INTENT(OUT)                                           :: QUV_AVERAGE_PLAN

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)         ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                         :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J

  ! SUM ALL NODES ALONG EACH CROSS-SECTION
    NUMER_XSEC = 0.0
    DENOR_XSEC = 0.0
    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
          NUMER_XSEC( I ) = NUMER_XSEC( I ) + SQRT( GUV( I , J ,2,2) ) * SQRT( GUV( I , J ,1,1) ) * QUV( I , J ,1)
          DENOR_XSEC( I ) = DENOR_XSEC( I ) + SQRT( GUV( I , J ,2,2) )
        END IF
      END DO
    END DO
   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI
      QUV_AVERAGE_XSEC( I ) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
    END DO

  ! SUM ALL CROSS-SECTIONS
    NUMER_PLAN = 0.0
    DENOR_PLAN = 0.0
   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI
      NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
      DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
    END DO
    QUV_AVERAGE_PLAN = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

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
  SUBROUTINE MR_AVERAGE_UV( NI , NJ , UV , UV_AVERAGE_PLAN , UV_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND)         ) :: UV_AVERAGE_XSEC
    REAL   (FDRD_KIND) , INTENT(OUT)                                           :: UV_AVERAGE_PLAN

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)         ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                         :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J

  ! SUM ALL NODES ALONG EACH CROSS-SECTION
    NUMER_XSEC = 0.0
    DENOR_XSEC = 0.0
    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
          NUMER_XSEC( I ) = NUMER_XSEC( I ) + MW( I , J ) * SQRT( GUV( I , J ,1,1) ) * UV( I , J ,1)
          DENOR_XSEC( I ) = DENOR_XSEC( I ) + MW( I , J )
        END IF
      END DO
    END DO
   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI
      UV_AVERAGE_XSEC( I ) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
    END DO

  ! SUM ALL CROSS-SECTIONS
    NUMER_PLAN = 0.0
    DENOR_PLAN = 0.0
   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI
      NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
      DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
    END DO
    UV_AVERAGE_PLAN = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

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
  SUBROUTINE MR_AVERAGE_VERT_UV

    IMPLICIT NONE

  END SUBROUTINE MR_AVERAGE_VERT_UV

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
  SUBROUTINE MR_AVERAGE_SS( NI , NJ , SS , SS_AVERAGE_PLAN , SS_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: SS
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND)     ) :: SS_AVERAGE_XSEC
    REAL   (FDRD_KIND) , INTENT(OUT)                                       :: SS_AVERAGE_PLAN

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)     ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                     :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J

  ! SUM ALL NODES ALONG EACH CROSS-SECTION
    NUMER_XSEC = 0.0
    DENOR_XSEC = 0.0
    DO J = 1 , NJ
     !!DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
          NUMER_XSEC( I ) = NUMER_XSEC( I ) + MW( I , J ) * SS( I , J )
          DENOR_XSEC( I ) = DENOR_XSEC( I ) + MW( I , J )
        END IF
      END DO
    END DO
   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI
      SS_AVERAGE_XSEC( I ) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
    END DO

  ! SUM ALL CROSS-SECTIONS
    NUMER_PLAN = 0.0
    DENOR_PLAN = 0.0
   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI
      NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
      DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
    END DO
    SS_AVERAGE_PLAN = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

  END SUBROUTINE MR_AVERAGE_SS

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
  SUBROUTINE MR_AVERAGE_VERT_SS

    IMPLICIT NONE

  END SUBROUTINE MR_AVERAGE_VERT_SS

  END MODULE MR_MOD_AVERAGE