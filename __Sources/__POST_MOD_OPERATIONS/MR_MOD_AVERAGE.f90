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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_AVERAGE

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS, ONLY : DSIGMA
    USE MR_DEF_ACTIVITY

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_AVERAGE_UV_BY_AREA , MR_AVERAGE_UV_BY_AREA_3D
    PUBLIC :: MR_AVERAGE_SS_BY_AREA , MR_AVERAGE_SS_BY_AREA_3D

    PUBLIC :: MR_AVERAGE_UV_BY_LINE , MR_AVERAGE_UV_BY_LINE_3D
    PUBLIC :: MR_AVERAGE_SS_BY_LINE , MR_AVERAGE_SS_BY_LINE_3D

    INTERFACE MR_AVERAGE_UV_BY_AREA
      MODULE PROCEDURE MR_AVERAGE_UV_BY_AREA_KIND4
      MODULE PROCEDURE MR_AVERAGE_UV_BY_AREA_KIND8
    END INTERFACE

    INTERFACE MR_AVERAGE_UV_BY_AREA_3D
      MODULE PROCEDURE MR_AVERAGE_UV_BY_AREA_3D_KIND4
      MODULE PROCEDURE MR_AVERAGE_UV_BY_AREA_3D_KIND8
    END INTERFACE

    INTERFACE MR_AVERAGE_SS_BY_AREA
      MODULE PROCEDURE MR_AVERAGE_SS_BY_AREA_KIND4
      MODULE PROCEDURE MR_AVERAGE_SS_BY_AREA_KIND8
    END INTERFACE

    INTERFACE MR_AVERAGE_SS_BY_AREA_3D
      MODULE PROCEDURE MR_AVERAGE_SS_BY_AREA_3D_KIND4
      MODULE PROCEDURE MR_AVERAGE_SS_BY_AREA_3D_KIND8
    END INTERFACE

    INTERFACE MR_AVERAGE_UV_BY_LINE
      MODULE PROCEDURE MR_AVERAGE_UV_BY_LINE_KIND4
      MODULE PROCEDURE MR_AVERAGE_UV_BY_LINE_KIND8
    END INTERFACE

    INTERFACE MR_AVERAGE_UV_BY_LINE_3D
      MODULE PROCEDURE MR_AVERAGE_UV_BY_LINE_3D_KIND4
      MODULE PROCEDURE MR_AVERAGE_UV_BY_LINE_3D_KIND8
    END INTERFACE

    INTERFACE MR_AVERAGE_SS_BY_LINE
      MODULE PROCEDURE MR_AVERAGE_SS_BY_LINE_KIND4
      MODULE PROCEDURE MR_AVERAGE_SS_BY_LINE_KIND8
    END INTERFACE

    INTERFACE MR_AVERAGE_SS_BY_LINE_3D
      MODULE PROCEDURE MR_AVERAGE_SS_BY_LINE_3D_KIND4
      MODULE PROCEDURE MR_AVERAGE_SS_BY_LINE_3D_KIND8
    END INTERFACE


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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_AVERAGE_UV_BY_AREA_KIND4( NI , NJ , UV , UV_AVERAGE_PLAN , UV_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV
    REAL   (4)         , INTENT(OUT) , DIMENSION(                         1:2) :: UV_AVERAGE_PLAN
    REAL   (4)         , INTENT(OUT) , DIMENSION(1:NI1(NI,4),             1:2) , OPTIONAL :: UV_AVERAGE_XSEC

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)         ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                         :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

    ! SUM ALL NODES ALONG EACH CROSS SECTION
      NUMER_XSEC = 0.0
      DENOR_XSEC = 0.0
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            NUMER_XSEC( I ) = NUMER_XSEC( I ) + MW( I , J ) * UV( I , J ,DIM) * SQRT( GUV( I , J ,DIM,DIM) )
            DENOR_XSEC( I ) = DENOR_XSEC( I ) + MW( I , J )
          END IF
        END DO
      END DO
      IF( PRESENT(UV_AVERAGE_XSEC) ) THEN
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          UV_AVERAGE_XSEC( I ,DIM) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
        END DO
      END IF

    ! SUM ALL CROSS SECTIONS
      NUMER_PLAN = 0.0
      DENOR_PLAN = 0.0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
        DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
      END DO
      UV_AVERAGE_PLAN(DIM) = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

    END DO

  END SUBROUTINE MR_AVERAGE_UV_BY_AREA_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_AVERAGE_UV_BY_AREA_KIND8( NI , NJ , UV , UV_AVERAGE_PLAN , UV_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV
    REAL   (8)         , INTENT(OUT) , DIMENSION(                         1:2) :: UV_AVERAGE_PLAN
    REAL   (8)         , INTENT(OUT) , DIMENSION(1:NI1(NI,8),             1:2) , OPTIONAL :: UV_AVERAGE_XSEC

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)         ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                         :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

    ! SUM ALL NODES ALONG EACH CROSS SECTION
      NUMER_XSEC = 0.0
      DENOR_XSEC = 0.0
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            NUMER_XSEC( I ) = NUMER_XSEC( I ) + MW( I , J ) * UV( I , J ,DIM) * SQRT( GUV( I , J ,DIM,DIM) )
            DENOR_XSEC( I ) = DENOR_XSEC( I ) + MW( I , J )
          END IF
        END DO
      END DO
      IF( PRESENT(UV_AVERAGE_XSEC) ) THEN
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          UV_AVERAGE_XSEC( I ,DIM) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
        END DO
      END IF

    ! SUM ALL CROSS SECTIONS
      NUMER_PLAN = 0.0
      DENOR_PLAN = 0.0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
        DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
      END DO
      UV_AVERAGE_PLAN(DIM) = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

    END DO

  END SUBROUTINE MR_AVERAGE_UV_BY_AREA_KIND8

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_AVERAGE_UV_BY_AREA_3D_KIND4( NI , NJ , NK , H , UV , UV_AVERAGE_PLAN , UV_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ         ) :: H

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2,1:NK) :: UV
    REAL   (4)         , INTENT(OUT) , DIMENSION(                         1:2     ) :: UV_AVERAGE_PLAN
    REAL   (4)         , INTENT(OUT) , DIMENSION(1:NI1(NI,4),             1:2     ) , OPTIONAL :: UV_AVERAGE_XSEC

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)              ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                              :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    INTEGER(KKID_KIND) :: K

    DO DIM = 1 , 2

    ! SUM ALL NODES ALONG EACH CROSS SECTION
      NUMER_XSEC = 0.0
      DENOR_XSEC = 0.0
      DO K = 1 , NK
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
              NUMER_XSEC( I ) = NUMER_XSEC( I ) + MW( I , J ) * H( I , J ) * DSIGMA *   &
              & UV( I , J ,DIM, K ) * SQRT( GUV( I , J ,DIM,DIM) )
              DENOR_XSEC( I ) = DENOR_XSEC( I ) + MW( I , J ) * H( I , J ) * DSIGMA
            END IF
          END DO
        END DO
      END DO
      IF( PRESENT(UV_AVERAGE_XSEC) ) THEN
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          UV_AVERAGE_XSEC( I ,DIM) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
        END DO
      END IF

    ! SUM ALL CROSS SECTIONS
      NUMER_PLAN = 0.0
      DENOR_PLAN = 0.0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
        DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
      END DO
      UV_AVERAGE_PLAN(DIM) = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

    END DO

  END SUBROUTINE MR_AVERAGE_UV_BY_AREA_3D_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_AVERAGE_UV_BY_AREA_3D_KIND8( NI , NJ , NK , H , UV , UV_AVERAGE_PLAN , UV_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ         ) :: H

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2,1:NK) :: UV
    REAL   (8)         , INTENT(OUT) , DIMENSION(                         1:2     ) :: UV_AVERAGE_PLAN
    REAL   (8)         , INTENT(OUT) , DIMENSION(1:NI1(NI,8),             1:2     ) , OPTIONAL :: UV_AVERAGE_XSEC

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)              ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                              :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    INTEGER(KKID_KIND) :: K

    DO DIM = 1 , 2

    ! SUM ALL NODES ALONG EACH CROSS SECTION
      NUMER_XSEC = 0.0
      DENOR_XSEC = 0.0
      DO K = 1 , NK
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
              NUMER_XSEC( I ) = NUMER_XSEC( I ) + MW( I , J ) * H( I , J ) * DSIGMA *   &
              & UV( I , J ,DIM, K ) * SQRT( GUV( I , J ,DIM,DIM) )
              DENOR_XSEC( I ) = DENOR_XSEC( I ) + MW( I , J ) * H( I , J ) * DSIGMA
            END IF
          END DO
        END DO
      END DO
      IF( PRESENT(UV_AVERAGE_XSEC) ) THEN
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          UV_AVERAGE_XSEC( I ,DIM) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
        END DO
      END IF

    ! SUM ALL CROSS SECTIONS
      NUMER_PLAN = 0.0
      DENOR_PLAN = 0.0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
        DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
      END DO
      UV_AVERAGE_PLAN(DIM) = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

    END DO

  END SUBROUTINE MR_AVERAGE_UV_BY_AREA_3D_KIND8

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_AVERAGE_SS_BY_AREA_KIND4( NI , NJ , SS , SS_AVERAGE_PLAN , SS_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: SS
    REAL   (4)         , INTENT(OUT)                                       :: SS_AVERAGE_PLAN
    REAL   (4)         , INTENT(OUT) , DIMENSION(1:NI1(NI,8)             ) , OPTIONAL :: SS_AVERAGE_XSEC

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)     ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                     :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J

  ! SUM ALL NODES ALONG EACH CROSS SECTION
    NUMER_XSEC = 0.0
    DENOR_XSEC = 0.0
    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
          NUMER_XSEC( I ) = NUMER_XSEC( I ) + MW( I , J ) * SS( I , J )
          DENOR_XSEC( I ) = DENOR_XSEC( I ) + MW( I , J )
        END IF
      END DO
    END DO
    IF( PRESENT(SS_AVERAGE_XSEC) ) THEN
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        SS_AVERAGE_XSEC( I ) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
      END DO
    END IF

  ! SUM ALL CROSS SECTIONS
    NUMER_PLAN = 0.0
    DENOR_PLAN = 0.0
   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI
      NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
      DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
    END DO
    SS_AVERAGE_PLAN = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

  END SUBROUTINE MR_AVERAGE_SS_BY_AREA_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_AVERAGE_SS_BY_AREA_KIND8( NI , NJ , SS , SS_AVERAGE_PLAN , SS_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: SS
    REAL   (8)         , INTENT(OUT)                                       :: SS_AVERAGE_PLAN
    REAL   (8)         , INTENT(OUT) , DIMENSION(1:NI1(NI,8)             ) , OPTIONAL :: SS_AVERAGE_XSEC

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)     ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                     :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J

  ! SUM ALL NODES ALONG EACH CROSS SECTION
    NUMER_XSEC = 0.0
    DENOR_XSEC = 0.0
    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
          NUMER_XSEC( I ) = NUMER_XSEC( I ) + MW( I , J ) * SS( I , J )
          DENOR_XSEC( I ) = DENOR_XSEC( I ) + MW( I , J )
        END IF
      END DO
    END DO
    IF( PRESENT(SS_AVERAGE_XSEC) ) THEN
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        SS_AVERAGE_XSEC( I ) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
      END DO
    END IF

  ! SUM ALL CROSS SECTIONS
    NUMER_PLAN = 0.0
    DENOR_PLAN = 0.0
   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI
      NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
      DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
    END DO
    SS_AVERAGE_PLAN = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

  END SUBROUTINE MR_AVERAGE_SS_BY_AREA_KIND8

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_AVERAGE_SS_BY_AREA_3D_KIND4( NI , NJ , NK , H , SS , SS_AVERAGE_PLAN , SS_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ     ) :: H

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) :: SS
    REAL   (4)         , INTENT(OUT)                                            :: SS_AVERAGE_PLAN
    REAL   (4)         , INTENT(OUT) , DIMENSION(1:NI1(NI,4)                  ) , OPTIONAL :: SS_AVERAGE_XSEC

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)          ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                          :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

  ! SUM ALL NODES ALONG EACH CROSS SECTION
    NUMER_XSEC = 0.0
    DENOR_XSEC = 0.0
    DO K = 1 , NK
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            NUMER_XSEC( I ) = NUMER_XSEC( I ) + MW( I , J ) * H( I , J ) * DSIGMA * SS( I , J , K )
            DENOR_XSEC( I ) = DENOR_XSEC( I ) + MW( I , J ) * H( I , J ) * DSIGMA
          END IF
        END DO
      END DO
    END DO
    IF( PRESENT(SS_AVERAGE_XSEC) ) THEN
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        SS_AVERAGE_XSEC( I ) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
      END DO
    END IF

  ! SUM ALL CROSS SECTIONS
    NUMER_PLAN = 0.0
    DENOR_PLAN = 0.0
   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI
      NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
      DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
    END DO
    SS_AVERAGE_PLAN = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

  END SUBROUTINE MR_AVERAGE_SS_BY_AREA_3D_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_AVERAGE_SS_BY_AREA_3D_KIND8( NI , NJ , NK , H , SS , SS_AVERAGE_PLAN , SS_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ     ) :: H

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) :: SS
    REAL   (8)         , INTENT(OUT)                                            :: SS_AVERAGE_PLAN
    REAL   (8)         , INTENT(OUT) , DIMENSION(1:NI1(NI,8)                  ) , OPTIONAL :: SS_AVERAGE_XSEC

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)          ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                          :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

  ! SUM ALL NODES ALONG EACH CROSS SECTION
    NUMER_XSEC = 0.0
    DENOR_XSEC = 0.0
    DO K = 1 , NK
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            NUMER_XSEC( I ) = NUMER_XSEC( I ) + MW( I , J ) * H( I , J ) * DSIGMA * SS( I , J , K )
            DENOR_XSEC( I ) = DENOR_XSEC( I ) + MW( I , J ) * H( I , J ) * DSIGMA
          END IF
        END DO
      END DO
    END DO
    IF( PRESENT(SS_AVERAGE_XSEC) ) THEN
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        SS_AVERAGE_XSEC( I ) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
      END DO
    END IF

  ! SUM ALL CROSS SECTIONS
    NUMER_PLAN = 0.0
    DENOR_PLAN = 0.0
   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI
      NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
      DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
    END DO
    SS_AVERAGE_PLAN = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

  END SUBROUTINE MR_AVERAGE_SS_BY_AREA_3D_KIND8

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_AVERAGE_UV_BY_LINE_KIND4( NI , NJ , UV , UV_AVERAGE_PLAN , UV_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV
    REAL   (4)         , INTENT(OUT) , DIMENSION(                         1:2) :: UV_AVERAGE_PLAN
    REAL   (4)         , INTENT(OUT) , DIMENSION(1:NI1(NI,4),             1:2) , OPTIONAL :: UV_AVERAGE_XSEC

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)         ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                         :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

    ! SUM ALL NODES ALONG EACH CROSS SECTION
      NUMER_XSEC = 0.0
      DENOR_XSEC = 0.0
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            NUMER_XSEC( I ) = NUMER_XSEC( I ) + SQRT( GUV( I , J ,2,2) ) * UV( I , J ,DIM) * SQRT( GUV( I , J ,DIM,DIM) )
            DENOR_XSEC( I ) = DENOR_XSEC( I ) + SQRT( GUV( I , J ,2,2) )
          END IF
        END DO
      END DO
      IF( PRESENT(UV_AVERAGE_XSEC) ) THEN
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          UV_AVERAGE_XSEC( I ,DIM) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
        END DO
      END IF

    ! SUM ALL CROSS SECTIONS
      NUMER_PLAN = 0.0
      DENOR_PLAN = 0.0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
        DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
      END DO
      UV_AVERAGE_PLAN(DIM) = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

    END DO

  END SUBROUTINE MR_AVERAGE_UV_BY_LINE_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_AVERAGE_UV_BY_LINE_KIND8( NI , NJ , UV , UV_AVERAGE_PLAN , UV_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV
    REAL   (8)         , INTENT(OUT) , DIMENSION(                         1:2) :: UV_AVERAGE_PLAN
    REAL   (8)         , INTENT(OUT) , DIMENSION(1:NI1(NI,8),             1:2) , OPTIONAL :: UV_AVERAGE_XSEC

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)         ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                         :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

    ! SUM ALL NODES ALONG EACH CROSS SECTION
      NUMER_XSEC = 0.0
      DENOR_XSEC = 0.0
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            NUMER_XSEC( I ) = NUMER_XSEC( I ) + SQRT( GUV( I , J ,2,2) ) * UV( I , J ,DIM) * SQRT( GUV( I , J ,DIM,DIM) )
            DENOR_XSEC( I ) = DENOR_XSEC( I ) + SQRT( GUV( I , J ,2,2) )
          END IF
        END DO
      END DO
      IF( PRESENT(UV_AVERAGE_XSEC) ) THEN
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          UV_AVERAGE_XSEC( I ,DIM) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
        END DO
      END IF

    ! SUM ALL CROSS SECTIONS
      NUMER_PLAN = 0.0
      DENOR_PLAN = 0.0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
        DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
      END DO
      UV_AVERAGE_PLAN(DIM) = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

    END DO

  END SUBROUTINE MR_AVERAGE_UV_BY_LINE_KIND8

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_AVERAGE_UV_BY_LINE_3D_KIND4( NI , NJ , NK , H , UV , UV_AVERAGE_PLAN , UV_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ         ) :: H

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2,1:NK) :: UV
    REAL   (4)         , INTENT(OUT) , DIMENSION(                         1:2     ) :: UV_AVERAGE_PLAN
    REAL   (4)         , INTENT(OUT) , DIMENSION(1:NI1(NI,4),             1:2     ) , OPTIONAL :: UV_AVERAGE_XSEC

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)              ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                              :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    INTEGER(KKID_KIND) :: K

    DO DIM = 1 , 2

    ! SUM ALL NODES ALONG EACH CROSS SECTION
      NUMER_XSEC = 0.0
      DENOR_XSEC = 0.0
      DO K = 1 , NK
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
              NUMER_XSEC( I ) = NUMER_XSEC( I ) + SQRT( GUV( I , J ,2,2) ) * H( I , J ) * DSIGMA *   &
              & UV( I , J ,DIM, K ) * SQRT( GUV( I , J ,DIM,DIM) )
              DENOR_XSEC( I ) = DENOR_XSEC( I ) + SQRT( GUV( I , J ,2,2) ) * H( I , J ) * DSIGMA
            END IF
          END DO
        END DO
      END DO
      IF( PRESENT(UV_AVERAGE_XSEC) ) THEN
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          UV_AVERAGE_XSEC( I ,DIM) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
        END DO
      END IF

    ! SUM ALL CROSS SECTIONS
      NUMER_PLAN = 0.0
      DENOR_PLAN = 0.0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
        DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
      END DO
      UV_AVERAGE_PLAN(DIM) = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

    END DO

  END SUBROUTINE MR_AVERAGE_UV_BY_LINE_3D_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_AVERAGE_UV_BY_LINE_3D_KIND8( NI , NJ , NK , H , UV , UV_AVERAGE_PLAN , UV_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ         ) :: H

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2,1:NK) :: UV
    REAL   (8)         , INTENT(OUT) , DIMENSION(                         1:2     ) :: UV_AVERAGE_PLAN
    REAL   (8)         , INTENT(OUT) , DIMENSION(1:NI1(NI,8),             1:2     ) , OPTIONAL :: UV_AVERAGE_XSEC

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)              ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                              :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    INTEGER(KKID_KIND) :: K

    DO DIM = 1 , 2

    ! SUM ALL NODES ALONG EACH CROSS SECTION
      NUMER_XSEC = 0.0
      DENOR_XSEC = 0.0
      DO K = 1 , NK
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
              NUMER_XSEC( I ) = NUMER_XSEC( I ) + SQRT( GUV( I , J ,2,2) ) * H( I , J ) * DSIGMA *   &
              & UV( I , J ,DIM, K ) * SQRT( GUV( I , J ,DIM,DIM) )
              DENOR_XSEC( I ) = DENOR_XSEC( I ) + SQRT( GUV( I , J ,2,2) ) * H( I , J ) * DSIGMA
            END IF
          END DO
        END DO
      END DO
      IF( PRESENT(UV_AVERAGE_XSEC) ) THEN
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          UV_AVERAGE_XSEC( I ,DIM) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
        END DO
      END IF

    ! SUM ALL CROSS SECTIONS
      NUMER_PLAN = 0.0
      DENOR_PLAN = 0.0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
        DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
      END DO
      UV_AVERAGE_PLAN(DIM) = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

    END DO

  END SUBROUTINE MR_AVERAGE_UV_BY_LINE_3D_KIND8

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_AVERAGE_SS_BY_LINE_KIND4( NI , NJ , SS , SS_AVERAGE_PLAN , SS_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: SS
    REAL   (4)         , INTENT(OUT)                                       :: SS_AVERAGE_PLAN
    REAL   (4)         , INTENT(OUT) , DIMENSION(1:NI1(NI,8)             ) , OPTIONAL :: SS_AVERAGE_XSEC

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)     ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                     :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J

  ! SUM ALL NODES ALONG EACH CROSS SECTION
    NUMER_XSEC = 0.0
    DENOR_XSEC = 0.0
    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
          NUMER_XSEC( I ) = NUMER_XSEC( I ) + SQRT( GUV( I , J ,2,2) ) * SS( I , J )
          DENOR_XSEC( I ) = DENOR_XSEC( I ) + SQRT( GUV( I , J ,2,2) )
        END IF
      END DO
    END DO
    IF( PRESENT(SS_AVERAGE_XSEC) ) THEN
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        SS_AVERAGE_XSEC( I ) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
      END DO
    END IF

  ! SUM ALL CROSS SECTIONS
    NUMER_PLAN = 0.0
    DENOR_PLAN = 0.0
   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI
      NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
      DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
    END DO
    SS_AVERAGE_PLAN = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

  END SUBROUTINE MR_AVERAGE_SS_BY_LINE_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_AVERAGE_SS_BY_LINE_KIND8( NI , NJ , SS , SS_AVERAGE_PLAN , SS_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: SS
    REAL   (8)         , INTENT(OUT)                                       :: SS_AVERAGE_PLAN
    REAL   (8)         , INTENT(OUT) , DIMENSION(1:NI1(NI,8)             ) , OPTIONAL :: SS_AVERAGE_XSEC

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)     ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                     :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J

  ! SUM ALL NODES ALONG EACH CROSS SECTION
    NUMER_XSEC = 0.0
    DENOR_XSEC = 0.0
    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
          NUMER_XSEC( I ) = NUMER_XSEC( I ) + SQRT( GUV( I , J ,2,2) ) * SS( I , J )
          DENOR_XSEC( I ) = DENOR_XSEC( I ) + SQRT( GUV( I , J ,2,2) )
        END IF
      END DO
    END DO
    IF( PRESENT(SS_AVERAGE_XSEC) ) THEN
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        SS_AVERAGE_XSEC( I ) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
      END DO
    END IF

  ! SUM ALL CROSS SECTIONS
    NUMER_PLAN = 0.0
    DENOR_PLAN = 0.0
   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI
      NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
      DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
    END DO
    SS_AVERAGE_PLAN = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

  END SUBROUTINE MR_AVERAGE_SS_BY_LINE_KIND8

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_AVERAGE_SS_BY_LINE_3D_KIND4( NI , NJ , NK , H , SS , SS_AVERAGE_PLAN , SS_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ     ) :: H

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) :: SS
    REAL   (4)         , INTENT(OUT)                                            :: SS_AVERAGE_PLAN
    REAL   (4)         , INTENT(OUT) , DIMENSION(1:NI1(NI,4)                  ) , OPTIONAL :: SS_AVERAGE_XSEC

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)          ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                          :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

  ! SUM ALL NODES ALONG EACH CROSS SECTION
    NUMER_XSEC = 0.0
    DENOR_XSEC = 0.0
    DO K = 1 , NK
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            NUMER_XSEC( I ) = NUMER_XSEC( I ) + SQRT( GUV( I , J ,2,2) ) * H( I , J ) * DSIGMA * SS( I , J , K )
            DENOR_XSEC( I ) = DENOR_XSEC( I ) + SQRT( GUV( I , J ,2,2) ) * H( I , J ) * DSIGMA
          END IF
        END DO
      END DO
    END DO
    IF( PRESENT(SS_AVERAGE_XSEC) ) THEN
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        SS_AVERAGE_XSEC( I ) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
      END DO
    END IF

  ! SUM ALL CROSS SECTIONS
    NUMER_PLAN = 0.0
    DENOR_PLAN = 0.0
   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI
      NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
      DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
    END DO
    SS_AVERAGE_PLAN = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

  END SUBROUTINE MR_AVERAGE_SS_BY_LINE_3D_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_AVERAGE_SS_BY_LINE_3D_KIND8( NI , NJ , NK , H , SS , SS_AVERAGE_PLAN , SS_AVERAGE_XSEC )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ     ) :: H

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) :: SS
    REAL   (8)         , INTENT(OUT)                                            :: SS_AVERAGE_PLAN
    REAL   (8)         , INTENT(OUT) , DIMENSION(1:NI1(NI,8)                  ) , OPTIONAL :: SS_AVERAGE_XSEC

    REAL   (CARD_KIND) ,               DIMENSION(1:NI1(NI,CARD_KIND)          ) :: NUMER_XSEC , DENOR_XSEC
    REAL   (CARD_KIND)                                                          :: NUMER_PLAN , DENOR_PLAN

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

  ! SUM ALL NODES ALONG EACH CROSS SECTION
    NUMER_XSEC = 0.0
    DENOR_XSEC = 0.0
    DO K = 1 , NK
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            NUMER_XSEC( I ) = NUMER_XSEC( I ) + SQRT( GUV( I , J ,2,2) ) * H( I , J ) * DSIGMA * SS( I , J , K )
            DENOR_XSEC( I ) = DENOR_XSEC( I ) + SQRT( GUV( I , J ,2,2) ) * H( I , J ) * DSIGMA
          END IF
        END DO
      END DO
    END DO
    IF( PRESENT(SS_AVERAGE_XSEC) ) THEN
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        SS_AVERAGE_XSEC( I ) = NUMER_XSEC( I ) / MAX( DENOR_XSEC( I ) , EPSILON(DENOR_XSEC) )
      END DO
    END IF

  ! SUM ALL CROSS SECTIONS
    NUMER_PLAN = 0.0
    DENOR_PLAN = 0.0
   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI
      NUMER_PLAN = NUMER_PLAN + NUMER_XSEC( I )
      DENOR_PLAN = DENOR_PLAN + DENOR_XSEC( I )
    END DO
    SS_AVERAGE_PLAN = NUMER_PLAN / MAX( DENOR_PLAN , EPSILON(DENOR_PLAN) )

  END SUBROUTINE MR_AVERAGE_SS_BY_LINE_3D_KIND8

  END MODULE MR_MOD_AVERAGE