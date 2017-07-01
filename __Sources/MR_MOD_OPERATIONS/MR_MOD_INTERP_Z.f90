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
  MODULE MR_MOD_INTERP_Z

    USE MR_KINDS

    USE MR_DEF_ACTIVITY

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_INTERP_Z_UV_W
    PUBLIC :: MR_INV_INTERP_Z_UV_FROM_UVW

    PUBLIC :: MR_INTERP_Z_SS_W
    PUBLIC :: MR_INV_INTERP_Z_SS_FROM_SW

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
  SUBROUTINE MR_INTERP_Z_UV_W( NI , NJ , NK , UV , UVW , UV0 , UVN )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2,1:NK) :: UV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2,0:NK) :: UVW

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2     ) , OPTIONAL :: UV0 , UVN

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    INTEGER(KKID_KIND) :: K

    K = 0
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
           !DIR$ FORCEINLINE
            CALL MR_INTERP_Z_UV_W_II_JJ_K0
          END DO
        END DO
      END DO
    !END K = 0

    DO K = 1 , NK-1
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
           !DIR$ FORCEINLINE
            CALL MR_INTERP_Z_UV_W_II_JJ_KK
          END DO
        END DO
      END DO
    END DO

    K = NK
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
           !DIR$ FORCEINLINE
            CALL MR_INTERP_Z_UV_W_II_JJ_KN
          END DO
        END DO
      END DO
    !END K = NK

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
  SUBROUTINE MR_INTERP_Z_UV_W_II_JJ_K0

    IMPLICIT NONE

    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      IF( PRESENT(UV0) ) THEN
        UVW( I , J ,DIM, K ) = UV0( I , J ,DIM)
      ELSE
        UVW( I , J ,DIM, K ) = UV( I , J ,DIM,K+1)
      END IF
    ELSE
      !BLOCK
        UVW( I , J ,DIM, K ) = 0.0
      !END BLOCK
    END IF

  END SUBROUTINE MR_INTERP_Z_UV_W_II_JJ_K0

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
  SUBROUTINE MR_INTERP_Z_UV_W_II_JJ_KK

    IMPLICIT NONE

    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      !BLOCK
        UVW( I , J ,DIM, K ) = 0.5 * ( UV( I , J ,DIM,K+1) + UV( I , J ,DIM, K ) )
      !END BLOCK
    ELSE
      !BLOCK
        UVW( I , J ,DIM, K ) = 0.0
      !END BLOCK
    END IF

  END SUBROUTINE MR_INTERP_Z_UV_W_II_JJ_KK

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
  SUBROUTINE MR_INTERP_Z_UV_W_II_JJ_KN

    IMPLICIT NONE

    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      IF( PRESENT(UVN) ) THEN
        UVW( I , J ,DIM, K ) = UVN( I , J ,DIM)
      ELSE
        UVW( I , J ,DIM, K ) = UV( I , J ,DIM, K )
      END IF
    ELSE
      !BLOCK
        UVW( I , J ,DIM, K ) = 0.0
      !END BLOCK
    END IF

  END SUBROUTINE MR_INTERP_Z_UV_W_II_JJ_KN

  END SUBROUTINE MR_INTERP_Z_UV_W

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
  SUBROUTINE MR_INV_INTERP_Z_UV_FROM_UVW( NI , NJ , NK , UVW , UV )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2,0:NK) :: UVW
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2,1:NK) :: UV

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    INTEGER(KKID_KIND) :: K

    DO K = 1 , NK

      DO DIM = 1 , 2

        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
              UV( I , J ,DIM, K ) = 0.5 * ( UVW( I , J ,DIM, K ) + UVW( I , J ,DIM,K-1) )
            ELSE
              UV( I , J ,DIM, K ) = 0.0
            END IF
          END DO
        END DO

      END DO

    END DO

  END SUBROUTINE MR_INV_INTERP_Z_UV_FROM_UVW

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
  SUBROUTINE MR_INTERP_Z_SS_W( NI , NJ , NK , SS , SW , SS0 , SSN )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) :: SS
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,0:NK) :: SW

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ     ) , OPTIONAL :: SS0 , SSN

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

    K = 0
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
         !DIR$ FORCEINLINE
          CALL MR_INTERP_Z_SS_W_II_JJ_K0
        END DO
      END DO
    !END K = 0

    DO K = 1 , NK-1
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
         !DIR$ FORCEINLINE
          CALL MR_INTERP_Z_SS_W_II_JJ_KK
        END DO
      END DO
    END DO

    K = NK
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
         !DIR$ FORCEINLINE
          CALL MR_INTERP_Z_SS_W_II_JJ_KN
        END DO
      END DO
    !END K = NK

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
  SUBROUTINE MR_INTERP_Z_SS_W_II_JJ_K0

    IMPLICIT NONE

    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      IF( PRESENT(SS0) ) THEN
        SW( I , J , K ) = SS0( I , J )
      ELSE
        SW( I , J , K ) = SS( I , J ,K+1)
      END IF
    ELSE
      !BLOCK
        SW( I , J , K ) = 0.0
      !END BLOCK
    END IF

  END SUBROUTINE MR_INTERP_Z_SS_W_II_JJ_K0

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
  SUBROUTINE MR_INTERP_Z_SS_W_II_JJ_KK

    IMPLICIT NONE

    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      !BLOCK
        SW( I , J , K ) = 0.5 * ( SS( I , J ,K+1) + SS( I , J , K ) )
      !END BLOCK
    ELSE
      !BLOCK
        SW( I , J , K ) = 0.0
      !END BLOCK
    END IF

  END SUBROUTINE MR_INTERP_Z_SS_W_II_JJ_KK

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
  SUBROUTINE MR_INTERP_Z_SS_W_II_JJ_KN

    IMPLICIT NONE

    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      IF( PRESENT(SSN) ) THEN
        SW( I , J , K ) = SSN( I , J )
      ELSE
        SW( I , J , K ) = SS( I , J , K )
      END IF
    ELSE
      !BLOCK
        SW( I , J , K ) = 0.0
      !END BLOCK
    END IF

  END SUBROUTINE MR_INTERP_Z_SS_W_II_JJ_KN

  END SUBROUTINE MR_INTERP_Z_SS_W

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
  SUBROUTINE MR_INV_INTERP_Z_SS_FROM_SW( NI , NJ , NK , SW , SS )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,0:NK) :: SW
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) :: SS

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

    DO K = 1 , NK

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            SS( I , J , K ) = 0.5 * ( SW( I , J , K ) + SW( I , J ,K-1) )
          ELSE
            SS( I , J , K ) = 0.0
          END IF
        END DO
      END DO

    END DO

  END SUBROUTINE MR_INV_INTERP_Z_SS_FROM_SW

  END MODULE MR_MOD_INTERP_Z