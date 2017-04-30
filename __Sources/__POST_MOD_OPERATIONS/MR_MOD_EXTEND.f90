#INCLUDE 'MR_H_ALIGN_PADDING.H'
!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_EXTEND

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_EXTEND_UV
    PUBLIC :: MR_EXTEND_SS

!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_EXTEND_ACTIVITY( NLOOPS , NI , NJ , ACTIVITY , EXTEND_ACTIVITY )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER            , INTENT(IN ) :: NLOOPS

    INTEGER(ACID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,ACID_KIND),1:NJ    ) :: ACTIVITY
    INTEGER(ACID_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI*NLOOPS,ACID_KIND),1:NJ    ) :: EXTEND_ACTIVITY

   !DIR$ FORCEINLINE
    CALL MR_EXTEND_ACTIVITY_FOR_ELEMS
   !END$ FORCEINLINE

!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_EXTEND_ACTIVITY_FOR_ELEMS

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER(IJID_KIND) :: P , Q
    INTEGER            :: LOOP

    LOOP = 1
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          EXTEND_ACTIVITY( I , J ) = ACTIVITY( I , J )
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO J = 1 , NJ
        Q = NJ-J+1
       !DIR$ VECTOR ALIGNED
        DO I = (LOOP-1)*NI+1 , LOOP*NI
          P = I-NI
          EXTEND_ACTIVITY( I , J ) = EXTEND_ACTIVITY( P , Q )
        END DO
      END DO

    END DO

  END SUBROUTINE MR_EXTEND_ACTIVITY_FOR_ELEMS

  END SUBROUTINE MR_EXTEND_ACTIVITY

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_EXTEND_UV( NLOOPS , NI , NJ , UV , UU , VV , UVO , ACTIVITY,   & 
  & EXTEND_UV , EXTEND_UU , EXTEND_VV, EXTEND_UVO , EXTEND_ACTIVITY )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER            , INTENT(IN ) :: NLOOPS

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ,1:2) :: UU
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ,1:2) :: VV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ,1:2) :: UVO
    INTEGER(ACID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,ACID_KIND),1:NJ    ) , OPTIONAL :: ACTIVITY

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI*NLOOPS,FDRD_KIND),1:NJ,1:2) :: EXTEND_UV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI*NLOOPS,FDRD_KIND),1:NJ,1:2) :: EXTEND_UU
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI*NLOOPS,FDRD_KIND),0:NJ,1:2) :: EXTEND_VV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI*NLOOPS,FDRD_KIND),0:NJ,1:2) :: EXTEND_UVO
    INTEGER(ACID_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI*NLOOPS,ACID_KIND),1:NJ    ) , OPTIONAL :: EXTEND_ACTIVITY

    REAL   (FDRD_KIND) , PARAMETER   , DIMENSION(                                1:2) :: FACTOR = (/+1.0,-1.0/)

   !DIR$ FORCEINLINE
    CALL MR_EXTEND_UV_FOR_W_NODES
   !DIR$ FORCEINLINE
    CALL MR_EXTEND_UV_FOR_U_NODES
   !DIR$ FORCEINLINE
    CALL MR_EXTEND_UV_FOR_V_NODES
   !DIR$ FORCEINLINE
    CALL MR_EXTEND_UV_FOR_O_NODES
   !END$ FORCEINLINE
    IF( PRESENT( ACTIVITY ) .AND. PRESENT( EXTEND_ACTIVITY ) ) THEN
      CALL MR_EXTEND_ACTIVITY( NLOOPS , NI , NJ , ACTIVITY , EXTEND_ACTIVITY )
    END IF

!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_EXTEND_UV_FOR_W_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER(IJID_KIND) :: P , Q
    INTEGER            :: DIM
    INTEGER            :: LOOP

    LOOP = 1
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            EXTEND_UV( I , J ,DIM) = UV( I , J ,DIM)
          END DO
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO DIM = 1 , 2

        DO J = 1 , NJ
          Q = NJ-J+1
         !DIR$ VECTOR ALIGNED
          DO I = (LOOP-1)*NI+1 , LOOP*NI
            P = I-NI
            EXTEND_UV( I , J ,DIM) = EXTEND_UV( P , Q ,DIM) * FACTOR(DIM)
          END DO
        END DO

      END DO

    END DO

  END SUBROUTINE MR_EXTEND_UV_FOR_W_NODES

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_EXTEND_UV_FOR_U_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER(IJID_KIND) :: P , Q
    INTEGER            :: DIM
    INTEGER            :: LOOP

    LOOP = 1
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 0 , NI
            EXTEND_UU( I , J ,DIM) = UU( I , J ,DIM)
          END DO
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO DIM = 1 , 2

        DO J = 1 , NJ
          Q = NJ-J+1
         !DIR$ VECTOR ALIGNED
          DO I = (LOOP-1)*NI+1 , LOOP*NI
            P = I-NI
            EXTEND_UU( I , J ,DIM) = EXTEND_UU( P , Q ,DIM) * FACTOR(DIM)
          END DO
        END DO

      END DO

    END DO

  END SUBROUTINE MR_EXTEND_UV_FOR_U_NODES

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_EXTEND_UV_FOR_V_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER(IJID_KIND) :: P , Q
    INTEGER            :: DIM
    INTEGER            :: LOOP

    LOOP = 1
      DO DIM = 1 , 2
        DO J = 0 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            EXTEND_VV( I , J ,DIM) = VV( I , J ,DIM)
          END DO
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO DIM = 1 , 2

        DO J = 0 , NJ
          Q = NJ-J+1
         !DIR$ VECTOR ALIGNED
          DO I = (LOOP-1)*NI+1 , LOOP*NI
            P = I-NI
            EXTEND_VV( I , J ,DIM) = EXTEND_VV( P ,Q-1,DIM) * FACTOR(DIM)
          END DO
        END DO

      END DO

    END DO

  END SUBROUTINE MR_EXTEND_UV_FOR_V_NODES

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_EXTEND_UV_FOR_O_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER(IJID_KIND) :: P , Q
    INTEGER            :: DIM
    INTEGER            :: LOOP

    LOOP = 1
      DO DIM = 1 , 2
        DO J = 0 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 0 , NI
            EXTEND_UVO( I , J ,DIM) = UVO( I , J ,DIM)
          END DO
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO DIM = 1 , 2

        DO J = 0 , NJ
          Q = NJ-J+1
         !DIR$ VECTOR ALIGNED
          DO I = (LOOP-1)*NI+1 , LOOP*NI
            P = I-NI
            EXTEND_UVO( I , J ,DIM) = EXTEND_UVO( P ,Q-1,DIM) * FACTOR(DIM)
          END DO
        END DO

      END DO

    END DO

  END SUBROUTINE MR_EXTEND_UV_FOR_O_NODES

  END SUBROUTINE MR_EXTEND_UV

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_EXTEND_SS( NLOOPS , NI , NJ , SS , SU , SV , SO , ACTIVITY ,   &
  & EXTEND_SS , EXTEND_SU , EXTEND_SV, EXTEND_SO , EXTEND_ACTIVITY )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER            , INTENT(IN ) :: NLOOPS

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: SS
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: SU
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: SV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: SO
    INTEGER(ACID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,ACID_KIND),1:NJ) , OPTIONAL :: ACTIVITY

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI*NLOOPS,FDRD_KIND),1:NJ) :: EXTEND_SS
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI*NLOOPS,FDRD_KIND),1:NJ) :: EXTEND_SU
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI*NLOOPS,FDRD_KIND),0:NJ) :: EXTEND_SV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI*NLOOPS,FDRD_KIND),0:NJ) :: EXTEND_SO
    INTEGER(ACID_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI*NLOOPS,ACID_KIND),1:NJ) , OPTIONAL :: EXTEND_ACTIVITY

   !DIR$ FORCEINLINE
    CALL MR_EXTEND_SS_FOR_W_NODES
   !DIR$ FORCEINLINE
    CALL MR_EXTEND_SS_FOR_U_NODES
   !DIR$ FORCEINLINE
    CALL MR_EXTEND_SS_FOR_V_NODES
   !DIR$ FORCEINLINE
    CALL MR_EXTEND_SS_FOR_O_NODES
   !END$ FORCEINLINE
    IF( PRESENT( ACTIVITY ) .AND. PRESENT( EXTEND_ACTIVITY ) ) THEN
      CALL MR_EXTEND_ACTIVITY( NLOOPS , NI , NJ , ACTIVITY , EXTEND_ACTIVITY )
    END IF

!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_EXTEND_SS_FOR_W_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER(IJID_KIND) :: P , Q
    INTEGER            :: LOOP

    LOOP = 1
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          EXTEND_SS( I , J ) = SS( I , J )
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO J = 1 , NJ
        Q = NJ-J+1
       !DIR$ VECTOR ALIGNED
        DO I = (LOOP-1)*NI+1 , LOOP*NI
          P = I-NI
          EXTEND_SS( I , J ) = EXTEND_SS( P , Q )
        END DO
      END DO

    END DO

  END SUBROUTINE MR_EXTEND_SS_FOR_W_NODES

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_EXTEND_SS_FOR_U_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER(IJID_KIND) :: P , Q
    INTEGER            :: LOOP

    LOOP = 1
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          EXTEND_SU( I , J ) = SU( I , J )
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO J = 1 , NJ
        Q = NJ-J+1
       !DIR$ VECTOR ALIGNED
        DO I = (LOOP-1)*NI+1 , LOOP*NI
          P = I-NI
          EXTEND_SU( I , J ) = EXTEND_SU( P , Q )
        END DO
      END DO

    END DO

  END SUBROUTINE MR_EXTEND_SS_FOR_U_NODES

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_EXTEND_SS_FOR_V_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER(IJID_KIND) :: P , Q
    INTEGER            :: LOOP

    LOOP = 1
      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          EXTEND_SV( I , J ) = SV( I , J )
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO J = 0 , NJ
        Q = NJ-J+1
       !DIR$ VECTOR ALIGNED
        DO I = (LOOP-1)*NI+1 , LOOP*NI
          P = I-NI
          EXTEND_SV( I , J ) = EXTEND_SV( P ,Q-1)
        END DO
      END DO

    END DO

  END SUBROUTINE MR_EXTEND_SS_FOR_V_NODES

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_EXTEND_SS_FOR_O_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER(IJID_KIND) :: P , Q
    INTEGER            :: LOOP

    LOOP = 1
      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          EXTEND_SO( I , J ) = SO( I , J )
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO J = 0 , NJ
        Q = NJ-J+1
       !DIR$ VECTOR ALIGNED
        DO I = (LOOP-1)*NI+1 , LOOP*NI
          P = I-NI
          EXTEND_SO( I , J ) = EXTEND_SO( P ,Q-1)
        END DO
      END DO

    END DO

  END SUBROUTINE MR_EXTEND_SS_FOR_O_NODES

  END SUBROUTINE MR_EXTEND_SS

  END MODULE MR_MOD_EXTEND