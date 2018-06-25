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

    PUBLIC :: MR_EXTEND_XY

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
  SUBROUTINE MR_EXTEND_ACTIVITY( NLOOPS , NI , NJ , ACTIVITY , ACTIVITY_ )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: NLOOPS

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(ACID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,ACID_KIND),1:NJ) :: ACTIVITY
    INTEGER(ACID_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI*NLOOPS,ACID_KIND),1:NJ) :: ACTIVITY_

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
          ACTIVITY_( I , J ) = ACTIVITY( I , J )
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO J = 1 , NJ
        Q = NJ-J+1
       !DIR$ VECTOR ALIGNED
        DO I = (LOOP-1)*NI+1 , LOOP*NI
          P = I-NI
          ACTIVITY_( I , J ) = ACTIVITY_( P , Q )
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
  & UV_ , UU_ , VV_ , UVO_ , ACTIVITY_ )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: NLOOPS

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ,1:2) :: UU
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ,1:2) :: VV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ,1:2) :: UVO
    INTEGER(ACID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,ACID_KIND),1:NJ    ) , OPTIONAL :: ACTIVITY

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI*NLOOPS,FDRD_KIND),1:NJ,1:2) :: UV_
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI*NLOOPS,FDRD_KIND),1:NJ,1:2) :: UU_
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI*NLOOPS,FDRD_KIND),0:NJ,1:2) :: VV_
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI*NLOOPS,FDRD_KIND),0:NJ,1:2) :: UVO_
    INTEGER(ACID_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI*NLOOPS,ACID_KIND),1:NJ    ) , OPTIONAL :: ACTIVITY_

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
    IF( PRESENT( ACTIVITY ) .AND. PRESENT( ACTIVITY_ ) ) THEN
      CALL MR_EXTEND_ACTIVITY( NLOOPS , NI , NJ , ACTIVITY , ACTIVITY_ )
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
            UV_( I , J ,DIM) = UV( I , J ,DIM)
          END DO
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO DIM = 1 , 2

        DO J = 1 , NJ
          Q = NJ-J+1
         !DIR$ IVDEP
         !DIR$ VECTOR ALIGNED
          DO I = (LOOP-1)*NI+1 , LOOP*NI
            P = I-NI
            UV_( I , J ,DIM) = UV_( P , Q ,DIM) * FACTOR(DIM)
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
            UU_( I , J ,DIM) = UU( I , J ,DIM)
          END DO
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO DIM = 1 , 2

        DO J = 1 , NJ
          Q = NJ-J+1
         !DIR$ IVDEP
         !DIR$ VECTOR ALIGNED
          DO I = (LOOP-1)*NI+1 , LOOP*NI
            P = I-NI
            UU_( I , J ,DIM) = UU_( P , Q ,DIM) * FACTOR(DIM)
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
            VV_( I , J ,DIM) = VV( I , J ,DIM)
          END DO
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO DIM = 1 , 2

        DO J = 0 , NJ
          Q = NJ-J+1
         !DIR$ IVDEP
         !DIR$ VECTOR ALIGNED
          DO I = (LOOP-1)*NI+1 , LOOP*NI
            P = I-NI
            VV_( I , J ,DIM) = VV_( P ,Q-1,DIM) * FACTOR(DIM)
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
            UVO_( I , J ,DIM) = UVO( I , J ,DIM)
          END DO
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO DIM = 1 , 2

        DO J = 0 , NJ
          Q = NJ-J+1
         !DIR$ IVDEP
         !DIR$ VECTOR ALIGNED
          DO I = (LOOP-1)*NI+1 , LOOP*NI
            P = I-NI
            UVO_( I , J ,DIM) = UVO_( P ,Q-1,DIM) * FACTOR(DIM)
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
  & SS_ , SU_ , SV_ , SO_ , ACTIVITY_ )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: NLOOPS

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: SS
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: SU
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: SV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: SO
    INTEGER(ACID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,ACID_KIND),1:NJ) , OPTIONAL :: ACTIVITY

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI*NLOOPS,FDRD_KIND),1:NJ) :: SS_
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI*NLOOPS,FDRD_KIND),1:NJ) :: SU_
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI*NLOOPS,FDRD_KIND),0:NJ) :: SV_
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI*NLOOPS,FDRD_KIND),0:NJ) :: SO_
    INTEGER(ACID_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI*NLOOPS,ACID_KIND),1:NJ) , OPTIONAL :: ACTIVITY_

   !DIR$ FORCEINLINE
    CALL MR_EXTEND_SS_FOR_W_NODES
   !DIR$ FORCEINLINE
    CALL MR_EXTEND_SS_FOR_U_NODES
   !DIR$ FORCEINLINE
    CALL MR_EXTEND_SS_FOR_V_NODES
   !DIR$ FORCEINLINE
    CALL MR_EXTEND_SS_FOR_O_NODES
   !END$ FORCEINLINE
    IF( PRESENT( ACTIVITY ) .AND. PRESENT( ACTIVITY_ ) ) THEN
      CALL MR_EXTEND_ACTIVITY( NLOOPS , NI , NJ , ACTIVITY , ACTIVITY_ )
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
          SS_( I , J ) = SS( I , J )
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO J = 1 , NJ
        Q = NJ-J+1
       !DIR$ IVDEP
       !DIR$ VECTOR ALIGNED
        DO I = (LOOP-1)*NI+1 , LOOP*NI
          P = I-NI
          SS_( I , J ) = SS_( P , Q )
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
          SU_( I , J ) = SU( I , J )
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO J = 1 , NJ
        Q = NJ-J+1
       !DIR$ IVDEP
       !DIR$ VECTOR ALIGNED
        DO I = (LOOP-1)*NI+1 , LOOP*NI
          P = I-NI
          SU_( I , J ) = SU_( P , Q )
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
          SV_( I , J ) = SV( I , J )
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO J = 0 , NJ
        Q = NJ-J+1
       !DIR$ IVDEP
       !DIR$ VECTOR ALIGNED
        DO I = (LOOP-1)*NI+1 , LOOP*NI
          P = I-NI
          SV_( I , J ) = SV_( P ,Q-1)
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
          SO_( I , J ) = SO( I , J )
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO J = 0 , NJ
        Q = NJ-J+1
       !DIR$ IVDEP
       !DIR$ VECTOR ALIGNED
        DO I = (LOOP-1)*NI+1 , LOOP*NI
          P = I-NI
          SO_( I , J ) = SO_( P ,Q-1)
        END DO
      END DO

    END DO

  END SUBROUTINE MR_EXTEND_SS_FOR_O_NODES

  END SUBROUTINE MR_EXTEND_SS

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
  SUBROUTINE MR_EXTEND_XY( NLOOPS , NI , NJ , XYUV , XYUU , XYVV , XYOO ,   &
  & XYUV_ , XYUU_ , XYVV_ , XYOO_ )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: NLOOPS

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (XYRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,XYRD_KIND),1:NJ,1:2) :: XYUV
    REAL   (XYRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,XYRD_KIND),1:NJ,1:2) :: XYUU
    REAL   (XYRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,XYRD_KIND),0:NJ,1:2) :: XYVV
    REAL   (XYRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,XYRD_KIND),0:NJ,1:2) :: XYOO

    REAL   (XYRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI*NLOOPS,XYRD_KIND),1:NJ,1:2) :: XYUV_
    REAL   (XYRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI*NLOOPS,XYRD_KIND),1:NJ,1:2) :: XYUU_
    REAL   (XYRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI*NLOOPS,XYRD_KIND),0:NJ,1:2) :: XYVV_
    REAL   (XYRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI*NLOOPS,XYRD_KIND),0:NJ,1:2) :: XYOO_

    REAL   (XYRD_KIND) , PARAMETER   , DIMENSION(                                1:2) :: FACTOR = (/+1.0,-1.0/)
    REAL   (XYRD_KIND) ,               DIMENSION(                                1:2) :: SHIFT

    INTEGER            :: DIM

    DO DIM = 1 , 2
      IF( MOD( NJ , 2 ) == 0 ) THEN
        SHIFT(DIM) = XYOO(NI,NJ/2  ,DIM) - XYOO(0,NJ/2  ,DIM)
      ELSE
        SHIFT(DIM) = XYUU(NI,NJ/2+1,DIM) - XYUU(0,NJ/2+1,DIM)
      END IF
    END DO

   !DIR$ FORCEINLINE
    CALL MR_EXTEND_XY_FOR_W_NODES
   !DIR$ FORCEINLINE
    CALL MR_EXTEND_XY_FOR_U_NODES
   !DIR$ FORCEINLINE
    CALL MR_EXTEND_XY_FOR_V_NODES
   !DIR$ FORCEINLINE
    CALL MR_EXTEND_XY_FOR_O_NODES
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
  SUBROUTINE MR_EXTEND_XY_FOR_W_NODES

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
            XYUV_( I , J ,DIM) = XYUV( I , J ,DIM)
          END DO
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO DIM = 1 , 2

        DO J = 1 , NJ
          Q = NJ-J+1
         !DIR$ IVDEP
         !DIR$ VECTOR ALIGNED
          DO I = (LOOP-1)*NI+1 , LOOP*NI
            P = I-NI
            XYUV_( I , J ,DIM) = XYUV_( P , Q ,DIM) * FACTOR(DIM) + SHIFT(DIM)
          END DO
        END DO

      END DO

    END DO

  END SUBROUTINE MR_EXTEND_XY_FOR_W_NODES

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
  SUBROUTINE MR_EXTEND_XY_FOR_U_NODES

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
            XYUU_( I , J ,DIM) = XYUU( I , J ,DIM)
          END DO
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO DIM = 1 , 2

        DO J = 1 , NJ
          Q = NJ-J+1
         !DIR$ IVDEP
         !DIR$ VECTOR ALIGNED
          DO I = (LOOP-1)*NI+1 , LOOP*NI
            P = I-NI
            XYUU_( I , J ,DIM) = XYUU_( P , Q ,DIM) * FACTOR(DIM) + SHIFT(DIM)
          END DO
        END DO

      END DO

    END DO

  END SUBROUTINE MR_EXTEND_XY_FOR_U_NODES

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
  SUBROUTINE MR_EXTEND_XY_FOR_V_NODES

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
            XYVV_( I , J ,DIM) = XYVV( I , J ,DIM)
          END DO
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO DIM = 1 , 2

        DO J = 0 , NJ
          Q = NJ-J+1
         !DIR$ IVDEP
         !DIR$ VECTOR ALIGNED
          DO I = (LOOP-1)*NI+1 , LOOP*NI
            P = I-NI
            XYVV_( I , J ,DIM) = XYVV_( P ,Q-1,DIM) * FACTOR(DIM) + SHIFT(DIM)
          END DO
        END DO

      END DO

    END DO

  END SUBROUTINE MR_EXTEND_XY_FOR_V_NODES

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
  SUBROUTINE MR_EXTEND_XY_FOR_O_NODES

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
            XYOO_( I , J ,DIM) = XYOO( I , J ,DIM)
          END DO
        END DO
      END DO
    !END LOOP = 1

    DO LOOP = 2 , NLOOPS

      DO DIM = 1 , 2

        DO J = 0 , NJ
          Q = NJ-J+1
         !DIR$ IVDEP
         !DIR$ VECTOR ALIGNED
          DO I = (LOOP-1)*NI+1 , LOOP*NI
            P = I-NI
            XYOO_( I , J ,DIM) = XYOO_( P ,Q-1,DIM) * FACTOR(DIM) + SHIFT(DIM)
          END DO
        END DO

      END DO

    END DO

  END SUBROUTINE MR_EXTEND_XY_FOR_O_NODES

  END SUBROUTINE MR_EXTEND_XY

  END MODULE MR_MOD_EXTEND