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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_TDMA

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_TDMA1 , MR_TDMA2

    INTERFACE MR_TDMA1
      MODULE PROCEDURE MR_TDMA1_KIND4
      MODULE PROCEDURE MR_TDMA1_KIND8
    END INTERFACE

    INTERFACE MR_TDMA2
      MODULE PROCEDURE MR_TDMA2_KIND4
      MODULE PROCEDURE MR_TDMA2_KIND8
    END INTERFACE

!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  (FUNCTION)
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
  SUBROUTINE MR_TDMA1_KIND4( NI , NJ , A , B , C , D , RES , EPS )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(2:NI2(NI,CARD_KIND)  ,1:NJ  ) :: A
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: B
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI2(NI,CARD_KIND)-1,1:NJ  ) :: C
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: D

    REAL   (4)         , INTENT(OUT) , DIMENSION(1:NI1(NI,4)          ,1:NJ  ) :: RES

    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: TMP

    REAL   (4)         , OPTIONAL    :: EPS

    INTEGER(IJID_KIND) :: I , J

    TMP = MR_FUNC_TDMA1( NI , NJ , A , B , C , D )

    IF( PRESENT(EPS) ) THEN

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          RES( I , J ) = MAX( TMP( I , J ) , EPS )
        END DO
      END DO

    ELSE

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          RES( I , J ) = TMP( I , J )
        END DO
      END DO

    END IF

  END SUBROUTINE MR_TDMA1_KIND4

!***********************************************************************************************************************************
! UNIT:
!
!  (FUNCTION)
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
  SUBROUTINE MR_TDMA1_KIND8( NI , NJ , A , B , C , D , RES , EPS )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(2:NI2(NI,CARD_KIND)  ,1:NJ  ) :: A
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: B
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI2(NI,CARD_KIND)-1,1:NJ  ) :: C
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: D

    REAL   (8)         , INTENT(OUT) , DIMENSION(1:NI1(NI,8)          ,1:NJ  ) :: RES

    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: TMP

    REAL   (8)         , OPTIONAL    :: EPS

    INTEGER(IJID_KIND) :: I , J

    TMP = MR_FUNC_TDMA1( NI , NJ , A , B , C , D )

    IF( PRESENT(EPS) ) THEN

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          RES( I , J ) = MAX( TMP( I , J ) , EPS )
        END DO
      END DO

    ELSE

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          RES( I , J ) = TMP( I , J )
        END DO
      END DO

    END IF

  END SUBROUTINE MR_TDMA1_KIND8

!***********************************************************************************************************************************
! UNIT:
!
!  (FUNCTION)
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
  FUNCTION MR_FUNC_TDMA1( NI , NJ , A , B , C , D ) RESULT( X )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(2:NI2(NI,CARD_KIND)  ,1:NJ  ) :: A
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: B
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI2(NI,CARD_KIND)-1,1:NJ  ) :: C
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: D

    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: X
    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND)         ) :: Y

    REAL   (CARD_KIND)                                           :: W
    REAL   (CARD_KIND) , DIMENSION(1:NI2(NI,CARD_KIND)-1       ) :: V

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ

    ! GAUSSIAN ELIMINATION
      I = 1
        W = B( I , J )
        Y( I ) = D( I , J ) / W
      !END I = 1
     !DIR$ VECTOR ALIGNED
      DO I = 2 , NI
        V(I-1) = C(I-1, J ) / W
        W = B( I , J ) - A( I , J ) * V(I-1)
        Y( I ) = ( D( I , J ) - A( I , J ) * Y(I-1) ) / W
      END DO

    ! BACK SUBSTITUTION
      I = NI
        X( I , J ) = Y( I )
      !END I = NI
     !DIR$ VECTOR ALIGNED
      DO I = NI-1 , 1 , -1
        X( I , J ) = Y( I ) - V( I ) * X(I+1, J )
      END DO

    END DO

  END FUNCTION MR_FUNC_TDMA1

!***********************************************************************************************************************************
! UNIT:
!
!  (FUNCTION)
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
  SUBROUTINE MR_TDMA2_KIND4( NI , NJ , A , B , C , D , RES , EPS )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,2:NJ  ) :: A
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: B
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ-1) :: C
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: D

    REAL   (4)         , INTENT(OUT) , DIMENSION(1:NI1(NI,4)          ,1:NJ  ) :: RES

    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: TMP

    REAL   (4)         , OPTIONAL    :: EPS

    INTEGER(IJID_KIND) :: I , J

    TMP = MR_FUNC_TDMA2( NI , NJ , A , B , C , D )

    IF( PRESENT(EPS) ) THEN

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          RES( I , J ) = MAX( TMP( I , J ) , EPS )
        END DO
      END DO

    ELSE

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          RES( I , J ) = TMP( I , J )
        END DO
      END DO

    END IF

  END SUBROUTINE MR_TDMA2_KIND4

!***********************************************************************************************************************************
! UNIT:
!
!  (FUNCTION)
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
  SUBROUTINE MR_TDMA2_KIND8( NI , NJ , A , B , C , D , RES , EPS )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,2:NJ  ) :: A
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: B
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ-1) :: C
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: D

    REAL   (8)         , INTENT(OUT) , DIMENSION(1:NI1(NI,8)          ,1:NJ  ) :: RES

    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: TMP

    REAL   (8)         , OPTIONAL    :: EPS

    INTEGER(IJID_KIND) :: I , J

    TMP = MR_FUNC_TDMA2( NI , NJ , A , B , C , D )

    IF( PRESENT(EPS) ) THEN

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          RES( I , J ) = MAX( TMP( I , J ) , EPS )
        END DO
      END DO

    ELSE

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          RES( I , J ) = TMP( I , J )
        END DO
      END DO

    END IF

  END SUBROUTINE MR_TDMA2_KIND8

!***********************************************************************************************************************************
! UNIT:
!
!  (FUNCTION)
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
  FUNCTION MR_FUNC_TDMA2( NI , NJ , A , B , C , D ) RESULT( X )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,2:NJ  ) :: A
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: B
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ-1) :: C
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: D

    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: X
    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ  ) :: Y

    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND)         ) :: W
    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND)  ,1:NJ-1) :: V

    INTEGER(IJID_KIND) :: I , J

  ! GAUSSIAN ELIMINATION
    J = 1
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        W( I ) = B( I , J )
        Y( I , J ) = D( I , J ) / W( I )
      END DO
    !END J = 1
    DO J = 2 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        V( I ,J-1) = C( I ,J-1) / W( I )
        W( I ) = B( I , J ) - A( I , J ) * V( I ,J-1)
        Y( I , J ) = ( D( I , J ) - A( I , J ) * Y( I ,J-1) ) / W( I )
      END DO
    END DO

  ! BACK SUBSTITUTION
    J = NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        X( I , J ) = Y( I , J )
      END DO
    !END J = NJ
    DO J = NJ-1 , 1 , -1
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        X( I , J ) = Y( I , J ) - V( I , J ) * X( I ,J+1)
      END DO
    END DO

  END FUNCTION MR_FUNC_TDMA2

  END MODULE MR_MOD_TDMA