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
  MODULE MR_MOD_TDMA3

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_TDMA3_UV
    PUBLIC :: MR_TDMA3_SS

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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_TDMA3_UV( NI , NJ , NK , A , B , C , D ) RESULT( X )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,    2:NK  ) :: A
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,    1:NK  ) :: B
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,    1:NK-1) :: C
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,1:2,1:NK  ) :: D

    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,1:2,1:NK  ) :: X
    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,1:2,1:NK  ) :: Y

    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ           ) :: W
    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,    1:NK-1) :: V

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    INTEGER(KKID_KIND) :: K

  ! GAUSSIAN ELIMINATION
    K = 1
      !BLOCK
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            W( I , J ) = B( I , J , K )
          END DO
        END DO
      !END BLOCK
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            Y( I , J ,DIM, K ) = D( I , J ,DIM, K ) / W( I , J )
          END DO
        END DO
      END DO
    !END K = 1
    DO K = 2 , NK
      !BLOCK
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            V( I , J ,K-1) = C( I , J ,K-1) / W( I , J )
            W( I , J ) = B( I , J , K ) - A( I , J , K ) * V( I , J ,K-1)
          END DO
        END DO
      !END BLOCK
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            Y( I , J ,DIM, K ) = ( D( I , J ,DIM, K ) - A( I , J , K ) * Y( I , J ,DIM,K-1) ) / W( I , J )
          END DO
        END DO
      END DO
    END DO

  ! BACK SUBSTITUTION
    K = NK
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            X( I , J ,DIM, K ) = Y( I , J ,DIM, K )
          END DO
        END DO
      END DO
    !END K = NK
    DO K = NK-1 , 1 , -1
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            X( I , J ,DIM, K ) = Y( I , J ,DIM, K ) - V( I , J , K ) * X( I , J ,DIM,K+1)
          END DO
        END DO
      END DO
    END DO

  END FUNCTION MR_TDMA3_UV

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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_TDMA3_SS( NI , NJ , NK , A , B , C , D ) RESULT( X )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,2:NK  ) :: A
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,1:NK  ) :: B
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,1:NK-1) :: C
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,1:NK  ) :: D

    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,1:NK  ) :: X
    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,1:NK  ) :: Y

    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ       ) :: W
    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,1:NK-1) :: V

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

  ! GAUSSIAN ELIMINATION
    K = 1
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          W( I , J ) = B( I , J , K )
        END DO
      END DO
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          Y( I , J , K ) = D( I , J , K ) / W( I , J )
        END DO
      END DO
    !END K = 1
    DO K = 2 , NK
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          V( I , J ,K-1) = C( I , J ,K-1) / W( I , J )
          W( I , J ) = B( I , J , K ) - A( I , J , K ) * V( I , J ,K-1)
        END DO
      END DO
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          Y( I , J , K ) = ( D( I , J , K ) - A( I , J , K ) * Y( I , J ,K-1) ) / W( I , J )
        END DO
      END DO
    END DO

  ! BACK SUBSTITUTION
    K = NK
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          X( I , J , K ) = Y( I , J , K )
        END DO
      END DO
    !END K = NK
    DO K = NK-1 , 1 , -1
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          X( I , J , K ) = Y( I , J , K ) - V( I , J , K ) * X( I , J ,K+1)
        END DO
      END DO
    END DO

  END FUNCTION MR_TDMA3_SS

  END MODULE MR_MOD_TDMA3