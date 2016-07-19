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

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,    2:NK  ) :: A
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,    1:NK  ) :: B
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,    1:NK-1) :: C
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,1:2,1:NK  ) :: D

    REAL   (CARD_KIND) , DIMENSION(1:NI,1:NJ,1:2,1:NK  ) :: X
    REAL   (CARD_KIND) , DIMENSION(1:NI,1:NJ,1:2,1:NK  ) :: Y

    REAL   (CARD_KIND) , DIMENSION(1:NI,1:NJ           ) :: W
    REAL   (CARD_KIND) , DIMENSION(1:NI,1:NJ,    1:NK-1) :: V

    INTEGER(KKID_KIND) :: K

    INTEGER :: DIM

  ! GAUSSIAN ELIMINATION
    W(:,:) = B(:,:, 1 )
    DO DIM = 1 , 2
      Y(:,:,DIM, 1 ) = D(:,:,DIM, 1 ) / W(:,:)
    END DO
    DO K = 2 , NK
      V(:,:,K-1) = C(:,:,K-1) / W(:,:)
      W(:,:) = B(:,:, K ) - A(:,:, K ) * V(:,:,K-1)
      DO DIM = 1 , 2
        Y(:,:,DIM, K ) = ( D(:,:,DIM, K ) - A(:,:, K ) * Y(:,:,DIM,K-1) ) / W(:,:)
      END DO
    END DO

  ! BACK SUBSTITUTION
    DO DIM = 1 , 2
      X(:,:,DIM,NK ) = Y(:,:,DIM,NK )
    END DO
    DO K = NK-1 , 1 , -1
      DO DIM = 1 , 2
        X(:,:,DIM, K ) = Y(:,:,DIM, K ) - V(:,:, K ) * X(:,:,DIM,K+1)
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

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,2:NK  ) :: A
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,1:NK  ) :: B
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,1:NK-1) :: C
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,1:NK  ) :: D

    REAL   (CARD_KIND) , DIMENSION(1:NI,1:NJ,1:NK  ) :: X
    REAL   (CARD_KIND) , DIMENSION(1:NI,1:NJ,1:NK  ) :: Y

    REAL   (CARD_KIND) , DIMENSION(1:NI,1:NJ       ) :: W
    REAL   (CARD_KIND) , DIMENSION(1:NI,1:NJ,1:NK-1) :: V

    INTEGER(KKID_KIND) :: K

  ! GAUSSIAN ELIMINATION
    W(:,:) = B(:,:, 1 )
    Y(:,:, 1 ) = D(:,:, 1 ) / W(:,:)
    DO K = 2 , NK
      V(:,:,K-1) = C(:,:,K-1) / W(:,:)
      W(:,:) = B(:,:, K ) - A(:,:, K ) * V(:,:,K-1)
      Y(:,:, K ) = ( D(:,:, K ) - A(:,:, K ) * Y(:,:,K-1) ) / W(:,:)
    END DO

  ! BACK SUBSTITUTION
    X(:,:,NK ) = Y(:,:,NK )
    DO K = NK-1 , 1 , -1
      X(:,:, K ) = Y(:,:, K ) - V(:,:, K ) * X(:,:,K+1)
    END DO

  END FUNCTION MR_TDMA3_SS

  END MODULE MR_MOD_TDMA3