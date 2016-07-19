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
  MODULE MR_MOD_TDMA
    
    USE MR_KINDS
    
    IMPLICIT NONE
    
    PRIVATE
    
    PUBLIC :: MR_TDMA1 , MR_TDMA2
    
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
  FUNCTION MR_TDMA1( NI , NJ , A , B , C , D ) RESULT( X )
  
    IMPLICIT NONE
    
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(2:NI  ,1:NJ) :: A
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI  ,1:NJ) :: B
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI-1,1:NJ) :: C
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI  ,1:NJ) :: D
    
    REAL   (CARD_KIND) , DIMENSION(1:NI  ,1:NJ) :: X
    REAL   (CARD_KIND) , DIMENSION(1:NI  ) :: Y
    
    REAL   (CARD_KIND)                     :: W
    REAL   (CARD_KIND) , DIMENSION(1:NI-1) :: V
    
    INTEGER(IJID_KIND) :: I , J
    
    DO J = 1 , NJ
    
    ! GAUSSIAN ELIMINATION
      W = B( 1 , J )
      Y( 1 ) = D( 1 , J ) / W
      DO I = 2 , NI
        V(I-1) = C(I-1, J ) / W
        W = B( I , J ) - A( I , J ) * V(I-1)
        Y( I ) = ( D( I , J ) - A( I , J ) * Y(I-1) ) / W
      END DO
      
    ! BACK SUBSTITUTION
      X(NI , J ) = Y(NI )
      DO I = NI-1 , 1 , -1
        X( I , J ) = Y( I ) - V( I ) * X(I+1, J )
      END DO
      
    END DO
    
  END FUNCTION MR_TDMA1
  
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
  FUNCTION MR_TDMA2( NI , NJ , A , B , C , D ) RESULT( X )
  
    IMPLICIT NONE
    
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI,2:NJ  ) :: A
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ  ) :: B
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ-1) :: C
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ  ) :: D
    
    REAL   (CARD_KIND) , DIMENSION(1:NI,1:NJ  ) :: X
    REAL   (CARD_KIND) , DIMENSION(1:NI,1:NJ  ) :: Y
    
    REAL   (CARD_KIND) , DIMENSION(1:NI       ) :: W
    REAL   (CARD_KIND) , DIMENSION(1:NI,1:NJ-1) :: V
    
    INTEGER(IJID_KIND) :: J
    
  ! GAUSSIAN ELIMINATION
    W(:) = B(:, 1 )
    Y(:, 1 ) = D(:, 1 ) / W(:)
    DO J = 2 , NJ
      V(:,J-1) = C(:,J-1) / W(:)
      W(:) = B(:, J ) - A(:, J ) * V(:,J-1)
      Y(:, J ) = ( D(:, J ) - A(:, J ) * Y(:,J-1) ) / W(:)
    END DO
    
  ! BACK SUBSTITUTION
    X(:,NJ ) = Y(:,NJ )
    DO J = NJ-1 , 1 , -1
      X(:, J ) = Y(:, J ) - V(:, J ) * X(:,J+1)
    END DO
    
  END FUNCTION MR_TDMA2
  
  END MODULE MR_MOD_TDMA