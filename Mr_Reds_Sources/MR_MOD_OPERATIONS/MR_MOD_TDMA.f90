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
    
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(2:NI2(CARD_KIND)  ,1:NJ  ) :: A
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ  ) :: B
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI2(CARD_KIND)-1,1:NJ  ) :: C
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ  ) :: D
    
    REAL   (CARD_KIND) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ  ) :: X
    REAL   (CARD_KIND) , DIMENSION(1:NI1(CARD_KIND)         ) :: Y
    
    REAL   (CARD_KIND)                                        :: W
    REAL   (CARD_KIND) , DIMENSION(1:NI2(CARD_KIND)-1       ) :: V
    
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
    
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(CARD_KIND)  ,2:NJ  ) :: A
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ  ) :: B
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ-1) :: C
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ  ) :: D
    
    REAL   (CARD_KIND) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ  ) :: X
    REAL   (CARD_KIND) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ  ) :: Y
    
    REAL   (CARD_KIND) , DIMENSION(1:NI1(CARD_KIND)         ) :: W
    REAL   (CARD_KIND) , DIMENSION(1:NI1(CARD_KIND)  ,1:NJ-1) :: V
    
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
    
  END FUNCTION MR_TDMA2
  
  END MODULE MR_MOD_TDMA