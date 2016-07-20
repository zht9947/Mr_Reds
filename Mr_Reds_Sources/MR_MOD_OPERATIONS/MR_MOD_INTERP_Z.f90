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
    PUBLIC :: MR_INTERP_Z_SS_W
    
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
  SUBROUTINE MR_INTERP_Z_UV_W( NI , NJ , NK , UV , UVW )
  
    IMPLICIT NONE
    
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK
    
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2,1:NK) :: UV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2,0:NK) :: UVW
    
    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    INTEGER(KKID_KIND) :: K
    
    K = 0
      DO DIM = 1 , 2
        DO J = 1 , NJ
          DO I = 1 , NI
            CALL MR_INTERP_Z_UV_W_II_JJ_K0( I , J ,DIM, K )
          END DO
        END DO
      END DO
    !END K = 0
    
    DO K = 1 , NK-1
      DO DIM = 1 , 2
        DO J = 1 , NJ
          DO I = 1 , NI
            CALL MR_INTERP_Z_UV_W_II_JJ_KK( I , J ,DIM, K )
          END DO
        END DO
      END DO
    END DO
    
    K = NK
      DO DIM = 1 , 2
        DO J = 1 , NJ
          DO I = 1 , NI
            CALL MR_INTERP_Z_UV_W_II_JJ_KN( I , J ,DIM, K )
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
  SUBROUTINE MR_INTERP_Z_UV_W_II_JJ_K0( I , J ,DIM, K )
  
    IMPLICIT NONE
    
    INTEGER(IJID_KIND) , INTENT(IN ) :: I , J
    INTEGER            , INTENT(IN ) :: DIM
    INTEGER(KKID_KIND) , INTENT(IN ) :: K
 
    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      UVW( I , J ,DIM, K ) = UV( I , J ,DIM,K+1)
    ELSE
      UVW( I , J ,DIM, K ) = 0.0
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
  SUBROUTINE MR_INTERP_Z_UV_W_II_JJ_KK( I , J ,DIM, K )
  
    IMPLICIT NONE
    
    INTEGER(IJID_KIND) , INTENT(IN ) :: I , J
    INTEGER            , INTENT(IN ) :: DIM
    INTEGER(KKID_KIND) , INTENT(IN ) :: K
    
    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      UVW( I , J ,DIM, K ) = 0.5 * ( UV( I , J ,DIM,K+1) + UV( I , J ,DIM, K ) )
    ELSE
      UVW( I , J ,DIM, K ) = 0.0
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
  SUBROUTINE MR_INTERP_Z_UV_W_II_JJ_KN( I , J ,DIM, K )
  
    IMPLICIT NONE
    
    INTEGER(IJID_KIND) , INTENT(IN ) :: I , J
    INTEGER            , INTENT(IN ) :: DIM
    INTEGER(KKID_KIND) , INTENT(IN ) :: K
    
    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      UVW( I , J ,DIM, K ) = UV( I , J ,DIM, K )
    ELSE
      UVW( I , J ,DIM, K ) = 0.0
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
  SUBROUTINE MR_INTERP_Z_SS_W( NI , NJ , NK , SS , SW )
  
    IMPLICIT NONE
    
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK
    
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:NK) :: SS
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,0:NK) :: SW
    
    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K
    
    K = 0
      DO J = 1 , NJ
        DO I = 1 , NI
          CALL MR_INTERP_Z_SS_W_II_JJ_K0( I , J , K )
        END DO
      END DO
    !END K = 0
    
    DO K = 1 , NK-1
      DO J = 1 , NJ
        DO I = 1 , NI
          CALL MR_INTERP_Z_SS_W_II_JJ_KK( I , J , K )
        END DO
      END DO
    END DO
    
    K = NK
      DO J = 1 , NJ
        DO I = 1 , NI
          CALL MR_INTERP_Z_SS_W_II_JJ_KN( I , J , K )
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
  SUBROUTINE MR_INTERP_Z_SS_W_II_JJ_K0( I , J , K )
  
    IMPLICIT NONE
    
    INTEGER(IJID_KIND) , INTENT(IN ) :: I , J
    INTEGER(KKID_KIND) , INTENT(IN ) :: K
    
    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      SW( I , J , K ) = SS( I , J ,K+1)
    ELSE
      SW( I , J , K ) = 0.0
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
  SUBROUTINE MR_INTERP_Z_SS_W_II_JJ_KK( I , J , K )
  
    IMPLICIT NONE
    
    INTEGER(IJID_KIND) , INTENT(IN ) :: I , J
    INTEGER(KKID_KIND) , INTENT(IN ) :: K
    
    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      SW( I , J , K ) = 0.5 * ( SS( I , J ,K+1) + SS( I , J , K ) )
    ELSE
      SW( I , J , K ) = 0.0
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
  SUBROUTINE MR_INTERP_Z_SS_W_II_JJ_KN( I , J , K )
  
    IMPLICIT NONE
    
    INTEGER(IJID_KIND) , INTENT(IN ) :: I , J
    INTEGER(KKID_KIND) , INTENT(IN ) :: K
    
    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      SW( I , J , K ) = SS( I , J , K )
    ELSE
      SW( I , J , K ) = 0.0
    END IF
    
  END SUBROUTINE MR_INTERP_Z_SS_W_II_JJ_KN
    
  END SUBROUTINE MR_INTERP_Z_SS_W
  
  END MODULE MR_MOD_INTERP_Z