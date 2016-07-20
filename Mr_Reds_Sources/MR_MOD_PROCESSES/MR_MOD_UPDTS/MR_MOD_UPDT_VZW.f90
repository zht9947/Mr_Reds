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
  MODULE MR_MOD_UPDT_VZW
  
    USE MR_KINDS
    
    USE MR_DEF_RANKS
    
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_ACTIVITY
    USE MR_DEF_TIMING
    
    IMPLICIT NONE
    
    PRIVATE
    
    PUBLIC :: MR_UPDT_VZW
    
    REAL(FDRD_KIND)    , ALLOCATABLE , DIMENSION(:,:      ) :: TBUVM
    
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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_UPDT_VZW
  
    USE MR_MOD_OPERATOR_SS
    USE MR_MOD_OPERATOR_UV
    
    USE MR_MOD_INTERP_Z
    
    IMPLICIT NONE
    
    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K
    
    ALLOCATE( TBUVM(1:NI1(FDRD_KIND),1:NJ) )
    
      TBUVM = .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. TBUV ) )
    
      DO K = 1 , NK
        DO J = 1 , NJ
          DO I = 1 , NI
            VZWW( I , J , K ) = V0 - KAR * SQRT(RBT) / EKZ * SQRT(TBUVM( I ,J )) * H( I , J ) * (1.0+SIGMA( K )) * SIGMA( K )
          END DO
        END DO
      END DO
    
    DEALLOCATE( TBUVM )
    
    CALL MR_INTERP_Z_SS_W( NI , NJ , NK , VZWW , VZW )
    
  END SUBROUTINE MR_UPDT_VZW
  
  END MODULE MR_MOD_UPDT_VZW