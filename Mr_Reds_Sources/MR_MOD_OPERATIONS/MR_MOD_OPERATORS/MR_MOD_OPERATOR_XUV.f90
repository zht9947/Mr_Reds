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
  MODULE MR_MOD_OPERATOR_XUV
    
    USE MR_KINDS
    
    IMPLICIT NONE
    
    PRIVATE
    
    PUBLIC :: OPERATOR( .MRXUVDTM. )
    PUBLIC :: OPERATOR( .MRXUVIVS. ) , OPERATOR( .MRXUVTPS. ) , OPERATOR( .MRXUVMAT. )
    
    INTERFACE OPERATOR( .MRXUVDTM. )
      MODULE PROCEDURE MR_TENSOR_DETERMINANT
    END INTERFACE
    
    INTERFACE OPERATOR( .MRXUVIVS. )
      MODULE PROCEDURE MR_TENSOR_INVERSE
    END INTERFACE

    INTERFACE OPERATOR( .MRXUVTPS. )
      MODULE PROCEDURE MR_TENSOR_TRANSPOSE
    END INTERFACE
    
    INTERFACE OPERATOR( .MRXUVMAT. )
      MODULE PROCEDURE MR_TENSOR_MATRIX_MULTIPLY
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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_TENSOR_DETERMINANT( XUV ) RESULT( XUVDTM )
  
    IMPLICIT NONE
    
    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV
    
    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:,:    ) :: XUVDTM
    
    INTEGER(IJID_KIND) :: I , J
    
    ALLOCATE( XUVDTM(1:SIZE(XUV,DIM=1),1:SIZE(XUV,DIM=2)) )
    
    DO J = 1 , SIZE(XUV,DIM=2)
      DO I = 1 , SIZE(XUV,DIM=1)
        XUVDTM( I , J ) = XUV( I , J ,1,1) * XUV( I , J ,2,2) - XUV( I , J ,2,1) * XUV( I , J ,1,2)
      END DO
    END DO
    
  END FUNCTION MR_TENSOR_DETERMINANT
  
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
  FUNCTION MR_TENSOR_INVERSE( MW , XUV ) RESULT( XUVIVS )
  
    IMPLICIT NONE
    
    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:    ) :: MW
    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV
    
    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVIVS
    
    INTEGER(IJID_KIND) :: I , J
    
    ALLOCATE( XUVIVS(1:SIZE(XUV,DIM=1),1:SIZE(XUV,DIM=2),1:2,1:2) )
    
    DO J = 1 , SIZE(XUV,DIM=2)
      DO I = 1 , SIZE(XUV,DIM=1)
        XUVIVS( I , J ,1,1) = XUV( I , J ,2,2) / MW( I , J )
        XUVIVS( I , J ,2,1) =-XUV( I , J ,2,1) / MW( I , J )
        XUVIVS( I , J ,1,2) =-XUV( I , J ,1,2) / MW( I , J )
        XUVIVS( I , J ,2,2) = XUV( I , J ,1,1) / MW( I , J )
      END DO
    END DO
    
  END FUNCTION MR_TENSOR_INVERSE
  
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
  FUNCTION MR_TENSOR_TRANSPOSE( XUV ) RESULT( XUVTPS )
  
    IMPLICIT NONE
    
    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV
    
    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVTPS
    
    INTEGER(IJID_KIND) :: I , J
    
    ALLOCATE( XUVTPS(1:SIZE(XUV,DIM=1),1:SIZE(XUV,DIM=2),1:2,1:2) )
    
    DO J = 1 , SIZE(XUV,DIM=2)
      DO I = 1 , SIZE(XUV,DIM=1)
        XUVTPS( I , J ,1,1) = XUV( I , J ,1,1)
        XUVTPS( I , J ,2,1) = XUV( I , J ,1,2)
        XUVTPS( I , J ,1,2) = XUV( I , J ,2,1)
        XUVTPS( I , J ,2,2) = XUV( I , J ,2,2)
      END DO
    END DO
    
  END FUNCTION MR_TENSOR_TRANSPOSE
  
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
  FUNCTION MR_TENSOR_MATRIX_MULTIPLY( XUV1 , XUV2 ) RESULT( XUVMAT )
  
    IMPLICIT NONE
    
    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV1 , XUV2
    
    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVMAT
    
    INTEGER(IJID_KIND) :: I , J
    
    ALLOCATE( XUVMAT(1:SIZE(XUV1,DIM=1),1:SIZE(XUV1,DIM=2),1:2,1:2) )
    
    DO J = 1 , SIZE(XUV1,DIM=2)
      DO I = 1 , SIZE(XUV1,DIM=1)
        XUVMAT( I , J ,1,1) = XUV1( I , J ,1,1) * XUV2( I , J ,1,1) + XUV1( I , J ,1,2) * XUV2( I , J ,2,1)
        XUVMAT( I , J ,2,1) = XUV1( I , J ,2,1) * XUV2( I , J ,1,1) + XUV1( I , J ,2,2) * XUV2( I , J ,2,1)
        XUVMAT( I , J ,1,2) = XUV1( I , J ,1,1) * XUV2( I , J ,1,2) + XUV1( I , J ,1,2) * XUV2( I , J ,2,2)
        XUVMAT( I , J ,2,2) = XUV1( I , J ,2,1) * XUV2( I , J ,1,2) + XUV1( I , J ,2,2) * XUV2( I , J ,2,2)
      END DO
    END DO
    
  END FUNCTION MR_TENSOR_MATRIX_MULTIPLY
  
  END MODULE MR_MOD_OPERATOR_XUV