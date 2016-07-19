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
    
    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:,:) :: XUVDTM
    
    ALLOCATE( XUVDTM , SOURCE = XUV(:,:,1,1) )
    
    XUVDTM(:,:) = XUV(:,:,1,1) * XUV(:,:,2,2) - XUV(:,:,2,1) * XUV(:,:,1,2)
    
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
    
    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: MW
    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV
    
    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVIVS
    
    ALLOCATE( XUVIVS , SOURCE = XUV )
    
    XUVIVS(:,:,1,1) = XUV(:,:,2,2) / MW(:,:)
    XUVIVS(:,:,2,1) =-XUV(:,:,2,1) / MW(:,:)
    XUVIVS(:,:,1,2) =-XUV(:,:,1,2) / MW(:,:)
    XUVIVS(:,:,2,2) = XUV(:,:,1,1) / MW(:,:)
    
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
    
    ALLOCATE( XUVTPS , SOURCE = XUV )
    
    XUVTPS(:,:,1,1) = XUV(:,:,1,1)
    XUVTPS(:,:,2,1) = XUV(:,:,1,2)
    XUVTPS(:,:,1,2) = XUV(:,:,2,1)
    XUVTPS(:,:,2,2) = XUV(:,:,2,2)
    
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
    
    ALLOCATE( XUVMAT , SOURCE = XUV1 )
    
    XUVMAT(:,:,1,1) = XUV1(:,:,1,1) * XUV2(:,:,1,1) + XUV1(:,:,1,2) * XUV2(:,:,2,1)
    XUVMAT(:,:,2,1) = XUV1(:,:,2,1) * XUV2(:,:,1,1) + XUV1(:,:,2,2) * XUV2(:,:,2,1)
    XUVMAT(:,:,1,2) = XUV1(:,:,1,1) * XUV2(:,:,1,2) + XUV1(:,:,1,2) * XUV2(:,:,2,2)
    XUVMAT(:,:,2,2) = XUV1(:,:,2,1) * XUV2(:,:,1,2) + XUV1(:,:,2,2) * XUV2(:,:,2,2)
    
  END FUNCTION MR_TENSOR_MATRIX_MULTIPLY
  
  END MODULE MR_MOD_OPERATOR_XUV