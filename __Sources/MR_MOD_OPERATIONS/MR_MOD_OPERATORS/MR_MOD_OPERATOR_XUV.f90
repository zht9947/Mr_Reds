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
    PUBLIC :: OPERATOR( .MRXUVTPS. ) , OPERATOR( .MRXUVIVS. ) , OPERATOR( .MRXUVMAT. )

    INTERFACE OPERATOR( .MRXUVDTM. )
      MODULE PROCEDURE MR_TENSOR_DETERMINANT_GJRD_KIND
      MODULE PROCEDURE MR_TENSOR_DETERMINANT_FDRD_KIND
    END INTERFACE

    INTERFACE OPERATOR( .MRXUVTPS. )
      MODULE PROCEDURE MR_TENSOR_TRANSPOSE_GJRD_KIND
      MODULE PROCEDURE MR_TENSOR_TRANSPOSE_FDRD_KIND
    END INTERFACE

    INTERFACE OPERATOR( .MRXUVIVS. )
      MODULE PROCEDURE MR_TENSOR_INVERSE_GJRD_KIND_X_GJRD_KIND
      MODULE PROCEDURE MR_TENSOR_INVERSE_FDRD_KIND_X_FDRD_KIND
    END INTERFACE

    INTERFACE OPERATOR( .MRXUVMAT. )
      MODULE PROCEDURE MR_TENSOR_MATRIX_MULTIPLY_GJRD_KIND_X_GJRD_KIND
      MODULE PROCEDURE MR_TENSOR_MATRIX_MULTIPLY_FDRD_KIND_X_GJRD_KIND
      MODULE PROCEDURE MR_TENSOR_MATRIX_MULTIPLY_GJRD_KIND_X_FDRD_KIND
      MODULE PROCEDURE MR_TENSOR_MATRIX_MULTIPLY_FDRD_KIND_X_FDRD_KIND
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
  FUNCTION MR_TENSOR_DETERMINANT_GJRD_KIND( XUV0 ) RESULT( XUVDTM )

    IMPLICIT NONE

    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0

    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:,:    ) :: XUVDTM

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( XUVDTM(1:SIZE(XUV0,DIM=1),1:SIZE(XUV0,DIM=2)) )

    DO J = 1 , SIZE(XUV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(XUV0,DIM=1)
        XUVDTM( I , J ) = XUV0( I , J ,1,1) * XUV0( I , J ,2,2) - XUV0( I , J ,2,1) * XUV0( I , J ,1,2)
      END DO
    END DO

  END FUNCTION MR_TENSOR_DETERMINANT_GJRD_KIND

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
  FUNCTION MR_TENSOR_DETERMINANT_FDRD_KIND( XUV0 ) RESULT( XUVDTM )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:    ) :: XUVDTM

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( XUVDTM(1:SIZE(XUV0,DIM=1),1:SIZE(XUV0,DIM=2)) )

    DO J = 1 , SIZE(XUV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(XUV0,DIM=1)
        XUVDTM( I , J ) = XUV0( I , J ,1,1) * XUV0( I , J ,2,2) - XUV0( I , J ,2,1) * XUV0( I , J ,1,2)
      END DO
    END DO

  END FUNCTION MR_TENSOR_DETERMINANT_FDRD_KIND

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
  FUNCTION MR_TENSOR_TRANSPOSE_GJRD_KIND( XUV0 ) RESULT( XUVTPS )

    IMPLICIT NONE

    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0

    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVTPS

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( XUVTPS(1:SIZE(XUV0,DIM=1),1:SIZE(XUV0,DIM=2),1:2,1:2) )

    DO J = 1 , SIZE(XUV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(XUV0,DIM=1)
        XUVTPS( I , J ,1,1) = XUV0( I , J ,1,1)
        XUVTPS( I , J ,2,1) = XUV0( I , J ,1,2)
        XUVTPS( I , J ,1,2) = XUV0( I , J ,2,1)
        XUVTPS( I , J ,2,2) = XUV0( I , J ,2,2)
      END DO
    END DO

  END FUNCTION MR_TENSOR_TRANSPOSE_GJRD_KIND

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
  FUNCTION MR_TENSOR_TRANSPOSE_FDRD_KIND( XUV0 ) RESULT( XUVTPS )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVTPS

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( XUVTPS(1:SIZE(XUV0,DIM=1),1:SIZE(XUV0,DIM=2),1:2,1:2) )

    DO J = 1 , SIZE(XUV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(XUV0,DIM=1)
        XUVTPS( I , J ,1,1) = XUV0( I , J ,1,1)
        XUVTPS( I , J ,2,1) = XUV0( I , J ,1,2)
        XUVTPS( I , J ,1,2) = XUV0( I , J ,2,1)
        XUVTPS( I , J ,2,2) = XUV0( I , J ,2,2)
      END DO
    END DO

  END FUNCTION MR_TENSOR_TRANSPOSE_FDRD_KIND

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
  FUNCTION MR_TENSOR_INVERSE_GJRD_KIND_X_GJRD_KIND( MW , XUV0 ) RESULT( XUVIVS )

    IMPLICIT NONE

    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:    ) :: MW
    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0

    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVIVS

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( XUVIVS(1:SIZE(XUV0,DIM=1),1:SIZE(XUV0,DIM=2),1:2,1:2) )

    DO J = 1 , SIZE(XUV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(XUV0,DIM=1)
        XUVIVS( I , J ,1,1) = XUV0( I , J ,2,2) / MW( I , J )
        XUVIVS( I , J ,2,1) =-XUV0( I , J ,2,1) / MW( I , J )
        XUVIVS( I , J ,1,2) =-XUV0( I , J ,1,2) / MW( I , J )
        XUVIVS( I , J ,2,2) = XUV0( I , J ,1,1) / MW( I , J )
      END DO
    END DO

  END FUNCTION MR_TENSOR_INVERSE_GJRD_KIND_X_GJRD_KIND

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
  FUNCTION MR_TENSOR_INVERSE_FDRD_KIND_X_FDRD_KIND( MW , XUV0 ) RESULT( XUVIVS )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:    ) :: MW
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVIVS

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( XUVIVS(1:SIZE(XUV0,DIM=1),1:SIZE(XUV0,DIM=2),1:2,1:2) )

    DO J = 1 , SIZE(XUV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(XUV0,DIM=1)
        XUVIVS( I , J ,1,1) = XUV0( I , J ,2,2) / MW( I , J )
        XUVIVS( I , J ,2,1) =-XUV0( I , J ,2,1) / MW( I , J )
        XUVIVS( I , J ,1,2) =-XUV0( I , J ,1,2) / MW( I , J )
        XUVIVS( I , J ,2,2) = XUV0( I , J ,1,1) / MW( I , J )
      END DO
    END DO

  END FUNCTION MR_TENSOR_INVERSE_FDRD_KIND_X_FDRD_KIND

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
  FUNCTION MR_TENSOR_MATRIX_MULTIPLY_GJRD_KIND_X_GJRD_KIND( XUV0 , XUV ) RESULT( XUVMAT )

    IMPLICIT NONE

    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0
    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV

    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVMAT

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( XUVMAT(1:SIZE(XUV0,DIM=1),1:SIZE(XUV0,DIM=2),1:2,1:2) )

    DO J = 1 , SIZE(XUV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(XUV0,DIM=1)
        XUVMAT( I , J ,1,1) = XUV0( I , J ,1,1) * XUV( I , J ,1,1) + XUV0( I , J ,1,2) * XUV( I , J ,2,1)
        XUVMAT( I , J ,2,1) = XUV0( I , J ,2,1) * XUV( I , J ,1,1) + XUV0( I , J ,2,2) * XUV( I , J ,2,1)
        XUVMAT( I , J ,1,2) = XUV0( I , J ,1,1) * XUV( I , J ,1,2) + XUV0( I , J ,1,2) * XUV( I , J ,2,2)
        XUVMAT( I , J ,2,2) = XUV0( I , J ,2,1) * XUV( I , J ,1,2) + XUV0( I , J ,2,2) * XUV( I , J ,2,2)
      END DO
    END DO

  END FUNCTION MR_TENSOR_MATRIX_MULTIPLY_GJRD_KIND_X_GJRD_KIND

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
  FUNCTION MR_TENSOR_MATRIX_MULTIPLY_FDRD_KIND_X_GJRD_KIND( XUV0 , XUV ) RESULT( XUVMAT )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0
    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVMAT

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( XUVMAT(1:SIZE(XUV0,DIM=1),1:SIZE(XUV0,DIM=2),1:2,1:2) )

    DO J = 1 , SIZE(XUV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(XUV0,DIM=1) , SIZE(XUV,DIM=1) )
        XUVMAT( I , J ,1,1) = XUV0( I , J ,1,1) * XUV( I , J ,1,1) + XUV0( I , J ,1,2) * XUV( I , J ,2,1)
        XUVMAT( I , J ,2,1) = XUV0( I , J ,2,1) * XUV( I , J ,1,1) + XUV0( I , J ,2,2) * XUV( I , J ,2,1)
        XUVMAT( I , J ,1,2) = XUV0( I , J ,1,1) * XUV( I , J ,1,2) + XUV0( I , J ,1,2) * XUV( I , J ,2,2)
        XUVMAT( I , J ,2,2) = XUV0( I , J ,2,1) * XUV( I , J ,1,2) + XUV0( I , J ,2,2) * XUV( I , J ,2,2)
      END DO
    END DO

  END FUNCTION MR_TENSOR_MATRIX_MULTIPLY_FDRD_KIND_X_GJRD_KIND

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
  FUNCTION MR_TENSOR_MATRIX_MULTIPLY_GJRD_KIND_X_FDRD_KIND( XUV , XUV0 ) RESULT( XUVMAT )

    IMPLICIT NONE

    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVMAT

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( XUVMAT(1:SIZE(XUV0,DIM=1),1:SIZE(XUV0,DIM=2),1:2,1:2) )

    DO J = 1 , SIZE(XUV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(XUV,DIM=1) , SIZE(XUV0,DIM=1) )
        XUVMAT( I , J ,1,1) = XUV( I , J ,1,1) * XUV0( I , J ,1,1) + XUV( I , J ,1,2) * XUV0( I , J ,2,1)
        XUVMAT( I , J ,2,1) = XUV( I , J ,2,1) * XUV0( I , J ,1,1) + XUV( I , J ,2,2) * XUV0( I , J ,2,1)
        XUVMAT( I , J ,1,2) = XUV( I , J ,1,1) * XUV0( I , J ,1,2) + XUV( I , J ,1,2) * XUV0( I , J ,2,2)
        XUVMAT( I , J ,2,2) = XUV( I , J ,2,1) * XUV0( I , J ,1,2) + XUV( I , J ,2,2) * XUV0( I , J ,2,2)
      END DO
    END DO

  END FUNCTION MR_TENSOR_MATRIX_MULTIPLY_GJRD_KIND_X_FDRD_KIND

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
  FUNCTION MR_TENSOR_MATRIX_MULTIPLY_FDRD_KIND_X_FDRD_KIND( XUV , XUV0 ) RESULT( XUVMAT )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVMAT

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( XUVMAT(1:SIZE(XUV0,DIM=1),1:SIZE(XUV0,DIM=2),1:2,1:2) )

    DO J = 1 , SIZE(XUV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(XUV0,DIM=1)
        XUVMAT( I , J ,1,1) = XUV( I , J ,1,1) * XUV0( I , J ,1,1) + XUV( I , J ,1,2) * XUV0( I , J ,2,1)
        XUVMAT( I , J ,2,1) = XUV( I , J ,2,1) * XUV0( I , J ,1,1) + XUV( I , J ,2,2) * XUV0( I , J ,2,1)
        XUVMAT( I , J ,1,2) = XUV( I , J ,1,1) * XUV0( I , J ,1,2) + XUV( I , J ,1,2) * XUV0( I , J ,2,2)
        XUVMAT( I , J ,2,2) = XUV( I , J ,2,1) * XUV0( I , J ,1,2) + XUV( I , J ,2,2) * XUV0( I , J ,2,2)
      END DO
    END DO

  END FUNCTION MR_TENSOR_MATRIX_MULTIPLY_FDRD_KIND_X_FDRD_KIND

  END MODULE MR_MOD_OPERATOR_XUV