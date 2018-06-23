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
      MODULE PROCEDURE MR_TENSOR_DETERMINANT_KIND4
      MODULE PROCEDURE MR_TENSOR_DETERMINANT_KIND8
    END INTERFACE

    INTERFACE OPERATOR( .MRXUVTPS. )
      MODULE PROCEDURE MR_TENSOR_TRANSPOSE_KIND4
      MODULE PROCEDURE MR_TENSOR_TRANSPOSE_KIND8
    END INTERFACE

    INTERFACE OPERATOR( .MRXUVIVS. )
      MODULE PROCEDURE MR_TENSOR_INVERSE_KIND4_X_KIND4
      MODULE PROCEDURE MR_TENSOR_INVERSE_KIND8_X_KIND4
      MODULE PROCEDURE MR_TENSOR_INVERSE_KIND4_X_KIND8
      MODULE PROCEDURE MR_TENSOR_INVERSE_KIND8_X_KIND8
    END INTERFACE

    INTERFACE OPERATOR( .MRXUVMAT. )
      MODULE PROCEDURE MR_TENSOR_MATRIX_MULTIPLY_KIND4_X_KIND4
      MODULE PROCEDURE MR_TENSOR_MATRIX_MULTIPLY_KIND8_X_KIND4
      MODULE PROCEDURE MR_TENSOR_MATRIX_MULTIPLY_KIND4_X_KIND8
      MODULE PROCEDURE MR_TENSOR_MATRIX_MULTIPLY_KIND8_X_KIND8
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
  FUNCTION MR_TENSOR_DETERMINANT_KIND4( XUV0 ) RESULT( XUVDTM )

    IMPLICIT NONE

    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0

    REAL   (4)         , ALLOCATABLE , DIMENSION(:,:    ) :: XUVDTM

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( XUVDTM(1:SIZE(XUV0,DIM=1),1:SIZE(XUV0,DIM=2)) )

    DO J = 1 , SIZE(XUV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(XUV0,DIM=1)
        XUVDTM( I , J ) = XUV0( I , J ,1,1) * XUV0( I , J ,2,2) - XUV0( I , J ,2,1) * XUV0( I , J ,1,2)
      END DO
    END DO

  END FUNCTION MR_TENSOR_DETERMINANT_KIND4

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
  FUNCTION MR_TENSOR_DETERMINANT_KIND8( XUV0 ) RESULT( XUVDTM )

    IMPLICIT NONE

    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0

    REAL   (8)         , ALLOCATABLE , DIMENSION(:,:    ) :: XUVDTM

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( XUVDTM(1:SIZE(XUV0,DIM=1),1:SIZE(XUV0,DIM=2)) )

    DO J = 1 , SIZE(XUV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(XUV0,DIM=1)
        XUVDTM( I , J ) = XUV0( I , J ,1,1) * XUV0( I , J ,2,2) - XUV0( I , J ,2,1) * XUV0( I , J ,1,2)
      END DO
    END DO

  END FUNCTION MR_TENSOR_DETERMINANT_KIND8

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
  FUNCTION MR_TENSOR_TRANSPOSE_KIND4( XUV0 ) RESULT( XUVTPS )

    IMPLICIT NONE

    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0

    REAL   (4)         , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVTPS

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

  END FUNCTION MR_TENSOR_TRANSPOSE_KIND4

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
  FUNCTION MR_TENSOR_TRANSPOSE_KIND8( XUV0 ) RESULT( XUVTPS )

    IMPLICIT NONE

    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0

    REAL   (8)         , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVTPS

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

  END FUNCTION MR_TENSOR_TRANSPOSE_KIND8

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
  FUNCTION MR_TENSOR_INVERSE_KIND4_X_KIND4( MW , XUV0 ) RESULT( XUVIVS )

    IMPLICIT NONE

    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:    ) :: MW
    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0

    REAL   (4)         , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVIVS

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( XUVIVS(1:SIZE(XUV0,DIM=1),1:SIZE(XUV0,DIM=2),1:2,1:2) )

    DO J = 1 , SIZE(XUV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(MW,DIM=1) , SIZE(XUV0,DIM=1) )
        XUVIVS( I , J ,1,1) = XUV0( I , J ,2,2) / MW( I , J )
        XUVIVS( I , J ,2,1) =-XUV0( I , J ,2,1) / MW( I , J )
        XUVIVS( I , J ,1,2) =-XUV0( I , J ,1,2) / MW( I , J )
        XUVIVS( I , J ,2,2) = XUV0( I , J ,1,1) / MW( I , J )
      END DO
    END DO

  END FUNCTION MR_TENSOR_INVERSE_KIND4_X_KIND4

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
  FUNCTION MR_TENSOR_INVERSE_KIND8_X_KIND4( MW , XUV0 ) RESULT( XUVIVS )

    IMPLICIT NONE

    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:    ) :: MW
    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0

    REAL   (4)         , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVIVS

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( XUVIVS(1:SIZE(XUV0,DIM=1),1:SIZE(XUV0,DIM=2),1:2,1:2) )

    DO J = 1 , SIZE(XUV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(MW,DIM=1) , SIZE(XUV0,DIM=1) )
        XUVIVS( I , J ,1,1) = XUV0( I , J ,2,2) / MW( I , J )
        XUVIVS( I , J ,2,1) =-XUV0( I , J ,2,1) / MW( I , J )
        XUVIVS( I , J ,1,2) =-XUV0( I , J ,1,2) / MW( I , J )
        XUVIVS( I , J ,2,2) = XUV0( I , J ,1,1) / MW( I , J )
      END DO
    END DO

  END FUNCTION MR_TENSOR_INVERSE_KIND8_X_KIND4

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
  FUNCTION MR_TENSOR_INVERSE_KIND4_X_KIND8( MW , XUV0 ) RESULT( XUVIVS )

    IMPLICIT NONE

    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:    ) :: MW
    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0

    REAL   (8)         , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVIVS

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( XUVIVS(1:SIZE(XUV0,DIM=1),1:SIZE(XUV0,DIM=2),1:2,1:2) )

    DO J = 1 , SIZE(XUV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(MW,DIM=1) , SIZE(XUV0,DIM=1) )
        XUVIVS( I , J ,1,1) = XUV0( I , J ,2,2) / MW( I , J )
        XUVIVS( I , J ,2,1) =-XUV0( I , J ,2,1) / MW( I , J )
        XUVIVS( I , J ,1,2) =-XUV0( I , J ,1,2) / MW( I , J )
        XUVIVS( I , J ,2,2) = XUV0( I , J ,1,1) / MW( I , J )
      END DO
    END DO

  END FUNCTION MR_TENSOR_INVERSE_KIND4_X_KIND8

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
  FUNCTION MR_TENSOR_INVERSE_KIND8_X_KIND8( MW , XUV0 ) RESULT( XUVIVS )

    IMPLICIT NONE

    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:    ) :: MW
    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0

    REAL   (8)         , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVIVS

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( XUVIVS(1:SIZE(XUV0,DIM=1),1:SIZE(XUV0,DIM=2),1:2,1:2) )

    DO J = 1 , SIZE(XUV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(MW,DIM=1) , SIZE(XUV0,DIM=1) )
        XUVIVS( I , J ,1,1) = XUV0( I , J ,2,2) / MW( I , J )
        XUVIVS( I , J ,2,1) =-XUV0( I , J ,2,1) / MW( I , J )
        XUVIVS( I , J ,1,2) =-XUV0( I , J ,1,2) / MW( I , J )
        XUVIVS( I , J ,2,2) = XUV0( I , J ,1,1) / MW( I , J )
      END DO
    END DO

  END FUNCTION MR_TENSOR_INVERSE_KIND8_X_KIND8

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
  FUNCTION MR_TENSOR_MATRIX_MULTIPLY_KIND4_X_KIND4( XUV0 , XUV ) RESULT( XUVMAT )

    IMPLICIT NONE

    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0
    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV

    REAL   (4)         , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVMAT

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

  END FUNCTION MR_TENSOR_MATRIX_MULTIPLY_KIND4_X_KIND4

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
  FUNCTION MR_TENSOR_MATRIX_MULTIPLY_KIND8_X_KIND4( XUV0 , XUV ) RESULT( XUVMAT )

    IMPLICIT NONE

    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0
    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV

    REAL   (8)         , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVMAT

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

  END FUNCTION MR_TENSOR_MATRIX_MULTIPLY_KIND8_X_KIND4

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
  FUNCTION MR_TENSOR_MATRIX_MULTIPLY_KIND4_X_KIND8( XUV0 , XUV ) RESULT( XUVMAT )

    IMPLICIT NONE

    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0
    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV

    REAL   (4)         , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVMAT

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

  END FUNCTION MR_TENSOR_MATRIX_MULTIPLY_KIND4_X_KIND8

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
  FUNCTION MR_TENSOR_MATRIX_MULTIPLY_KIND8_X_KIND8( XUV0 , XUV ) RESULT( XUVMAT )

    IMPLICIT NONE

    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV0
    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV

    REAL   (8)         , ALLOCATABLE , DIMENSION(:,:,:,:) :: XUVMAT

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

  END FUNCTION MR_TENSOR_MATRIX_MULTIPLY_KIND8_X_KIND8

  END MODULE MR_MOD_OPERATOR_XUV