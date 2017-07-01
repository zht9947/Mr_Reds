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
  MODULE MR_MOD_OPERATOR_UV

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: OPERATOR( .MRUVMTP. )
    PUBLIC :: OPERATOR( .MRUVDIV. )
    PUBLIC :: OPERATOR( .MRUVSCL. ) , OPERATOR( .MRUVTFM. ) , OPERATOR( .MRUVROT. )
    PUBLIC :: OPERATOR( .MRUVSQR. ) , OPERATOR( .MRUVDOT. )

    INTERFACE OPERATOR( .MRUVMTP. )
      MODULE PROCEDURE MR_VECTOR_MULTIPLY_BY_SS_FDRD_KIND_X_FDRD_KIND
      MODULE PROCEDURE MR_VECTOR_MULTIPLY_BY_SS_CARD_KIND_X_FDRD_KIND
    END INTERFACE

    INTERFACE OPERATOR( .MRUVDIV. )
      MODULE PROCEDURE MR_VECTOR_DIVIDE_BY_SS_FDRD_KIND_X_FDRD_KIND
      MODULE PROCEDURE MR_VECTOR_DIVIDE_BY_SS_CARD_KIND_X_FDRD_KIND
    END INTERFACE

    INTERFACE OPERATOR( .MRUVSCL. )
      MODULE PROCEDURE MR_VECTOR_SCALE_BY_MW_GJRD_KIND_X_FDRD_KIND
    END INTERFACE

    INTERFACE OPERATOR( .MRUVTFM. )
      MODULE PROCEDURE MR_VECTOR_TRANSFORM_BY_XUV_GJRD_KIND_X_FDRD_KIND
    END INTERFACE

    INTERFACE OPERATOR( .MRUVROT. )
      MODULE PROCEDURE MR_VECTOR_ROTATE_90CW_FDRD_KIND
    END INTERFACE

    INTERFACE OPERATOR( .MRUVSQR. )
      MODULE PROCEDURE MR_VECTOR_SQUARE_FDRD_KIND
    END INTERFACE

    INTERFACE OPERATOR( .MRUVDOT. )
      MODULE PROCEDURE MR_VECTOR_DOT_PRODUCT_FDRD_KIND_X_FDRD_KIND
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
  FUNCTION MR_VECTOR_MULTIPLY_BY_SS_FDRD_KIND_X_FDRD_KIND( UV0 , SS ) RESULT( UVMTP )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV0
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:  ) :: SS

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVMTP

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    ALLOCATE( UVMTP(1:SIZE(UV0,DIM=1),1:SIZE(UV0,DIM=2),1:2) )

    DO DIM = 1 , 2

      DO J = 1 , SIZE(UV0,DIM=2)
       !DIR$ VECTOR ALIGNED
        DO I = 1 , SIZE(UV0,DIM=1)
          UVMTP( I , J ,DIM) = UV0( I , J ,DIM) * SS( I , J )
        END DO
      END DO

    END DO

  END FUNCTION MR_VECTOR_MULTIPLY_BY_SS_FDRD_KIND_X_FDRD_KIND

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
  FUNCTION MR_VECTOR_MULTIPLY_BY_SS_CARD_KIND_X_FDRD_KIND( UV0 , SS ) RESULT( UVMTP )

    IMPLICIT NONE

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV0
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:  ) :: SS

    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVMTP

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    ALLOCATE( UVMTP(1:SIZE(UV0,DIM=1),1:SIZE(UV0,DIM=2),1:2) )

    DO DIM = 1 , 2

      DO J = 1 , SIZE(UV0,DIM=2)
       !DIR$ VECTOR ALIGNED
        DO I = 1 , MIN( SIZE(UV0,DIM=1) , SIZE(SS,DIM=1) )
          UVMTP( I , J ,DIM) = UV0( I , J ,DIM) * SS( I , J )
        END DO
      END DO

    END DO

  END FUNCTION MR_VECTOR_MULTIPLY_BY_SS_CARD_KIND_X_FDRD_KIND

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
  FUNCTION MR_VECTOR_DIVIDE_BY_SS_FDRD_KIND_X_FDRD_KIND( UV0 , SS ) RESULT( UVDIV )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV0
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:  ) :: SS

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVDIV

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    ALLOCATE( UVDIV(1:SIZE(UV0,DIM=1),1:SIZE(UV0,DIM=2),1:2) )

    DO DIM = 1 , 2

      DO J = 1 , SIZE(UV0,DIM=2)
       !DIR$ VECTOR ALIGNED
        DO I = 1 , SIZE(UV0,DIM=1)
          UVDIV( I , J ,DIM) = UV0( I , J ,DIM) / SS( I , J )
        END DO
      END DO

    END DO

  END FUNCTION MR_VECTOR_DIVIDE_BY_SS_FDRD_KIND_X_FDRD_KIND

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
  FUNCTION MR_VECTOR_DIVIDE_BY_SS_CARD_KIND_X_FDRD_KIND( UV0 , SS ) RESULT( UVDIV )

    IMPLICIT NONE

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV0
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:  ) :: SS

    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVDIV

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    ALLOCATE( UVDIV(1:SIZE(UV0,DIM=1),1:SIZE(UV0,DIM=2),1:2) )

    DO DIM = 1 , 2

      DO J = 1 , SIZE(UV0,DIM=2)
       !DIR$ VECTOR ALIGNED
        DO I = 1 , MIN( SIZE(UV0,DIM=1) , SIZE(SS,DIM=1) )
          UVDIV( I , J ,DIM) = UV0( I , J ,DIM) / SS( I , J )
        END DO
      END DO

    END DO

  END FUNCTION MR_VECTOR_DIVIDE_BY_SS_CARD_KIND_X_FDRD_KIND

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
  FUNCTION MR_VECTOR_SCALE_BY_MW_GJRD_KIND_X_FDRD_KIND( MW , UV0 ) RESULT( UVSCL )

    IMPLICIT NONE

    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:  ) :: MW
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV0

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVSCL

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    ALLOCATE( UVSCL(1:SIZE(UV0,DIM=1),1:SIZE(UV0,DIM=2),1:2) )

    DO DIM = 1 , 2

      DO J = 1 , SIZE(UV0,DIM=2)
       !DIR$ VECTOR ALIGNED
        DO I = 1 , MIN( SIZE(MW,DIM=1) , SIZE(UV0,DIM=1) )
          UVSCL( I , J ,DIM) = MW( I , J ) * UV0( I , J ,DIM)
        END DO
      END DO

    END DO

  END FUNCTION MR_VECTOR_SCALE_BY_MW_GJRD_KIND_X_FDRD_KIND

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
  FUNCTION MR_VECTOR_TRANSFORM_BY_XUV_GJRD_KIND_X_FDRD_KIND( XUV , UV0 ) RESULT( UVTFM )

    IMPLICIT NONE

    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:  ) :: UV0

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:  ) :: UVTFM

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    ALLOCATE( UVTFM(1:SIZE(UV0,DIM=1),1:SIZE(UV0,DIM=2),1:2) )

    DO DIM = 1 , 2

      DO J = 1 , SIZE(UV0,DIM=2)
       !DIR$ VECTOR ALIGNED
        DO I = 1 , MIN( SIZE(XUV,DIM=1) , SIZE(UV0,DIM=1) )
          UVTFM( I , J ,DIM) = XUV( I , J ,DIM,1) * UV0( I , J ,1) + XUV( I , J ,DIM,2) * UV0( I , J ,2)
        END DO
      END DO

    END DO

  END FUNCTION MR_VECTOR_TRANSFORM_BY_XUV_GJRD_KIND_X_FDRD_KIND

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
!   2015-06-10    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_VECTOR_ROTATE_90CW_FDRD_KIND( UV0 ) RESULT( UVROT )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV0

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVROT

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( UVROT(1:SIZE(UV0,DIM=1),1:SIZE(UV0,DIM=2),1:2) )

    DO J = 1 , SIZE(UV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(UV0,DIM=1)
        UVROT( I , J ,1) =+UV0( I , J ,2)
        UVROT( I , J ,2) =-UV0( I , J ,1)
      END DO
    END DO

  END FUNCTION MR_VECTOR_ROTATE_90CW_FDRD_KIND

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
!   2015-06-10    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_VECTOR_SQUARE_FDRD_KIND( UV0 ) RESULT( UVSQR )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV0

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:  ) :: UVSQR

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( UVSQR(1:SIZE(UV0,DIM=1),1:SIZE(UV0,DIM=2)) )

    DO J = 1 , SIZE(UV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(UV0,DIM=1)
        UVSQR( I , J ) = UV0( I , J ,1) * UV0( I , J ,1) + UV0( I , J ,2) * UV0( I , J ,2)
      END DO
    END DO

  END FUNCTION MR_VECTOR_SQUARE_FDRD_KIND

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
!   2015-06-10    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_VECTOR_DOT_PRODUCT_FDRD_KIND_X_FDRD_KIND( UV0 , UV ) RESULT( UVDOT )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV0
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:  ) :: UVDOT

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( UVDOT(1:SIZE(UV0,DIM=1),1:SIZE(UV0,DIM=2)) )

    DO J = 1 , SIZE(UV0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(UV0,DIM=1)
        UVDOT( I , J ) = UV0( I , J ,1) * UV( I , J ,1) + UV0( I , J ,2) * UV( I , J ,2)
      END DO
    END DO

  END FUNCTION MR_VECTOR_DOT_PRODUCT_FDRD_KIND_X_FDRD_KIND

  END MODULE MR_MOD_OPERATOR_UV