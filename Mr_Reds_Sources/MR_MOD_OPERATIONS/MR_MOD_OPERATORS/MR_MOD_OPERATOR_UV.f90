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
      MODULE PROCEDURE MR_VECTOR_MULTIPLY_BY_SS
      MODULE PROCEDURE MR_VECTOR_MULTIPLY_BY_SS_CARD_KIND
    END INTERFACE

    INTERFACE OPERATOR( .MRUVDIV. )
      MODULE PROCEDURE MR_VECTOR_DIVIDE_BY_SS
      MODULE PROCEDURE MR_VECTOR_DIVIDE_BY_SS_CARD_KIND
    END INTERFACE

    INTERFACE OPERATOR( .MRUVSCL. )
      MODULE PROCEDURE MR_VECTOR_SCALE_BY_MW
    END INTERFACE

    INTERFACE OPERATOR( .MRUVTFM. )
      MODULE PROCEDURE MR_VECTOR_TRANSFORM_BY_XUV
    END INTERFACE

    INTERFACE OPERATOR( .MRUVROT. )
      MODULE PROCEDURE MR_VECTOR_ROTATE_90CW
    END INTERFACE

    INTERFACE OPERATOR( .MRUVSQR. )
      MODULE PROCEDURE MR_VECTOR_SQUARE
    END INTERFACE

    INTERFACE OPERATOR( .MRUVDOT. )
      MODULE PROCEDURE MR_VECTOR_DOT_PRODUCT
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
  FUNCTION MR_VECTOR_MULTIPLY_BY_SS( UV , SS ) RESULT( UVMTP )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:  ) :: SS

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVMTP

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    
    ALLOCATE( UVMTP(1:SIZE(UV,DIM=1),1:SIZE(UV,DIM=2),1:2) )

    DO DIM = 1 , 2
    
      DO J = 1 , SIZE(UV,DIM=2)
        DO I = 1 , SIZE(UV,DIM=1)
          UVMTP( I , J ,DIM) = UV( I , J ,DIM) * SS( I , J )
        END DO
      END DO
      
    END DO

  END FUNCTION MR_VECTOR_MULTIPLY_BY_SS

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
  FUNCTION MR_VECTOR_MULTIPLY_BY_SS_CARD_KIND( UV , SS ) RESULT( UVMTP )

    IMPLICIT NONE

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:  ) :: SS

    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVMTP

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    
    ALLOCATE( UVMTP(1:SIZE(UV,DIM=1),1:SIZE(UV,DIM=2),1:2) )

    DO DIM = 1 , 2
    
      DO J = 1 , SIZE(UV,DIM=2)
        DO I = 1 , MIN( SIZE(UV,DIM=1) , SIZE(SS,DIM=1) )
          UVMTP( I , J ,DIM) = UV( I , J ,DIM) * SS( I , J )
        END DO
      END DO
      
    END DO

  END FUNCTION MR_VECTOR_MULTIPLY_BY_SS_CARD_KIND

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
  FUNCTION MR_VECTOR_DIVIDE_BY_SS( UV , SS ) RESULT( UVDIV )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:  ) :: SS

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVDIV

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    ALLOCATE( UVDIV(1:SIZE(UV,DIM=1),1:SIZE(UV,DIM=2),1:2) )

    DO DIM = 1 , 2
    
      DO J = 1 , SIZE(UV,DIM=2)
        DO I = 1 , SIZE(UV,DIM=1)
          UVDIV( I , J ,DIM) = UV( I , J ,DIM) / SS( I , J )
        END DO
      END DO
      
    END DO

  END FUNCTION MR_VECTOR_DIVIDE_BY_SS

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
  FUNCTION MR_VECTOR_DIVIDE_BY_SS_CARD_KIND( UV , SS ) RESULT( UVDIV )

    IMPLICIT NONE

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:  ) :: SS

    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVDIV

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    ALLOCATE( UVDIV(1:SIZE(UV,DIM=1),1:SIZE(UV,DIM=2),1:2) )

    DO DIM = 1 , 2
    
      DO J = 1 , SIZE(UV,DIM=2)
        DO I = 1 , MIN( SIZE(UV,DIM=1) , SIZE(SS,DIM=1) )
          UVDIV( I , J ,DIM) = UV( I , J ,DIM) / SS( I , J )
        END DO
      END DO
      
    END DO

  END FUNCTION MR_VECTOR_DIVIDE_BY_SS_CARD_KIND

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
  FUNCTION MR_VECTOR_SCALE_BY_MW( MW , UV ) RESULT( UVSCL )

    IMPLICIT NONE

    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:  ) :: MW
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVSCL

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    
    ALLOCATE( UVSCL(1:SIZE(UV,DIM=1),1:SIZE(UV,DIM=2),1:2) )

    DO DIM = 1 , 2
    
      DO J = 1 , SIZE(UV,DIM=2)
        DO I = 1 , MIN( SIZE(MW,DIM=1) , SIZE(UV,DIM=1) )
          UVSCL( I , J ,DIM) = MW( I , J ) * UV( I , J ,DIM)
        END DO
      END DO
      
    END DO

  END FUNCTION MR_VECTOR_SCALE_BY_MW

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
  FUNCTION MR_VECTOR_TRANSFORM_BY_XUV( XUV , UV ) RESULT( UVTFM )

    IMPLICIT NONE

    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:,:) :: XUV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:  ) :: UV

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:  ) :: UVTFM

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    
    ALLOCATE( UVTFM(1:SIZE(UV,DIM=1),1:SIZE(UV,DIM=2),1:2) )

    DO DIM = 1 , 2

      DO J = 1 , SIZE(UV,DIM=2)
        DO I = 1 , MIN( SIZE(XUV,DIM=1) , SIZE(UV,DIM=1) )
          UVTFM( I , J ,DIM) = XUV( I , J ,DIM,1) * UV( I , J ,1) + XUV( I , J ,DIM,2) * UV( I , J ,2)
        END DO
      END DO

    END DO

  END FUNCTION MR_VECTOR_TRANSFORM_BY_XUV

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
  FUNCTION MR_VECTOR_ROTATE_90CW( UV ) RESULT( UVROT )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVROT
    
    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( UVROT(1:SIZE(UV,DIM=1),1:SIZE(UV,DIM=2),1:2) )

    DO J = 1 , SIZE(UV,DIM=2)
      DO I = 1 , SIZE(UV,DIM=1)
        UVROT( I , J ,1) =+UV( I , J ,2)
        UVROT( I , J ,2) =-UV( I , J ,1)
      END DO
    END DO

  END FUNCTION MR_VECTOR_ROTATE_90CW

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
  FUNCTION MR_VECTOR_SQUARE( UV ) RESULT( UVSQR )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:  ) :: UVSQR
    
    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( UVSQR(1:SIZE(UV,DIM=1),1:SIZE(UV,DIM=2)) )

    DO J = 1 , SIZE(UV,DIM=2)
      DO I = 1 , SIZE(UV,DIM=1)
        UVSQR( I , J ) = UV( I , J ,1) * UV( I , J ,1) + UV( I , J ,2) * UV( I , J ,2)
      END DO
    END DO

  END FUNCTION MR_VECTOR_SQUARE

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
  FUNCTION MR_VECTOR_DOT_PRODUCT( UV1 , UV2 ) RESULT( UVDOT )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV1 , UV2

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:  ) :: UVDOT
    
    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( UVDOT(1:SIZE(UV1,DIM=1),1:SIZE(UV1,DIM=2)) )

    DO J = 1 , SIZE(UV1,DIM=2)
      DO I = 1 , SIZE(UV1,DIM=1)
        UVDOT( I , J ) = UV1( I , J ,1) * UV2( I , J ,1) + UV1( I , J ,2) * UV2( I , J ,2)
      END DO
    END DO

  END FUNCTION MR_VECTOR_DOT_PRODUCT

  END MODULE MR_MOD_OPERATOR_UV