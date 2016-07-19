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
      MODULE PROCEDURE MR_VECTOR_SCALE_BY_MUV
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
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: SS

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVMTP

    INTEGER :: DIM

    ALLOCATE( UVMTP , SOURCE = UV )

    DO DIM = 1 , 2

      UVMTP(:,:,DIM) = UV(:,:,DIM) * SS(:,:)

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
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: SS

    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVMTP

    INTEGER :: DIM

    ALLOCATE( UVMTP , SOURCE = UV )

    DO DIM = 1 , 2

      UVMTP(:,:,DIM) = UV(:,:,DIM) * SS(:,:)

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
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: SS

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVDIV

    INTEGER :: DIM

    ALLOCATE( UVDIV , SOURCE = UV )

    DO DIM = 1 , 2

      UVDIV(:,:,DIM) = UV(:,:,DIM) / SS(:,:)

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
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: SS

    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVDIV

    INTEGER :: DIM

    ALLOCATE( UVDIV , SOURCE = UV )

    DO DIM = 1 , 2

      UVDIV(:,:,DIM) = UV(:,:,DIM) / SS(:,:)

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
  FUNCTION MR_VECTOR_SCALE_BY_MUV( MW , UV ) RESULT( UVSCL )

    IMPLICIT NONE

    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: MW
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVSCL

    INTEGER :: DIM

    ALLOCATE( UVSCL , SOURCE = UV )

    DO DIM = 1 , 2

      UVSCL(:,:,DIM) = MW(:,:) * UV(:,:,DIM)

    END DO

  END FUNCTION MR_VECTOR_SCALE_BY_MUV

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
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:,:) :: UV

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:) :: UVTFM

    INTEGER :: DIM

    ALLOCATE( UVTFM , SOURCE = UV )

    DO DIM = 1 , 2

      UVTFM(:,:,DIM) = XUV(:,:,DIM,1) * UV(:,:,1) + XUV(:,:,DIM,2) * UV(:,:,2)

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

    ALLOCATE( UVROT , SOURCE = UV )

    UVROT(:,:,1) = + UV(:,:,2)
    UVROT(:,:,2) = - UV(:,:,1)

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

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:) :: UVSQR

    ALLOCATE( UVSQR , SOURCE = UV(:,:,1) )

    UVSQR(:,:) = UV(:,:,1) * UV(:,:,1) + UV(:,:,2) * UV(:,:,2)

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

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:) :: UVDOT

    ALLOCATE( UVDOT , SOURCE = UV1(:,:,1) )

    UVDOT(:,:) = UV1(:,:,1) * UV2(:,:,1) + UV1(:,:,2) * UV2(:,:,2)

  END FUNCTION MR_VECTOR_DOT_PRODUCT

  END MODULE MR_MOD_OPERATOR_UV