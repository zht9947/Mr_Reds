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
  MODULE MR_MOD_OPERATOR_SS

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: OPERATOR( .MRSSMTP. )
    PUBLIC :: OPERATOR( .MRSSDIV. )
    PUBLIC :: OPERATOR( .MRSSSCL. )
    PUBLIC :: OPERATOR( .MRSSSQR. ) , OPERATOR( .MRSSQRT. )

    INTERFACE OPERATOR( .MRSSMTP. )
      MODULE PROCEDURE MR_SCALAR_MULTIPLY_BY_SS_FDRD_KIND_X_FDRD_KIND
      MODULE PROCEDURE MR_SCALAR_MULTIPLY_BY_SS_CARD_KIND_X_FDRD_KIND
    END INTERFACE

    INTERFACE OPERATOR( .MRSSDIV. )
      MODULE PROCEDURE MR_SCALAR_DIVIDE_BY_SS_FDRD_KIND_X_FDRD_KIND
      MODULE PROCEDURE MR_SCALAR_DIVIDE_BY_SS_CARD_KIND_X_FDRD_KIND
    END INTERFACE

    INTERFACE OPERATOR( .MRSSSCL. )
      MODULE PROCEDURE MR_SCALAR_SCALE_BY_MW_GJRD_KIND_X_FDRD_KIND
    END INTERFACE

    INTERFACE OPERATOR( .MRSSSQR. )
      MODULE PROCEDURE MR_SCALAR_SQUARE_FDRD_KIND
    END INTERFACE

    INTERFACE OPERATOR( .MRSSQRT. )
      MODULE PROCEDURE MR_SCALAR_SQUARE_ROOT_FDRD_KIND
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
  FUNCTION MR_SCALAR_MULTIPLY_BY_SS_FDRD_KIND_X_FDRD_KIND( SS0 , SS ) RESULT( SSMTP )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: SS0
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: SS

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:) :: SSMTP

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSMTP(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(SS0,DIM=1)
        SSMTP( I , J ) = SS0( I , J ) * SS( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_MULTIPLY_BY_SS_FDRD_KIND_X_FDRD_KIND

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
  FUNCTION MR_SCALAR_MULTIPLY_BY_SS_CARD_KIND_X_FDRD_KIND( SS0 , SS ) RESULT( SSMTP )

    IMPLICIT NONE

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: SS0
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: SS

    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:,:) :: SSMTP

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSMTP(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(SS0,DIM=1) , SIZE(SS,DIM=1) )
        SSMTP( I , J ) = SS0( I , J ) * SS( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_MULTIPLY_BY_SS_CARD_KIND_X_FDRD_KIND

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
  FUNCTION MR_SCALAR_DIVIDE_BY_SS_FDRD_KIND_X_FDRD_KIND( SS0 , SS ) RESULT( SSDIV )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: SS0
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: SS

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:) :: SSDIV

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSDIV(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(SS0,DIM=1)
        SSDIV( I , J ) = SS0( I , J ) / SS( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_DIVIDE_BY_SS_FDRD_KIND_X_FDRD_KIND

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
  FUNCTION MR_SCALAR_DIVIDE_BY_SS_CARD_KIND_X_FDRD_KIND( SS0 , SS ) RESULT( SSDIV )

    IMPLICIT NONE

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: SS0
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: SS

    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:,:) :: SSDIV

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSDIV(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(SS0,DIM=1) , SIZE(SS,DIM=1) )
        SSDIV( I , J ) = SS0( I , J ) / SS( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_DIVIDE_BY_SS_CARD_KIND_X_FDRD_KIND

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
  FUNCTION MR_SCALAR_SCALE_BY_MW_GJRD_KIND_X_FDRD_KIND( MW , SS0 ) RESULT( SSSCL )

    IMPLICIT NONE

    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: MW
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: SS0

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:) :: SSSCL

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSSCL(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(MW,DIM=1) , SIZE(SS0,DIM=1) )
        SSSCL( I , J ) = MW( I , J ) * SS0( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_SCALE_BY_MW_GJRD_KIND_X_FDRD_KIND

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
  FUNCTION MR_SCALAR_SQUARE_FDRD_KIND( SS0 ) RESULT( SSSQR )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: SS0

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:) :: SSSQR

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSSQR(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(SS0,DIM=1)
        SSSQR( I , J ) = SS0( I , J ) * SS0( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_SQUARE_FDRD_KIND

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
  FUNCTION MR_SCALAR_SQUARE_ROOT_FDRD_KIND( SS0 ) RESULT( SSQRT )

    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(:,:) :: SS0

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:) :: SSQRT

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSQRT(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(SS0,DIM=1)
        SSQRT( I , J ) = SQRT( SS0( I , J ) )
      END DO
    END DO

  END FUNCTION MR_SCALAR_SQUARE_ROOT_FDRD_KIND

  END MODULE MR_MOD_OPERATOR_SS