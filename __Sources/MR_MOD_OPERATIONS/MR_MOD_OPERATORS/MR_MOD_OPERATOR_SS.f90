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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_OPERATOR_SS

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_COPY_SS
    PUBLIC :: OPERATOR( .MRSSMTP. )
    PUBLIC :: OPERATOR( .MRSSDIV. )
    PUBLIC :: OPERATOR( .MRSSSCL. )
    PUBLIC :: OPERATOR( .MRSSSQR. ) , OPERATOR( .MRSSQRT. )

    INTERFACE MR_COPY_SS
      MODULE PROCEDURE MR_COPY_SS_KIND4_X_KIND4
      MODULE PROCEDURE MR_COPY_SS_KIND8_X_KIND4
      MODULE PROCEDURE MR_COPY_SS_KIND4_X_KIND8
      MODULE PROCEDURE MR_COPY_SS_KIND8_X_KIND8
    END INTERFACE

    INTERFACE OPERATOR( .MRSSMTP. )
      MODULE PROCEDURE MR_SCALAR_MULTIPLY_BY_SS_KIND4_X_KIND4
      MODULE PROCEDURE MR_SCALAR_MULTIPLY_BY_SS_KIND8_X_KIND4
      MODULE PROCEDURE MR_SCALAR_MULTIPLY_BY_SS_KIND4_X_KIND8
      MODULE PROCEDURE MR_SCALAR_MULTIPLY_BY_SS_KIND8_X_KIND8
    END INTERFACE

    INTERFACE OPERATOR( .MRSSDIV. )
      MODULE PROCEDURE MR_SCALAR_DIVIDE_BY_SS_KIND4_X_KIND4
      MODULE PROCEDURE MR_SCALAR_DIVIDE_BY_SS_KIND8_X_KIND4
      MODULE PROCEDURE MR_SCALAR_DIVIDE_BY_SS_KIND4_X_KIND8
      MODULE PROCEDURE MR_SCALAR_DIVIDE_BY_SS_KIND8_X_KIND8
    END INTERFACE

    INTERFACE OPERATOR( .MRSSSCL. )
      MODULE PROCEDURE MR_SCALAR_SCALE_BY_MW_KIND4_X_KIND4
      MODULE PROCEDURE MR_SCALAR_SCALE_BY_MW_KIND8_X_KIND4
      MODULE PROCEDURE MR_SCALAR_SCALE_BY_MW_KIND4_X_KIND8
      MODULE PROCEDURE MR_SCALAR_SCALE_BY_MW_KIND8_X_KIND8
    END INTERFACE

    INTERFACE OPERATOR( .MRSSSQR. )
      MODULE PROCEDURE MR_SCALAR_SQUARE_KIND4
      MODULE PROCEDURE MR_SCALAR_SQUARE_KIND8
    END INTERFACE

    INTERFACE OPERATOR( .MRSSQRT. )
      MODULE PROCEDURE MR_SCALAR_SQUARE_ROOT_KIND4
      MODULE PROCEDURE MR_SCALAR_SQUARE_ROOT_KIND8
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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_COPY_SS_KIND4_X_KIND4( SS , SS0 )

    IMPLICIT NONE

    REAL   (4)         , INTENT(OUT) , DIMENSION(:,:) :: SS
    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:) :: SS0

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(SS0,DIM=1) , SIZE(SS,DIM=1) )
        SS( I , J ) = SS0( I , J )
      END DO
    END DO

  END SUBROUTINE MR_COPY_SS_KIND4_X_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_COPY_SS_KIND8_X_KIND4( SS , SS0 )

    IMPLICIT NONE

    REAL   (8)         , INTENT(OUT) , DIMENSION(:,:) :: SS
    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:) :: SS0

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(SS0,DIM=1) , SIZE(SS,DIM=1) )
        SS( I , J ) = SS0( I , J )
      END DO
    END DO

  END SUBROUTINE MR_COPY_SS_KIND8_X_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_COPY_SS_KIND4_X_KIND8( SS , SS0 )

    IMPLICIT NONE

    REAL   (4)         , INTENT(OUT) , DIMENSION(:,:) :: SS
    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:) :: SS0

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(SS0,DIM=1) , SIZE(SS,DIM=1) )
        SS( I , J ) = SS0( I , J )
      END DO
    END DO

  END SUBROUTINE MR_COPY_SS_KIND4_X_KIND8

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_COPY_SS_KIND8_X_KIND8( SS , SS0 )

    IMPLICIT NONE

    REAL   (8)         , INTENT(OUT) , DIMENSION(:,:) :: SS
    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:) :: SS0

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(SS0,DIM=1) , SIZE(SS,DIM=1) )
        SS( I , J ) = SS0( I , J )
      END DO
    END DO

  END SUBROUTINE MR_COPY_SS_KIND8_X_KIND8

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_SCALAR_MULTIPLY_BY_SS_KIND4_X_KIND4( SS0 , SS ) RESULT( SSMTP )

    IMPLICIT NONE

    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:) :: SS0
    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:) :: SS

    REAL   (4)         , ALLOCATABLE , DIMENSION(:,:) :: SSMTP

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSMTP(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(SS0,DIM=1) , SIZE(SS,DIM=1) )
        SSMTP( I , J ) = SS0( I , J ) * SS( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_MULTIPLY_BY_SS_KIND4_X_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_SCALAR_MULTIPLY_BY_SS_KIND8_X_KIND4( SS0 , SS ) RESULT( SSMTP )

    IMPLICIT NONE

    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:) :: SS0
    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:) :: SS

    REAL   (8)         , ALLOCATABLE , DIMENSION(:,:) :: SSMTP

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSMTP(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(SS0,DIM=1) , SIZE(SS,DIM=1) )
        SSMTP( I , J ) = SS0( I , J ) * SS( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_MULTIPLY_BY_SS_KIND8_X_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_SCALAR_MULTIPLY_BY_SS_KIND4_X_KIND8( SS0 , SS ) RESULT( SSMTP )

    IMPLICIT NONE

    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:) :: SS0
    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:) :: SS

    REAL   (4)         , ALLOCATABLE , DIMENSION(:,:) :: SSMTP

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSMTP(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(SS0,DIM=1) , SIZE(SS,DIM=1) )
        SSMTP( I , J ) = SS0( I , J ) * SS( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_MULTIPLY_BY_SS_KIND4_X_KIND8

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_SCALAR_MULTIPLY_BY_SS_KIND8_X_KIND8( SS0 , SS ) RESULT( SSMTP )

    IMPLICIT NONE

    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:) :: SS0
    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:) :: SS

    REAL   (8)         , ALLOCATABLE , DIMENSION(:,:) :: SSMTP

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSMTP(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(SS0,DIM=1) , SIZE(SS,DIM=1) )
        SSMTP( I , J ) = SS0( I , J ) * SS( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_MULTIPLY_BY_SS_KIND8_X_KIND8

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_SCALAR_DIVIDE_BY_SS_KIND4_X_KIND4( SS0 , SS ) RESULT( SSDIV )

    IMPLICIT NONE

    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:) :: SS0
    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:) :: SS

    REAL   (4)         , ALLOCATABLE , DIMENSION(:,:) :: SSDIV

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSDIV(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(SS0,DIM=1) , SIZE(SS,DIM=1) )
        SSDIV( I , J ) = SS0( I , J ) / SS( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_DIVIDE_BY_SS_KIND4_X_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_SCALAR_DIVIDE_BY_SS_KIND8_X_KIND4( SS0 , SS ) RESULT( SSDIV )

    IMPLICIT NONE

    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:) :: SS0
    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:) :: SS

    REAL   (8)         , ALLOCATABLE , DIMENSION(:,:) :: SSDIV

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSDIV(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(SS0,DIM=1) , SIZE(SS,DIM=1) )
        SSDIV( I , J ) = SS0( I , J ) / SS( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_DIVIDE_BY_SS_KIND8_X_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_SCALAR_DIVIDE_BY_SS_KIND4_X_KIND8( SS0 , SS ) RESULT( SSDIV )

    IMPLICIT NONE

    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:) :: SS0
    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:) :: SS

    REAL   (4)         , ALLOCATABLE , DIMENSION(:,:) :: SSDIV

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSDIV(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(SS0,DIM=1) , SIZE(SS,DIM=1) )
        SSDIV( I , J ) = SS0( I , J ) / SS( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_DIVIDE_BY_SS_KIND4_X_KIND8

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_SCALAR_DIVIDE_BY_SS_KIND8_X_KIND8( SS0 , SS ) RESULT( SSDIV )

    IMPLICIT NONE

    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:) :: SS0
    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:) :: SS

    REAL   (8)         , ALLOCATABLE , DIMENSION(:,:) :: SSDIV

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSDIV(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(SS0,DIM=1) , SIZE(SS,DIM=1) )
        SSDIV( I , J ) = SS0( I , J ) / SS( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_DIVIDE_BY_SS_KIND8_X_KIND8

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_SCALAR_SCALE_BY_MW_KIND4_X_KIND4( MW , SS0 ) RESULT( SSSCL )

    IMPLICIT NONE

    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:) :: MW
    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:) :: SS0

    REAL   (4)         , ALLOCATABLE , DIMENSION(:,:) :: SSSCL

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSSCL(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(MW,DIM=1) , SIZE(SS0,DIM=1) )
        SSSCL( I , J ) = MW( I , J ) * SS0( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_SCALE_BY_MW_KIND4_X_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_SCALAR_SCALE_BY_MW_KIND8_X_KIND4( MW , SS0 ) RESULT( SSSCL )

    IMPLICIT NONE

    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:) :: MW
    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:) :: SS0

    REAL   (4)         , ALLOCATABLE , DIMENSION(:,:) :: SSSCL

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSSCL(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(MW,DIM=1) , SIZE(SS0,DIM=1) )
        SSSCL( I , J ) = MW( I , J ) * SS0( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_SCALE_BY_MW_KIND8_X_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_SCALAR_SCALE_BY_MW_KIND4_X_KIND8( MW , SS0 ) RESULT( SSSCL )

    IMPLICIT NONE

    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:) :: MW
    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:) :: SS0

    REAL   (8)         , ALLOCATABLE , DIMENSION(:,:) :: SSSCL

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSSCL(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(MW,DIM=1) , SIZE(SS0,DIM=1) )
        SSSCL( I , J ) = MW( I , J ) * SS0( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_SCALE_BY_MW_KIND4_X_KIND8

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_SCALAR_SCALE_BY_MW_KIND8_X_KIND8( MW , SS0 ) RESULT( SSSCL )

    IMPLICIT NONE

    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:) :: MW
    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:) :: SS0

    REAL   (8)         , ALLOCATABLE , DIMENSION(:,:) :: SSSCL

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSSCL(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , MIN( SIZE(MW,DIM=1) , SIZE(SS0,DIM=1) )
        SSSCL( I , J ) = MW( I , J ) * SS0( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_SCALE_BY_MW_KIND8_X_KIND8

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_SCALAR_SQUARE_KIND4( SS0 ) RESULT( SSSQR )

    IMPLICIT NONE

    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:) :: SS0

    REAL   (4)         , ALLOCATABLE , DIMENSION(:,:) :: SSSQR

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSSQR(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(SS0,DIM=1)
        SSSQR( I , J ) = SS0( I , J ) * SS0( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_SQUARE_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_SCALAR_SQUARE_KIND8( SS0 ) RESULT( SSSQR )

    IMPLICIT NONE

    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:) :: SS0

    REAL   (8)         , ALLOCATABLE , DIMENSION(:,:) :: SSSQR

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSSQR(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(SS0,DIM=1)
        SSSQR( I , J ) = SS0( I , J ) * SS0( I , J )
      END DO
    END DO

  END FUNCTION MR_SCALAR_SQUARE_KIND8

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_SCALAR_SQUARE_ROOT_KIND4( SS0 ) RESULT( SSQRT )

    IMPLICIT NONE

    REAL   (4)         , INTENT(IN ) , DIMENSION(:,:) :: SS0

    REAL   (4)         , ALLOCATABLE , DIMENSION(:,:) :: SSQRT

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSQRT(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(SS0,DIM=1)
        SSQRT( I , J ) = SQRT( SS0( I , J ) )
      END DO
    END DO

  END FUNCTION MR_SCALAR_SQUARE_ROOT_KIND4

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_SCALAR_SQUARE_ROOT_KIND8( SS0 ) RESULT( SSQRT )

    IMPLICIT NONE

    REAL   (8)         , INTENT(IN ) , DIMENSION(:,:) :: SS0

    REAL   (8)         , ALLOCATABLE , DIMENSION(:,:) :: SSQRT

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( SSQRT(1:SIZE(SS0,DIM=1),1:SIZE(SS0,DIM=2)) )

    DO J = 1 , SIZE(SS0,DIM=2)
     !DIR$ VECTOR ALIGNED
      DO I = 1 , SIZE(SS0,DIM=1)
        SSQRT( I , J ) = SQRT( SS0( I , J ) )
      END DO
    END DO

  END FUNCTION MR_SCALAR_SQUARE_ROOT_KIND8

  END MODULE MR_MOD_OPERATOR_SS