#INCLUDE 'MR_H_ALIGN_PADDING.H'
!***********************************************************************************************************************************
! UNIT:
!
!  ()
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
!   2016-07-05    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_GEN_GRID_SYS

    USE MR_KINDS

    USE MR_MAC_PI

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_GEN_NDID
    PUBLIC :: MR_GEN_EMID

!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  ()
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
!   2016-07-05    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_GEN_NDID( NI , NJ , NDIDW , NDIDU , NDIDV , NDIDO )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(NDID_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,NDID_KIND),1:NJ) :: NDIDW
    INTEGER(NDID_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,NDID_KIND),1:NJ) :: NDIDU
    INTEGER(NDID_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,NDID_KIND),0:NJ) :: NDIDV
    INTEGER(NDID_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,NDID_KIND),0:NJ) :: NDIDO

    INTEGER(NDID_KIND) , DIMENSION(0:NI0(2*NI,NDID_KIND),0:2*NJ) :: NDID_ALL

    INTEGER(IJID_KIND) :: I , J

    CALL MR_GEN_NDID_ALL( NI , NJ , NDID_ALL )

    DO J = 1 , NJ
      !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        NDIDW( I , J ) = NDID_ALL(2*I-1,2*J-1)
      END DO
    END DO

    DO J = 1 , NJ
      !DIR$ VECTOR ALIGNED
      DO I = 0 , NI
        NDIDU( I , J ) = NDID_ALL(2*I  ,2*J-1)
      END DO
    END DO

    DO J = 0 , NJ
      !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        NDIDV( I , J ) = NDID_ALL(2*I-1,2*J  )
      END DO
    END DO

    DO J = 0 , NJ
      !DIR$ VECTOR ALIGNED
      DO I = 0 , NI
        NDIDO( I , J ) = NDID_ALL(2*I  ,2*J  )
      END DO
    END DO

!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  ()
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
!   2016-07-05    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_GEN_NDID_ALL( NI , NJ , NDID_ALL )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(NDID_KIND) , INTENT(OUT) , DIMENSION(0:NI0(2*NI,NDID_KIND),0:2*NJ) :: NDID_ALL

    INTEGER(IJID_KIND) :: I , J

    DO J = 0 , 2*NJ
     !DIR$ VECTOR ALIGNED
      DO I = 0 , 2*NI

        NDID_ALL( I , J ) = (2*NI+1) * J + (I+1)

      END DO
    END DO

  END SUBROUTINE MR_GEN_NDID_ALL

  END SUBROUTINE MR_GEN_NDID

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_GEN_EMID( NI , NJ , EMIDW )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(EMID_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,EMID_KIND),1:NJ) :: EMIDW

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI

        EMIDW( I , J ) = NI * (J-1) + I

      END DO
    END DO

  END SUBROUTINE MR_GEN_EMID

  END MODULE MR_MOD_GEN_GRID_SYS