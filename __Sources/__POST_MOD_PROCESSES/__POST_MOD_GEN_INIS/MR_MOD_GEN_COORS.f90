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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_GEN_COORS

    USE MR_KINDS

    USE MR_MAC_PI

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_GEN_XY

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_GEN_XY( THETA0 , BTH , LTH , NBENDS , NI , NJ  , XYUV , XYUU , XYVV , XYOO )

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN ) :: THETA0
    REAL   (XYRD_KIND) , INTENT(IN ) :: BTH , LTH

    INTEGER            , INTENT(IN ) :: NBENDS

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (XYRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,XYRD_KIND),1:NJ,1:2) :: XYUV
    REAL   (XYRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,XYRD_KIND),1:NJ,1:2) :: XYUU
    REAL   (XYRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,XYRD_KIND),0:NJ,1:2) :: XYVV
    REAL   (XYRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,XYRD_KIND),0:NJ,1:2) :: XYOO

    REAL   (XYRD_KIND) , DIMENSION(0:NI0(2*NI,XYRD_KIND),0:2*NJ,1:2) :: XY_ALL

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    CALL MR_GEN_XY_ALL( THETA0 , BTH , LTH , NBENDS , NI , NJ , XY_ALL )

    DO DIM = 1 , 2
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          XYUV( I , J ,DIM) = XY_ALL(2*I-1,2*J-1,DIM)
        END DO
      END DO
    END DO

    DO DIM = 1 , 2
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          XYUU( I , J ,DIM) = XY_ALL(2*I  ,2*J-1,DIM)
        END DO
      END DO
    END DO

    DO DIM = 1 , 2
      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          XYVV( I , J ,DIM) = XY_ALL(2*I-1,2*J  ,DIM)
        END DO
      END DO
    END DO

    DO DIM = 1 , 2
      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          XYOO( I , J ,DIM) = XY_ALL(2*I  ,2*J  ,DIM)
        END DO
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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_GEN_XY_ALL( THETA0 , BTH , LTH , NBENDS , NI , NJ , XY_ALL )

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN ) :: THETA0
    REAL   (XYRD_KIND) , INTENT(IN ) :: BTH , LTH

    INTEGER            , INTENT(IN ) :: NBENDS

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (XYRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(2*NI,XYRD_KIND),0:2*NJ,1:2) :: XY_ALL

    REAL   (PARD_KIND) :: DXI , DYJ

    REAL   (PARD_KIND) :: THETA

    INTEGER(IJID_KIND) :: I , J

    DXI = 0.5_PARD_KIND * NBENDS / NI
    DYJ = 1.0_PARD_KIND / NJ

    XY_ALL( 0 ,NJ,1) = 0.0
    XY_ALL( 0 ,NJ,2) = 0.0
   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI
      THETA = THETA0 * COS(  2.0 * PI * ( ( I-0.5 ) * DXI )  )

      XY_ALL(2*I-1,NJ,1) = XY_ALL(2*I-2,NJ,1) + 0.5 * DXI * LTH * COS(THETA)
      XY_ALL(2*I-1,NJ,2) = XY_ALL(2*I-2,NJ,2) + 0.5 * DXI * LTH * SIN(THETA)

      XY_ALL(2*I  ,NJ,1) = XY_ALL(2*I-1,NJ,1) + 0.5 * DXI * LTH * COS(THETA)
      XY_ALL(2*I  ,NJ,2) = XY_ALL(2*I-1,NJ,2) + 0.5 * DXI * LTH * SIN(THETA)

    END DO

    DO J = NJ+1 , 2*NJ , +1
     !DIR$ VECTOR ALIGNED
      DO I = 0 , 2*NI
        THETA = THETA0 * COS(  2.0 * PI * ( 0.5 * I * DXI )  )

        XY_ALL( I , J ,1) = XY_ALL( I ,J-1,1) - 0.5 * DYJ * BTH * SIN(THETA)
        XY_ALL( I , J ,2) = XY_ALL( I ,J-1,2) + 0.5 * DYJ * BTH * COS(THETA)

      END DO
    END DO

    DO J = NJ-1 , 0 , -1
     !DIR$ VECTOR ALIGNED
      DO I = 0 , 2*NI
        THETA = THETA0 * COS(  2.0 * PI * ( 0.5 * I * DXI )  )

        XY_ALL( I , J ,1) = XY_ALL( I ,J+1,1) + 0.5 * DYJ * BTH * SIN(THETA)
        XY_ALL( I , J ,2) = XY_ALL( I ,J+1,2) - 0.5 * DYJ * BTH * COS(THETA)

      END DO
    END DO

  END SUBROUTINE MR_GEN_XY_ALL

  END SUBROUTINE MR_GEN_XY

  END MODULE MR_MOD_GEN_COORS