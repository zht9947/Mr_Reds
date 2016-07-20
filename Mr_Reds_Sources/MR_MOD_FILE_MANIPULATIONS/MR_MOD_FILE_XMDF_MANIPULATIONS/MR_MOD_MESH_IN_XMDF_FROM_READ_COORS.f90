#INCLUDE 'MR_H_ALIGN_PADDING.H'
!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE)
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
  MODULE MR_MOD_READ_COORS

    USE XMDF

    USE MR_KINDS

    USE MR_DEF_GRID_SYS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_READ_XY

!***********************************************************************************************************************************

  CONTAINS

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
  SUBROUTINE MR_READ_XY( MESH_IN_XMDF_ID , NND , NI , NJ , XYUV , XYUU , XYVV , XYOO , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MESH_IN_XMDF_ID

    INTEGER(NDID_KIND) , INTENT(IN ) :: NND
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (XYRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(XYRD_KIND),1:NJ,1:2) :: XYUV
    REAL   (XYRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(XYRD_KIND),1:NJ,1:2) :: XYUU
    REAL   (XYRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(XYRD_KIND),0:NJ,1:2) :: XYVV
    REAL   (XYRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(XYRD_KIND),0:NJ,1:2) :: XYOO

    REAL   (8)         , DIMENSION(1:2,1:NND) :: XY_ARRAY

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    ERRMSG = ""
    CALL XF_READ_X_NODE_LOCATIONS( MESH_IN_XMDF_ID , NND , XY_ARRAY( 1 ,1:NND) , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in reading X coordinate of each node from mesh"
      RETURN
    END IF
    CALL XF_READ_Y_NODE_LOCATIONS( MESH_IN_XMDF_ID , NND , XY_ARRAY( 2 ,1:NND) , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in reading Y coordinate of each node from mesh"
      RETURN
    END IF

    DO DIM = 1 , 2

      DO J = 1 , NJ
        DO I = 1 , NI
          XYUV( I , J ,DIM) = XY_ARRAY(DIM, NDIDW( I , J ) )
        END DO
      END DO

    END DO

    DO DIM = 1 , 2

      DO J = 1 , NJ
        DO I = 0 , NI
          XYUU( I , J ,DIM) = XY_ARRAY(DIM, NDIDU( I , J ) )
        END DO
      END DO

    END DO

    DO DIM = 1 , 2

      DO J = 0 , NJ
        DO I = 1 , NI
          XYVV( I , J ,DIM) = XY_ARRAY(DIM, NDIDV( I , J ) )
        END DO
      END DO

    END DO

    DO DIM = 1 , 2

      DO J = 0 , NJ
        DO I = 0 , NI
          XYOO( I , J ,DIM) = XY_ARRAY(DIM, NDIDO( I , J ) )
        END DO
      END DO

    END DO

  END SUBROUTINE MR_READ_XY

  END MODULE MR_MOD_READ_COORS