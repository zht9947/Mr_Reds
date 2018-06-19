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
  SUBROUTINE MR_READ_XY( MESH_IN_XMDF_ID , NND , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
  & XYUV , XYUU , XYVV , XYOO , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MESH_IN_XMDF_ID

    INTEGER(NDID_KIND) , INTENT(IN ) :: NND
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(EMID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,EMID_KIND),1:NJ    ) :: EMIDW

    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,NDID_KIND),1:NJ    ) :: NDIDW
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,NDID_KIND),1:NJ    ) :: NDIDU
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,NDID_KIND),0:NJ    ) :: NDIDV
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,NDID_KIND),0:NJ    ) :: NDIDO

    REAL   (XYRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,XYRD_KIND),1:NJ,1:2) :: XYUV
    REAL   (XYRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,XYRD_KIND),1:NJ,1:2) :: XYUU
    REAL   (XYRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,XYRD_KIND),0:NJ,1:2) :: XYVV
    REAL   (XYRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,XYRD_KIND),0:NJ,1:2) :: XYOO

    REAL   (8)         , DIMENSION(1:2,1:NND) :: XY_ARRAY

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    CALL XF_READ_X_NODE_LOCATIONS( MESH_IN_XMDF_ID , NND , XY_ARRAY( 1 ,1:NND) , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in reading X coordinates of all node from mesh"
      RETURN
    END IF
    CALL XF_READ_Y_NODE_LOCATIONS( MESH_IN_XMDF_ID , NND , XY_ARRAY( 2 ,1:NND) , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in reading Y coordinates of all node from mesh"
      RETURN
    END IF

   !DIR$ FORCEINLINE
    CALL MR_READ_XY_UNPACK_FOR_W_NODES
   !DIR$ FORCEINLINE
    CALL MR_READ_XY_UNPACK_FOR_U_NODES
   !DIR$ FORCEINLINE
    CALL MR_READ_XY_UNPACK_FOR_V_NODES
   !DIR$ FORCEINLINE
    CALL MR_READ_XY_UNPACK_FOR_O_NODES
   !END$ FORCEINLINE

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
  SUBROUTINE MR_READ_XY_UNPACK_FOR_W_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 1 , NI
          XYUV( I , J ,DIM) = XY_ARRAY(DIM, NDIDW( I , J ) )
        END DO
      END DO

    END DO

  END SUBROUTINE MR_READ_XY_UNPACK_FOR_W_NODES

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
  SUBROUTINE MR_READ_XY_UNPACK_FOR_U_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 0 , NI
          XYUU( I , J ,DIM) = XY_ARRAY(DIM, NDIDU( I , J ) )
        END DO
      END DO

    END DO

  END SUBROUTINE MR_READ_XY_UNPACK_FOR_U_NODES

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
  SUBROUTINE MR_READ_XY_UNPACK_FOR_V_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 1 , NI
          XYVV( I , J ,DIM) = XY_ARRAY(DIM, NDIDV( I , J ) )
        END DO
      END DO

    END DO

  END SUBROUTINE MR_READ_XY_UNPACK_FOR_V_NODES

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
  SUBROUTINE MR_READ_XY_UNPACK_FOR_O_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 0 , NI
          XYOO( I , J ,DIM) = XY_ARRAY(DIM, NDIDO( I , J ) )
        END DO
      END DO

    END DO

  END SUBROUTINE MR_READ_XY_UNPACK_FOR_O_NODES

  END SUBROUTINE MR_READ_XY

  END MODULE MR_MOD_READ_COORS