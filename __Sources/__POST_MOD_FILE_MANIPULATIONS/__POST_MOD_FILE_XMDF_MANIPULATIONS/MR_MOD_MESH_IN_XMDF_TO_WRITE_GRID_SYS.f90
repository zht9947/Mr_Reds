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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_WRITE_GRID_SYS

    USE XMDF

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_WRITE_NDID

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_WRITE_NDID( MESH_IN_XMDF_ID , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MESH_IN_XMDF_ID

    INTEGER(EMID_KIND) , INTENT(IN ) :: NEM
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER            , PARAMETER   :: NND_IN_EM = 8 , NND_CD_IN_EM = NND_IN_EM + 1
    INTEGER            , PARAMETER   :: EM_TYPE = 212   ! QUADRATIC QUADRILATERAL WITH CENTROID

    INTEGER(EMID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,EMID_KIND),1:NJ) :: EMIDW

    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,NDID_KIND),1:NJ) :: NDIDW
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,NDID_KIND),1:NJ) :: NDIDU
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,NDID_KIND),0:NJ) :: NDIDV
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,NDID_KIND),0:NJ) :: NDIDO

    INTEGER(NDID_KIND) , DIMENSION( 1:NND_CD_IN_EM , 1:NEM ) :: NDID_ARRAY
    INTEGER            , DIMENSION(                  1:NEM ) :: EM_TYPE_ARRAY

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

   !DIR$ FORCEINLINE
    CALL MR_WRITE_NDID_PACK_FOR_ELEMS
   !END$ FORCEINLINE

    ERRMSG = ""

    CALL XF_SET_NUMBER_OF_ELEMENTS( MESH_IN_XMDF_ID , NEM , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in setting number of elements in mesh"
      RETURN
    END IF

    EM_TYPE_ARRAY = EM_TYPE
    CALL XF_WRITE_ELEM_TYPES( MESH_IN_XMDF_ID , NEM , EM_TYPE_ARRAY , NONE , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in setting types of all elements in mesh"
      RETURN
    END IF

    CALL XF_WRITE_ELEM_NODE_IDS( MESH_IN_XMDF_ID , NEM , NND_CD_IN_EM , NDID_ARRAY , NONE , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in writing node ids of all elements to mesh"
      RETURN
    END IF

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_WRITE_NDID_PACK_FOR_ELEMS

    IMPLICIT NONE
 
    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED, ALWAYS
      DO I = 1 , NI
  
        NDID_ARRAY( 1 , EMIDW( I , J ) ) = NDIDO(I-1,J-1)
        NDID_ARRAY( 2 , EMIDW( I , J ) ) = NDIDV( I ,J-1)
        NDID_ARRAY( 3 , EMIDW( I , J ) ) = NDIDO( I ,J-1)
        NDID_ARRAY( 4 , EMIDW( I , J ) ) = NDIDU( I , J )
        NDID_ARRAY( 5 , EMIDW( I , J ) ) = NDIDO( I , J )
        NDID_ARRAY( 6 , EMIDW( I , J ) ) = NDIDV( I , J )
        NDID_ARRAY( 7 , EMIDW( I , J ) ) = NDIDO(I-1, J )
        NDID_ARRAY( 8 , EMIDW( I , J ) ) = NDIDU(I-1, J )
        NDID_ARRAY( 9 , EMIDW( I , J ) ) = NDIDW( I , J )

      END DO
    END DO

  END SUBROUTINE MR_WRITE_NDID_PACK_FOR_ELEMS

  END SUBROUTINE MR_WRITE_NDID

  END MODULE MR_MOD_WRITE_GRID_SYS