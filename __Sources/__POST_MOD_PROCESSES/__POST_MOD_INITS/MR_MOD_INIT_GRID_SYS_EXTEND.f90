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
  MODULE MR_MOD_INIT_GRID_SYS_EXTEND

    USE MR_KINDS

    USE MR_DEF_RANKS , ONLY : NI , NJ
    USE MR_DEF_RANKS_EXTEND
    USE MR_DEF_GRID_SYS_EXTEND

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_INIT_GRID_SYS_EXTEND

!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
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
  SUBROUTINE MR_INIT_GRID_SYS_EXTEND( FILE_XMDF_NAME_EXTEND , NLOOPS , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MESH_IN_XMDF

    USE MR_MOD_GET_RANKS2

    USE MR_MOD_READ_GRID_SYS

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME_EXTEND

    INTEGER                          :: FILE_XMDF_ID_EXTEND , MESH_IN_XMDF_ID_EXTEND

    INTEGER            , INTENT(IN ) :: NLOOPS

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY

    CALL MR_INIT_EMID_EXTEND( NLOOPS )

    ERRMSG = ""

    CALL MR_OPEN_FILE_XMDF( FILE_XMDF_NAME_EXTEND , "READ" , FILE_XMDF_ID_EXTEND , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME_EXTEND)
      RETURN
    END IF

    CALL MR_OPEN_MESH_IN_XMDF( FILE_XMDF_ID_EXTEND , MESH_IN_XMDF_ID_EXTEND , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME_EXTEND)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_EXTEND , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_GET_NND_NEM( MESH_IN_XMDF_ID_EXTEND , EXTEND_NND , EXTEND_NEM , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME_EXTEND)
      CALL MR_CLOSE_MESH_IN_XMDF( FILE_XMDF_ID_EXTEND , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_EXTEND , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    ELSE IF( EXTEND_NND /= ((2*NI*NLOOPS+1)*(2*NJ+1)) ) THEN
      ERROR = - 5502
      ERRMSG = "Inconsistent number of nodes (NND) "   &
      //"gotten from mesh /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME_EXTEND)
      CALL MR_CLOSE_MESH_IN_XMDF( FILE_XMDF_ID_EXTEND , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_EXTEND , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    ELSE IF( EXTEND_NEM /= (NI*NLOOPS*NJ) ) THEN
      ERROR = - 5501
      ERRMSG = "Inconsistent number of elements (NEM) "   &
      //"gotten from mesh /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME_EXTEND)
      CALL MR_CLOSE_MESH_IN_XMDF( FILE_XMDF_ID_EXTEND , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_EXTEND , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_READ_NDID( MESH_IN_XMDF_ID_EXTEND , EXTEND_NEM , NI*NLOOPS , NJ ,   &
    & EXTEND_EMIDW , EXTEND_NDIDW , EXTEND_NDIDU , EXTEND_NDIDV , EXTEND_NDIDO , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME_EXTEND)
      CALL MR_CLOSE_MESH_IN_XMDF( FILE_XMDF_ID_EXTEND , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_EXTEND , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID_EXTEND , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME_EXTEND)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_EXTEND , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_EXTEND , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME_EXTEND)
      RETURN
    END IF

  END SUBROUTINE MR_INIT_GRID_SYS_EXTEND

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
  SUBROUTINE MR_INIT_EMID_EXTEND( NLOOPS )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: NLOOPS

    INTEGER(IJID_KIND)               :: I , J
    INTEGER(EMID_KIND)               :: EMID

    EMID = 0
    DO J = 1 , NJ
      DO I = 1 , NI*NLOOPS
        EMID = EMID + 1

        EXTEND_EMIDW( I , J ) = EMID

      END DO
    END DO

  END SUBROUTINE MR_INIT_EMID_EXTEND

  END MODULE MR_MOD_INIT_GRID_SYS_EXTEND