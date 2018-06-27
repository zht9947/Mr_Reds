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
  MODULE MR_MOD_DO_SCALE_OUTPUT_MESH_DATA

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_COORS
    USE MR_DEF_GRID_SYS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_DO_SCALE_OUTPUT_MESH_DATA

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_DO_SCALE_OUTPUT_MESH_DATA( FILE_XMDF_NAME , FILE_XMDF_NAME_ , SCALE , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MESH_IN_XMDF

    USE MR_MOD_WRITE_GRID_SYS
    USE MR_MOD_WRITE_COORS

    USE MR_MOD_SCALE

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME , FILE_XMDF_NAME_

    REAL   (PARD_KIND) , INTENT(IN ) :: SCALE

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    CALL MR_OUTPUT_GRID_SYS( FILE_XMDF_NAME_ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when outputting scaled grid system"
    ELSE

      ALLOCATE( XYUV(1:NI1(NI,XYRD_KIND),1:NJ,1:2) )
      ALLOCATE( XYUU(0:NI0(NI,XYRD_KIND),1:NJ,1:2) , XYVV(1:NI1(NI,XYRD_KIND),0:NJ,1:2) )
      ALLOCATE( XYOO(0:NI0(NI,XYRD_KIND),0:NJ,1:2) )

      CALL MR_INIT_COORS( FILE_XMDF_NAME , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        ERRMSG = TRIM(ERRMSG)//" when initializing coordinates"
      ELSE

        CALL MR_SCALE_XY( SCALE , NI , NJ , XYUV , XYUU , XYVV , XYOO )

        CALL MR_OUTPUT_COORS( FILE_XMDF_NAME_ , ERROR , ERRMSG )
        IF( ERROR < 0 ) THEN
          ERRMSG = TRIM(ERRMSG)//" when outputting scaled coordinates"
        END IF

      END IF

      DEALLOCATE( XYUV , XYUU , XYVV , XYOO )

    END IF

  END SUBROUTINE MR_DO_SCALE_OUTPUT_MESH_DATA

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_INIT_COORS( FILE_XMDF_NAME , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MESH_IN_XMDF

    USE MR_MOD_READ_COORS

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME

    INTEGER                          :: FILE_XMDF_ID , MESH_IN_XMDF_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY

    CALL MR_OPEN_FILE_XMDF( FILE_XMDF_NAME , "READ" , FILE_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME)
      RETURN
    END IF

    CALL MR_OPEN_MESH_IN_XMDF( FILE_XMDF_ID , MESH_IN_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_READ_XY( MESH_IN_XMDF_ID , NND , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
    & XYUV , XYUU , XYVV , XYOO , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME)
      RETURN
    END IF

  END SUBROUTINE MR_INIT_COORS

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_OUTPUT_GRID_SYS( FILE_XMDF_NAME , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MESH_IN_XMDF

    USE MR_MOD_WRITE_GRID_SYS

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME

    INTEGER                          :: FILE_XMDF_ID , MESH_IN_XMDF_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY

    CALL MR_OPEN_FILE_XMDF( FILE_XMDF_NAME , "WRITE" , FILE_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME)
      RETURN
    END IF

    CALL MR_OPEN_MESH_IN_XMDF( FILE_XMDF_ID , MESH_IN_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_WRITE_NDID( MESH_IN_XMDF_ID , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME)
      RETURN
    END IF

  END SUBROUTINE MR_OUTPUT_GRID_SYS

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_OUTPUT_COORS( FILE_XMDF_NAME , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MESH_IN_XMDF

    USE MR_MOD_WRITE_COORS

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME

    INTEGER                          :: FILE_XMDF_ID , MESH_IN_XMDF_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY

    CALL MR_OPEN_FILE_XMDF( FILE_XMDF_NAME , "WRITE" , FILE_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME)
      RETURN
    END IF

    CALL MR_OPEN_MESH_IN_XMDF( FILE_XMDF_ID , MESH_IN_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_WRITE_XY( MESH_IN_XMDF_ID , NND , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
    & XYUV , XYUU , XYVV , XYOO , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME)
      RETURN
    END IF

  END SUBROUTINE MR_OUTPUT_COORS

  END MODULE MR_MOD_DO_SCALE_OUTPUT_MESH_DATA