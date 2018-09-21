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
  MODULE MR_MOD_GEN_OUTPUT_MESH_DATA

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_COORS
    USE MR_DEF_GRID_SYS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_GEN_OUTPUT_MESH_DATA

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
  SUBROUTINE MR_GEN_OUTPUT_MESH_DATA( FILE_XMDF_NAME , THETA0 , BTH , LTH , NBENDS , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MESH_IN_XMDF

    USE MR_MOD_GEN_GRID_SYS
    USE MR_MOD_GEN_COORS

    USE MR_MOD_WRITE_GRID_SYS
    USE MR_MOD_WRITE_COORS

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME

    REAL   (PARD_KIND) , INTENT(IN ) :: THETA0
    REAL   (XYRD_KIND) , INTENT(IN ) :: BTH , LTH

    INTEGER            , INTENT(IN ) :: NBENDS

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ALLOCATE( EMIDW(1:NI1(NI,EMID_KIND),1:NJ) )

    ALLOCATE( NDIDW(1:NI1(NI,NDID_KIND),1:NJ) )
    ALLOCATE( NDIDU(0:NI0(NI,NDID_KIND),1:NJ) , NDIDV(1:NI1(NI,NDID_KIND),0:NJ) )
    ALLOCATE( NDIDO(0:NI0(NI,NDID_KIND),0:NJ) )

    CALL MR_GEN_EMID( NI , NJ , EMIDW )
    CALL MR_GEN_NDID( NI , NJ , NDIDW , NDIDU , NDIDV , NDIDO )

    CALL MR_OUTPUT_GRID_SYS( FILE_XMDF_NAME , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when outputting generated grid system"
    ELSE

      ALLOCATE( XYUV(1:NI1(NI,XYRD_KIND),1:NJ,1:2) )
      ALLOCATE( XYUU(0:NI0(NI,XYRD_KIND),1:NJ,1:2) , XYVV(1:NI1(NI,XYRD_KIND),0:NJ,1:2) )
      ALLOCATE( XYOO(0:NI0(NI,XYRD_KIND),0:NJ,1:2) )

      CALL MR_GEN_XY( THETA0 , BTH , LTH , NBENDS , NI , NJ , XYUV , XYUU , XYVV , XYOO )

      CALL MR_OUTPUT_COORS( FILE_XMDF_NAME , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        ERRMSG = TRIM(ERRMSG)//" when outputting generated coordinates"
      END IF

      DEALLOCATE( XYUV , XYUU , XYVV , XYOO )

    END IF

    DEALLOCATE( EMIDW , NDIDW , NDIDU , NDIDV , NDIDO )

  END SUBROUTINE MR_GEN_OUTPUT_MESH_DATA

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

    CALL MR_OPEN_FILE_XMDF( FILE_XMDF_NAME , FILE_XMDF_ID , ERROR , ERRMSG , READONLY=.FALSE. )
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

    CALL MR_OPEN_FILE_XMDF( FILE_XMDF_NAME , FILE_XMDF_ID , ERROR , ERRMSG , READONLY=.FALSE. )
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

  END MODULE MR_MOD_GEN_OUTPUT_MESH_DATA