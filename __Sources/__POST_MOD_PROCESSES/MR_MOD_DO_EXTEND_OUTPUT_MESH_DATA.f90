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
  MODULE MR_MOD_DO_EXTEND_OUTPUT_MESH_DATA

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_RANKS_
    USE MR_DEF_COORS
    USE MR_DEF_COORS_
    USE MR_DEF_GRID_SYS
    USE MR_DEF_GRID_SYS_

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_DO_EXTEND_OUTPUT_MESH_DATA

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
  SUBROUTINE MR_DO_EXTEND_OUTPUT_MESH_DATA( FILE_XMDF_NAME , FILE_XMDF_NAME_ , NLOOPS , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MESH_IN_XMDF

    USE MR_MOD_GEN_GRID_SYS

    USE MR_MOD_EXTEND

    USE MR_MOD_WRITE_GRID_SYS
    USE MR_MOD_WRITE_COORS

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME , FILE_XMDF_NAME_

    INTEGER            , INTENT(IN ) :: NLOOPS

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    CALL MR_GEN_EMID( NI_ , NJ_ , EMIDW_ )
    CALL MR_GEN_NDID( NI_ , NJ_ , NDIDW_ , NDIDU_ , NDIDV_ , NDIDO_ )

    CALL MR_OUTPUT_GRID_SYS_( FILE_XMDF_NAME_ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when outputting extended grid system to it"
    ELSE

      ALLOCATE( XYUV(1:NI1(NI,XYRD_KIND),1:NJ,1:2) )
      ALLOCATE( XYUU(0:NI0(NI,XYRD_KIND),1:NJ,1:2) , XYVV(1:NI1(NI,XYRD_KIND),0:NJ,1:2) )
      ALLOCATE( XYOO(0:NI0(NI,XYRD_KIND),0:NJ,1:2) )

      CALL MR_INIT_COORS( FILE_XMDF_NAME , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        DEALLOCATE( XYUV , XYUU , XYVV , XYOO )
      ELSE

        ALLOCATE( XYUV_(1:NI1(NI_,XYRD_KIND),1:NJ_,1:2) )
        ALLOCATE( XYUU_(0:NI0(NI_,XYRD_KIND),1:NJ_,1:2) , XYVV_(1:NI1(NI_,XYRD_KIND),0:NJ_,1:2) )
        ALLOCATE( XYOO_(0:NI0(NI_,XYRD_KIND),0:NJ_,1:2) )

        CALL MR_EXTEND_XY( NLOOPS , NI , NJ ,   &
        & XYUV , XYUU , XYVV , XYOO ,   &
        & XYUV_ , XYUU_ , XYVV_ , XYOO_ )

        DEALLOCATE( XYUV , XYUU , XYVV , XYOO )

        CALL MR_OUTPUT_COORS_( FILE_XMDF_NAME_ , ERROR , ERRMSG )
        IF( ERROR < 0 ) THEN
          ERRMSG = TRIM(ERRMSG)//" when outputting extended coordinates to it"
        END IF

        DEALLOCATE( XYUV_ , XYUU_ , XYVV_ , XYOO_ )

      END IF

    END IF

  END SUBROUTINE MR_DO_EXTEND_OUTPUT_MESH_DATA

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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_OUTPUT_GRID_SYS_( FILE_XMDF_NAME_ , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MESH_IN_XMDF

    USE MR_MOD_WRITE_GRID_SYS

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME_

    INTEGER                          :: FILE_XMDF_ID_ , MESH_IN_XMDF_ID_

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY

    CALL MR_OPEN_FILE_XMDF( FILE_XMDF_NAME_ , "WRITE" , FILE_XMDF_ID_ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME_)
      RETURN
    END IF

    CALL MR_OPEN_MESH_IN_XMDF( FILE_XMDF_ID_ , MESH_IN_XMDF_ID_ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME_)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_ , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_WRITE_NDID( MESH_IN_XMDF_ID_ , NEM_ , NI_ , NJ_ , EMIDW_ , NDIDW_ , NDIDU_ , NDIDV_ , NDIDO_ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME_)
      CALL MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID_ , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_ , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID_ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME_)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_ , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME_)
      RETURN
    END IF

  END SUBROUTINE MR_OUTPUT_GRID_SYS_

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
  SUBROUTINE MR_OUTPUT_COORS_( FILE_XMDF_NAME_ , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MESH_IN_XMDF

    USE MR_MOD_WRITE_COORS

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME_

    INTEGER                          :: FILE_XMDF_ID_ , MESH_IN_XMDF_ID_

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY

    CALL MR_OPEN_FILE_XMDF( FILE_XMDF_NAME_ , "WRITE" , FILE_XMDF_ID_ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME_)
      RETURN
    END IF

    CALL MR_OPEN_MESH_IN_XMDF( FILE_XMDF_ID_ , MESH_IN_XMDF_ID_ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME_)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_ , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_WRITE_XY( MESH_IN_XMDF_ID_ , NND_ , NI_ , NJ_ , EMIDW_ , NDIDW_ , NDIDU_ , NDIDV_ , NDIDO_ ,   &
    & XYUV_ , XYUU_ , XYVV_ , XYOO_ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME_)
      CALL MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID_ , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_ , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID_ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME_)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_ , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME_)
      RETURN
    END IF

  END SUBROUTINE MR_OUTPUT_COORS_

  END MODULE MR_MOD_DO_EXTEND_OUTPUT_MESH_DATA