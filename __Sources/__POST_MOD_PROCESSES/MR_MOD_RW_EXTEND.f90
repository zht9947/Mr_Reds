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
  MODULE MR_MOD_RW_EXTEND

    USE XMDF

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_RANKS_EXTEND

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_RW_EXTEND

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
  SUBROUTINE MR_RW_EXTEND( FILE_XMDF_NAME , FILE_XMDF_NAME_EXTEND , ITS , T , NLOOPS , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MULTI_DSETS

    USE MR_MOD_RW_EXTEND_FIELD_VARS_N_ACTIVITY

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME
    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME_EXTEND

    INTEGER                          :: FILE_XMDF_ID , MULTI_DSETS_ID
    INTEGER                          :: FILE_XMDF_ID_EXTEND , MULTI_DSETS_ID_EXTEND

    INTEGER(TSID_KIND) , INTENT(IN ) :: ITS

    REAL   (TMRD_KIND) , INTENT(IN ) :: T

    INTEGER            , INTENT(IN ) :: NLOOPS

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY

    ERRMSG = ""

   !BLOCK
  ! OPEN SOURCE
    CALL MR_OPEN_FILE_XMDF( FILE_XMDF_NAME , "READ" , FILE_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME)
      RETURN
    END IF

    CALL MR_OPEN_MULTI_DSETS( FILE_XMDF_ID , MULTI_DSETS_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF
   !END BLOCK

   !BLOCK
  ! OPEN TARGET
    CALL MR_OPEN_FILE_XMDF( FILE_XMDF_NAME_EXTEND , "WRITE" , FILE_XMDF_ID_EXTEND , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME_EXTEND)
      RETURN
    END IF

    CALL MR_OPEN_MULTI_DSETS( FILE_XMDF_ID_EXTEND , MULTI_DSETS_ID_EXTEND , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME_EXTEND)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_EXTEND , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF
   !END BLOCK

    CALL MR_RW_EXTEND_UV( MULTI_DSETS_ID , MULTI_DSETS_ID_EXTEND , ITS , T ,   &
    & NND , EXTEND_NND , NEM , EXTEND_NEM , NLOOPS , NI , NJ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME)//" and "//TRIM(FILE_XMDF_NAME_EXTEND)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID_EXTEND , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_EXTEND , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_RW_EXTEND_SS( MULTI_DSETS_ID , MULTI_DSETS_ID_EXTEND , ITS , T ,   &
    & NND , EXTEND_NND , NEM , EXTEND_NEM , NLOOPS , NI , NJ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME)//" and "//TRIM(FILE_XMDF_NAME_EXTEND)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID_EXTEND , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_EXTEND , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

   !BLOCK
  ! CLOSE SOURCE
    CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME)
      RETURN
    END IF
   !END BLOCK

   !BLOCK
  ! CLOSE TARGET
    CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID_EXTEND , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME_EXTEND)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_EXTEND , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_EXTEND , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME_EXTEND)
      RETURN
    END IF
   !END BLOCK

  END SUBROUTINE MR_RW_EXTEND

  END MODULE MR_MOD_RW_EXTEND