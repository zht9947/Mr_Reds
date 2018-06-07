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
  MODULE MR_MOD_RW_RESAMPLE

    USE XMDF

    USE MR_KINDS

    USE MR_DEF_RANKS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_RW_RESAMPLE

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
  SUBROUTINE MR_RW_RESAMPLE( FILE_XMDF_NAME , FILE_XMDF_NAME_RESAMPLE , ITS , T , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MULTI_DSETS

    USE MR_MOD_RW_RESAMPLE_FIELD_VARS_N_ACTIVITY

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME
    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME_RESAMPLE

    INTEGER                          :: FILE_XMDF_ID , MULTI_DSETS_ID
    INTEGER                          :: FILE_XMDF_ID_RESAMPLE , MULTI_DSETS_ID_RESAMPLE

    INTEGER(TSID_KIND) , INTENT(IN ) :: ITS

    REAL   (TMRD_KIND) , INTENT(IN ) :: T

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY

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
    CALL MR_OPEN_FILE_XMDF( FILE_XMDF_NAME_RESAMPLE , "WRITE" , FILE_XMDF_ID_RESAMPLE , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME_RESAMPLE)
      RETURN
    END IF

    CALL MR_OPEN_MULTI_DSETS( FILE_XMDF_ID_RESAMPLE , MULTI_DSETS_ID_RESAMPLE , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME_RESAMPLE)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_RESAMPLE , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF
   !END BLOCK

    CALL MR_RW_RESAMPLE_UV( MULTI_DSETS_ID , MULTI_DSETS_ID_RESAMPLE , ITS , T ,   &
    & NND , NEM , NI , NJ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME)//" and "//TRIM(FILE_XMDF_NAME_RESAMPLE)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID_RESAMPLE , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_RESAMPLE , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_RW_RESAMPLE_SS( MULTI_DSETS_ID , MULTI_DSETS_ID_RESAMPLE , ITS , T ,   &
    & NND , NEM , NI , NJ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME)//" and "//TRIM(FILE_XMDF_NAME_RESAMPLE)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID_RESAMPLE , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_RESAMPLE , ERROR_DUMMY , ERRMSG_DUMMY )
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
    CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID_RESAMPLE , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME_RESAMPLE)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_RESAMPLE , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID_RESAMPLE , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME_RESAMPLE)
      RETURN
    END IF
   !END BLOCK

  END SUBROUTINE MR_RW_RESAMPLE

  END MODULE MR_MOD_RW_RESAMPLE