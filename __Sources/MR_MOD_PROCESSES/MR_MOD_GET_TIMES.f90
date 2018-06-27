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
  MODULE MR_MOD_GET_TIMES

    USE MR_KINDS

    USE MR_DEF_FIELD_VARS_DSET_NAMES

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_GET_NTSS
    PUBLIC :: MR_GET_T_ITS

    PUBLIC :: MR_GET_NTSS_N_T_NTSS

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
! DEFREADION OF VARIABLES:
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
  SUBROUTINE MR_GET_NTSS( FILE_XMDF_NAME , NTSS , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MULTI_DSETS

    USE MR_MOD_GET_DSET_TIMES

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME

    INTEGER                          :: FILE_XMDF_ID , MULTI_DSETS_ID

    INTEGER(TSID_KIND) , INTENT(OUT) :: NTSS

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY

    CHARACTER( 2**08 )               :: PATH_DSET_IN_MULTI_DSETS

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

  ! GET NTSS FROM ZB
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/"//DSET_NAME_ZB
    CALL MR_GET_DSET_NTSS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , NTSS , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

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

  END SUBROUTINE MR_GET_NTSS

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!   TO
!
! DEFREADION OF VARIABLES:
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
  SUBROUTINE MR_GET_T_ITS( FILE_XMDF_NAME , NTSS , ITS , T_ITS , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MULTI_DSETS

    USE MR_MOD_GET_DSET_TIMES

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME

    INTEGER                          :: FILE_XMDF_ID , MULTI_DSETS_ID

    INTEGER(TSID_KIND) , INTENT(IN ) :: NTSS
    INTEGER(TSID_KIND) , INTENT(IN ) :: ITS

    REAL   (TMRD_KIND) , INTENT(OUT) :: T_ITS

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY

    CHARACTER( 2**08 )               :: PATH_DSET_IN_MULTI_DSETS

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

  ! GET T OF ITS FROM ZB
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/"//DSET_NAME_ZB
    CALL MR_GET_DSET_T_ITS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , NTSS , ITS , T_ITS , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

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

  END SUBROUTINE MR_GET_T_ITS

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!   TO
!
! DEFREADION OF VARIABLES:
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
  SUBROUTINE MR_GET_NTSS_N_T_NTSS( FILE_XMDF_NAME , NTSS , T_NTSS , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MULTI_DSETS

    USE MR_MOD_GET_DSET_TIMES

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME

    INTEGER                          :: FILE_XMDF_ID , MULTI_DSETS_ID

    INTEGER(TSID_KIND) , INTENT(OUT) :: NTSS
    REAL   (TMRD_KIND) , INTENT(OUT) :: T_NTSS

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY

    CHARACTER( 2**08 )               :: PATH_DSET_IN_MULTI_DSETS

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

  ! GET NTSS AND T OF NTSS FROM ZB
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/"//DSET_NAME_ZB
    CALL MR_GET_DSET_NTSS_N_T_NTSS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , NTSS , T_NTSS , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

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

  END SUBROUTINE MR_GET_NTSS_N_T_NTSS

  END MODULE MR_MOD_GET_TIMES