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
  MODULE MR_MOD_CREATE_FILE_XMDF

    USE XMDF

    USE MR_ERRORS_FILE_MANIPULATE

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CREATE_FILE_XMDF

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
  SUBROUTINE MR_CREATE_FILE_XMDF( FILE_XMDF_NAME , FILE_XMDF_STATUS , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MESH_IN_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MULTI_DSETS

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME
    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_STATUS

    INTEGER                          :: FILE_XMDF_ID , MESH_IN_XMDF_ID , MULTI_DSETS_ID , PROP_GP_ID

    CHARACTER( 2**05 )               :: GUID
    INTEGER                          :: LUID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY

    ERRMSG = ""
    SELECT CASE( TRIM(FILE_XMDF_STATUS) )
    CASE( "OVERWRITE" , "OverWrite" , "Overwrite" , "OVER" , "Over" , "O" , "overwrite", "o" )
      CALL XF_CREATE_FILE( TRIM(FILE_XMDF_NAME) , .TRUE. , FILE_XMDF_ID , ERROR )
      IF( ERROR < 0 ) THEN
        ERRMSG = "Error in creating file"
        RETURN
      END IF
    CASE DEFAULT
      CALL XF_CREATE_FILE( TRIM(FILE_XMDF_NAME) , .FALSE. , FILE_XMDF_ID , ERROR )
      IF( ERROR < 0 ) THEN
        ERROR = ERROR_CANNOT_CREATE_NEW_FILE
        ERRMSG = "Error in creating file"
        RETURN
      END IF
    END SELECT

    CALL MR_GEN_GUID_N_LUID

    CALL XF_CREATE_GROUP_FOR_MESH( FILE_XMDF_ID , TRIM(XF_PATH_MESH_IN_XMDF) , MESH_IN_XMDF_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in creating mesh"//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file"
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL XF_CREATE_MESH_PROPERTY_GROUP( MESH_IN_XMDF_ID , PROP_GP_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in creating property group in mesh"
    ELSE

      CALL XF_WRITE_PROPERTY_STRING( PROP_GP_ID , "Guid" ,   &
      & GUID(1:8)//"-"//GUID(9:12)//"-"//GUID(13:16)//"-"//GUID(17:20)//"-"//GUID(21:32) , ERROR )
      IF( ERROR < 0 ) THEN
        ERRMSG = "Error in writing Guid to property group in mesh"
      ELSE

        CALL XF_WRITE_PROPERTY_UINT( PROP_GP_ID , "Id" , 1 , (/LUID/) , NONE , ERROR )
        IF( ERROR < 0 ) THEN
          ERRMSG = "Error in writing Id to property group in mesh"
        END IF

      END IF

      CALL XF_CLOSE_GROUP( PROP_GP_ID , ERROR_DUMMY )
      IF( ERROR_DUMMY < 0 .AND. ERROR >= 0 ) THEN
        ERROR = ERROR_DUMMY
        ERRMSG = "Error in closing property group in mesh"
      END IF

    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file"
      CALL MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL XF_CREATE_MULTI_DATASETS_GROUP( MESH_IN_XMDF_ID , TRIM(MULTI_DATASET_LOCATION) ,   &
    & GUID(1:8)//"-"//GUID(9:12)//"-"//GUID(13:16)//"-"//GUID(17:20)//"-"//GUID(21:32) , MULTI_DSETS_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in creating multiple datasets"//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file"
      CALL MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file"
      CALL MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file"
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR, ERRMSG )
    IF( ERROR < 0 ) THEN
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
!                            GUID    :    GLOBALLY UNIQUE IDENTIFIER, VER. 4, A RANDOM HEX DIGITS STRING
!                                         HAVING THE FORMAT "00000000-0000-0000-0000-000000000000",
!                                         SEE MORE DETAILS IN https://en.wikipedia.org/wiki/Universally_unique_identifier
!                            LUID    :    LOCALLY UNIQUE IDENTIFIER, A RANDOM UNSIGNED INTEGER
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_GEN_GUID_N_LUID

    IMPLICIT NONE

    CHARACTER(2**04) , PARAMETER   :: SZHEX = "0123456789abcdef-"
    CHARACTER(2**05) , PARAMETER   :: SZTMP = "XXXXXXXXXXXX4XXXYXXXXXXXXXXXXXXX"

    REAL                           :: R_RANDOM
    INTEGER                        :: IDX

    INTEGER                        :: ICR

    CALL RANDOM_SEED()

    CALL RANDOM_NUMBER( R_RANDOM )
    LUID = MOD( INT( R_RANDOM * 99999 ) , 99999 ) + 1

    DO ICR = 1 , 32

      CALL RANDOM_NUMBER( R_RANDOM )
      IDX = MOD( INT( R_RANDOM * 15 ) , 15 ) + 1

      SELECT CASE( SZTMP(ICR:ICR) )
      CASE( "X" )
        GUID(ICR:ICR) = SZHEX(IDX:IDX)
      CASE( "Y" )
        IDX = IOR( IAND( (IDX-1) , 3 ) , 8 ) + 1
        GUID(ICR:ICR) = SZHEX(IDX:IDX)
      CASE( "4" )
        GUID(ICR:ICR) = "4"
      END SELECT

    END DO

  END SUBROUTINE MR_GEN_GUID_N_LUID

  END SUBROUTINE MR_CREATE_FILE_XMDF

  END MODULE MR_MOD_CREATE_FILE_XMDF