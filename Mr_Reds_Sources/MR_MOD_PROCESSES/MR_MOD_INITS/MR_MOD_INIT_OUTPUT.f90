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
  MODULE MR_MOD_INIT_OUTPUT

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_FIELD_VARS_DSET_NAMES
    USE MR_DEF_FIELD_VARS_UNITS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_INIT_OUTPUT

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
  SUBROUTINE MR_INIT_OUTPUT( FILE_XMDF_NAME , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MULTI_DSETS

    USE MR_MOD_CREATE_DSET

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME

    INTEGER                          :: FILE_XMDF_ID , MULTI_DSETS_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY

    CHARACTER( 2**08 )               :: PATH_DSET_IN_MULTI_DSETS
    CHARACTER( 2**05 )               :: UNIT_DSET

    CHARACTER( 2**03 )               :: K_CHAR
    INTEGER(KKID_KIND)               :: K

    ERRMSG = ""

    CALL MR_OPEN_FILE_XMDF( FILE_XMDF_NAME , "WRITE" , FILE_XMDF_ID , ERROR , ERRMSG )
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

  ! CREATE ZB, ZS AND H
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/"//DSET_NAME_ZB
    UNIT_DSET = UNIT_ZB
    CALL MR_CREATE_DSET_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , UNIT_DSET , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing output for ZB(:,:) "   &
      //"in multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//DSET_NAME_ZS
    UNIT_DSET = UNIT_ZS
    CALL MR_CREATE_DSET_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , UNIT_DSET , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing output for ZS(:,:) "   &
      //"in multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//DSET_NAME_H
    UNIT_DSET = UNIT_H
    CALL MR_CREATE_DSET_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , UNIT_DSET , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing output for H(:,:) "   &
      //"in multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! CREATE UVA
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//DSET_NAME_UVA
    UNIT_DSET = UNIT_UVA
    CALL MR_CREATE_DSET_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , UNIT_DSET , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing output for UVA(:,:) "   &
      //"in multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! CREATE UV
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/"//DSET_NAME_UV
    UNIT_DSET = UNIT_UV
    CALL MR_CREATE_DSET_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , UNIT_DSET , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing output for UV(:,:,"//TRIM(ADJUSTL(K_CHAR))//") "   &
      //"in multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

  ! CREATE W
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/"//DSET_NAME_W
    UNIT_DSET = UNIT_W
    CALL MR_CREATE_DSET_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , UNIT_DSET , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing output for WW(:,:,"//TRIM(ADJUSTL(K_CHAR))//") "   &
      //"in multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

  ! CREATE TBUV
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//DSET_NAME_TBUV
    UNIT_DSET = UNIT_TBUV
    CALL MR_CREATE_DSET_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , UNIT_DSET , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing output for TBUV(:,:) "   &
      //"in multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! CREATE VZWW
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/Eddy Viscosity/"//DSET_NAME_VZWW
    UNIT_DSET = UNIT_VZWW
    CALL MR_CREATE_DSET_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , UNIT_DSET , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing output for VZWW(:,:,"//TRIM(ADJUSTL(K_CHAR))//") "   &
      //"in multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

  ! CREATE KI
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/Eddy Viscosity/"   &
    //"K-Epsilon Model/"//DSET_NAME_KI
    UNIT_DSET = UNIT_KI
    CALL MR_CREATE_DSET_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , UNIT_DSET , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing output for KI(:,:,"//TRIM(ADJUSTL(K_CHAR))//") "   &
      //"in multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

  ! CREATE DI
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/Eddy Viscosity/"   &
    //"K-Epsilon Model/"//DSET_NAME_DI
    UNIT_DSET = UNIT_DI
    CALL MR_CREATE_DSET_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , UNIT_DSET , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing output for DI(:,:,"//TRIM(ADJUSTL(K_CHAR))//") "   &
      //"in multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

  ! CREATE R
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/"//DSET_NAME_R
    UNIT_DSET = UNIT_R
    CALL MR_CREATE_DSET_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , UNIT_DSET , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing output for R(:,:,"//TRIM(ADJUSTL(K_CHAR))//") "   &
      //"in multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

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

  END SUBROUTINE MR_INIT_OUTPUT

  END MODULE MR_MOD_INIT_OUTPUT