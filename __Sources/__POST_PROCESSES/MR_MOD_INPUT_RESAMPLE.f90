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
  MODULE MR_MOD_INPUT_RESAMPLE

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_FIELD_VARS_DSET_NAMES
    USE MR_DEF_ACTIVITY
    USE MR_DEF_TIMING

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_INPUT_RESAMPLE

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
  SUBROUTINE MR_INPUT_RESAMPLE( FILE_XMDF_NAME , ITS , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MULTI_DSETS

    USE MR_MOD_GET_DSET_TIMES

    USE MR_MOD_READ_FIELD_VARS_N_ACTIVITY

    USE MR_MOD_INTERP_Z

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME

    INTEGER(TSID_KIND) , INTENT(IN ) :: ITS

    INTEGER                          :: FILE_XMDF_ID , MULTI_DSETS_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY

    REAL   (PARD_KIND)               :: DUMMY_BASE , DUMMY_REF

    CHARACTER( 2**08 )               :: PATH_DSET_IN_MULTI_DSETS

    CHARACTER( 2**03 )               :: K_CHAR
    INTEGER(KKID_KIND)               :: K

    ERRMSG = ""

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

  ! READ TBFUV
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//DSET_NAME_TBFUV
    DUMMY_BASE = 0.0 ; DUMMY_REF = TUVR
    CALL MR_READ_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & UV=TBFUV ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing TBFUV(:,:) "   &
      //"from multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! READ TBUV
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//DSET_NAME_TBUV
    DUMMY_BASE = 0.0 ; DUMMY_REF = TUVR
    CALL MR_READ_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & UV=TBUV ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing TBUV(:,:) "   &
      //"from multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! READ KI
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/Eddy Viscosity/"   &
    //"K-Epsilon Model/"//DSET_NAME_KI
    DUMMY_BASE = 0.0 ; DUMMY_REF = KIR
    CALL MR_READ_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & SS=KI(:,:, K ) ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing KI(:,:,"//TRIM(ADJUSTL(K_CHAR))//") "   &
      //"to multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

  ! READ DI
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/Eddy Viscosity/"   &
    //"K-Epsilon Model/"//DSET_NAME_DI
    DUMMY_BASE = 0.0 ; DUMMY_REF = DIR
    CALL MR_READ_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & SS=DI(:,:, K ) ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing DI(:,:,"//TRIM(ADJUSTL(K_CHAR))//") "   &
      //"from multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

  ! READ ZB
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/"//DSET_NAME_ZB
    DUMMY_BASE = 0.0 ; DUMMY_REF = ZR
    CALL MR_READ_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,    &
    & SS=ZB ,   &
    & ACTIVITY=ACTIVITY ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing ZB(:,:) "   &
      //"from multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! READ ZS
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//DSET_NAME_ZS
    DUMMY_BASE = 0.0 ; DUMMY_REF = SR
    CALL MR_READ_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,    &
    & SS=ZS , SU=ZSU , SV=ZSV ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing ZS(:,:) "   &
      //"from multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! READ H
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//DSET_NAME_H
    DUMMY_BASE = 0.0 ; DUMMY_REF = ZR
    CALL MR_READ_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & SS=H , SU=HU , SV=HV ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing H(:,:) "   &
      //"from multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! READ UVA
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//DSET_NAME_UVA
    DUMMY_BASE = 0.0 ; DUMMY_REF = UVR
    CALL MR_READ_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & UV=UVA , U=UA , V=VA ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing UVA(:,:) "   &
      //"from multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! READ UV
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/"//DSET_NAME_UV
    DUMMY_BASE = 0.0 ; DUMMY_REF = UVR
    CALL MR_READ_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & UV=UV(:,:,1:2, K ) , U=U(:,:, K ) , V=V(:,:, K ) ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing UV(:,:,"//TRIM(ADJUSTL(K_CHAR))//") "   &
      //"from multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

  ! READ WW
    DO K = 1 , NK
  
    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/"//DSET_NAME_WW
    DUMMY_BASE = 0.0 ; DUMMY_REF = WR
    CALL MR_READ_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & SS=WW(:,:, K ) ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing WW(:,:,"//TRIM(ADJUSTL(K_CHAR))//") "   &
      //"from multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF
  
    END DO

  ! READ R
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/"//DSET_NAME_R
    DUMMY_BASE = R0 ; DUMMY_REF = RR
    CALL MR_READ_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & SS=R(:,:, K ) ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing R(:,:,"//TRIM(ADJUSTL(K_CHAR))//") "   &
      //"from multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

  ! READ VZWW
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/Eddy Viscosity/"//DSET_NAME_VZWW
    DUMMY_BASE = 0.0 ; DUMMY_REF = VZR
    CALL MR_READ_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & SS=VZWW(:,:, K ) ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing VZWW(:,:,"//TRIM(ADJUSTL(K_CHAR))//") "   &
      //"from multiple datasets /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)
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

  END SUBROUTINE MR_INPUT_RESAMPLE

  END MODULE MR_MOD_INPUT_RESAMPLE