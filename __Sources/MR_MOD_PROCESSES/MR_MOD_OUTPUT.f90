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
  MODULE MR_MOD_OUTPUT

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_GRID_SYS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_FIELD_VARS_DSET_NAMES
    USE MR_DEF_ACTIVITY

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_OUTPUT

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: ZBU , ZBV , ZBOO

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: ZSOO
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: HOO

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: HSTOROO

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
  SUBROUTINE MR_OUTPUT( FILE_XMDF_NAME , T , ERROR , ERRMSG , OVERWRITE )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MULTI_DSETS

    USE MR_MOD_WRITE_FIELD_VARS_N_ACTIVITY

    USE MR_MOD_INTERP_XY
    USE MR_MOD_INTERP_Z

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME

    REAL   (TMRD_KIND) , INTENT(IN ) :: T

    INTEGER                          :: FILE_XMDF_ID , MULTI_DSETS_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY

    LOGICAL            , INTENT(IN ) , OPTIONAL :: OVERWRITE

    REAL   (PARD_KIND)               :: DUMMY_BASE , DUMMY_REF

    CHARACTER( 2**08 )               :: PATH_DSET_IN_MULTI_DSETS

    CHARACTER( 2**03 )               :: K_CHAR
    INTEGER(KKID_KIND)               :: K

    CALL MR_OPEN_FILE_XMDF( FILE_XMDF_NAME , FILE_XMDF_ID , ERROR , ERRMSG , READONLY=.FALSE. )
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

  ! WRITE TBFUV
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//TRIM(DSET_NAME_TBFUV)
    DUMMY_BASE = 0.0 ; DUMMY_REF = TUVR
    IF( PRESENT(OVERWRITE) .AND. OVERWRITE ) THEN
      CALL MR_WRITE_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO , JUV , JUU , JVV , JOO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & UV=TBFUV ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG , OVERWRITE=OVERWRITE )
    ELSE
      CALL MR_WRITE_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO , JUV , JUU , JVV , JOO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & UV=TBFUV ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG )
    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when outputting TBFUV(:,:) to it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! WRITE TBUV
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//TRIM(DSET_NAME_TBUV)
    DUMMY_BASE = 0.0 ; DUMMY_REF = TUVR
    IF( PRESENT(OVERWRITE) .AND. OVERWRITE ) THEN
      CALL MR_WRITE_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO , JUV , JUU , JVV , JOO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & UV=TBUV ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG , OVERWRITE=OVERWRITE )
    ELSE
      CALL MR_WRITE_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO , JUV , JUU , JVV , JOO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & UV=TBUV ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG )
    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when outputting TBUV(:,:) to it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! WRITE KI
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/Eddy Viscosity/"   &
    //"K-Epsilon Model/"//TRIM(DSET_NAME_KI)//" ("//TRIM(ADJUSTL(K_CHAR))//")"
    DUMMY_BASE = 0.0 ; DUMMY_REF = KIR
    IF( PRESENT(OVERWRITE) .AND. OVERWRITE ) THEN
      CALL MR_WRITE_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & SS=KI(:,:, K ) ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG , OVERWRITE=OVERWRITE )
    ELSE
      CALL MR_WRITE_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & SS=KI(:,:, K ) ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG )
    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when outputting KI(:,:,"//TRIM(ADJUSTL(K_CHAR))//") to it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

  ! WRITE DI
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/Eddy Viscosity/"   &
    //"K-Epsilon Model/"//TRIM(DSET_NAME_DI)//" ("//TRIM(ADJUSTL(K_CHAR))//")"
    DUMMY_BASE = 0.0 ; DUMMY_REF = DIR
    IF( PRESENT(OVERWRITE) .AND. OVERWRITE ) THEN
      CALL MR_WRITE_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & SS=DI(:,:, K ) ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG , OVERWRITE=OVERWRITE )
    ELSE
      CALL MR_WRITE_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & SS=DI(:,:, K ) ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG )
    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when outputting DI(:,:,"//TRIM(ADJUSTL(K_CHAR))//") to it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

  ! WRITE R
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/"   &
    //TRIM(DSET_NAME_R)//" ("//TRIM(ADJUSTL(K_CHAR))//")"
    DUMMY_BASE = R0 ; DUMMY_REF = RR
    IF( PRESENT(OVERWRITE) .AND. OVERWRITE ) THEN
      CALL MR_WRITE_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & SS=R(:,:, K ) ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG , OVERWRITE=OVERWRITE )
    ELSE
      CALL MR_WRITE_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & SS=R(:,:, K ) ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG )
    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when outputting R(:,:,"//TRIM(ADJUSTL(K_CHAR))//") to it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

   !BLOCK
  ! WRITE ZB, ZS AND H
    ALLOCATE( ZBU(0:NI0(NI,FDRD_KIND),1:NJ) , ZBV(1:NI1(NI,FDRD_KIND),0:NJ) )
      CALL MR_INTERP_XY_SS_U( NI , NJ , ZB , ZBU )
      CALL MR_INTERP_XY_SS_V( NI , NJ , ZB , ZBV )

    ALLOCATE( ZBOO(0:NI0(NI,FDRD_KIND),0:NJ) )
      CALL MR_CALC_ZBOO( NI , NJ , ZBU , ZBV , ZBOO )

  ! WRITE ZB
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/"//TRIM(DSET_NAME_ZB)
    DUMMY_BASE = 0.0 ; DUMMY_REF = ZR
    IF( PRESENT(OVERWRITE) .AND. OVERWRITE ) THEN
      CALL MR_WRITE_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & SS=ZB , SU=ZBU , SV=ZBV , SO=ZBOO ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG , OVERWRITE=OVERWRITE )
    ELSE
      CALL MR_WRITE_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & SS=ZB , SU=ZBU , SV=ZBV , SO=ZBOO ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG )
    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when outputting ZB(:,:) to it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      DEALLOCATE( ZBU , ZBV )
      DEALLOCATE( ZBOO )
      RETURN
    END IF

    DEALLOCATE( ZBU , ZBV )

    ALLOCATE( ZSOO(0:NI0(NI,FDRD_KIND),0:NJ) )
      CALL MR_CALC_ZSOO( NI , NJ , ZSU , ZSV , ZSOO )

  ! WRITE ZS
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//TRIM(DSET_NAME_ZS)
    DUMMY_BASE = 0.0 ; DUMMY_REF = SR
    IF( PRESENT(OVERWRITE) .AND. OVERWRITE ) THEN
      CALL MR_WRITE_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & SS=ZS , SU=ZSU , SV=ZSV , SO=ZSOO ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG , OVERWRITE=OVERWRITE )
    ELSE
      CALL MR_WRITE_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & SS=ZS , SU=ZSU , SV=ZSV , SO=ZSOO ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG )
    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when outputting ZS(:,:) to it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      DEALLOCATE( ZSOO )
      DEALLOCATE( ZBOO )
      RETURN
    END IF

    ALLOCATE( HOO(0:NI0(NI,FDRD_KIND),0:NJ) )
      ALLOCATE( HSTOROO(0:NI0(NI,FDRD_KIND),0:NJ) ) ; HSTOROO = 0.0
        CALL MR_CALC_HOO( NI , NJ , HSTOROO , ZSOO , ZBOO , HOO )
      DEALLOCATE( HSTOROO )

    DEALLOCATE( ZSOO , ZBOO )

  ! WRITE H
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//TRIM(DSET_NAME_H)
    DUMMY_BASE = 0.0 ; DUMMY_REF = ZR
    IF( PRESENT(OVERWRITE) .AND. OVERWRITE ) THEN
      CALL MR_WRITE_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & SS=H , SU=HU , SV=HV , SO=HOO ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG , OVERWRITE=OVERWRITE)
    ELSE
      CALL MR_WRITE_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & SS=H , SU=HU , SV=HV , SO=HOO ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG )
    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when outputting H(:,:) to it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      DEALLOCATE( HOO )
      RETURN
    END IF

    DEALLOCATE( HOO )
   !END BLOCK

  ! WRITE UVA
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//TRIM(DSET_NAME_UVA)
    DUMMY_BASE = 0.0 ; DUMMY_REF = UVR
    IF( PRESENT(OVERWRITE) .AND. OVERWRITE ) THEN
      CALL MR_WRITE_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO , JUV , JUU , JVV , JOO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & UV=UVA , U=UA , V=VA ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG , OVERWRITE=OVERWRITE )
    ELSE
      CALL MR_WRITE_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO , JUV , JUU , JVV , JOO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & UV=UVA , U=UA , V=VA ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG )
    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when outputting UVA(:,:) to it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! WRITE UV
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/"   &
    //TRIM(DSET_NAME_UV)//" ("//TRIM(ADJUSTL(K_CHAR))//")"
    DUMMY_BASE = 0.0 ; DUMMY_REF = UVR
    IF( PRESENT(OVERWRITE) .AND. OVERWRITE ) THEN
      CALL MR_WRITE_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO , JUV , JUU , JVV , JOO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & UV=UV(:,:,1:2, K ) , U=U(:,:, K ) , V=V(:,:, K ) ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG , OVERWRITE=OVERWRITE )
    ELSE
      CALL MR_WRITE_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO , JUV , JUU , JVV , JOO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & UV=UV(:,:,1:2, K ) , U=U(:,:, K ) , V=V(:,:, K ) ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG )
    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when outputting UV(:,:,"//TRIM(ADJUSTL(K_CHAR))//") to it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

   !BLOCK
    ALLOCATE( WW(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) )

  ! CONVERT W TO WW
    CALL MR_CONVERT_W_TO_WW( NI , NJ , NK , UA , VA , W , WW )

  ! WRITE WW
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/"   &
    //TRIM(DSET_NAME_WW)//" ("//TRIM(ADJUSTL(K_CHAR))//")"
    DUMMY_BASE = 0.0 ; DUMMY_REF = WR
    IF( PRESENT(OVERWRITE) .AND. OVERWRITE ) THEN
      CALL MR_WRITE_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & SS=WW(:,:, K ) ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG , OVERWRITE=OVERWRITE )
    ELSE
      CALL MR_WRITE_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & SS=WW(:,:, K ) ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG )
    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when outputting WW(:,:,"//TRIM(ADJUSTL(K_CHAR))//") to it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

    DEALLOCATE( WW )
   !END BLOCK

  ! WRITE VZWW
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/Eddy Viscosity/"   &
    //TRIM(DSET_NAME_VZWW)//" ("//TRIM(ADJUSTL(K_CHAR))//")"
    DUMMY_BASE = 0.0 ; DUMMY_REF = VZR
    IF( PRESENT(OVERWRITE) .AND. OVERWRITE ) THEN
      CALL MR_WRITE_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & SS=VZWW(:,:, K ) , SU=VXYU(:,:, K ) , SV=VXYV(:,:, K ) ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG , OVERWRITE=OVERWRITE )
    ELSE
      CALL MR_WRITE_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , T ,   &
      & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
      & DUMMY_BASE , DUMMY_REF ,   &
      & SS=VZWW(:,:, K ) , SU=VXYU(:,:, K ) , SV=VXYV(:,:, K ) ,   &
      & ACTIVITY=ACTIVITY , ERROR=ERROR , ERRMSG=ERRMSG )
    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when outputting VZWW(:,:,"//TRIM(ADJUSTL(K_CHAR))//") to it"
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

  END SUBROUTINE MR_OUTPUT

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
  SUBROUTINE MR_CALC_ZSOO( NI , NJ , ZSU , ZSV , ZSOO )

    USE MR_MOD_INTERP_XY

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: ZSU
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: ZSV

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: ZSOO

    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: ZSUO
    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: ZSVO

    INTEGER(IJID_KIND) :: I , J

    CALL MR_INTERP_XY_SS_O_U( NI , NJ , ZSU , ZSUO )
    CALL MR_INTERP_XY_SS_O_V( NI , NJ , ZSV , ZSVO )

    DO J = 0 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 0 , NI
        ZSOO( I , J ) = ( ZSUO( I , J )  + ZSVO( I , J )  ) / 2.0
      END DO
    END DO

  END SUBROUTINE MR_CALC_ZSOO

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
  SUBROUTINE MR_CALC_ZBOO( NI , NJ , ZBU , ZBV , ZBOO )

    USE MR_MOD_INTERP_XY

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: ZBU
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: ZBV

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: ZBOO

    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: ZBUO
    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: ZBVO

    INTEGER(IJID_KIND) :: I , J

    CALL MR_INTERP_XY_SS_O_U( NI , NJ , ZBU , ZBUO )
    CALL MR_INTERP_XY_SS_O_V( NI , NJ , ZBV , ZBVO )

    DO J = 0 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 0 , NI
        ZBOO( I , J ) = ( ZBUO( I , J )  + ZBVO( I , J )  ) / 2.0
      END DO
    END DO

  END SUBROUTINE MR_CALC_ZBOO

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
  SUBROUTINE MR_CALC_HOO( NI , NJ , HSTOROO , ZSOO , ZBOO , HOO )

    USE MR_MOD_FUNC_H

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: HSTOROO
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: ZSOO , ZBOO

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: HOO

    INTEGER(IJID_KIND) :: I , J

    DO J = 0 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 0 , NI
        HOO( I , J ) = MR_FUNC_H( HSTOROO( I , J ) , ZSOO( I , J ) , ZBOO( I , J ) )
      END DO
    END DO

  END SUBROUTINE MR_CALC_HOO

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
  SUBROUTINE MR_CONVERT_W_TO_WW( NI , NJ , NK , UA , VA , W , WW )

    USE MR_MOD_OPERATOR_SS
    USE MR_MOD_CALC_GRAD_XY

    USE MR_MOD_INTERP_Z

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ     ) :: UA
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ     ) :: VA

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,0:NK) :: W

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) :: WW

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: REDC_GRAD_UVA

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

    CALL MR_INV_INTERP_Z_SS_FROM_SW( NI , NJ , NK , W , WW )

    CALL MR_CALC_REDC_GRAD_XY_SS( NI , NJ ,   &
    & ( MU .MRSSSCL. ( UA .MRSSMTP. HU ) ) , ( MV .MRSSSCL. ( VA .MRSSMTP. HV ) ) , REDC_GRAD_UVA )

    DO K = 1 , NK

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          WW( I , J , K ) = WW( I , J , K ) * H( I , J ) -   &
          & ( 1.0 + SIGMA( K ) ) * REDC_GRAD_UVA( I , J ) / MW( I , J )
        END DO
      END DO

    END DO

  END SUBROUTINE MR_CONVERT_W_TO_WW

  END MODULE MR_MOD_OUTPUT