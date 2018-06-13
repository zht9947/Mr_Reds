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
  MODULE MR_MOD_INPUT

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_FIELD_VARS_DSET_NAMES
    USE MR_DEF_ACTIVITY

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_INPUT

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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_INPUT( FILE_XMDF_NAME , ITS , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MULTI_DSETS

    USE MR_MOD_READ_FIELD_VARS_N_ACTIVITY

    USE MR_MOD_INTERP_XY
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
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//TRIM(DSET_NAME_TBFUV)
    DUMMY_BASE = 0.0 ; DUMMY_REF = TUVR
    CALL MR_READ_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & UV=TBFUV ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when inputting TBFUV(:,:) from it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! READ TBUV
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//TRIM(DSET_NAME_TBUV)
    DUMMY_BASE = 0.0 ; DUMMY_REF = TUVR
    CALL MR_READ_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & UV=TBUV ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when inputting TBUV(:,:) from it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! READ KI
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/Eddy Viscosity/"   &
    //"K-Epsilon Model/"//TRIM(DSET_NAME_KI)//" ("//TRIM(ADJUSTL(K_CHAR))//")"
    DUMMY_BASE = 0.0 ; DUMMY_REF = KIR
    CALL MR_READ_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & SS=KI(:,:, K ) ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when inputting KI(:,:,"//TRIM(ADJUSTL(K_CHAR))//") from it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

  ! READ DI
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/Eddy Viscosity/"   &
    //"K-Epsilon Model/"//TRIM(DSET_NAME_DI)//" ("//TRIM(ADJUSTL(K_CHAR))//")"
    DUMMY_BASE = 0.0 ; DUMMY_REF = DIR
    CALL MR_READ_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & SS=DI(:,:, K ) ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when inputting DI(:,:,"//TRIM(ADJUSTL(K_CHAR))//") from it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

  ! READ R
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/"   &
    //TRIM(DSET_NAME_R)//" ("//TRIM(ADJUSTL(K_CHAR))//")"
    DUMMY_BASE = R0 ; DUMMY_REF = RR
    CALL MR_READ_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & SS=R(:,:, K ) ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when inputting R(:,:,"//TRIM(ADJUSTL(K_CHAR))//") from it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

   !BLOCK
    RI       =   0.000E+0
   !END BLOCK

   !BLOCK
    CSS      =   0.000E+0
    QSBUV    =   0.000E+0   ; QSBU    =   0.000E+0   ; QSBV    =   0.000E+0
   !END BLOCK

  ! READ ZB
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/"//TRIM(DSET_NAME_ZB)
    DUMMY_BASE = 0.0 ; DUMMY_REF = ZR
    CALL MR_READ_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,    &
    & SS=ZB ,   &
    & ACTIVITY=ACTIVITY ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when inputting ZB(:,:) from it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! READ ZS
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//TRIM(DSET_NAME_ZS)
    DUMMY_BASE = 0.0 ; DUMMY_REF = SR
    CALL MR_READ_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,    &
    & SS=ZS , SU=ZSU , SV=ZSV ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when inputting ZS(:,:) from it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! READ H
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//TRIM(DSET_NAME_H)
    DUMMY_BASE = 0.0 ; DUMMY_REF = ZR
    CALL MR_READ_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & SS=H , SU=HU , SV=HV ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when inputting H(:,:) from it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! READ UVA
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/"//TRIM(DSET_NAME_UVA)
    DUMMY_BASE = 0.0 ; DUMMY_REF = UVR
    CALL MR_READ_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & UV=UVA , U=UA , V=VA ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when inputting UVA(:,:) from it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

  ! READ UV
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/"   &
    //TRIM(DSET_NAME_UV)//" ("//TRIM(ADJUSTL(K_CHAR))//")"
    DUMMY_BASE = 0.0 ; DUMMY_REF = UVR
    CALL MR_READ_UV( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & UV=UV(:,:,1:2, K ) , U=U(:,:, K ) , V=V(:,:, K ) ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when inputting UV(:,:,"//TRIM(ADJUSTL(K_CHAR))//") from it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

   !BLOCK
    ALLOCATE( WW(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) )

  ! READ WW
    DO K = 1 , NK
  
    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/"   &
    //TRIM(DSET_NAME_WW)//" ("//TRIM(ADJUSTL(K_CHAR))//")"
    DUMMY_BASE = 0.0 ; DUMMY_REF = WR
    CALL MR_READ_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & SS=WW(:,:, K ) ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when inputting WW(:,:,"//TRIM(ADJUSTL(K_CHAR))//") from it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF
  
    END DO

  ! CONVERT WW TO W
    CALL MR_CONVERT_WW_TO_W( NI , NJ , NK , UA , VA , WW , W )

    DEALLOCATE( WW )
   !END BLOCK

   !BLOCK
  ! READ VZWW
    DO K = 1 , NK

    WRITE( K_CHAR , '(I<LEN(K_CHAR)>)' ) K
    PATH_DSET_IN_MULTI_DSETS = "Mr.Reds/Hydrodynamics/By Layers/"//"K"//TRIM(ADJUSTL(K_CHAR))//"/Eddy Viscosity/"   &
    //TRIM(DSET_NAME_VZWW)//" ("//TRIM(ADJUSTL(K_CHAR))//")"
    DUMMY_BASE = 0.0 ; DUMMY_REF = VZR
    CALL MR_READ_SS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , ITS ,   &
    & NND , NEM , NI , NJ , DUMMY_BASE , DUMMY_REF ,   &
    & SS=VZWW(:,:, K ) , SU=VXYU(:,:, K ) , SV=VXYV(:,:, K ) ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MULTI_DSETS)//" in file "//TRIM(FILE_XMDF_NAME)   &
      //" when inputting VZWW(:,:,"//TRIM(ADJUSTL(K_CHAR))//") from it"
      CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    END DO

  ! INTERPOLATE VZWW TO VZW
    CALL MR_INTERP_Z_SS_W( NI , NJ , NK , VZWW , VZW )
   !END BLOCK

   !BLOCK
    DZWW     =   VZWW       ; DXYU    =   VXYU       ; DXYV    =   VXYV
    DZW      =   VZW
   !END BLOCK

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

  END SUBROUTINE MR_INPUT

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
  SUBROUTINE MR_CONVERT_WW_TO_W( NI , NJ , NK , UA , VA , WW , W )

    USE MR_MOD_OPERATOR_SS

    USE MR_MOD_CALC_GRAD_XY
    USE MR_MOD_INTERP_Z

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ     ) :: UA
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ     ) :: VA

    REAL   (FDRD_KIND) , INTENT(INOUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) :: WW

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,0:NK) :: W

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ     ) :: REDC_GRAD_UVA

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ     ) :: WB , WS

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

    CALL MR_CALC_REDC_GRAD_XY_SS( NI , NJ ,   &
    & ( MU .MRSSSCL. ( UA .MRSSMTP. HU ) ) , ( MV .MRSSSCL. ( VA .MRSSMTP. HV ) ) , REDC_GRAD_UVA )

    DO K = 1 , NK

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          WW( I , J , K ) = ( WW( I , J , K ) +   &
          & ( 1.0 + SIGMA( K ) ) * REDC_GRAD_UVA( I , J ) / MW( I , J ) ) / H( I , J )
        END DO
      END DO

    END DO

    WB = 0.0 ; WS = 0.0

    CALL MR_INTERP_Z_SS_W( NI , NJ , NK , WW , W , SS0=WB , SSN=WS )

  END SUBROUTINE MR_CONVERT_WW_TO_W

  END MODULE MR_MOD_INPUT