#INCLUDE 'MR_H_PROGRAM_METADATA.H'
!***********************************************************************************************************************************
! UNIT:
!
!  (PROGRAM)
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
  PROGRAM MR_REDS

    USE MR_KINDS

    USE MR_DEF_TIMING
    USE MR_DEF_CONSTS_N_REF_PARS , ONLY : COR

    USE MR_NUM_START_MODE

    USE MR_MOD_DETER_START_MODE

    USE MR_MOD_INIT_PRJ
    USE MR_MOD_INIT_RANKS
    USE MR_MOD_INIT_RANKS_PLUS
    USE MR_MOD_INIT_SLOPE

    USE MR_MOD_CTRL_CONFIRM_START_MODE

    USE MR_MOD_ECHO_PRJ
    USE MR_MOD_CTRL_CONFIRM_PRJ_SETS

    USE MR_MOD_MALLOC_GRID_SYS
    USE MR_MOD_MALLOC_CURVED_GEOS
    USE MR_MOD_MALLOC_FIELD_VARS
    USE MR_MOD_MALLOC_ACTIVITY

    USE MR_MOD_INIT_CONSTS_N_REF_PARS
    USE MR_MOD_INIT_GRID_SYS
    USE MR_MOD_INIT_CURVED_GEOS
    USE MR_MOD_INIT_FIELD_VARS_N_ACTIVITY

    USE MR_MOD_INIT_OUTPUT

    USE MR_MOD_UPDT_TBUV
    USE MR_MOD_UPDT_KI_N_DI
    USE MR_MOD_UPDT_HYD
    USE MR_MOD_UPDT_H
    USE MR_MOD_UPDT_VZWW

    USE MR_MOD_OUTPUT

    IMPLICIT NONE

    CHARACTER( 2**08 ) :: FILE_XMDF
    CHARACTER( 2**08 ) :: FILE_PRJ

    REAL   (PARD_KIND) :: HTH

    INTEGER(TSID_KIND) :: ITS

    REAL   (TMRD_KIND) :: T

    INTEGER            :: ERROR
    CHARACTER( 2**10 ) :: ERRMSG

   !BLOCK
  ! MANAGE THE VERSION
    WRITE(*,'( A ," by ", A ," [ver.", A ,"]")') TRIM(INNERNAME) , TRIM(CONTRIBUTOR) , TRIM(SEMVER)
   !END BLOCK

  ! GET COMMAND ARGUMENTS FROM COMMAND LINE
    CALL MR_INIT_COMMAND_LINE( ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      WRITE(*,'(/,"PLEASE RUN ", A ," with the following command arguments:")') TRIM(INNERNAME)
      WRITE(*,'(  "  1- (non-optional)")')
      WRITE(*,'(  "      Mesh file''s path\name, in XMDF format;")')
      WRITE(*,'(  "  2- (optional)")')
      WRITE(*,'(  "      Project file''s path\name, which specifies the running parameters, in TEXT format;")')
      WRITE(*,'(  "    Or,")')
      WRITE(*,'(  "      If omitted, default values will be assigned to these parameters;")')
      WRITE(*,'(  "Note,")')
      WRITE(*,'(  "  ALL the arguments 1--2 MUST be given in sequence.")')
      STOP
    END IF

  ! DETERMINE START MODE BY DETECTING XMDF FILE
    CALL MR_DETER_START_MODE( FILE_XMDF , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF

    WRITE(*,'( )')

    WRITE(*,'("Initialize project... ", $ )')
    IF( FILE_PRJ /= "" ) THEN
      CALL MR_INIT_PRJ( FILE_PRJ , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF
    END IF
    CALL MR_INIT_RANKS( FILE_XMDF , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    SELECT CASE( START_MODE )
    CASE( COLD_MODE )
      WRITE(*,'(//,"No datasets seem in the XMDF file.")')
      CALL MR_CTRL_CONFIRM_START_MODE_COLD( HTH )
      WRITE(*,'(/,"Initialize project... ", $ )')
    CASE( HOT_MODE )
      WRITE(*,'(//,"Datasets have been detected in the XMDF file.")')
      CALL MR_CTRL_CONFIRM_START_MODE_HOT
      CALL MR_INIT_RANKS_PLUS( FILE_XMDF , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF
      CALL MR_INIT_SLOPE( FILE_XMDF , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF
      WRITE(*,'(/,"Initialize project... ", $ )')
    END SELECT
    WRITE(*,'("Done!")')

    WRITE(*,'( )')

    CALL MR_ECHO_PRJ

    WRITE(*,'( )')

    CALL MR_CTRL_CONFIRM_PRJ_SETS

    WRITE(*,'( )')

    WRITE(*,'(2X,"Allocate memories... ", $ )')
    CALL MR_MALLOC_GRID_SYS
    CALL MR_MALLOC_CURVED_GEOS
    CALL MR_MALLOC_FIELD_VARS
    CALL MR_MALLOC_ACTIVITY
    WRITE(*,'("Done!")')

    WRITE(*,'( )')

    WRITE(*,'("Initialize constants and reference parameters... ", $ )')
    CALL MR_INIT_CONSTS_N_REF_PARS( ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done!")')

    WRITE(*,'("Initialize grid system... ", $ )')
    CALL MR_INIT_GRID_SYS( FILE_XMDF , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done!")')

    WRITE(*,'("Initialize curved geometry... ", $ )')
    CALL MR_INIT_CURVED_GEOS( FILE_XMDF , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done!")')

    WRITE(*,'("Initialize field variables and activity... ", $ )')
    SELECT CASE( START_MODE )
    CASE( COLD_MODE )
      CALL MR_INIT_FIELD_VARS_N_ACTIVITY_COLD( HTH , T )
      WRITE(*,'("Done!")')
      WRITE(*,'("Initialize output... ", $ )')
      CALL MR_INIT_OUTPUT( FILE_XMDF , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      ELSE
        CALL MR_OUTPUT( FILE_XMDF , T , ERROR , ERRMSG )
        IF( ERROR < 0 ) THEN
          WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
          STOP
        END IF
      END IF
    CASE( HOT_MODE )
      CALL MR_INIT_FIELD_VARS_N_ACTIVITY_HOT( FILE_XMDF , T , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF
    END SELECT
    WRITE(*,'("Done!")')

    WRITE(*,'( )')

    WRITE(*,'(8X,"Compute...  0.00%", A , $ )') ACHAR(13)

    DT = DT * COR   ! NONDIMENSIONALIZE DT

    DO ITS = 1 , NTSS

      CALL MR_UPDT_TBUV
      CALL MR_UPDT_KI_N_DI
     !CALL MR_UPDT_SED
      CALL MR_UPDT_HYD
      CALL MR_UPDT_H
     !CALL MR_UPDT_R
      CALL MR_UPDT_VZWW

      T = T + ( DT / COR )
      IF( MOD( ITS , NTSS_OUTPUT ) == 0 ) THEN

        CALL MR_OUTPUT( FILE_XMDF , T , ERROR , ERRMSG )
        IF( ERROR < 0 ) THEN
          WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
          STOP
        END IF

      END IF

      WRITE(*,'(8X,"Compute...",F6.2,"%", A , $ )') REAL(ITS)/REAL(NTSS)*100.00 , ACHAR(13)

    END DO

    WRITE(*,'(8X,"Compute... Done! ")')

    WRITE(*,'(/,"The result has been written into the file:",/,4X, A )') TRIM(FILE_XMDF)

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
  SUBROUTINE MR_INIT_COMMAND_LINE( ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_DEFAULT
    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF

    IMPLICIT NONE

    CHARACTER( 2**08 )               :: CHAR_ARGUMENT
    CHARACTER( 2**03 )               :: I_ARG_CHAR
    INTEGER                          :: I_ARG

    INTEGER                          :: FILE_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""

  ! HELP DETECT
    IF( COMMAND_ARGUMENT_COUNT() == 1 ) THEN
      CALL GET_COMMAND_ARGUMENT( 1 , CHAR_ARGUMENT , STATUS=ERROR )
      IF( ERROR /= 0 ) THEN
        ERROR = - ABS(ERROR)
        ERRMSG = "Error in getting command arguments"
        RETURN
      ELSE
        SELECT CASE( TRIM(CHAR_ARGUMENT) )
        CASE( "--H" , "--h" , "--HELP" , "--HELp" , "--HElp" , "--Help" , "--help" ,   &
        &      "-H" ,  "-h" ,  "-HELP" ,  "-HELp" ,  "-HElp" ,  "-Help" ,  "-help"   &
        )
          ERROR = - 1
          ERRMSG = "Help information is displayed as below"
          RETURN
        END SELECT
      END IF
    END IF

  ! NUMBER OF COMMAND ARGUMENTS DETECT
    IF( COMMAND_ARGUMENT_COUNT() < 1 ) THEN
      ERROR = - 1
      ERRMSG = "Not enough command arguments"
      RETURN
    ELSE IF( COMMAND_ARGUMENT_COUNT() > 2 ) THEN
      ERROR = - 1
      ERRMSG = "Too many command arguments"
      RETURN
    END IF

    I_ARG = 1
    WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG
  ! GET XMDF FILE'S PATH\NAME
    CALL GET_COMMAND_ARGUMENT( I_ARG , FILE_XMDF , STATUS=ERROR )
    IF( ERROR == - 1 ) THEN
      ERRMSG = "Mesh file's path\name too long!"
      RETURN
    ELSE IF( ERROR /= 0 ) THEN
      ERROR = - ABS(ERROR)
      ERRMSG = "Error in getting command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))//" as mesh file"
      RETURN
    ELSE
    ! VERIFY XMDF FILE'S OPENING AND CLOSING
      CALL MR_OPEN_FILE_XMDF( FILE_XMDF , FILE_ID , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF)//" as mesh file"
        RETURN
      ELSE
        CALL MR_CLOSE_FILE_XMDF( FILE_ID , ERROR , ERRMSG )
        IF( ERROR < 0 ) THEN
          ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF)
          RETURN
        END IF
      END IF
    END IF

    IF( COMMAND_ARGUMENT_COUNT() > 1 ) THEN

      I_ARG = 2
      WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG
    ! GET PROJECT FILE'S PATH\NAME
      CALL GET_COMMAND_ARGUMENT( I_ARG , FILE_PRJ , STATUS=ERROR )
      IF( ERROR == - 1 ) THEN
        ERRMSG = "Project file's path\name too long"
        RETURN
      ELSE IF( ERROR /= 0 ) THEN
        ERROR = - ABS(ERROR)
        ERRMSG = "Error in getting command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))//" as project file"
        RETURN
      ELSE
      ! VERIFY PROJECT FILE'S OPENING AND CLOSING
        CALL MR_OPEN_FILE_DEFAULT( FILE_PRJ , FILE_ID , ERROR , ERRMSG )
        IF( ERROR < 0 ) THEN
          ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_PRJ)//" as project file"
          RETURN
        ELSE
          CALL MR_CLOSE_FILE_DEFAULT( FILE_ID , ERROR , ERRMSG )
          IF( ERROR < 0 ) THEN
            ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_PRJ)
            RETURN
          END IF
        END IF
      END IF

    ELSE
     !BLOCK
    ! ASSIGN DEFAULT VALUES TO OPTIONAL ARGUMENTS
      FILE_PRJ = ""
     !END BLOCK
    END IF

  END SUBROUTINE MR_INIT_COMMAND_LINE

  END PROGRAM MR_REDS