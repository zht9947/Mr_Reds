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

    REAL   (TMRD_KIND) , ALLOCATABLE :: DT_ALTER
    REAL   (PARD_KIND) , ALLOCATABLE :: PHI_ALTER
    INTEGER(TSID_KIND) , ALLOCATABLE :: NTSS_ALTER
    INTEGER(TSID_KIND) , ALLOCATABLE :: NTSS_OUTPUT_ALTER
    REAL   (TMRD_KIND) , ALLOCATABLE :: T_START_ALTER

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
      WRITE(*,'(  "  3- (optional)")')
      WRITE(*,'(  "      ONE or MORE alternative timing options with the following format:")')
      WRITE(*,'(  "        --<identifier> <value>")')
      WRITE(*,'(  "    where <identifier> must be chosen from the following list and corresponding <value>")')
      WRITE(*,'(  "    needs to be specified:")')
      WRITE(*,'(  "    A-  dt")')
      WRITE(*,'(  "        Time interval, in seconds;")')
      WRITE(*,'(  "    B-  phi")')
      WRITE(*,'(  "        Time relaxation factor;")')
      WRITE(*,'(  "    C-  nsteps")')
      WRITE(*,'(  "        Total number of timesteps computed;")')
      WRITE(*,'(  "    D-  ndsteps")')
      WRITE(*,'(  "        Number of timesteps between two successive outputs;")')
      WRITE(*,'(  "    E-  t0")')
      WRITE(*,'(  "        Staring time (only for cold mode), in either relative or Julian sense, in seconds;")')
      WRITE(*,'(  "    Or,")')
      WRITE(*,'(  "      If omitted, default values or those read from the project file will be assigned to")')
      WRITE(*,'(  "    ALL these timing variables;")')
      WRITE(*,'(  "  Note,")')
      WRITE(*,'(  "    ALL the alternative options A--E can be specified in any order;")')
      WRITE(*,'(  "Note,")')
      WRITE(*,'(  "  ALL the arguments 1--3 MUST be given in sequence.")')
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
    IF( ALLOCATED( DT_ALTER ) ) THEN
      DT = DT_ALTER
    END IF
    IF( ALLOCATED( PHI_ALTER ) ) THEN
      PHI = PHI_ALTER
    END IF
    IF( ALLOCATED( NTSS_ALTER ) ) THEN
      NTSS = NTSS_ALTER
    END IF
    IF( ALLOCATED( NTSS_OUTPUT_ALTER ) ) THEN
      NTSS_OUTPUT = NTSS_OUTPUT_ALTER
    END IF
    IF( ALLOCATED( T_START_ALTER ) ) THEN
      T_START = T_START_ALTER
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

    USE MR_MOD_OPERATOR_CHAR_STRING

    IMPLICIT NONE

    CHARACTER( 2**08 )               :: CHAR_ARGUMENT
    CHARACTER( 2**03 )               :: I_ARG_CHAR
    INTEGER                          :: I_ARG

    INTEGER                          :: I_ARG_ALTER_START

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
        SELECT CASE( .MRCHARUPPER.(TRIM(CHAR_ARGUMENT)) )
        CASE( "--HELP" , "-HELP" , "--H" , "-H" )
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
    ELSE IF( COMMAND_ARGUMENT_COUNT() > 12 ) THEN
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
      CALL GET_COMMAND_ARGUMENT( I_ARG , CHAR_ARGUMENT , STATUS=ERROR )
      IF( ERROR /= 0 ) THEN
        ERROR = - ABS(ERROR)
        ERRMSG = "Error in getting command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
        RETURN
      ELSE
        IF( CHAR_ARGUMENT(1:2) /= "--" ) THEN
          FILE_PRJ = TRIM(CHAR_ARGUMENT)
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
        ELSE
          I_ARG = I_ARG - 1
          FILE_PRJ = ""
        END IF
      END IF

    ELSE
     !BLOCK
    ! ASSIGN DEFAULT VALUES TO OPTIONAL ARGUMENTS
      FILE_PRJ = ""
     !END BLOCK
    END IF

    I_ARG_ALTER_START = I_ARG + 1
  ! LOOP FOR ALTERNATIVE OPTIONS
    DO I_ARG = I_ARG_ALTER_START , COMMAND_ARGUMENT_COUNT() , 2

      WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG
    ! GET ALTERNATIVE OPTION IDENTIFIER
      CALL GET_COMMAND_ARGUMENT( I_ARG , CHAR_ARGUMENT , STATUS=ERROR )
      IF( ERROR /= 0 ) THEN
        ERROR = - ABS(ERROR)
        ERRMSG = "Error in getting command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
        RETURN
      ELSE IF( CHAR_ARGUMENT(1:2) /= "--" ) THEN
        ERROR = - 1
        ERRMSG = "There ought to be an alternative option identifier started with ""--"" "   &
        //"in command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
        RETURN
      ELSE

        SELECT CASE( .MRCHARUPPER.(TRIM(CHAR_ARGUMENT)) )
        CASE( "--DT" )
          ALLOCATE( DT_ALTER )

          WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG + 1
        ! GET ALTERNATIVE TIME INTERVAL, IN SECONDS
          CALL GET_COMMAND_ARGUMENT( I_ARG + 1 , CHAR_ARGUMENT , STATUS=ERROR )
          IF( ERROR /= 0 ) THEN
            ERROR = - ABS(ERROR)
            ERRMSG = "Error in getting command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
            RETURN
          ELSE
            IF( VERIFY( TRIM(CHAR_ARGUMENT) , "-+0123456789Ee." ) /= 0 ) THEN
              ERROR = - 1
              ERRMSG = "Illegal character in command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
              RETURN
            ELSE
              READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) DT_ALTER
              IF( ERROR /= 0 ) THEN
                ERROR = - ABS(ERROR)
                ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
                RETURN
              ELSE IF( DT_ALTER <= 0.0 ) THEN
                ERROR = - 1
                ERRMSG = "Illegal value for command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
                RETURN
              END IF
            END IF
          END IF

        CASE( "--PHI" )
          ALLOCATE( PHI_ALTER )

          WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG + 1
        ! GET ALTERNATIVE TIME RELAXATION FACTOR
          CALL GET_COMMAND_ARGUMENT( I_ARG + 1 , CHAR_ARGUMENT , STATUS=ERROR )
          IF( ERROR /= 0 ) THEN
            ERROR = - ABS(ERROR)
            ERRMSG = "Error in getting command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
            RETURN
          ELSE
            IF( VERIFY( TRIM(CHAR_ARGUMENT) , "-+0123456789Ee." ) /= 0 ) THEN
              ERROR = - 1
              ERRMSG = "Illegal character in command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
              RETURN
            ELSE
              READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) PHI_ALTER
              IF( ERROR /= 0 ) THEN
                ERROR = - ABS(ERROR)
                ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
                RETURN
              ELSE IF( PHI_ALTER < 0.0 ) THEN
                ERROR = - 1
                ERRMSG = "Illegal value for command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
                RETURN
              END IF
            END IF
          END IF

        CASE( "--NSTEPS" , "--NTSS" )
          ALLOCATE( NTSS_ALTER )

          WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG + 1
        ! GET ALTERNATIVE TOTAL NUMBER OF TIMESTEPS COMPUTED
          CALL GET_COMMAND_ARGUMENT( I_ARG + 1 , CHAR_ARGUMENT , STATUS=ERROR )
          IF( ERROR /= 0 ) THEN
            ERROR = - ABS(ERROR)
            ERRMSG = "Error in getting command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
            RETURN
          ELSE
            IF( VERIFY( TRIM(CHAR_ARGUMENT) , "0123456789" ) /= 0 ) THEN
              ERROR = - 1
              ERRMSG = "Illegal character in command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
              RETURN
            ELSE
              READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) NTSS_ALTER
              IF( ERROR /= 0 ) THEN
                ERROR = - ABS(ERROR)
                ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
                RETURN
              END IF
            END IF
          END IF

        CASE( "--NDSTEPS" , "--NDTSS" )
          ALLOCATE( NTSS_OUTPUT_ALTER )

          WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG + 1
        ! GET ALTERNATIVE NUMBER OF TIMESTEPS BETWEEN TWO OUTPUTS
          CALL GET_COMMAND_ARGUMENT( I_ARG + 1 , CHAR_ARGUMENT , STATUS=ERROR )
          IF( ERROR /= 0 ) THEN
            ERROR = - ABS(ERROR)
            ERRMSG = "Error in getting command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
            RETURN
          ELSE
            IF( VERIFY( TRIM(CHAR_ARGUMENT) , "0123456789" ) /= 0 ) THEN
              ERROR = - 1
              ERRMSG = "Illegal character in command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
              RETURN
            ELSE
              READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) NTSS_OUTPUT_ALTER
              IF( ERROR /= 0 ) THEN
                ERROR = - ABS(ERROR)
                ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
                RETURN
              ELSE IF( NTSS_OUTPUT_ALTER == 0 ) THEN
                ERROR = - 1
                ERRMSG = "Illegal value for command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
                RETURN
              END IF
            END IF
          END IF

        CASE( "--T0" )
          ALLOCATE( T_START_ALTER )

          WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG + 1
        ! GET ALTERNATIVE DEFLECTION ANGLE AT REFERENCE CROSSOVER SECTION, IN DEGREES
          CALL GET_COMMAND_ARGUMENT( I_ARG + 1 , CHAR_ARGUMENT , STATUS=ERROR )
          IF( ERROR /= 0 ) THEN
            ERROR = - ABS(ERROR)
            ERRMSG = "Error in getting command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
            RETURN
          ELSE
            IF( VERIFY( TRIM(CHAR_ARGUMENT) , "-+0123456789Ee." ) /= 0 ) THEN
              ERROR = - 1
              ERRMSG = "Illegal character in command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
              RETURN
            ELSE
              READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) T_START_ALTER
              IF( ERROR /= 0 ) THEN
                ERROR = - ABS(ERROR)
                ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
                RETURN
              END IF
            END IF
          END IF

        CASE DEFAULT
          ERROR = - 1
          ERRMSG = "Illegal alternative option identifier from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
          RETURN
        END SELECT

      END IF

    END DO

  END SUBROUTINE MR_INIT_COMMAND_LINE

  END PROGRAM MR_REDS