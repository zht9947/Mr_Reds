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
  PROGRAM MR_REDS_AVERAGE

    USE MR_ERRORS_FILE_MANIPULATE

    USE MR_KINDS

    USE MR_MOD_CTRL_RETRY_CREATING_FILES

    USE MR_MOD_INIT_PRJ
    USE MR_MOD_ECHO_PRJ
    USE MR_MOD_CTRL_PRJ_SETS_CORRECT

    USE MR_MOD_INIT_RANKS

    USE MR_MOD_MALLOC_GRID_SYS
    USE MR_MOD_MALLOC_CURVED_GEOS
    USE MR_MOD_MALLOC_FIELD_VARS
    USE MR_MOD_MALLOC_FIELD_VARS_AVERAGE
    USE MR_MOD_MALLOC_ACTIVITY

    USE MR_MOD_INIT_GRID_SYS
    USE MR_MOD_INIT_CURVED_GEOS

    USE MR_MOD_INIT_OUTPUT_AVERAGE

    USE MR_MOD_GET_TIMES
    USE MR_MOD_INPUT
    USE MR_MOD_UPDT_FIELD_VARS_AVERAGE
    USE MR_MOD_OUTPUT_AVERAGE

    IMPLICIT NONE

    CHARACTER( 2**08 ) :: FILE_PRJ
    CHARACTER( 2**08 ) :: FILE_XMDF
    CHARACTER( 2**08 ) :: FILE_AVERAGE

    INTEGER(TSID_KIND) :: ITS

    REAL   (TMRD_KIND) :: T

    INTEGER            :: ERROR
    CHARACTER( 2**10 ) :: ERRMSG

   !BLOCK
  ! MANAGE THE VERSION
    WRITE(*,'( A ,"_Average by ", A ," [ver.", A ,"]")') TRIM(INNERNAME) , TRIM(CONTRIBUTOR) , TRIM(SEMVER)
   !END BLOCK

  ! GET COMMAND ARGUMENTS FROM COMMAND LINE
  ! AND SET OUTPUT FILES'S PATH\NAMES
    CALL MR_INIT_COMMAND_LINE( ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      WRITE(*,'(/,"PLEASE RUN ", A ,"_Average with the following command arguments:")') TRIM(INNERNAME)
      WRITE(*,'(  "  1- (non-optional)")')
      WRITE(*,'(  "      Project file''s path\name, in TEXT format;")')
      WRITE(*,'(  "  2- (non-optional)")')
      WRITE(*,'(  "      Mesh file''s path\name, in XMDF format;")')
      WRITE(*,'(  "Note,")')
      WRITE(*,'(  "  ALL the arguments MUST be given in sequence.")')
      STOP
    END IF
  ! CREATE OUTPUT FILES
    CALL MR_INIT_OUTPUT_FILES( "NEWCREATE" , ERROR , ERRMSG )
    IF( ERROR == ERROR_CANNOT_CREATE_NEW_FILE ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      WRITE(*,'(/,"Files with the same names may already exist.")')
      CALL MR_CTRL_RETRY_CREATING_FILES
      CALL MR_INIT_OUTPUT_FILES( "OVERWRITE" , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF
    ELSE IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF

    WRITE(*,'( )')

    WRITE(*,'("Initialize project... ", $ )')
    CALL MR_INIT_PRJ( FILE_PRJ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done! ")')

    CALL MR_ECHO_PRJ

    WRITE(*,'( )')

    CALL MR_CTRL_PRJ_SETS_CORRECT

    WRITE(*,'( )')

    WRITE(*,'("Initialize ranks... ", $ )')
    CALL MR_INIT_RANKS( FILE_XMDF , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    ELSE
      WRITE(*,'("Done! ")')
      WRITE(*,'(2X,"Allocate memories... ", $ )')
      CALL MR_MALLOC_GRID_SYS
      CALL MR_MALLOC_CURVED_GEOS
      CALL MR_MALLOC_FIELD_VARS
      CALL MR_MALLOC_FIELD_VARS_AVERAGE
      CALL MR_MALLOC_ACTIVITY
    END IF
    WRITE(*,'("Done! ")')

    WRITE(*,'( )')

    WRITE(*,'("Initialize grid system... ", $ )')
    CALL MR_INIT_GRID_SYS( FILE_XMDF , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done! ")')

    WRITE(*,'("Initialize curved geometry... ", $ )')
    CALL MR_INIT_CURVED_GEOS( FILE_XMDF , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done! ")')

    WRITE(*,'("Initialize output... ", $ )')
    CALL MR_INIT_OUTPUT_AVERAGE( FILE_AVERAGE , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done! ")')

    WRITE(*,'( )')

    WRITE(*,'("Get the maximum number of timesteps and corresponding time, ")')
    WRITE(*,'("and the data of the last time will be averaged... ", $ )')
    CALL MR_GET_NTSS_N_T_NTSS( FILE_XMDF , ITS , T , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done! ")')

    WRITE(*,'( )')

    WRITE(*,'("Input... ", $ )')
    CALL MR_INPUT( FILE_XMDF , ITS , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done! ")')

    CALL MR_UPDT_FIELD_VARS_AVERAGE

    WRITE(*,'("Output... ", $ )')
    CALL MR_OUTPUT_AVERAGE( FILE_AVERAGE, T , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done! ")')

    WRITE(*,'(/,"The result has been written into the file:",/,4X, A )') TRIM(FILE_AVERAGE)

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
    IF( COMMAND_ARGUMENT_COUNT() < 2 ) THEN
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
      CALL MR_OPEN_FILE_DEFAULT( FILE_PRJ , "READ" , FILE_ID , ERROR , ERRMSG )
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

    I_ARG = 2
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
      CALL MR_OPEN_FILE_XMDF( FILE_XMDF , "READ" , FILE_ID , ERROR , ERRMSG )
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

  ! SET AVERAGE FILE'S PATH\NAME
    FILE_AVERAGE = TRIM(FILE_XMDF)//".average.txt"

  END SUBROUTINE MR_INIT_COMMAND_LINE

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
  SUBROUTINE MR_INIT_OUTPUT_FILES( FILE_STATUS , ERROR , ERRMSG )

    USE MR_MOD_CREATE_FILE_DEFAULT

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_STATUS

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    CALL MR_CREATE_FILE_DEFAULT( FILE_AVERAGE , FILE_STATUS , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_AVERAGE)//" as output file"
      RETURN
    END IF

  END SUBROUTINE MR_INIT_OUTPUT_FILES

  END PROGRAM MR_REDS_AVERAGE