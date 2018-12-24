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
  PROGRAM MR_REDS_EXTEND

    USE MR_0_SKIP_MODE

    USE MR_ERRORS_FILE_MANIPULATE

    USE MR_MOD_CTRL_RETRY_CREATING_FILES

    USE MR_KINDS

    USE MR_MOD_INIT_RANKS
    USE MR_MOD_INIT_RANKS_EXTEND
    USE MR_MOD_INIT_RANKS_PLUS
    USE MR_MOD_INIT_SLOPE

    USE MR_MOD_MALLOC_GRID_SYS
    USE MR_MOD_MALLOC_GRID_SYS_

    USE MR_MOD_INIT_GRID_SYS

    USE MR_MOD_DO_EXTEND_OUTPUT_MESH_DATA

    USE MR_MOD_GET_TIMES
    USE MR_MOD_IO_EXTEND

    IMPLICIT NONE

    CHARACTER( 2**08 ) :: FILE_XMDF
    CHARACTER( 2**08 ) :: FILE_XMDF_

    INTEGER            :: NLOOPS

    INTEGER(TSID_KIND) :: NTSS
    INTEGER(TSID_KIND) :: ITS

    REAL   (TMRD_KIND) :: T

    INTEGER            :: ERROR
    CHARACTER( 2**10 ) :: ERRMSG

   !BLOCK
  ! MANAGE THE VERSION
    WRITE(*,'( A ,"_Extend by ", A ," [ver.", A ,"]")') TRIM(INNERNAME) , TRIM(CONTRIBUTOR) , TRIM(SEMVER)
   !END BLOCK

  ! GET COMMAND ARGUMENTS FROM COMMAND LINE
  ! AND SET OUTPUT FILES' PATH\NAMES
    CALL MR_INIT_COMMAND_LINE( ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      WRITE(*,'(/,"PLEASE RUN ", A ,"_Extend with the following command arguments:")') TRIM(INNERNAME)
      WRITE(*,'(  "  1- (non-optional)")')
      WRITE(*,'(  "      Source file''s path\name containing the mesh and the data to be extened,")')
      WRITE(*,'(  "    in XMDF format;")')
      WRITE(*,'(  "  2- (optional)")')
      WRITE(*,'(  "      Number of loops to which the source is expected to be extended;")')
      WRITE(*,'(  "    Or,")')
      WRITE(*,'(  "      If omitted, the source will be just duplicated;")')
      WRITE(*,'(  "  3- (optional)")')
      WRITE(*,'(  "      An alternative option, which tells the program to run on skip mode so that all")')
      WRITE(*,'(  "    the runtime inputs from user can be skipped, with the following form:")')
      WRITE(*,'(  "        --skip")')
      WRITE(*,'(  "      Careful with this option and make sure you really know what will be skipped;")')
      WRITE(*,'(  "    Or,")')
      WRITE(*,'(  "      If omitted, the program will by default run on non-skip mode;")')
      WRITE(*,'(  "Note,")')
      WRITE(*,'(  "  ALL the arguments 1--3 MUST be given in sequence.")')
      STOP
    END IF
  ! CREATE OUTPUT FILES
    CALL MR_INIT_OUTPUT_FILES( ERROR , ERRMSG , OVERWRITE=.FALSE. )
    IF( ERROR == ERROR_CANNOT_CREATE_NEW_FILE ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      WRITE(*,'(/,"Files with the same names may already exist.")')
      CALL MR_CTRL_RETRY_CREATING_FILES
      CALL MR_INIT_OUTPUT_FILES( ERROR , ERRMSG , OVERWRITE=.TRUE. )
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
    CALL MR_INIT_RANKS( FILE_XMDF , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    CALL MR_INIT_RANKS_EXTEND( NLOOPS )
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
    WRITE(*,'("Done!")')

    WRITE(*,'( )')

    WRITE(*,'(2X,"Allocate memories... ", $ )')
    CALL MR_MALLOC_GRID_SYS
    CALL MR_MALLOC_GRID_SYS_
    WRITE(*,'("Done!")')

    WRITE(*,'( )')

    WRITE(*,'(4X,"Extend mesh... ", $ )')
    CALL MR_INIT_GRID_SYS( FILE_XMDF , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    ELSE
      CALL MR_DO_EXTEND_OUTPUT_MESH_DATA( FILE_XMDF , FILE_XMDF_ , NLOOPS , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF
    END IF
    WRITE(*,'("Done!")')

    WRITE(*,'( )')

    WRITE(*,'(8X,"Extend data...  0.00%", A , $ )') ACHAR(13)

  ! GET NTSS
    CALL MR_GET_NTSS( FILE_XMDF , NTSS , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF

    DO ITS = 0 , NTSS

    ! GET T OF ITS
      CALL MR_GET_T_ITS( FILE_XMDF , NTSS , ITS , T , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF

      CALL MR_IO_EXTEND( FILE_XMDF , FILE_XMDF_ , ITS , T , NLOOPS , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF

      WRITE(*,'(8X,"Extend data...",F6.2,"%", A , $ )') REAL(ITS+1)/REAL(NTSS+1)*100.00 , ACHAR(13)

    END DO

    WRITE(*,'(8X,"Extend data... Done! ")')

    WRITE(*,'(/,"The result has been written into the file:",/,4X, A )') TRIM(FILE_XMDF_)


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

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF

    USE MR_MOD_OPERATOR_CHAR_STRING

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
        SELECT CASE( .MRCHARUPPER.(TRIM(CHAR_ARGUMENT)) )
        CASE( "--HELP" , "-HELP" , "--H" , "-H" )
          ERROR = - 1
          ERRMSG = "Help information is displayed as below"
          RETURN
        END SELECT
      END IF
    END IF

    I_ARG = 0

    I_ARG = I_ARG + 1
    IF( COMMAND_ARGUMENT_COUNT() < I_ARG ) THEN
      ERROR = - 1
      ERRMSG = "Not enough command arguments"
      RETURN
    ELSE
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
    END IF

  ! SET EXTENDED XMDF FILE'S PATH\NAME
    FILE_XMDF_ = TRIM(FILE_XMDF)//". extend"

    I_ARG = I_ARG + 1
    IF( COMMAND_ARGUMENT_COUNT() < I_ARG ) THEN
     !BLOCK
    ! ASSIGN DEFAULT VALUES TO OPTIONAL ARGUMENTS
      NLOOPS = 1
     !END BLOCK
      FILE_XMDF_ = TRIM(FILE_XMDF_)//" (1)"
    ELSE
      WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG
    ! GET NUMBER OF EXTENTED LOOPS
      CALL GET_COMMAND_ARGUMENT( I_ARG , CHAR_ARGUMENT , STATUS=ERROR )
      IF( ERROR /= 0 ) THEN
        ERROR = - ABS(ERROR)
        ERRMSG = "Error in getting command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
        RETURN
      ELSE IF( CHAR_ARGUMENT(1:2) == "--" ) THEN
       !BLOCK
        I_ARG = I_ARG - 1
      ! ASSIGN DEFAULT VALUES TO OPTIONAL ARGUMENTS
        NLOOPS = 1
       !END BLOCK
        FILE_XMDF_ = TRIM(FILE_XMDF_)//" (1)"
      ELSE IF( VERIFY( TRIM(CHAR_ARGUMENT) , "0123456789" ) /= 0 ) THEN
        ERROR = - 1
        ERRMSG = "Illegal character in command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
        RETURN
      ELSE
        READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) NLOOPS
        IF( ERROR /= 0 ) THEN
          ERROR = - ABS(ERROR)
          ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
          RETURN
        END IF
        FILE_XMDF_ = TRIM(FILE_XMDF_)//" ("//TRIM(CHAR_ARGUMENT)//")"
      END IF
    END IF

    FILE_XMDF_ = TRIM(FILE_XMDF_)//".h5"

    I_ARG = I_ARG + 1
  ! LOOP FOR ALTERNATIVE OPTIONS
    DO WHILE( I_ARG <= COMMAND_ARGUMENT_COUNT() )

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
        CASE( "--SKIP" )
          IF( RUN_ON_SKIP_MODE == .FALSE. ) THEN
            RUN_ON_SKIP_MODE = .TRUE.
          ELSE
            ERROR = - 1
            ERRMSG = TRIM(CHAR_ARGUMENT)//" has been specified more than once"
            RETURN
          END IF

          I_ARG = I_ARG + 1

        CASE DEFAULT
          ERROR = - 1
          ERRMSG = "Illegal alternative option identifier from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
          RETURN
        END SELECT

      END IF

    END DO

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
  SUBROUTINE MR_INIT_OUTPUT_FILES( ERROR , ERRMSG , OVERWRITE )

    USE MR_MOD_CREATE_FILE_XMDF

    IMPLICIT NONE

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    LOGICAL            , INTENT(IN ) :: OVERWRITE

    CALL MR_CREATE_FILE_XMDF( FILE_XMDF_ , ERROR , ERRMSG , OVERWRITE )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_)//" as target file"
      RETURN
    END IF

  END SUBROUTINE MR_INIT_OUTPUT_FILES

  END PROGRAM MR_REDS_EXTEND