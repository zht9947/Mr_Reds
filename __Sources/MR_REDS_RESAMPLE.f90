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
  PROGRAM MR_REDS_RESAMPLE

    USE MR_0_SKIP_MODE

    USE MR_ERRORS_FILE_MANIPULATE

    USE MR_MOD_CTRL_RETRY_CREATING_FILES

    USE MR_KINDS

    USE MR_MOD_INIT_RANKS
    USE MR_MOD_INIT_RANKS_PLUS
    USE MR_MOD_INIT_SLOPE

    USE MR_MOD_MALLOC_GRID_SYS

    USE MR_MOD_INIT_GRID_SYS
    
    USE MR_MOD_DO_DUPLICATE_OUTPUT_MESH_DATA

    USE MR_MOD_GET_TIMES
    USE MR_MOD_IO_RESAMPLE

    IMPLICIT NONE

    CHARACTER( 2**08 ) :: FILE_XMDF
    CHARACTER( 2**08 ) :: FILE_XMDF_

    INTEGER(TSID_KIND) :: ITS_START , ITS_END , ITS_STRIDE

    INTEGER(TSID_KIND) :: NTSS
    INTEGER(TSID_KIND) :: ITS

    REAL   (TMRD_KIND) :: T

    INTEGER            :: ERROR
    CHARACTER( 2**10 ) :: ERRMSG

   !BLOCK
  ! MANAGE THE VERSION
    WRITE(*,'( A ,"_Resample by ", A ," [ver.", A ,"]")') TRIM(INNERNAME) , TRIM(CONTRIBUTOR) , TRIM(SEMVER)
   !END BLOCK

  ! GET COMMAND ARGUMENTS FROM COMMAND LINE
  ! AND SET OUTPUT FILES' PATH\NAMES
    CALL MR_INIT_COMMAND_LINE( ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      WRITE(*,'(/,"PLEASE RUN ", A ,"_Resample with the following command arguments:")') TRIM(INNERNAME)
      WRITE(*,'(  "  1- (non-optional)")')
      WRITE(*,'(  "      Source file''s path\name containing the mesh and the data to be resampled,")')
      WRITE(*,'(  "    in XMDF format;")')
      WRITE(*,'(  "  2- (optional)")')
      WRITE(*,'(  "      ONE of the following resampling timestep control specifiers:")')
      WRITE(*,'(  "    A- [<from>]:[<to>]")')
      WRITE(*,'(  "        <from> is the index of timestep from which the resampling will be started;")')
      WRITE(*,'(  "        <to> is the index of timestep to which the resampling will be ended;")')
      WRITE(*,'(  "            (Note: The first timestep is indexed as 0 in this program;)")')
      WRITE(*,'(  "        In this case, resampling will be started from the timestep specified by <from>")')
      WRITE(*,'(  "      and ended to the timestep specified by <to>; the increment of timestep will be")')
      WRITE(*,'(  "      specified by a subsequent command argument if it exsits, or set to 1 by default;")')
      WRITE(*,'(  "        Each of <from> and <to> is allowed to be omitted: if <from> is omitted, it will")')
      WRITE(*,'(  "      be set to the first timestep, i.e. 0, by default; if <to> is omitted, it will be")')
      WRITE(*,'(  "      set to the last timestep by default;")')
      WRITE(*,'(  "            (Note: Use colon to separate <from> and <to>; and if only ONE out of them")')
      WRITE(*,'(  "                   is omitted, the colon should not;)")')
      WRITE(*,'(  "            (Note: No space is allowed either before or after the colon;)")')
      WRITE(*,'(  "    B-  <by>")')
      WRITE(*,'(  "        <by> is the increment of timestep by which the resampling will be done;")')
      WRITE(*,'(  "        In this case, resampling will be started from the first timestep and ended to")')
      WRITE(*,'(  "      the last timestep, with an increment of timestep specified by <by>;")')
      WRITE(*,'(  "    C-  / (the character ""/"" itself)")')
      WRITE(*,'(  "        In this case, only the last timestep will be retained;")')
      WRITE(*,'(  "    Or,")')
      WRITE(*,'(  "      If omitted, the source will be just duplicated;")')
      WRITE(*,'(  "  3- (optional)")')
      WRITE(*,'(  "      This command argument can be specified ONLY when command argument no.2 is")')
      WRITE(*,'(  "    specified as case A, i.e. [<from>]:[<to>], and should be of the following form:")')
      WRITE(*,'(  "        <by>  ")')
      WRITE(*,'(  "        <by> is the increment of timestep by which the resampling will be done;")')
      WRITE(*,'(  "        In this case, resampling will be started from the timestep specified by <from>")')
      WRITE(*,'(  "      and ended to the timestep specified by <to>, with an increment of timestep")')
      WRITE(*,'(  "      specified by <by>;")')
      WRITE(*,'(  "    Or,")')
      WRITE(*,'(  "      If omitted, the increment of timestep will be set to 1 by default;")')
      WRITE(*,'(  "  4- (optional)")')
      WRITE(*,'(  "      An alternative option, which tells the program to run on skip mode so that all")')
      WRITE(*,'(  "    the runtime inputs from user can be skipped, with the following form:")')
      WRITE(*,'(  "        --skip")')
      WRITE(*,'(  "      Careful with this option and make sure you really know what will be skipped;")')
      WRITE(*,'(  "    Or,")')
      WRITE(*,'(  "      If omitted, the program will by default run on non-skip mode;")')
      WRITE(*,'(  "Note,")')
      WRITE(*,'(  "  ALL the arguments 1--4 MUST be given in sequence.")')
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
    WRITE(*,'("Done!")')

    WRITE(*,'( )')

    WRITE(*,'(4X,"Duplicate mesh... ", $ )')
    CALL MR_INIT_GRID_SYS( FILE_XMDF , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    ELSE
      CALL MR_DO_DUPLICATE_OUTPUT_MESH_DATA( FILE_XMDF , FILE_XMDF_ , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF
    END IF
    WRITE(*,'("Done!")')

    WRITE(*,'( )')

    WRITE(*,'(8X,"Resample data...  0.00%", A , $ )') ACHAR(13)

  ! GET NTSS
    CALL MR_GET_NTSS( FILE_XMDF , NTSS , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
  ! REASSIGN ITS_END
    ITS_END = MIN( ITS_END , NTSS )
  ! REASSIGN ITS_START
    ITS_START = MIN( ITS_START , ITS_END )

    DO ITS = ITS_START , ITS_END , ITS_STRIDE

    ! GET T OF ITS
      CALL MR_GET_T_ITS( FILE_XMDF , NTSS , ITS , T , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF

      CALL MR_IO_RESAMPLE( FILE_XMDF , FILE_XMDF_ , ITS , T , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF

      WRITE(*,'(8X,"Resample data...",F6.2,"%", A , $ )')   &
      & REAL((ITS-ITS_START)/ITS_STRIDE+1)/REAL((ITS_END-ITS_START)/ITS_STRIDE+1)*100.00 , ACHAR(13)

    END DO

    WRITE(*,'(8X,"Resample data... Done! ")')

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

    INTEGER                          :: IDX
    INTEGER                          :: ICR

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

  ! SET RESAMPLED XMDF FILE'S PATH\NAME
    FILE_XMDF_ = TRIM(FILE_XMDF)//". resample"

    I_ARG = I_ARG + 1
    IF( COMMAND_ARGUMENT_COUNT() < I_ARG ) THEN
     !BLOCK
    ! ASSIGN DEFAULT VALUES TO OPTIONAL ARGUMENTS
      ITS_END = HUGE(ITS_END)
      ITS_START = 0
      ITS_STRIDE = 1
     !END BLOCK
      FILE_XMDF_ = TRIM(FILE_XMDF_)//" (from 0 to end by 1)"
    ELSE
      WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG
    ! GET RESAMPLING TIMESTEP CONTROL SPECIFIER
      CALL GET_COMMAND_ARGUMENT( I_ARG , CHAR_ARGUMENT , STATUS=ERROR )
      IF( ERROR /= 0 ) THEN
        ERROR = - ABS(ERROR)
        ERRMSG = "Error in getting command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
        RETURN
      ELSE IF( CHAR_ARGUMENT(1:2) == "--" ) THEN
       !BLOCK
        I_ARG = I_ARG - 1
      ! ASSIGN DEFAULT VALUES TO OPTIONAL ARGUMENTS
        ITS_END = HUGE(ITS_END)
        ITS_START = 0
        ITS_STRIDE = 1
       !END BLOCK
        FILE_XMDF_ = TRIM(FILE_XMDF_)//" (from 0 to end by 1)"
     !A SPECIAL CASE
      ELSE IF( TRIM(CHAR_ARGUMENT) == "/" ) THEN
       !BLOCK
      ! IDENTIFY ITS_START WITH ITS_END
        ITS_END = HUGE(ITS_END)
        ITS_START = ITS_END
        ITS_STRIDE = 1
       !END BLOCK
        FILE_XMDF_ = TRIM(FILE_XMDF_)//" (only end)"
     !END A SPECIAL CASE
      ELSE IF( VERIFY( TRIM(CHAR_ARGUMENT) , "0123456789:" ) /= 0 ) THEN
        ERROR = - 1
        ERRMSG = "Illegal character in command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
        RETURN
      ELSE IF( INDEX( TRIM(CHAR_ARGUMENT) , ":" ) /= 0 ) THEN
        IDX = INDEX( TRIM(CHAR_ARGUMENT) , ":" )
        CHAR_ARGUMENT(IDX:IDX) = ","
        IF( INDEX( TRIM(CHAR_ARGUMENT) , ":" ) /= 0 ) THEN
          ERROR = - 1
          ERRMSG = "Too many colons in command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
          RETURN
        END IF
      ! CASE ITS_START:ITS_END
        IF( TRIM(CHAR_ARGUMENT(:IDX-1)) == "" ) THEN
         !BLOCK
        ! ASSIGN DEFAULT VALUES TO ITS_START
          ITS_START = 0
         !END BLOCK
          FILE_XMDF_ = TRIM(FILE_XMDF_)//" (from 0"
        ELSE
          READ( CHAR_ARGUMENT(:IDX-1) , * , IOSTAT=ERROR ) ITS_START
          IF( ERROR /= 0 ) THEN
            ERROR = - ABS(ERROR)
            ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
            RETURN
          END IF
          FILE_XMDF_ = TRIM(FILE_XMDF_)//" (from "//TRIM(CHAR_ARGUMENT(:IDX-1))
        END IF
        IF( TRIM(CHAR_ARGUMENT(IDX+1:)) == "" ) THEN
         !BLOCK
        ! ASSIGN DEFAULT VALUES TO ITS_END
          ITS_END = HUGE(ITS_END)
         !END BLOCK
          FILE_XMDF_ = TRIM(FILE_XMDF_)//" to end"
        ELSE
          READ( CHAR_ARGUMENT(IDX+1:) , * , IOSTAT=ERROR ) ITS_END
          IF( ERROR /= 0 ) THEN
            ERROR = - ABS(ERROR)
            ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
            RETURN
          END IF
          FILE_XMDF_ = TRIM(FILE_XMDF_)//" to "//TRIM(CHAR_ARGUMENT(IDX+1:))
        END IF

        I_ARG = I_ARG + 1
        IF( COMMAND_ARGUMENT_COUNT() < I_ARG ) THEN
         !BLOCK
        ! ASSIGN DEFAULT VALUES TO OPTIONAL ARGUMENTS
          ITS_STRIDE = 1
         !END BLOCK
          FILE_XMDF_ = TRIM(FILE_XMDF_)//" by 1)"
        ELSE
          WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG
        ! GET ADDITIONAL RESAMPLING TIMESTEP CONTROL SPECIFIER
          CALL GET_COMMAND_ARGUMENT( I_ARG , CHAR_ARGUMENT , STATUS=ERROR )
          IF( ERROR /= 0 ) THEN
            ERROR = - ABS(ERROR)
            ERRMSG = "Error in getting command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
            RETURN
          ELSE IF( CHAR_ARGUMENT(1:2) == "--" ) THEN
           !BLOCK
            I_ARG = I_ARG - 1
          ! ASSIGN DEFAULT VALUES TO OPTIONAL ARGUMENTS
            ITS_STRIDE = 1
           !END BLOCK
            FILE_XMDF_ = TRIM(FILE_XMDF_)//" by 1)"
          ELSE IF( VERIFY( TRIM(CHAR_ARGUMENT) , "0123456789" ) /= 0 ) THEN
            ERROR = - 1
            ERRMSG = "Illegal character in command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
            RETURN
          ELSE
            READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) ITS_STRIDE
            IF( ERROR /= 0 ) THEN
              ERROR = - ABS(ERROR)
              ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
              RETURN
            END IF
            FILE_XMDF_ = TRIM(FILE_XMDF_)//" by "//TRIM(CHAR_ARGUMENT)//")"
          END IF
        END IF

      ELSE
      ! CASE ITS_STRIDE
       !BLOCK
      ! ASSIGN DEFAULT VALUES TO ITS_START AND ITS_END
        ITS_END = HUGE(ITS_END)
        ITS_START = 0
       !END BLOCK
        READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) ITS_STRIDE
        IF( ERROR /= 0 ) THEN
          ERROR = - ABS(ERROR)
          ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
          RETURN
        END IF
        FILE_XMDF_ = TRIM(FILE_XMDF_)//" (from 0 to end by "//TRIM(CHAR_ARGUMENT)//")"
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

  END PROGRAM MR_REDS_RESAMPLE