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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  PROGRAM MR_REDS_RESAMPLE

    USE MR_ERRORS

    USE MR_KINDS

    USE MR_MOD_CTRL_RETRY_CREATING_FILES

    USE MR_MOD_INIT_RANKS

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
      WRITE(*,'(  "      Resampling timestep control statement which can be input as:")')
      WRITE(*,'(  "        A character:")')
      WRITE(*,'(  "          ""/"" ")')
      WRITE(*,'(  "            In this case, only the last timestep will be retained;")')
      WRITE(*,'(  "      Or")')
      WRITE(*,'(  "        An iteration control statement with one out of the following formats:")')
      WRITE(*,'(  "          ""[start-timestep],[end-timestep],[stride-timestep]"" ")')
      WRITE(*,'(  "            In this case, resampling will be started from [start-timestep]")')
      WRITE(*,'(  "          and ended to [end-timestep] with an increment of [stride-timestep];")')
      WRITE(*,'(  "            Each of [start-timestep], [end-timestep] and [stride-timestep]")')
      WRITE(*,'(  "          is allowed to be omitted; if so, they will be set as their defaults;")')
      WRITE(*,'(  "            For [start-timestep], the default is the first timestep;")')
      WRITE(*,'(  "              <Note: The first timestep is indexed as 0 in this program;>")')
      WRITE(*,'(  "            For [end-timestep], the default is the last timestep;")')
      WRITE(*,'(  "            For [stride-timestep], the default is 1;")')
      WRITE(*,'(  "              <Note: Use ONLY commas to separate [start-timestep], [end-timestep]")')
      WRITE(*,'(  "                   and [stride-timestep], rather than spaces or colons; and even")')
      WRITE(*,'(  "                   if one or two or all of them are omitted, commas should not.>")')
      WRITE(*,'(  "              <Note: No space is allowed either before or after commas.>")')
      WRITE(*,'(  "        Or")')
      WRITE(*,'(  "          ""[start-timestep],[end-timestep]"" ")')
      WRITE(*,'(  "            In this case, resampling will be started from [start-timestep]")')
      WRITE(*,'(  "          and ended to [end-timestep] with a default increment of 1;")')
      WRITE(*,'(  "            Each of [start-timestep] and [end-timestep] is allowed to be")')
      WRITE(*,'(  "          omitted; if so, they will be set as their defaults;")')
      WRITE(*,'(  "              <Note: As above.>")')
      WRITE(*,'(  "        Or")')
      WRITE(*,'(  "          ""[stride-timestep]"" ")')
      WRITE(*,'(  "            In this case, resampling will be started from the first timestep")')
      WRITE(*,'(  "          and ended to the last timestep with an increment of [stride-timestep];")')
      WRITE(*,'(  "      Or")')
      WRITE(*,'(  "        If omitted, the source will be just duplicated;")')
      WRITE(*,'(  "  ALL the arguments must be given in sequence.")')
      STOP
    END IF

    WRITE(*,'( )')

    WRITE(*,'("Verify input files... ", $ )')
    CALL MR_INIT_INPUT_FILES( ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done! ")')

    WRITE(*,'("Create output files... ", $ )')
    CALL MR_INIT_OUTPUT_FILES( "NEWCREATE" , ERROR , ERRMSG )
    IF( ERROR == ERROR_CREATING_NEW_FILE ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      WRITE(*,'(/,"Files with the same names may already exist.")')
      CALL MR_CTRL_RETRY_CREATING_FILES
      WRITE(*,'( )')
      WRITE(*,'("Create output files... ", $ )')
      CALL MR_INIT_OUTPUT_FILES( "OVERWRITE" , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF
    ELSE IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done! ")')

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
    END IF
    WRITE(*,'("Done! ")')

    WRITE(*,'( )')

    WRITE(*,'(4X,"Duplicate mesh... ", $ )')
    CALL MR_INIT_GRID_SYS( FILE_XMDF , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    ELSE
      CALL MR_DO_DUPLICATE_OUTPUT_MESH_DATA( FILE_XMDF , FILE_XMDF_ , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF
    END IF
    WRITE(*,'("Done! ")')

    WRITE(*,'( )')

  ! GET NTSS
    CALL MR_GET_NTSS( FILE_XMDF , NTSS , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
  ! REASSIGN ITS_END
    ITS_END = MIN( ITS_END , NTSS )
  ! REASSIGN ITS_START
    ITS_START = MIN( ITS_START , ITS_END )

    WRITE(*,'(8X,"Resample data...  0.00%", A , $ )') ACHAR(13)
  
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
  
    WRITE(*,'(/,"The result has been written into the file: ",/,4X, A )') TRIM(FILE_XMDF_)


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
  SUBROUTINE MR_INIT_COMMAND_LINE( ERROR , ERRMSG )

    IMPLICIT NONE

    CHARACTER( 2**08 )               :: CHAR_ARGUMENT

    INTEGER                          :: NDELIS

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
        SELECT CASE( TRIM(CHAR_ARGUMENT) )
        CASE( "--HELP" , "--HELp" , "--HElp" , "--Help" , "--help" ,   &
        &      "-HELP" ,  "-HELp" ,  "-HElp" ,  "-Help" ,  "-help"   &
        )
          ERROR = - 999999
          ERRMSG = "Help information is displayed as below"
          RETURN
        END SELECT
      END IF
    END IF

    IF( COMMAND_ARGUMENT_COUNT() < 1 ) THEN
      ERROR = - 11
      ERRMSG = "Not enough command arguments"
      RETURN
    ELSE IF( COMMAND_ARGUMENT_COUNT() > 2 ) THEN
      ERROR = - 12
      ERRMSG = "Too many command arguments"
      RETURN
    END IF

  ! GET XMDF FILE'S PATH\NAME
    CALL GET_COMMAND_ARGUMENT( 1 , FILE_XMDF , STATUS=ERROR )
    IF( ERROR == - 1 ) THEN
      ERRMSG = "XMDF File's path too long!"
      RETURN
    ELSE IF( ERROR /= 0 ) THEN
      ERROR = - ABS(ERROR)
      ERRMSG = "Error in getting command argument No.1 as source file"
      RETURN
    END IF

    FILE_XMDF_ = TRIM(FILE_XMDF)//". resample"

    IF( COMMAND_ARGUMENT_COUNT() > 1 ) THEN

    ! GET RESAMPLING TIMESTEP CONTROL STATEMENT
      CALL GET_COMMAND_ARGUMENT( 2 , CHAR_ARGUMENT , STATUS=ERROR )
      IF( ERROR /= 0 ) THEN
        ERROR = - ABS(ERROR)
        ERRMSG = "Error in getting command argument no.2"
        RETURN
      ELSE

        ITS_START = 0 ; ITS_END = HUGE( ITS_END ) ; ITS_STRIDE = 1

        IF( TRIM(CHAR_ARGUMENT) == "/" ) THEN
         !BLOCK
        ! IDENTIFY START WITH END
          ITS_START = ITS_END
         !END BLOCK
          FILE_XMDF_ = TRIM(FILE_XMDF_)//" (only last)"
        ELSE
          IF( VERIFY( TRIM(CHAR_ARGUMENT) , "0123456789," ) /= 0 ) THEN
            ERROR = - 1
            ERRMSG = "Illegal character in command argument no.2"
            RETURN
          ELSE

            NDELIS = 0
            DO ICR = 1 , LEN_TRIM(CHAR_ARGUMENT)
              IF( CHAR_ARGUMENT(ICR:ICR) == "," ) THEN
                NDELIS = NDELIS + 1
              END IF
            END DO

            SELECT CASE( NDELIS )
            CASE( 3 : )
              ERROR = - 1
              ERRMSG = "Too many commas in command argument no.2"
              RETURN
            CASE( 2 )
            ! CASE START,END,STRIDE
              READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) ITS_START , ITS_END , ITS_STRIDE
              IF( ERROR == -1 ) THEN
                ERROR = 0
              ELSE IF( ERROR /= 0 ) THEN
                ERROR = - ABS(ERROR)
                ERRMSG = "Error in reading a value from command argument no.2"
                RETURN
              END IF
            CASE( 1 )
            ! CASE START,END
              READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) ITS_START , ITS_END
              IF( ERROR == -1 ) THEN
                ERROR = 0
              ELSE IF( ERROR /= 0 ) THEN
                ERROR = - ABS(ERROR)
                ERRMSG = "Error in reading a value from command argument no.2"
                RETURN
              END IF
            CASE( 0 )
            ! CASE STRIDE
              READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) ITS_STRIDE
              IF( ERROR == -1 ) THEN
                ERROR = 0
              ELSE IF( ERROR /= 0 ) THEN
                ERROR = - ABS(ERROR)
                ERRMSG = "Error in reading a value from command argument no.2"
                RETURN
              END IF
            END SELECT

            FILE_XMDF_ = TRIM(FILE_XMDF)//" ("//TRIM(CHAR_ARGUMENT)//")"
  
          END IF

        END IF
  
      END IF

    ELSE
     !BLOCK
    ! ASSIGN DEFAULT VALUES TO OPTIONAL ARGUMENTS
      ITS_START = 0 ; ITS_END = HUGE( ITS_END ) ; ITS_STRIDE = 1
     !END BLOCK
      FILE_XMDF_ = TRIM(FILE_XMDF_)//" (,,)"
    END IF

    FILE_XMDF_ = TRIM(FILE_XMDF_)//".h5"

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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_INIT_INPUT_FILES( ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF

    IMPLICIT NONE

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER            :: FILE_ID

    CALL MR_OPEN_FILE_XMDF( FILE_XMDF , "READ" , FILE_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF)//" as source file"
      RETURN
    ELSE
      CALL MR_CLOSE_FILE_XMDF( FILE_ID , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF)
        RETURN
      END IF
    END IF

  END SUBROUTINE MR_INIT_INPUT_FILES

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
  SUBROUTINE MR_INIT_OUTPUT_FILES( FILE_STATUS , ERROR , ERRMSG )

    USE MR_MOD_CREATE_FILE_XMDF

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_STATUS

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    CALL MR_CREATE_FILE_XMDF( FILE_XMDF_ , FILE_STATUS , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_)
      RETURN
    END IF

  END SUBROUTINE MR_INIT_OUTPUT_FILES

  END PROGRAM MR_REDS_RESAMPLE