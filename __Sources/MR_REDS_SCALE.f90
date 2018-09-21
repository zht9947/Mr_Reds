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
  PROGRAM MR_REDS_SCALE

    USE MR_ERRORS_FILE_MANIPULATE

    USE MR_MOD_CTRL_RETRY_CREATING_FILES

    USE MR_KINDS

    USE MR_MOD_INIT_RANKS
    USE MR_MOD_INIT_RANKS_PLUS
    USE MR_MOD_INIT_SLOPE

    USE MR_MOD_MALLOC_GRID_SYS

    USE MR_MOD_INIT_GRID_SYS
    
    USE MR_MOD_DO_SCALE_OUTPUT_MESH_DATA

    USE MR_MOD_GET_TIMES
    USE MR_MOD_IO_SCALE

    IMPLICIT NONE

    CHARACTER( 2**08 ) :: FILE_XMDF
    CHARACTER( 2**08 ) :: FILE_XMDF_

    REAL   (PARD_KIND) :: SCALE

    INTEGER(TSID_KIND) :: NTSS
    INTEGER(TSID_KIND) :: ITS

    REAL   (TMRD_KIND) :: T

    INTEGER            :: ERROR
    CHARACTER( 2**10 ) :: ERRMSG

   !BLOCK
  ! MANAGE THE VERSION
    WRITE(*,'( A ,"_Scale by ", A ," [ver.", A ,"]")') TRIM(INNERNAME) , TRIM(CONTRIBUTOR) , TRIM(SEMVER)
   !END BLOCK

  ! GET COMMAND ARGUMENTS FROM COMMAND LINE
  ! AND SET OUTPUT FILES' PATH\NAMES
    CALL MR_INIT_COMMAND_LINE( ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      WRITE(*,'(/,"PLEASE RUN ", A ,"_Scale with the following command arguments:")') TRIM(INNERNAME)
      WRITE(*,'(  "  1- (non-optional)")')
      WRITE(*,'(  "      Source file''s path\name containing the mesh and the data to be scaled,")')
      WRITE(*,'(  "    in XMDF format;")')
      WRITE(*,'(  "  2- (optional)")')
      WRITE(*,'(  "      Scale factor by which the source is expected be scaled;")')
      WRITE(*,'(  "    Or,")')
      WRITE(*,'(  "      If omitted, the source will be just duplicated;")')
      WRITE(*,'(  "Note,")')
      WRITE(*,'(  "  ALL the arguments MUST be given in sequence.")')
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

    WRITE(*,'(4X,"Scale mesh... ", $ )')
    CALL MR_INIT_GRID_SYS( FILE_XMDF , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    ELSE
      CALL MR_DO_SCALE_OUTPUT_MESH_DATA( FILE_XMDF , FILE_XMDF_ , SCALE , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF
    END IF
    WRITE(*,'("Done!")')

    WRITE(*,'( )')

    WRITE(*,'(8X,"Scale data...  0.00%", A , $ )') ACHAR(13)

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

      T = T * SQRT( SCALE )
      CALL MR_IO_SCALE( FILE_XMDF , FILE_XMDF_ , ITS , T , SCALE , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF

      WRITE(*,'(8X,"Scale data...",F6.2,"%", A , $ )') REAL(ITS+1)/REAL(NTSS+1)*100.00 , ACHAR(13)
  
    END DO

    WRITE(*,'(8X,"Scale data... Done! ")')

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
      ERRMSG = "XMDF File's path too long!"
      RETURN
    ELSE IF( ERROR /= 0 ) THEN
      ERROR = - ABS(ERROR)
      ERRMSG = "Error in getting command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))//" as source file"
      RETURN
    ELSE
    ! VERIFY XMDF FILE'S OPENING AND CLOSING
      CALL MR_OPEN_FILE_XMDF( FILE_XMDF , FILE_ID , ERROR , ERRMSG , READONLY=.TRUE. )
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
    END IF

    FILE_XMDF_ = TRIM(FILE_XMDF)//". scale"

    IF( COMMAND_ARGUMENT_COUNT() > 1 ) THEN

      I_ARG = 2
      WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG
    ! GET SCALE FACTOR
      CALL GET_COMMAND_ARGUMENT( I_ARG , CHAR_ARGUMENT , STATUS=ERROR )
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
          READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) SCALE
          IF( ERROR /= 0 ) THEN
            ERROR = - ABS(ERROR)
            ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
            RETURN
          ELSE IF( SCALE <= 0.0 ) THEN
            ERROR = - 1
            ERRMSG = "Illegal value for command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
            RETURN
          END IF
          FILE_XMDF_ = TRIM(FILE_XMDF_)//" ("//TRIM(CHAR_ARGUMENT)//")"
        END IF
      END IF

    ELSE
     !BLOCK
    ! ASSIGN DEFAULT VALUES TO OPTIONAL ARGUMENTS
      SCALE = 1.0
     !END BLOCK
      FILE_XMDF_ = TRIM(FILE_XMDF_)//" (1.0)"
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

  END PROGRAM MR_REDS_SCALE