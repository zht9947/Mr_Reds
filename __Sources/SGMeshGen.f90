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
  PROGRAM SGMESHGEN

    USE MR_ERRORS_FILE_MANIPULATE

    USE MR_KINDS

    USE MR_MAC_PI

    USE MR_DEF_RANKS

    USE MR_MOD_CTRL_RETRY_CREATING_FILES

    USE MR_MOD_GEN_OUTPUT_MESH_DATA

    IMPLICIT NONE

    CHARACTER( 2**08 ) :: FILE_XMDF

    REAL   (PARD_KIND) :: THETA0
    REAL   (PARD_KIND) :: LAMBTH2BTH
    REAL   (XYRD_KIND) :: BTH , LTH

    INTEGER            :: NBENDS

    INTEGER            :: ERROR
    CHARACTER( 2**10 ) :: ERRMSG

   !BLOCK
  ! MANAGE THE VERSION
    WRITE(*,'("Sine-Generated Meandering River''s")')
    WRITE(*,'("Mesh Generator for ", A ," by ", A ," [ver.", A ,"]")') TRIM(INNERNAME) , TRIM(CONTRIBUTOR) , TRIM(SEMVER)
   !END BLOCK

  ! GET COMMAND ARGUMENTS FROM COMMAND LINE
  ! AND SET OUTPUT FILES' PATH\NAMES
    CALL MR_INIT_COMMAND_LINE( ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      WRITE(*,'(/,"PLEASE RUN the mesh generator with the following command arguments:")')
      WRITE(*,'(  "  1- (non-optional)")')
      WRITE(*,'(  "      Deflection angle of channel ceterline at reference crossover section, in degrees;")')
      WRITE(*,'(  "        Absolute value of the deflection angle must be smaller than 138 degree; moreover,")')
      WRITE(*,'(  "      negative value is permitted;")')
      WRITE(*,'(  "  2- (non-optional)")')
      WRITE(*,'(  "      Channel width, in meters;")')
      WRITE(*,'(  "  3- (non-optional)")')
      WRITE(*,'(  "      Channel wavelength-to-width ratio;")')
      WRITE(*,'(  "        Besides a real number, this argument can also be input as [<proportion-to-pi>]pi,")')
      WRITE(*,'(  "      e.g. pi, 2pi, etc.; then the channel wavelength-to-width ratio will be calculated")')
      WRITE(*,'(  "      as a corresponding proportion multiplied by pi¡Ö3.141592653589793...;")')
      WRITE(*,'(  "  4- (non-optional)")')
      WRITE(*,'(  "      Number of columns into which the whole length of ONE meander bend will be divided;")')
      WRITE(*,'(  "  5- (non-optional)")')
      WRITE(*,'(  "      Number of rows into which the whole width of the channel will be divided;")')
      WRITE(*,'(  "  6- (optional)")')
      WRITE(*,'(  "      Number of meander bends that are supposed to be illustrated;")')
      WRITE(*,'(  "    Or,")')
      WRITE(*,'(  "      If omitted, only ONE meander bend will be illustrated;")')
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

  ! GENERATE AND OUTPUT MESH DATA
    CALL MR_GEN_OUTPUT_MESH_DATA( FILE_XMDF , THETA0 , BTH , LTH , NBENDS , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF

    WRITE(*,'(/,"The mesh has been created into the file: ",/,4X, A )') TRIM(FILE_XMDF)

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

    IMPLICIT NONE

    CHARACTER( 2**08 )               :: CHAR_ARGUMENT
    CHARACTER( 2**03 )               :: I_ARG_CHAR
    INTEGER                          :: I_ARG

    INTEGER                          :: IDX

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
    IF( COMMAND_ARGUMENT_COUNT() < 5 ) THEN
      ERROR = - 1
      ERRMSG = "Not enough command arguments"
      RETURN
    ELSE IF( COMMAND_ARGUMENT_COUNT() > 6 ) THEN
      ERROR = - 1
      ERRMSG = "Too many command arguments"
      RETURN
    END IF

  ! SET XMDF FILE'S PATH\NAME
    FILE_XMDF = "Channel"

    I_ARG = 1
    WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG
  ! GET DEFLECTION ANGLE AT REFERENCE CROSSOVER SECTION, IN DEGREES
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
        READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) THETA0
        IF( ERROR /= 0 ) THEN
          ERROR = - ABS(ERROR)
          ERRMSG = "Error in reading value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
          RETURN
        ELSE IF( THETA0 <= -138.0 .OR. THETA0 >= +138.0 ) THEN
          ERROR = - 1
          ERRMSG = "Illegal value for command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
          RETURN
        END IF
        FILE_XMDF = TRIM(FILE_XMDF)//" "//TRIM(CHAR_ARGUMENT)//"deg,"
      END IF
    END IF

    I_ARG = 2
    WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG
  ! GET CHANNEL WIDTH, IN METERS
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
        READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) BTH
        IF( ERROR /= 0 ) THEN
          ERROR = - ABS(ERROR)
          ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
          RETURN
        ELSE IF( BTH <= 0.0 ) THEN
          ERROR = - 1
          ERRMSG = "Illegal value for command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
          RETURN
        END IF
        FILE_XMDF = TRIM(FILE_XMDF)//" "//TRIM(CHAR_ARGUMENT)//"m"
      END IF
    END IF

    I_ARG = 3
    WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG
  ! GET CHANNEL WAVELENGTH-TO-WIDTH RATIO
    CALL GET_COMMAND_ARGUMENT( I_ARG , CHAR_ARGUMENT , STATUS=ERROR )
    IF( ERROR /= 0 ) THEN
      ERROR = - ABS(ERROR)
      ERRMSG = "Error in getting command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
      RETURN
    ELSE
      IF( VERIFY( TRIM(CHAR_ARGUMENT) , "-+0123456789Ee.PpIi" ) /= 0 ) THEN
        ERROR = - 1
        ERRMSG = "Illegal character in command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
        RETURN
      ELSE
        IDX = MAX( INDEX(CHAR_ARGUMENT,"PI") , INDEX(CHAR_ARGUMENT,"Pi") , INDEX(CHAR_ARGUMENT,"pi") )
        IF( IDX /= 0 ) THEN
          CHAR_ARGUMENT(IDX:IDX+1) = ""
        END IF

        IF( VERIFY( TRIM(CHAR_ARGUMENT) , "-+0123456789Ee." ) /= 0 ) THEN
          ERROR = - 1
          ERRMSG = "Illegal character in command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
          RETURN
        ELSE
          READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) LAMBTH2BTH
          IF( ERROR == -1 ) THEN
            LAMBTH2BTH = 1.0
          ELSE IF( ERROR /= 0 ) THEN
            ERROR = - ABS(ERROR)
            ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
            RETURN
          ELSE IF( LAMBTH2BTH <= 0.0 ) THEN
            ERROR = - 1
            ERRMSG = "Illegal value for command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
            RETURN
          END IF
        END IF

        IF( IDX /= 0 ) THEN
          LAMBTH2BTH = LAMBTH2BTH * PI
          FILE_XMDF = TRIM(FILE_XMDF)//" ("//TRIM(CHAR_ARGUMENT)//"pi),"
        ELSE
          FILE_XMDF = TRIM(FILE_XMDF)//" ("//TRIM(CHAR_ARGUMENT)//"),"
        END IF

      END IF

    END IF

    I_ARG = 4
    WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG
  ! GET NUMBER OF COLUMNS IN ONE MEANDER BEND
    CALL GET_COMMAND_ARGUMENT( I_ARG , CHAR_ARGUMENT , STATUS=ERROR )
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
        READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) NI
        IF( ERROR /= 0 ) THEN
          ERROR = - ABS(ERROR)
          ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
          RETURN
        END IF
        FILE_XMDF = TRIM(FILE_XMDF)//" "//TRIM(CHAR_ARGUMENT)//"x"
      END IF
    END IF

    I_ARG = 5
    WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG
  ! GET NUMBER OF ROWS
    CALL GET_COMMAND_ARGUMENT( I_ARG , CHAR_ARGUMENT , STATUS=ERROR )
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
        READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) NJ
        IF( ERROR /= 0 ) THEN
          ERROR = - ABS(ERROR)
          ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
          RETURN
        END IF
        FILE_XMDF = TRIM(FILE_XMDF)//TRIM(CHAR_ARGUMENT)
      END IF
    END IF

    IF( COMMAND_ARGUMENT_COUNT() > 5 ) THEN

      I_ARG = 6
      WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG
    ! GET NUMBER OF MEANDER BENDS
      CALL GET_COMMAND_ARGUMENT( I_ARG , CHAR_ARGUMENT , STATUS=ERROR )
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
          READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) NBENDS
          IF( ERROR /= 0 ) THEN
            ERROR = - ABS(ERROR)
            ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
            RETURN
          END IF
          FILE_XMDF = TRIM(FILE_XMDF)//" ("//TRIM(CHAR_ARGUMENT)//")"
        END IF
      END IF

    ELSE
     !BLOCK
    ! ASSIGN DEFAULT VALUES TO OPTIONAL ARGUMENTS
      NBENDS = 1
     !END BLOCK
      FILE_XMDF = TRIM(FILE_XMDF)//" (1)"
    END IF

    FILE_XMDF = TRIM(FILE_XMDF)//".h5"

  ! CHANGE THETA0'S UNIT FROM DEGREES TO RADIANS
    THETA0 = THETA0 * PI / 180.0

  ! CALCULATE CHANNEL LENGTH OF A CYCLE
    LTH = BTH * LAMBTH2BTH / &
    &   ( 1.0 - 2.2499997*(THETA0/3.0)**02 + 1.2656208*(THETA0/3.0)**04 - 0.3163866*(THETA0/3.0)**06   &
    &         + 0.0444479*(THETA0/3.0)**08 - 0.0039444*(THETA0/3.0)**10 + 0.0002100*(THETA0/3.0)**12 )
    ! POLYMINAL APPROXIMATION OF BESSEL FUNCTION OF THETA0 OF FIRST KIND AND ZEROTH ORDER

  ! CHANGE NI'S VALUE FROM NUMBER OF COLUMNS IN ONE MEANDER BEND
  ! TO TOTAL NUMBER OF COLUMNS IN ENTIRE CHANNEL
    NI = NI * NBENDS

  ! CALCULATE NUMBER OF NODES AND ELEMENTS
    NND = (2*NI+1) * (2*NJ+1)
    NEM =       NI * NJ

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

   LOGICAL             , INTENT(IN ) :: OVERWRITE

    CALL MR_CREATE_FILE_XMDF( FILE_XMDF , ERROR , ERRMSG , OVERWRITE )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF)
      RETURN
    END IF

  END SUBROUTINE MR_INIT_OUTPUT_FILES

  END PROGRAM SGMESHGEN