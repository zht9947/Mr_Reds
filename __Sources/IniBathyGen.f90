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
  PROGRAM INIBATHYGEN

    USE MR_ERRORS_NONLINEAR_EQN_SOLVE

    USE MR_KINDS

    USE MR_MAC_PI

    USE MR_DEF_RANKS
    USE MR_DEF_MEANDER_PARS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_ERROR_ARRAY

    USE MR_MOD_INIT_RANKS

    USE MR_MOD_MALLOC_CONSTS_N_REF_PARS
    USE MR_MOD_MALLOC_GRID_SYS
    USE MR_MOD_MALLOC_CURVED_GEOS
    USE MR_MOD_MALLOC_CURVED_GEOS_ADDITIONAL
    USE MR_MOD_MALLOC_FIELD_VARS
    USE MR_MOD_MALLOC_ACTIVITY
    USE MR_MOD_MALLOC_ERROR_ARRAY

    USE MR_MOD_INIT_CONSTS_N_REF_PARS_DEFAULT
    USE MR_MOD_INIT_GRID_SYS
    USE MR_MOD_INIT_CURVED_GEOS
    USE MR_MOD_INIT_MEANDER_PARS
    USE MR_MOD_INIT_FIELD_VARS_N_ACTIVITY

    USE MR_MOD_INIT_OUTPUT

    USE MR_MOD_GEN_INI_ZB

    USE MR_MOD_UPDT_H

    USE MR_MOD_OUTPUT

    IMPLICIT NONE

    CHARACTER( 2**08 ) :: FILE_PRJ
    CHARACTER( 2**08 ) :: FILE_XMDF

    REAL   (TMRD_KIND) :: T_START

    REAL   (PARD_KIND) :: HTH
    REAL   (PARD_KIND) :: DZB_BK_MIN , DZB_BK_MAX
    REAL   (PARD_KIND) :: XI0 , XXIM

    INTEGER            :: ERROR
    CHARACTER( 2**10 ) :: ERRMSG

    CHARACTER( 2**03 ) :: I_CHAR
    INTEGER(IJID_KIND) :: I

    NK = 10
    CALL MR_MALLOC_KK_CONSTS_N_REF_PARS

    NBENDS = 1

    HTH = 10.0
    DZB_BK_MIN = -4.00 ; DZB_BK_MAX = +3.00
    XI0 = 0.223-0.5 ; XXIM = 0.15

   !BLOCK
  ! MANAGE THE VERSION
    WRITE(*,'("An Initial Bathymetry Generator for ", A ," by ", A ," [ver.", A ,"]")') TRIM(INNERNAME) , TRIM(CONTRIBUTOR) , TRIM(SEMVER)
   !END BLOCK

  ! GET COMMAND ARGUMENTS FROM COMMAND LINE
    CALL MR_INIT_COMMAND_LINE( ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      WRITE(*,'(/,"PLEASE RUN ", A ," with the following command arguments:")') TRIM(INNERNAME)
      WRITE(*,'(  "  1- (non-optional)")')
      WRITE(*,'(  "      Mesh file''s path\name, in XMDF format;")')
      WRITE(*,'(  "Note,")')
      WRITE(*,'(  "  ALL the arguments MUST be given in sequence.")')
      STOP
    END IF

    WRITE(*,'( )')

    WRITE(*,'("Initialize project... ", $ )')
    CALL MR_INIT_CONSTS_N_REF_PARS_DEFAULT( ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done! ")')

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
      CALL MR_MALLOC_CURVED_GEOS_GVV
      CALL MR_MALLOC_FIELD_VARS
      CALL MR_MALLOC_ACTIVITY
      CALL MR_MALLOC_ERROR_1D_ARRAY
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

    WRITE(*,'("Initialize meandering parameters... ", $ )')
    CALL MR_INIT_MEANDER_PARS( ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done! ")')

    print*, theta0 * 180.0 / pi
    print*, bth , lth , lambth
    print*, lambth2bth , sinuosity

    WRITE(*,'("Initialize field variables and activity... ", $ )')
    IF( .TRUE. ) THEN
      T_START = 0.0
      CALL MR_INIT_FIELD_VARS_N_ACTIVITY_COLD
      WRITE(*,'("Done! ")')
      WRITE(*,'("Initialize output... ", $ )')
      CALL MR_INIT_OUTPUT( FILE_XMDF , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF
    ELSE
      CALL MR_INIT_FIELD_VARS_N_ACTIVITY_HOT( FILE_XMDF , T_START , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF
    END IF
    WRITE(*,'("Done! ")')

    WRITE(*,'( )')

    WRITE(*,'(8X,"Generate bathymetry and update depth... ", A , $ )') ACHAR(13)

    CALL MR_GEN_INI_ZB( HTH , DZB_BK_MIN , DZB_BK_MAX , XI0 , XXIM , NBENDS , NI , NJ , ZB , ERROR_1D_ARRAY )
    IF( ANY( ERROR_1D_ARRAY < 0 ) ) THEN
      WRITE(*,'( )')
      DO I = 1 , NI
        WRITE( I_CHAR , '(I<LEN(I_CHAR)>)' ) I
        SELECT CASE( ERROR_1D_ARRAY( I ) )
        CASE( ERROR_BISECT_SOLVE_NO_UNIQUE_ROOT_IN_REGION )
          WRITE(*,'("CS-", A ,": ", A ,"!")') TRIM(ADJUSTL(I_CHAR)) ,   &
          & "No unique root in given region when using bisection method to solve alpha"
        CASE( ERROR_NEWTON_SOLVE_ZERO_DERIVATIVE )
          WRITE(*,'("CS-", A ,": ", A ,"!")') TRIM(ADJUSTL(I_CHAR)) ,   &
          & "Zero deivative when using newton method to solve xxxi"
        CASE( ERROR_NEWTON_SOLVE_MAX_NUMBER_OF_ITERATION )
          WRITE(*,'("CS-", A ,": ", A ,"!")') TRIM(ADJUSTL(I_CHAR)) ,   &
          & "Max number of iterations when using newton method to solve xxxi"
        END SELECT
      END DO
      STOP
    END IF

    CALL MR_UPDT_H

    CALL MR_OUTPUT( FILE_XMDF , T_START , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF

    WRITE(*,'(8X,"Generate bathymetry and update depth... Done! ")')

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

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF

    IMPLICIT NONE

    CHARACTER( 2**08 )               :: CHAR_ARGUMENT

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

  ! GET XMDF FILE'S PATH\NAME
    CALL GET_COMMAND_ARGUMENT( 1 , FILE_XMDF , STATUS=ERROR )
    IF( ERROR == - 1 ) THEN
      ERRMSG = "Mesh file's path\name too long!"
      RETURN
    ELSE IF( ERROR /= 0 ) THEN
      ERROR = - ABS(ERROR)
      ERRMSG = "Error in getting command argument no.1 as mesh file"
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

  END SUBROUTINE MR_INIT_COMMAND_LINE

  END PROGRAM INIBATHYGEN