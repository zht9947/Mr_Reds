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

    USE MR_NUM_START_MODE

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

    USE MR_MOD_CTRL_COLD_MODE_STARTED

    USE MR_MOD_AVERAGE

    USE MR_MOD_GEN_INI_ZB

    USE MR_MOD_UPDT_H

    USE MR_MOD_INIT_OUTPUT

    USE MR_MOD_OUTPUT

    IMPLICIT NONE

    CHARACTER( 2**08 ) :: FILE_PRJ
    CHARACTER( 2**08 ) :: FILE_XMDF

    REAL   (TMRD_KIND) :: T_START

    REAL   (PARD_KIND) :: HTH
    REAL   (PARD_KIND) :: DZB_BK_MIN , DZB_BK_MAX
    REAL   (PARD_KIND) :: XI0 , XXIM

    REAL   (TMRD_KIND) , ALLOCATABLE :: T_START_ALTER
    REAL   (PARD_KIND) , ALLOCATABLE :: DZB_BK_MIN_ALTER
    REAL   (PARD_KIND) , ALLOCATABLE :: XI0_ALTER , XXIM_ALTER
    REAL   (GJRD_KIND) , ALLOCATABLE :: THETA0_ALTER
    REAL   (GJRD_KIND) , ALLOCATABLE :: BTH_ALTER

    INTEGER            :: ERROR
    CHARACTER( 2**10 ) :: ERRMSG

    CHARACTER( 2**03 ) :: I_CHAR
    INTEGER(IJID_KIND) :: I

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
      WRITE(*,'(  "  2- (non-optional)")')
      WRITE(*,'(  "      Number of layers into which the whole depth is expected to be divided;")')
      WRITE(*,'(  "  3- (non-optional)")')
      WRITE(*,'(  "      Maximum bed deformation at banks, (+) positive, in meters;")')
      WRITE(*,'(  "  4- (optional)")')
      WRITE(*,'(  "      Number of meander bends that the mesh contains;")')
      WRITE(*,'(  "    Or,")')
      WRITE(*,'(  "      If omitted, only ONE meander bend is supposed to be contained;")')
      WRITE(*,'(  "  5- (optional)")')
      WRITE(*,'(  "      ONE or MORE alternative options, which change the default values of")')
      WRITE(*,'(  "    corresponding variables, with the following format:")')
      WRITE(*,'(  "        --<identifier> <value>")')
      WRITE(*,'(  "    where <identifier> must be chosen from the following list and corresponding")')
      WRITE(*,'(  "    <value> needs to be specified:")')
      WRITE(*,'(  "    A-  dzb_bk_min")')
      WRITE(*,'(  "        Minimum bed deformation at banks, (-) negative, in meters;")')
      WRITE(*,'(  "    B-  xi0")')
      WRITE(*,'(  "        Dimensionless longitudinal coordinate of cross section where zero bed")')
      WRITE(*,'(  "      deformation occurs;")')
      WRITE(*,'(  "    C-  xxim")')
      WRITE(*,'(  "        Difference in dimensionless longitudinal coordinate between the cross")')
      WRITE(*,'(  "      section where the largest bed deformation occurs and its nearest upstream")')
      WRITE(*,'(  "      cross section where zero bed deformation occurs;")')
      WRITE(*,'(  "        i.e. xxim = xim - xi0;")')
      WRITE(*,'(  "    D-  theta0")')
      WRITE(*,'(  "        Deflection angle of channel ceterline at reference crossover section,")')
      WRITE(*,'(  "      in degrees;")')
      WRITE(*,'(  "        theta0 is used to determine xi0 and xxim; if any or both of xi0 and xxim is")')
      WRITE(*,'(  "      specified together with theta0, the former takes precedence over the latter;")')
      WRITE(*,'(  "      if neither xi0 or xxim nor theta0 is specified, xi0 or xxim will be determined")')
      WRITE(*,'(  "      by the theta0 read from the mesh;")')
      WRITE(*,'(  "    E-  b")')
      WRITE(*,'(  "        Channel width, in meters;")')
      WRITE(*,'(  "        b is used to calculte the width-to-depth ratio and then determine dzb_bk_min")')
      WRITE(*,'(  "      by considering that the ratio of the maximum to the minimum bed deformation at")')
      WRITE(*,'(  "      banks is a function of the width-to-depth ratio; b takes no effect if it is")')
      WRITE(*,'(  "      specified together with dzb_bk_min; if neither dzb_bk_min nor b is specified,")')
      WRITE(*,'(  "      dzb_bk_min will be determined by the b read from the mesh;")')
      WRITE(*,'(  "    F-  t")')
      WRITE(*,'(  "        Time stamped on the generated data, in either relative or Julian sense,")')
      WRITE(*,'(  "      in seconds;")')
      WRITE(*,'(  "        t takes effect only when the mesh file contains no datasets; if not so,")')
      WRITE(*,'(  "      t will be determined as the time recorded in the datasets;")')
      WRITE(*,'(  "    Or,")')
      WRITE(*,'(  "      If omitted, ALL these variables will be assigned default values;")')
      WRITE(*,'(  "  Note,")')
      WRITE(*,'(  "    ALL the alternative options A--F can be specified in any order;")')
      WRITE(*,'(  "But,")')
      WRITE(*,'(  "  ALL the arguments 1--5 MUST be given in sequence.")')
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
      CALL MR_MALLOC_CURVED_GEOS_GOO
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

    WRITE(*,'("Initialize field variables and activity on hot mode... ", $ )')
    CALL MR_INIT_FIELD_VARS_N_ACTIVITY_HOT( FILE_XMDF , T_START , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      WRITE(*,'( )')
      CALL MR_CTRL_COLD_MODE_STARTED( HTH )
      WRITE(*,'( )')
      WRITE(*,'("Initialize field variables and activity on cold mode... ", $ )')
      CALL MR_INIT_FIELD_VARS_N_ACTIVITY_COLD( HTH )
      IF( ALLOCATED( T_START_ALTER ) )  THEN
        T_START  =   T_START_ALTER
      ELSE
        T_START  =   0.0
      END IF
      START_MODE = COLD_MODE
    ELSE
      CALL MR_AVERAGE_SS( NI , NJ , H , HTH )
      START_MODE = HOT_MODE
    END IF
    WRITE(*,'("Done! ")')

    WRITE(*,'("Initialize essential parameters... ", $ )')
    CALL MR_INIT_MEANDER_PARS( ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    ELSE
      IF( ALLOCATED( THETA0_ALTER ) ) THETA0 = THETA0_ALTER
      IF( ALLOCATED( BTH_ALTER ) ) BTH = BTH_ALTER
      IF( ALLOCATED( XI0_ALTER ) ) THEN
        XI0  = XI0_ALTER
      ELSE
        XI0  = 0.0033 * (THETA0**4) + SIGN(0.0198,THETA0) * (THETA0**3) - 0.1100 * (THETA0*THETA0) + SIGN(0.25,THETA0)
      END IF
      IF( ALLOCATED( XXIM_ALTER ) ) THEN
        XXIM = XXIM_ALTER
      ELSE
        XXIM = 0.25 - 0.0327670 * ( ABS(THETA0)**3.36 ) * ( ( 2.4048255 - ABS(THETA0) )**2.66 )
      END IF
      IF( ALLOCATED( DZB_BK_MIN_ALTER ) ) THEN
        DZB_BK_MIN = DZB_BK_MIN_ALTER
      ELSE
        DZB_BK_MIN = - DZB_BK_MAX *   &
        ( 1.0 + 2.0 /   &
          ( 1.0 +   &
            EXP( - 0.33 * ( BTH/HTH - 15.85 ) )   &
          )   &
        )
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

    SELECT CASE( START_MODE )
    CASE( COLD_MODE )
      CALL MR_INIT_OUTPUT( FILE_XMDF , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      ELSE
        CALL MR_OUTPUT( FILE_XMDF , T_START , ERROR , ERRMSG , OVERWRITE=.FALSE. )
        IF( ERROR < 0 ) THEN
          WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
          STOP
        END IF
      END IF
    CASE( HOT_MODE )
      CALL MR_OUTPUT( FILE_XMDF , T_START , ERROR , ERRMSG , OVERWRITE=.TRUE. )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF
    END SELECT

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
    ELSE IF( COMMAND_ARGUMENT_COUNT() > 16 ) THEN
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

    I_ARG = 2
    WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG
  ! GET NUMBER OF LAYERS THAT THE WHOLE DEPTH IS EXPECTED TO BE DIVIDED INTO
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
        READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) NK
        IF( ERROR /= 0 ) THEN
          ERROR = - ABS(ERROR)
          ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
          RETURN
        END IF
        CALL MR_MALLOC_KK_CONSTS_N_REF_PARS
      END IF
    END IF

    I_ARG = 3
    WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG
  ! GET MAXIMUM BED DEFORMATION (POSITIVE) AT BANKS, IN METERS
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
        READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) DZB_BK_MAX
        IF( ERROR /= 0 ) THEN
          ERROR = - ABS(ERROR)
          ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
          RETURN
        ELSE IF( DZB_BK_MAX < 0.0 ) THEN
          ERROR = - 1
          ERRMSG = "Illegal value for command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
          RETURN
        END IF
      END IF
    END IF

    IF( COMMAND_ARGUMENT_COUNT() > 3 ) THEN

      I_ARG = 4
      WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG
    ! GET NUMBER OF MEANDER BENDS
      CALL GET_COMMAND_ARGUMENT( I_ARG , CHAR_ARGUMENT , STATUS=ERROR )
      IF( ERROR /= 0 ) THEN
        ERROR = - ABS(ERROR)
        ERRMSG = "Error in getting command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
        RETURN
      ELSE IF( CHAR_ARGUMENT(1:2) /= "--" ) THEN
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
        END IF
        I_ARG_ALTER_START = 5
      ELSE
       !BLOCK
      ! ASSIGN DEFAULT VALUES TO OPTIONAL ARGUMENTS
        NBENDS = 1
       !END BLOCK
        I_ARG_ALTER_START = 4
      END IF

    ELSE
     !BLOCK
    ! ASSIGN DEFAULT VALUES TO OPTIONAL ARGUMENTS
      NBENDS = 1
     !END BLOCK
      I_ARG_ALTER_START = 4
    END IF

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

        SELECT CASE( TRIM(CHAR_ARGUMENT) )
        CASE( "--DZB_BK_MIN" , "--Dzb_Bk_Min" , "--Dzb_bk_min" , "--dzb_bk_min" )
          ALLOCATE( DZB_BK_MIN_ALTER )

          WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG+1
        ! GET ALTERNATIVE MINIMUM BED DEFORMATION (NEGATIVE) AT BANKS, IN METERS
          CALL GET_COMMAND_ARGUMENT( I_ARG+1 , CHAR_ARGUMENT , STATUS=ERROR )
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
              READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) DZB_BK_MIN_ALTER
              IF( ERROR /= 0 ) THEN
                ERROR = - ABS(ERROR)
                ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
                RETURN
              ELSE IF( DZB_BK_MIN_ALTER > 0.0 ) THEN
                ERROR = - 1
                ERRMSG = "Illegal value for command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
                RETURN
              END IF
            END IF
          END IF

        CASE( "--XI0" , "--Xi0" , "--xi0" )
          ALLOCATE( XI0_ALTER )

          WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG+1
        ! GET ALTERNATIVE XI-LOCATION OF CROSS SECTION WHERE ZERO BED DEFORMATION OCCURS
          CALL GET_COMMAND_ARGUMENT( I_ARG+1 , CHAR_ARGUMENT , STATUS=ERROR )
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
              READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) XI0_ALTER
              IF( ERROR /= 0 ) THEN
                ERROR = - ABS(ERROR)
                ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
                RETURN
              END IF
            END IF
          END IF

        CASE( "--XXIM" , "--XXim" , "--Xxim" , "--xxim" )
          ALLOCATE( XXIM_ALTER )

          WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG+1
        ! GET ALTERNATIVE XXI-LOCATION OF CROSS SECTION WHERE MAXIMUM BED DEFORMATION OCCURS
          CALL GET_COMMAND_ARGUMENT( I_ARG+1 , CHAR_ARGUMENT , STATUS=ERROR )
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
              READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) XXIM_ALTER
              IF( ERROR /= 0 ) THEN
                ERROR = - ABS(ERROR)
                ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
                RETURN
              ELSE IF( XXIM_ALTER <= 0.0 .OR. XXIM_ALTER >= 0.5 ) THEN
                ERROR = - 1
                ERRMSG = "Illegal value for command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
                RETURN
              END IF
            END IF
          END IF

        CASE( "--THETA0" , "--Theta0" , "--theta0" )
          ALLOCATE( THETA0_ALTER )

          WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG+1
        ! GET ALTERNATIVE DEFLECTION ANGLE AT REFERENCE CROSSOVER SECTION, IN DEGREES
          CALL GET_COMMAND_ARGUMENT( I_ARG+1 , CHAR_ARGUMENT , STATUS=ERROR )
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
              READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) THETA0_ALTER
              IF( ERROR /= 0 ) THEN
                ERROR = - ABS(ERROR)
                ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
                RETURN
              ELSE IF( THETA0_ALTER <= -138.0 .OR. THETA0_ALTER > +138.0 ) THEN
                ERROR = - 1
                ERRMSG = "Illegal value for command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
                RETURN
              END IF
            END IF
          END IF

          THETA0_ALTER = THETA0_ALTER * PI / 180.0

        CASE( "--B" , "--b" , "--BTH" , "--Bth" , "--bth" )
          ALLOCATE( BTH_ALTER )

          WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG+1
        ! GET ALTERNATIVE DEFLECTION ANGLE AT REFERENCE CROSSOVER SECTION, IN DEGREES
          CALL GET_COMMAND_ARGUMENT( I_ARG+1 , CHAR_ARGUMENT , STATUS=ERROR )
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
              READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) BTH_ALTER
              IF( ERROR /= 0 ) THEN
                ERROR = - ABS(ERROR)
                ERRMSG = "Error in reading a value from command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
                RETURN
              ELSE IF( BTH_ALTER <= 0.0 ) THEN
                ERROR = - 1
                ERRMSG = "Illegal value for command argument no."//TRIM(ADJUSTL(I_ARG_CHAR))
                RETURN
              END IF
            END IF
          END IF

        CASE( "--T" , "--t" , "--T_START" , "--T_Start" , "--T_start" , "--t_start" )
          ALLOCATE(T_START_ALTER)

          WRITE( I_ARG_CHAR , '(I<LEN(I_ARG_CHAR)>)' ) I_ARG+1
        ! GET ALTERNATIVE STARTING TIME, IN SECONDS
          CALL GET_COMMAND_ARGUMENT( I_ARG+1 , CHAR_ARGUMENT , STATUS=ERROR )
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

  END PROGRAM INIBATHYGEN