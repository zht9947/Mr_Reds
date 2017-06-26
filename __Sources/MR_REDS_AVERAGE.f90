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
  PROGRAM MR_REDS_AVERAGE

    USE MR_KINDS

    USE MR_MOD_INIT_PRJ

    USE MR_MOD_MALLOC_GRID_SYS
    USE MR_MOD_MALLOC_CURVED_GEOS
    USE MR_MOD_MALLOC_FIELD_VARS
    USE MR_MOD_MALLOC_FIELD_VARS_AVERAGE
    USE MR_MOD_MALLOC_ACTIVITY

    USE MR_MOD_INIT_GRID_SYS
    USE MR_MOD_INIT_CURVED_GEOS

    USE MR_MOD_INIT_OUTPUT_AVERAGE

    USE MR_MOD_GET_TIMES
    USE MR_MOD_INPUT_AVERAGE
    USE MR_MOD_UPDT_FIELD_VARS_AVERAGE
    USE MR_MOD_OUTPUT_AVERAGE

    USE MR_MOD_ECHO_PRJ

    IMPLICIT NONE

    CHARACTER( 2**08 ) :: FILE_PRJ
    CHARACTER( 2**08 ) :: FILE_XMDF
    CHARACTER( 2**08 ) :: FILE_AVERAGE

    INTEGER(TSID_KIND) :: ITS

    REAL   (TMRD_KIND) :: T

    LOGICAL            :: FLAG_PRJ_SETS_CORRECT
    LOGICAL            :: FLAG_COLD_START

    INTEGER            :: ERROR
    CHARACTER( 2**10 ) :: ERRMSG

   !BLOCK
  ! MANAGE THE VERSION
    WRITE(*,'( A ,"_Average by ", A ," [ver.", A ,"]",/)') TRIM(INNERNAME) , TRIM(CONTRIBUTOR) , TRIM(SEMVER)
   !END BLOCK

   !BLOCK
  ! GET PATH\NAMES OF INPUT FILES FROM COMMAND LINE
  ! AND MEANWHILE SET PATH\NAMES OF OUTPUT FILES
    CALL MR_INIT_FILES( ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
   !END BLOCK

    WRITE(*,'("Initialize project... ", $ )')
    CALL MR_INIT_PRJ( FILE_PRJ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done! ")')

    CALL MR_ECHO_PRJ

   !BLOCK
  ! ALLOCATE MEMORIES FOR PROJECT
    CALL MR_MALLOC_GRID_SYS
    CALL MR_MALLOC_CURVED_GEOS
    CALL MR_MALLOC_FIELD_VARS
    CALL MR_MALLOC_FIELD_VARS_AVERAGE
    CALL MR_MALLOC_ACTIVITY
   !END BLOCK

    WRITE(*,'( )')

    WRITE(*,'("Initialize grid system... ", $ )')
    CALL MR_INIT_GRID_SYS( FILE_XMDF , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done! ")')

    WRITE(*,'("Initialize curved geometries... ", $ )')
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
    CALL MR_INPUT_AVERAGE( FILE_XMDF , ITS , ERROR , ERRMSG )
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

    WRITE(*,'(/,"The result has been written into the file: ",/,4X, A )') TRIM(FILE_AVERAGE)

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
  SUBROUTINE MR_INIT_FILES( ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""

    IF( COMMAND_ARGUMENT_COUNT() < 2 ) THEN
      ERROR = - 1
      ERRMSG = "Not enough command arguments as input files"
      RETURN
    END IF

  ! GET PROJECT FILE'S PATH\NAME
    CALL GET_COMMAND_ARGUMENT( 1 , FILE_PRJ , STATUS=ERROR )
    IF( ERROR == - 1 ) THEN
      ERRMSG = "PROJECT File's path too long"
      RETURN
    ELSE IF( ERROR /= 0 ) THEN
      ERROR = - ABS(ERROR)
      ERRMSG = "Error in getting command argument No.1 as PROJECT File"
      RETURN
    END IF

  ! GET XMDF FILE'S PATH\NAME
    CALL GET_COMMAND_ARGUMENT( 2 , FILE_XMDF , STATUS=ERROR )
    IF( ERROR == - 1 ) THEN
      ERRMSG = "XMDF File's path too long!"
      RETURN
    ELSE IF( ERROR /= 0 ) THEN
      ERROR = - ABS(ERROR)
      ERRMSG = "Error in getting command argument No.2 as XMDF File"
      RETURN
    END IF

  ! SET AVERAGE FILE'S PATH\NAME
    FILE_AVERAGE = TRIM(FILE_XMDF)//".average.txt"

  END SUBROUTINE MR_INIT_FILES

  END PROGRAM MR_REDS_AVERAGE