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
  PROGRAM MR_REDS

    USE MR_KINDS

    USE MR_DEF_TIMING
    USE MR_DEF_CONSTS_N_REF_PARS

    USE MR_MOD_INIT_PRJ

    USE MR_MOD_ECHO_PRJ

    USE MR_NUM_START_MODE
    USE MR_MOD_DETER_START_MODE

    USE MR_MOD_MALLOC_GRID_SYS
    USE MR_MOD_MALLOC_CURVED_GEOS
    USE MR_MOD_MALLOC_FIELD_VARS
    USE MR_MOD_MALLOC_ACTIVITY

    USE MR_MOD_INIT_GRID_SYS
    USE MR_MOD_INIT_CURVED_GEOS
    USE MR_MOD_INIT_FIELD_VARS_N_ACTIVITY

    USE MR_MOD_INIT_OUTPUT

    USE MR_MOD_UPDT_TBUV
    USE MR_MOD_UPDT_KI_N_DI
    USE MR_MOD_UPDT_HYD
    USE MR_MOD_UPDT_H
    USE MR_MOD_UPDT_VZW

    USE MR_MOD_OUTPUT

    IMPLICIT NONE

    CHARACTER( 2**08 ) :: FILE_PRJ
    CHARACTER( 2**08 ) :: FILE_XMDF

    INTEGER(TSID_KIND) :: ITS

    REAL   (TMRD_KIND) :: T

    INTEGER            :: ERROR
    CHARACTER( 2**10 ) :: ERRMSG

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

    CALL MR_DETER_START_MODE

   !BLOCK
  ! ALLOCATE MEMORIES FOR PROJECT
    CALL MR_MALLOC_GRID_SYS
    CALL MR_MALLOC_CURVED_GEOS
    CALL MR_MALLOC_FIELD_VARS
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

    WRITE(*,'("Initialize field variables and activities... ", $ )')
    SELECT CASE( START_MODE )
    CASE( COLD_MODE )
      CALL MR_INIT_FIELD_VARS_N_ACTIVITY_COLD
      WRITE(*,'("Done! ")')
      WRITE(*,'("Initialize output... ", $ )')
      CALL MR_INIT_OUTPUT( FILE_XMDF , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      ELSE
        CALL MR_OUTPUT( FILE_XMDF , T_START , ERROR , ERRMSG )
        IF( ERROR < 0 ) THEN
          WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
          STOP
        END IF
      END IF
    CASE( HOT_MODE )
      CALL MR_INIT_FIELD_VARS_N_ACTIVITY_HOT( FILE_XMDF , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF
    END SELECT
    WRITE(*,'("Done! ")')

    WRITE(*,'( )')

    T = T_START

    WRITE(*,'(8X,"Compute...  0.00%", A , $ )') ACHAR(13)

    DO ITS = 1 , NTSS

      CALL MR_UPDT_TBUV
      CALL MR_UPDT_KI_N_DI
     !CALL MR_UPDT_SED
      CALL MR_UPDT_HYD
      CALL MR_UPDT_H
     !CALL MR_UPDT_R
     !CALL MR_UPDT_VZUV
      CALL MR_UPDT_VZW

      T = T + DT/COR
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

    WRITE(*,'(/,"The result has been written into the file: ",/,4X, A )') TRIM(FILE_XMDF)

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

  END SUBROUTINE MR_INIT_FILES

  END PROGRAM MR_REDS