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
  PROGRAM MR_REDS

    USE MR_KINDS

    USE MR_DEF_TIMING
    USE MR_DEF_CONSTS_N_REF_PARS

    USE MR_MOD_INIT_PRJ

    USE MR_MOD_ECHO_PRJ

    USE MR_MOD_MALLOC_GRID_SYS
    USE MR_MOD_MALLOC_CURVED_GEOS
    USE MR_MOD_MALLOC_FIELD_VARS_RESAMPLE
    USE MR_MOD_MALLOC_ACTIVITY

    USE MR_MOD_INIT_GRID_SYS
    USE MR_MOD_INIT_CURVED_GEOS

    USE MR_MOD_INIT_OUTPUT

    USE MR_MOD_GENER_GET_TIMES
    USE MR_MOD_INPUT_RESAMPLE
    USE MR_MOD_OUTPUT_RESAMPLE

    IMPLICIT NONE

    CHARACTER( 2**08 ) :: FILE_PRJ
    CHARACTER( 2**08 ) :: FILE_XMDF
    CHARACTER( 2**08 ) :: FILE_XMDF_RESAMPLE

    INTEGER(TSID_KIND) :: ITS
    INTEGER(TSID_KIND) :: ITS_START , ITS_STRIDE , ITS_END

    REAL   (TMRD_KIND) :: T

    INTEGER            :: ERROR
    CHARACTER( 2**10 ) :: ERRMSG

   !BLOCK
  ! MANAGE THE VERSION
    WRITE(*,'( A ,"_Resample by ", A ," [ver.", A ,"]",/)') TRIM(INNERNAME) , TRIM(CONTRIBUTOR) , TRIM(SEMVER)
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
    CALL MR_MALLOC_FIELD_VARS_RESAMPLE
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
    CALL MR_INIT_OUTPUT( FILE_XMDF_RESAMPLE , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done! ")')

    WRITE(*,'( )')

  ! GET NTSS
    CALL MR_GENER_GET_NTSS( FILE_XMDF , NTSS , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
  ! REASSIGN ITS_END
    IF( ITS_END > NTSS ) THEN
      ITS_END = NTSS
    END IF

    WRITE(*,'(8X,"Resample...  0.00%", A , $ )') ACHAR(13)

    DO ITS = ITS_START , ITS_END , ITS_STRIDE

    ! GET T OF ITS
      CALL MR_GENER_GET_T_ITS( FILE_XMDF , NTSS , ITS , T , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF

      CALL MR_INPUT_RESAMPLE( FILE_XMDF , ITS , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF

      CALL MR_OUTPUT_RESAMPLE( FILE_XMDF_RESAMPLE , T , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF

      WRITE(*,'(8X,"Resample...",F6.2,"%", A , $ )') REAL(ITS)/REAL(ITS_END)*100.00 , ACHAR(13)

    END DO

    WRITE(*,'(8X,"Resample... Done! ")')

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

    CHARACTER( 2**08 )               :: CHAR_ARGUMENT

    INTEGER                          :: IDELI1 , IDELI2

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    IF( COMMAND_ARGUMENT_COUNT() < 3 ) THEN
      ERROR = - 1
      ERRMSG = "Not enough command arguments as input & output files"
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

  ! GET XMDF_RESAMPLE FILE'S PATH\NAME
    CALL GET_COMMAND_ARGUMENT( 3 , FILE_XMDF_RESAMPLE , STATUS=ERROR )
    IF( ERROR == - 1 ) THEN
      ERRMSG = "XMDF_RESAMPLE File's path too long!"
      RETURN
    ELSE IF( ERROR /= 0 ) THEN
      ERROR = - ABS(ERROR)
      ERRMSG = "Error in getting command argument No.3 as XMDF_RESAMPLE File"
      RETURN
    END IF

  ! DETERMINE DEFAULT ITS_START:ITS_STRIDE:ITS_END
    ITS_START = 0 ; ITS_STRIDE = 1 ; ITS_END = HUGE( ITS_END )
  ! GET USER-SPECIFIED ITS_START:ITS_STRIDE:ITS_END
    CALL GET_COMMAND_ARGUMENT( 4 , CHAR_ARGUMENT , STATUS=ERROR )
    IF( ERROR == 0 ) THEN
      IF( VERIFY( TRIM(CHAR_ARGUMENT) , "0123456789:" ) /= 0 ) THEN
        ERROR = - 1
        ERRMSG = "Illegal character in command argument No.4 as ITS_START:ITS_STRIDE:ITS_END"
        RETURN
      ELSE
        IF( SCAN( TRIM(CHAR_ARGUMENT) , ":" ) /= 0 ) THEN
          IDELI1 = SCAN( TRIM(CHAR_ARGUMENT) , ":" )
          CHAR_ARGUMENT(IDELI1:IDELI1) = " "
          IF( SCAN( TRIM(CHAR_ARGUMENT) , ":" ) /= 0 ) THEN
            IDELI2 = SCAN( TRIM(CHAR_ARGUMENT) , ":" )
            CHAR_ARGUMENT(IDELI2:IDELI2) = " "
            IF( SCAN( TRIM(CHAR_ARGUMENT) , ":" ) /= 0 ) THEN
              ERROR = - 1
              ERRMSG = "Too many colons in command argument No.4 as ITS_START:ITS_STRIDE:ITS_END"
              RETURN
            ELSE
            ! CASE ITS_START:ITS_STRIDE:ITS_END
              READ( CHAR_ARGUMENT(1:IDELI1-1) , * , IOSTAT=ERROR ) ITS_START
              READ( CHAR_ARGUMENT(IDELI1+1:IDELI2-1) , * , IOSTAT=ERROR ) ITS_STRIDE
              READ( CHAR_ARGUMENT(IDELI2+1:LEN_TRIM(CHAR_ARGUMENT)) , * , IOSTAT=ERROR ) ITS_END
              ERROR = 0
            END IF
          ELSE
          ! CASE ITS_START:ITS_END
            READ( CHAR_ARGUMENT(1:IDELI1-1) , * , IOSTAT=ERROR ) ITS_START
            READ( CHAR_ARGUMENT(IDELI1+1:LEN_TRIM(CHAR_ARGUMENT)) , * , IOSTAT=ERROR ) ITS_END
            ERROR = 0
          END IF
        ELSE
        ! CASE ITS_STRIDE
          READ( CHAR_ARGUMENT(1:LEN_TRIM(CHAR_ARGUMENT)) , * , IOSTAT=ERROR ) ITS_STRIDE
          ERROR = 0
        END IF
      END IF
    END IF

  END SUBROUTINE MR_INIT_FILES

  END PROGRAM MR_REDS