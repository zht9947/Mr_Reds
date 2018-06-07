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
  PROGRAM MR_REDS_EXTEND

    USE MR_KINDS

    USE MR_DEF_RANKS

    USE MR_MOD_MALLOC_GRID_SYS
    USE MR_MOD_MALLOC_GRID_SYS_EXTEND

    USE MR_MOD_INIT_GRID_SYS
    USE MR_MOD_INIT_GRID_SYS_EXTEND

    USE MR_MOD_GET_NI_NJ
    USE MR_MOD_GET_TIMES

    USE MR_MOD_RW_EXTEND

    IMPLICIT NONE

    CHARACTER( 2**08 ) :: FILE_XMDF
    CHARACTER( 2**08 ) :: FILE_XMDF_EXTEND

    INTEGER            :: NLOOPS

    INTEGER(TSID_KIND) :: NTSS
    INTEGER(TSID_KIND) :: ITS_START , ITS_STRIDE , ITS_END
    INTEGER(TSID_KIND) :: ITS

    REAL   (TMRD_KIND) :: T

    INTEGER            :: ERROR
    CHARACTER( 2**10 ) :: ERRMSG

   !BLOCK
  ! MANAGE THE VERSION
    WRITE(*,'( A ,"_Extend by ", A ," [ver.", A ,"]",/)') TRIM(INNERNAME) , TRIM(CONTRIBUTOR) , TRIM(SEMVER)
   !END BLOCK

   !BLOCK
  ! GET THE SOURCE AND TARGET FILES' PATH\NAMES
  ! AS WELL AS RUNNING ARGUMENTS FROM COMMAND LINE
    CALL MR_INIT_FILES( ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
   !END BLOCK

   !BLOCK
  ! GET NI AND NJ FROM THE SOURCE
    CALL MR_GET_NI_NJ( FILE_XMDF , NI , NJ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
   !END BLOCK

   !BLOCK
  ! ALLOCATE MEMORIES
    CALL MR_MALLOC_GRID_SYS
    CALL MR_MALLOC_GRID_SYS_EXTEND( NLOOPS )
   !END BLOCK

    WRITE(*,'("Initialize grid system of the source... ", $ )')
    CALL MR_INIT_GRID_SYS( FILE_XMDF , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF
    WRITE(*,'("Done! ")')

    WRITE(*,'("Initialize grid system of the target... ", $ )')
    CALL MR_INIT_GRID_SYS_EXTEND( FILE_XMDF_EXTEND , NLOOPS , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
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

    WRITE(*,'(8X,"Extend...  0.00%", A , $ )') ACHAR(13)

    DO ITS = ITS_START , ITS_END , ITS_STRIDE

    ! GET T OF ITS
      CALL MR_GET_T_ITS( FILE_XMDF , NTSS , ITS , T , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF

      CALL MR_RW_EXTEND( FILE_XMDF , FILE_XMDF_EXTEND , ITS , T , NLOOPS , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF
      
     WRITE(*,'(8X,"Extend...",F6.2,"%", A , $ )')   &
     & REAL((ITS-ITS_START)/ITS_STRIDE+1)/REAL((ITS_END-ITS_START)/ITS_STRIDE+1)*100.00 , ACHAR(13)

    END DO

    WRITE(*,'(8X,"Extend... Done! ")')

    WRITE(*,'(/,"The result has been written into the file: ",/,4X, A )') TRIM(FILE_XMDF_EXTEND)

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

    ERRMSG = ""

    IF( COMMAND_ARGUMENT_COUNT() < 2 ) THEN
      ERROR = - 1
      ERRMSG = "Not enough command arguments as input & output files"
      RETURN
    END IF

  ! GET XMDF FILE'S PATH\NAME
    CALL GET_COMMAND_ARGUMENT( 1 , FILE_XMDF , STATUS=ERROR )
    IF( ERROR == - 1 ) THEN
      ERRMSG = "XMDF File's path too long!"
      RETURN
    ELSE IF( ERROR /= 0 ) THEN
      ERROR = - ABS(ERROR)
      ERRMSG = "Error in getting command argument No.1 as XMDF File"
      RETURN
    END IF

  ! GET XMDF_EXTEND FILE'S PATH\NAME
    CALL GET_COMMAND_ARGUMENT( 2 , FILE_XMDF_EXTEND , STATUS=ERROR )
    IF( ERROR == - 1 ) THEN
      ERRMSG = "XMDF_EXTEND File's path too long!"
      RETURN
    ELSE IF( ERROR /= 0 ) THEN
      ERROR = - ABS(ERROR)
      ERRMSG = "Error in getting command argument No.2 as XMDF_EXTEND File"
      RETURN
    END IF

  ! DETERMINE DEFAULT NLOOPS
    NLOOPS = 1
  ! GET USER-SPECIFIED NLOOPS
    CALL GET_COMMAND_ARGUMENT( 3 , CHAR_ARGUMENT , STATUS=ERROR )
    IF( ERROR /= 0 ) THEN
      ERROR = + ABS(ERROR)
      RETURN
    ELSE
      IF( VERIFY( TRIM(CHAR_ARGUMENT) , "0123456789" ) /= 0 ) THEN
        ERROR = - 1
        ERRMSG = "Illegal character in command argument No.3 as NLOOPS"
        RETURN
      ELSE
        READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) NLOOPS
        ERROR = 0
      END IF
    END IF

  ! DETERMINE DEFAULT ITS_START:ITS_STRIDE:ITS_END
    ITS_START = 0 ; ITS_STRIDE = 1 ; ITS_END = HUGE( ITS_END )
  ! GET USER-SPECIFIED ITS_START:ITS_STRIDE:ITS_END
    CALL GET_COMMAND_ARGUMENT( 4 , CHAR_ARGUMENT , STATUS=ERROR )
    IF( ERROR /= 0 ) THEN
      ERROR = + ABS(ERROR)
      RETURN
    ELSE
      IF( TRIM(CHAR_ARGUMENT) == "/" ) THEN
        ITS_START = ITS_END
      ELSE
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
    END IF

  END SUBROUTINE MR_INIT_FILES

  END PROGRAM MR_REDS_EXTEND