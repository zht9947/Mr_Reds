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

    USE MR_MOD_IO_EXTEND

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

    WRITE(*,'(8X,"Extend...  0.00%", A , $ )') ACHAR(13)

    DO ITS = 0 , NTSS

    ! GET T OF ITS
      CALL MR_GET_T_ITS( FILE_XMDF , NTSS , ITS , T , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF

      CALL MR_IO_EXTEND( FILE_XMDF , FILE_XMDF_EXTEND , ITS , T , NLOOPS , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
        STOP
      END IF

      WRITE(*,'(8X,"Extend...",F6.2,"%", A , $ )')   &
      & REAL(ITS+1)/REAL(NTSS+1)*100.00 , ACHAR(13)

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

   !BLOCK
  ! DETERMINE DEFAULT RUNNING ARGUMENTS
  ! DETERMINE DEFAULT NLOOPS
    NLOOPS = 1
   !END BLOCK

  ! GET USER-SPECIFIED NLOOPS
    CALL GET_COMMAND_ARGUMENT( 3 , CHAR_ARGUMENT , STATUS=ERROR )
    IF( ERROR /= 0 ) THEN
      ERROR = + ABS(ERROR)
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

  END SUBROUTINE MR_INIT_FILES

  END PROGRAM MR_REDS_EXTEND