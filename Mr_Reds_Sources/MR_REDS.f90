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

    USE MR_DEF_FILES
    USE MR_DEF_TIMING
    USE MR_DEF_CONSTS_N_REF_PARS

    USE MR_MOD_INIT_PRJ
    USE MR_MOD_INIT_GRID_SYS
    USE MR_MOD_INIT_CURVED_GEOS
    USE MR_MOD_INIT_FIELD_VARS_N_ACTIVITY
    USE MR_MOD_INIT_OUTPUT

    USE MR_MOD_UPDT_TBUV
    USE MR_MOD_UPDT_HYD
    USE MR_MOD_UPDT_H
    USE MR_MOD_UPDT_VZW

    USE MR_MOD_OUTPUT

    USE MR_MOD_ECHO_PRJ

    IMPLICIT NONE

    INTEGER(TSID_KIND) :: ITS

    REAL   (TMRD_KIND) :: T

    INTEGER            :: ERROR
    CHARACTER( 2**10 ) :: ERRMSG

    FILE_PRJ = "C:\Users\ZHT9947\Desktop\Case.txt"
    FILE_XMDF = "R:\Case.h5"

    WRITE(*,'("Initialize project... ", $ )')
    CALL MR_INIT_PRJ( FILE_PRJ , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    ELSE
      WRITE(*,'("Done! ")')
    END IF

    CALL MR_ECHO_PRJ

    WRITE(*,'( )')

    WRITE(*,'("Initialize grid system... ", $ )')
    CALL MR_INIT_GRID_SYS( FILE_XMDF , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    ELSE
      WRITE(*,'("Done! ")')
    END IF

    WRITE(*,'("Initialize curved geometries... ", $ )')
    CALL MR_INIT_CURVED_GEOS( FILE_XMDF , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    ELSE
      WRITE(*,'("Done! ")')
    END IF


    CALL MR_INIT_FIELD_VARS_N_ACTIVITY


    WRITE(*,'("Initialize output... ", $ )')
    CALL MR_INIT_OUTPUT( FILE_XMDF , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    ELSE
      WRITE(*,'("Done! ")')
    END IF

    WRITE(*,'(2X,"The result will be written into the file ",/,4X, A ,".")') TRIM(FILE_XMDF)

    WRITE(*,'( )')

    WRITE(*,'(8X,"Compute...  0.00%",A, $ )') ACHAR(13)

    T = T_START
    CALL MR_OUTPUT( FILE_XMDF , T , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
      STOP
    END IF

    DO ITS = 1 , NTSS

      CALL MR_UPDT_TBUV

      CALL MR_UPDT_HYD
      IF( MOD( ITS , ITS_OUTPUT ) == 0 ) THEN
        CALL MR_UPDT_WW
      END IF

      CALL MR_UPDT_H

      CALL MR_UPDT_VZW

      T = T + DT/COR
      IF( MOD( ITS , ITS_OUTPUT ) == 0 ) THEN

        CALL MR_OUTPUT( FILE_XMDF , T , ERROR , ERRMSG )
        IF( ERROR < 0 ) THEN
          WRITE(*,'(//,2X, A ,"!")') TRIM(ERRMSG)
          STOP
        END IF

      END IF

      WRITE (*,'(8X,"Compute... ",F6.2,"%",A, $ )') REAL(ITS)/REAL(NTSS)*100.00 , ACHAR(13)

    END DO

    WRITE(*,'(/,"Done! ")')

  END PROGRAM MR_REDS