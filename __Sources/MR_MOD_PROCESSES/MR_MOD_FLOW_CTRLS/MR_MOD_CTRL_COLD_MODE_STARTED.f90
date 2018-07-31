!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE)
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
  MODULE MR_MOD_CTRL_COLD_MODE_STARTED

    USE MR_KINDS

    USE MR_DEF_CONSTS_N_REF_PARS , ONLY : ZR

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CTRL_COLD_MODE_STARTED

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
  SUBROUTINE MR_CTRL_COLD_MODE_STARTED( HTH )

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(OUT) :: HTH

    INTEGER            :: ERROR

    CHARACTER(   1   ) :: Y_OR_N

    CHARACTER( 2**08 ) :: CHAR_ARGUMENT

    DO
      WRITE(*,'("Initialize on cold mode? (Y/N): ", $ )')
      READ(*,*) Y_OR_N
      SELECT CASE( Y_OR_N )
      CASE( "Y" , "y" )
        EXIT
      CASE( "N" , "n" )
        STOP
      END SELECT
    END DO

    HTH = ZR

    DO
      WRITE(*,'(2X,"The channel-averaged depth will be set as: ")')
      WRITE(*,'(2X, F16.6 ," m")') HTH

      DO
        WRITE(*,'(4X,"Is it correct? (Y/N): ", $ )')
        READ(*,*) Y_OR_N
        SELECT CASE( Y_OR_N )
        CASE( "Y" , "y" )
          RETURN
        CASE( "N" , "n" )

          DO
            WRITE(*,'(6X,"Type in the correct value or ""C"" to cease: ", $ )')
            READ(*,*) CHAR_ARGUMENT
            SELECT CASE( CHAR_ARGUMENT(1:1) )
            CASE( "C" , "c" )
              STOP
            CASE DEFAULT
              IF( VERIFY( TRIM(CHAR_ARGUMENT) , "-+0123456789Ee." ) /= 0 ) THEN
                !BLOCK
                  WRITE(*,'(8X,"Illegal character detected!")')
                !END BLOCK
              ELSE
                READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) HTH
                IF( ERROR /= 0 ) THEN
                  WRITE(*,'(8X,"Error in reading a value!")')
                ELSE IF( HTH <= 0.0 ) THEN
                  WRITE(*,'(8X,"Illegal value!")')
                ELSE
                  EXIT
                END IF
              END IF
            END SELECT
          END DO

          EXIT

        END SELECT
      END DO

    END DO

  END SUBROUTINE MR_CTRL_COLD_MODE_STARTED

  END MODULE MR_MOD_CTRL_COLD_MODE_STARTED