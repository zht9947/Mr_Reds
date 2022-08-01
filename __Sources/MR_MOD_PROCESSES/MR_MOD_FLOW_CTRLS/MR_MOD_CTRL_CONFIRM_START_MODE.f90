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
  MODULE MR_MOD_CTRL_CONFIRM_START_MODE

    USE MR_0_SKIP_MODE

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_SLOPE

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CTRL_CONFIRM_START_MODE_COLD
    PUBLIC :: MR_CTRL_CONFIRM_START_MODE_HOT

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
  SUBROUTINE MR_CTRL_CONFIRM_START_MODE_COLD( HTH )

    USE MR_MOD_OPERATOR_CHAR_STRING

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(OUT) :: HTH

    CHARACTER(   1   ) :: Y_OR_N

    INTEGER            :: ERROR

    CHARACTER( 2**08 ) :: CHAR_ARGUMENT

    DO
      WRITE(*,'("Run on cold mode? (Y/N): ", $ )')
      IF( RUN_ON_SKIP_MODE ) THEN
        Y_OR_N = "Y"
        WRITE(*,'(A)') TRIM(Y_OR_N(1:1))
      ELSE
        READ(*,*) Y_OR_N
      END IF
      SELECT CASE( .MRCHARUPPER.(TRIM(Y_OR_N(1:1))) )
      CASE( "Y" )
        EXIT
      CASE( "N" )
        STOP
      END SELECT
    END DO

    WRITE(*,'( )')

    HTH = ZR
  ! SET CHANNEL-AVERAGED DEPTH
    SET_HTH :   &
    DO
      WRITE(*,'(2X,"The channel-averaged depth will be set to: ")')
      WRITE(*,'(2X, F16.6 ," m")') HTH

      DO
        WRITE(*,'(4X,"Is the value correct? (Y/N): ", $ )')
        IF( RUN_ON_SKIP_MODE ) THEN
          Y_OR_N = "Y"
          WRITE(*,'(A)') TRIM(Y_OR_N(1:1))
        ELSE
          READ(*,*) Y_OR_N
        END IF
        SELECT CASE( .MRCHARUPPER.(TRIM(Y_OR_N(1:1))) )
        CASE( "Y" )
          EXIT SET_HTH
        CASE( "N" )

          DO
            WRITE(*,'(6X,"Type in the correct value or ""C"" to cease: ", $ )')
            READ(*,*) CHAR_ARGUMENT
            SELECT CASE( .MRCHARUPPER.(TRIM(CHAR_ARGUMENT(1:1))) )
            CASE( "C" )
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

    END DO SET_HTH

    WRITE(*,'( )')

  ! SET PROPERTIES
    SET_PROPS :   &
    DO
      WRITE(*,'(2X,"Specify manually /PROPERTIES for datasets? (Y/N): ", $ )')
      IF( RUN_ON_SKIP_MODE ) THEN
        Y_OR_N = "N"
        WRITE(*,'(A)') TRIM(Y_OR_N(1:1))
      ELSE
        READ(*,*) Y_OR_N
      END IF
      SELECT CASE( .MRCHARUPPER.(TRIM(Y_OR_N(1:1))) )
      CASE( "Y" )

        WRITE(*,'(2X,"(Type in ""C"" to cease)")')

        WRITE(*,'(4X,"/PROPERTIES")')

        WRITE(*,'(6X,"/By Layers")')

        DO
          WRITE(*,'(8X,"Number of Layers = ", $ )')
          READ(*,*) CHAR_ARGUMENT
          SELECT CASE( .MRCHARUPPER.(TRIM(CHAR_ARGUMENT(1:1))) )
          CASE( "C" )
            STOP
          CASE DEFAULT
            IF( VERIFY( TRIM(CHAR_ARGUMENT) , "+0123456789" ) /= 0 ) THEN
              !BLOCK
                WRITE(*,'(10X,"Illegal character detected!")')
              !END BLOCK
            ELSE
              READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) NK
              IF( ERROR /= 0 ) THEN
                WRITE(*,'(10X,"Error in reading a value!")')
              ELSE IF( NK <= 0 ) THEN
                WRITE(*,'(10X,"Illegal value!")')
              ELSE IF( NK > 32 ) THEN
                WRITE(*,'(10X,"The program allows a number of layers NOT exceeding 32!")')
              ELSE
                EXIT
              END IF
            END IF
          END SELECT
        END DO

        WRITE(*,'(6X,"/By Sediment Sizes")')

        DO
          WRITE(*,'(8X,"Number of Sediment Sizes = ", $ )')
          READ(*,*) CHAR_ARGUMENT
          SELECT CASE( .MRCHARUPPER.(TRIM(CHAR_ARGUMENT(1:1))) )
          CASE( "C" )
            STOP
          CASE DEFAULT
            IF( VERIFY( TRIM(CHAR_ARGUMENT) , "+0123456789" ) /= 0 ) THEN
              !BLOCK
                WRITE(*,'(10X,"Illegal character detected!")')
              !END BLOCK
            ELSE
              READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) NKS
              IF( ERROR /= 0 ) THEN
                WRITE(*,'(10X,"Error in reading a value!")')
              ELSE IF( NKS <= 0 ) THEN
                WRITE(*,'(10X,"Illegal value!")')
              ELSE IF( NKS > 8 ) THEN
                WRITE(*,'(10X,"The program allows a number of sediment sizes NOT exceeding 8!")')
              ELSE

                DO
                  WRITE(*,'(10X,"Sediment Sizes = ", $ )')
                  READ(*,*) CHAR_ARGUMENT
                  SELECT CASE( .MRCHARUPPER.(TRIM(CHAR_ARGUMENT(1:1))) )
                  CASE( "C" )
                    STOP
                  CASE DEFAULT
                    IF( VERIFY( TRIM(CHAR_ARGUMENT) , "-+0123456789Ee." ) /= 0 ) THEN
                      !BLOCK
                        WRITE(*,'(12X,"Illegal character detected!")')
                      !END BLOCK
                    ELSE
                      READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) D0(1:NKS)
                      IF( ERROR /= 0 ) THEN
                        WRITE(*,'(12X,"Error in reading a value!")')
                      ELSE IF( MINVAL(D0(1:NKS)) <= 0.0 ) THEN
                        WRITE(*,'(12X,"Illegal value!")')
                      ELSE
                        D0(1:NKS) = D0(1:NKS) / 1000.0
                        EXIT
                      END IF
                    END IF
                  END SELECT
                END DO

                EXIT

              END IF
            END IF
          END SELECT
        END DO

        DO
          WRITE(*,'(6X,"Slope = ", $ )')
          READ(*,*) CHAR_ARGUMENT
          SELECT CASE( .MRCHARUPPER.(TRIM(CHAR_ARGUMENT(1:1))) )
          CASE( "C" )
            STOP
          CASE DEFAULT
            IF( VERIFY( TRIM(CHAR_ARGUMENT) , "-+0123456789Ee." ) /= 0 ) THEN
              !BLOCK
                WRITE(*,'(8X,"Illegal character detected!")')
              !END BLOCK
            ELSE
              READ( CHAR_ARGUMENT , * , IOSTAT=ERROR ) SLOPE
              IF( ERROR /= 0 ) THEN
                WRITE(*,'(8X,"Error in reading a value!")')
              ELSE
                EXIT
              END IF
            END IF
          END SELECT
        END DO

        DO
          WRITE(*,'(4X,"Is above input correct? (Y/N): ", $ )')
          READ(*,*) Y_OR_N
          SELECT CASE( .MRCHARUPPER.(TRIM(Y_OR_N(1:1))) )
          CASE( "Y" )
            EXIT SET_PROPS
          CASE( "N" )
            EXIT
          END SELECT
        END DO

      CASE( "N" )
        EXIT
      END SELECT
    END DO SET_PROPS

  END SUBROUTINE MR_CTRL_CONFIRM_START_MODE_COLD

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
  SUBROUTINE MR_CTRL_CONFIRM_START_MODE_HOT

    USE MR_MOD_OPERATOR_CHAR_STRING

    IMPLICIT NONE

    CHARACTER(   1   ) :: Y_OR_N

    DO
      WRITE(*,'("Run on hot mode? (Y/N): ", $ )')
      IF( RUN_ON_SKIP_MODE ) THEN
        Y_OR_N = "Y"
        WRITE(*,'(A)') TRIM(Y_OR_N(1:1))
      ELSE
        READ(*,*) Y_OR_N
      END IF
      SELECT CASE( .MRCHARUPPER.(TRIM(Y_OR_N(1:1))) )
      CASE( "Y" )
        EXIT
      CASE( "N" )
        STOP
      END SELECT
    END DO

  END SUBROUTINE MR_CTRL_CONFIRM_START_MODE_HOT

  END MODULE MR_MOD_CTRL_CONFIRM_START_MODE