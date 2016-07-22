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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_INIT_PRJ

    USE MR_KINDS

    USE MR_DEF_PRJ_METADATA
    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_SLOPE
    USE MR_DEF_TIMING

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_INIT_PRJ

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
  SUBROUTINE MR_INIT_PRJ( FILE_PRJ_NAME , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_PRJ
    
    USE MR_MOD_ACCESS_REC
    USE MR_MOD_AWAYOUT_REC_ELEMENTS

    USE MR_MOD_ACQUIRE_PRJ_METADATA
    USE MR_MOD_GET_RANKS
    USE MR_MOD_READ_SLOPE
    USE MR_MOD_READ_TIMING
    USE MR_MOD_READ_CONSTS_N_REF_PARS

    USE MR_MOD_MALLOC_CONSTS_N_REF_PARS
    USE MR_MOD_MALLOC_GRID_SYS
    USE MR_MOD_MALLOC_CURVED_GEOS
    USE MR_MOD_MALLOC_FIELD_VARS
    USE MR_MOD_MALLOC_ACTIVITY

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_PRJ_NAME

    INTEGER                          :: FILE_PRJ_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY
    
    CHARACTER( 2**10 )               :: REC
    
    INTEGER                          :: REC_ID
    CHARACTER( 2**03 )               :: REC_ID_CHAR
    
    CHARACTER( 2**05 )               :: LABEL
    CHARACTER( 2**08 )               :: ALIAS

    INTEGER(KKID_KIND)               :: K

    ERRMSG = ""

    REC_ID = 0

    CALL MR_OPEN_FILE_PRJ( FILE_PRJ_NAME , FILE_PRJ_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_PRJ_NAME)
      RETURN
    END IF
    
    DO WHILE( .NOT. EOF(FILE_PRJ_ID) )
      CALL MR_ACCESS_REC( FILE_PRJ_ID , REC , ERROR , ERRMSG )
      REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID
      IF( ERROR < 0 ) THEN
        ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
        //"when initializing Project from file "//TRIM(FILE_PRJ_NAME)
        CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
        RETURN
      ELSE IF( REC == "" ) THEN
        CYCLE
      END IF

      CALL MR_AWAYOUT_LABEL( REC , LABEL , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
        //"when initializing Project from file "//TRIM(FILE_PRJ_NAME)
        CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
        RETURN
      END IF

      SELECT CASE( TRIM(LABEL) )
      CASE( "PRJMETADATA" )
        DO WHILE( .NOT. EOF(FILE_PRJ_ID) )
          CALL MR_ACCESS_REC( FILE_PRJ_ID , REC , ERROR , ERRMSG )
          REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID
          IF( ERROR < 0 ) THEN
            ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
            //"when initializing Project Metadata from file "//TRIM(FILE_PRJ_NAME)
            CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
            RETURN
          ELSE IF( REC == "" ) THEN
            CYCLE
          END IF

          CALL MR_AWAYOUT_LABEL( REC , LABEL , ERROR , ERRMSG )
          IF( ERROR < 0 ) THEN
            ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
            //"when initializing Project Metadata from file "//TRIM(FILE_PRJ_NAME)
            CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
            RETURN
          END IF

          SELECT CASE( TRIM(LABEL) )
          CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "IJK" , "NKS" , "TAB" )
            BACKSPACE( FILE_PRJ_ID )
            REC_ID = REC_ID - 1
            EXIT
          CASE( "TITLE" )
            CALL MR_ACQUIRE_PRJ_METADATA( REC , PRJ_TITLE , ERROR , ERRMSG )
            IF( ERROR < 0 ) THEN
              ERRMSG = TRIM(ERRMSG)//" from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
              //"when initializing Project Title from file "//TRIM(FILE_PRJ_NAME)
              CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
              RETURN
            END IF
          CASE( "ABSTRACT" )
            CALL MR_ACQUIRE_PRJ_METADATA( REC , PRJ_ABSTRACT , ERROR , ERRMSG )
            IF( ERROR < 0 ) THEN
              ERRMSG = TRIM(ERRMSG)//" from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
              //"when initializing Project Abstract from file "//TRIM(FILE_PRJ_NAME)
              CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
              RETURN
            END IF
          END SELECT

        END DO

      CASE( "IJK" )
        CALL MR_GET_NI_NJ_NK( REC , NI , NJ , NK , ERROR , ERRMSG )
        IF( ERROR < 0 ) THEN
          ERRMSG = TRIM(ERRMSG)//" from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
          //"when initializing Grid Ranks from file "//TRIM(FILE_PRJ_NAME)
          CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
          RETURN
        END IF

        CALL MR_MALLOC_KK_CONSTS_N_REF_PARS( NK )

      ! CALCULATE DSIGMA & SIGMA COORDINATES
        DSIGMA = 1.0 / NK

        SIGMA(NK) = - 0.5 * DSIGMA
        DO K = NK-1 , 1 , -1
          SIGMA( K ) = SIGMA(K+1) - DSIGMA
        END DO

      CASE( "DKS" )
        CALL MR_READ_PAR( REC , D0 , ERROR , ERRMSG )
        IF( ERROR < 0 ) THEN
          ERRMSG = TRIM(ERRMSG)//" ""Grain size"" "   &
          //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
          //"when initializing Grain Size from file "//TRIM(FILE_PRJ_NAME)
          CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
          RETURN
        END IF

      ! CONVERT UNIT FROM MILLIMETERS TO METERS
        D0 = D0 / 1000.0

      ! CALCULATE DS, TCRS, WS & RBS

      CASE( "TAB" )
        CALL MR_AWAYOUT_ALIAS( REC , ALIAS , ERROR , ERRMSG )
        IF( ERROR < 0 ) THEN
          ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
          //"when initializing Project Groups from file "//TRIM(FILE_PRJ_NAME)
          CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
          RETURN
        END IF

        SELECT CASE( TRIM(ALIAS) )
        CASE( "Timing" )
          DO WHILE( .NOT. EOF(FILE_PRJ_ID) )
            CALL MR_ACCESS_REC( FILE_PRJ_ID , REC , ERROR , ERRMSG )
            REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID
            IF( ERROR < 0 ) THEN
              ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
              //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
              CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
              RETURN
            ELSE IF( REC == "" ) THEN
              CYCLE
            END IF

            CALL MR_AWAYOUT_LABEL( REC , LABEL , ERROR , ERRMSG )
            IF( ERROR < 0 ) THEN
              ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
              //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
              CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
              RETURN
            END IF

            SELECT CASE( TRIM(LABEL) )
            CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "IJK" , "NKS" , "TAB" )
              BACKSPACE( FILE_PRJ_ID )
              REC_ID = REC_ID - 1
              EXIT
            CASE( "VAR" )
              CALL MR_AWAYOUT_ALIAS( REC , ALIAS , ERROR , ERRMSG )
              IF( ERROR < 0 ) THEN
                ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                RETURN
              END IF

              SELECT CASE( TRIM(ALIAS) )
              CASE( "Computational time interval" )
                CALL MR_READ_T( REC , DT , ERROR , ERRMSG )
                IF( ERROR < 0 ) THEN
                  ERRMSG = TRIM(ERRMSG)//" ""Computational time interval"" "   &
                  //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                  //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                  CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                  RETURN
                END IF
              CASE( "Starting time" )
                CALL MR_READ_T( REC , T_START , ERROR , ERRMSG )
                IF( ERROR < 0 ) THEN
                  ERRMSG = TRIM(ERRMSG)//" ""Starting time"" "   &
                  //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                  //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                  CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                  RETURN
                END IF
              CASE( "Total number of time steps for computing" )
                CALL MR_READ_ITS( REC , NTSS , ERROR , ERRMSG )
                IF( ERROR < 0 ) THEN
                  ERRMSG = TRIM(ERRMSG)//" ""Total number of time steps for computing"" "   &
                  //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                  //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                  CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                  RETURN
                END IF
              CASE( "Inteval of number of time steps for outputting" )
                CALL MR_READ_ITS( REC , ITS_OUTPUT , ERROR , ERRMSG )
                IF( ERROR < 0 ) THEN
                  ERRMSG = TRIM(ERRMSG)//" ""Inteval of number of time steps for outputting"" "   &
                  //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                  //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                  CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                  RETURN
                END IF
              CASE( "Time relaxation factor" )
                CALL MR_READ_TIME_FACTOR( REC , PHI , ERROR , ERRMSG )
                IF( ERROR < 0 ) THEN
                  ERRMSG = TRIM(ERRMSG)//" ""Time relaxation factor"" "   &
                  //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                  //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                  CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                  RETURN
                END IF
              END SELECT

            END SELECT

          END DO

        CASE( "Flow Plane Slope" )
          DO WHILE( .NOT. EOF(FILE_PRJ_ID) )
            CALL MR_ACCESS_REC( FILE_PRJ_ID , REC , ERROR , ERRMSG )
            REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID
            IF( ERROR < 0 ) THEN
              ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
              //"when initializing Flow Plane Slope from file "//TRIM(FILE_PRJ_NAME)
              CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
              RETURN
            ELSE IF( REC == "" ) THEN
              CYCLE
            END IF

            CALL MR_AWAYOUT_LABEL( REC , LABEL , ERROR , ERRMSG )
            IF( ERROR < 0 ) THEN
              ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
              //"when initializing Flow Plane Slope from file "//TRIM(FILE_PRJ_NAME)
              CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
              RETURN
            END IF
            
            SELECT CASE( TRIM(LABEL) )
            CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "IJK" , "NKS" , "TAB" )
              BACKSPACE( FILE_PRJ_ID )
              REC_ID = REC_ID - 1
              EXIT
            CASE( "VAR" )
              CALL MR_AWAYOUT_ALIAS( REC , ALIAS , ERROR , ERRMSG )
              IF( ERROR < 0 ) THEN
                ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                //"when initializing Flow Plane Slope from file "//TRIM(FILE_PRJ_NAME)
                CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                RETURN
              END IF

              SELECT CASE( TRIM(ALIAS) )
              CASE( "Flow plane slope" )
                CALL MR_READ_SLOPE( REC , SLOPE , ERROR , ERRMSG )
                IF( ERROR < 0 ) THEN
                  ERRMSG = TRIM(ERRMSG)//" ""Flow plane slope"" "   &
                  //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                  //"when initializing Flow Plane Slope from file "//TRIM(FILE_PRJ_NAME)
                  CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                  RETURN
                END IF
              END SELECT

            END SELECT

          END DO

        CASE( "Constants and Reference Parameters" )
          DO WHILE( .NOT. EOF(FILE_PRJ_ID) )
            CALL MR_ACCESS_REC( FILE_PRJ_ID , REC , ERROR , ERRMSG ) 
            REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID
            IF( ERROR < 0 ) THEN
              ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
              //"when initializing Constants and Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
              CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
              RETURN
            ELSE IF( REC == "" ) THEN
              CYCLE
            END IF

            CALL MR_AWAYOUT_LABEL( REC , LABEL , ERROR , ERRMSG )
            IF( ERROR < 0 ) THEN
              ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
              //"when initializing Constants and Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
              CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
              RETURN
            END IF

            SELECT CASE( TRIM(LABEL) )
            CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "IJK" , "NKS" , "TAB" )
              BACKSPACE( FILE_PRJ_ID )
              REC_ID = REC_ID - 1
              EXIT
            CASE( "TAC" )
              CALL MR_AWAYOUT_ALIAS( REC , ALIAS , ERROR , ERRMSG )
              IF( ERROR < 0 ) THEN
                ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                //"when initializing Constants and Reference Parameters Groups from file "//TRIM(FILE_PRJ_NAME)
                CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                RETURN
              END IF

              SELECT CASE( TRIM(ALIAS) )
              CASE( "Water Physical Properties" )
                DO WHILE( .NOT. EOF(FILE_PRJ_ID) )
                  CALL MR_ACCESS_REC( FILE_PRJ_ID , REC , ERROR , ERRMSG )
                  REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID
                  IF( ERROR < 0 ) THEN
                    ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                    //"when initializing Constants and Reference Parameters\"   &
                    //"Water Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                    CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                    RETURN
                  ELSE IF( REC == "" ) THEN
                    CYCLE
                  END IF

                  CALL MR_AWAYOUT_LABEL( REC , LABEL , ERROR , ERRMSG )
                  IF( ERROR < 0 ) THEN
                    ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                    //"when initializing Constants and Reference Parameters\"   &
                    //"Water Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                    CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                    RETURN
                  END IF

                  SELECT CASE( TRIM(LABEL) )
                  CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "IJK" , "NKS" , "TAB" , "TAC" )
                    BACKSPACE( FILE_PRJ_ID )
                    REC_ID = REC_ID - 1
                    EXIT
                  CASE( "VAR" )
                    CALL MR_AWAYOUT_ALIAS( REC , ALIAS , ERROR , ERRMSG )
                    IF( ERROR < 0 ) THEN
                      ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                      //"when initializing Constants and Reference Parameters\"   &
                      //"Water Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                      CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                      RETURN
                    END IF

                    SELECT CASE( TRIM(ALIAS) )
                    CASE( "Water density" )
                      CALL MR_READ_PAR( REC , R0 , ERROR , ERRMSG )
                      IF( ERROR < 0 ) THEN
                        ERRMSG = TRIM(ERRMSG)//" ""Water density"" "   &
                        //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                        //"when initializing Constants and Reference Parameters\"   &
                        //"Water Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                        CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                        RETURN
                      END IF
                    CASE( "Water kinematic viscosity" )
                      CALL MR_READ_PAR( REC , V0 , ERROR , ERRMSG )
                      IF( ERROR < 0 ) THEN
                        ERRMSG = TRIM(ERRMSG)//" ""Water kinematic viscosity"" "   &
                        //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                        //"when initializing Constants and Reference Parameters\"   &
                        //"Water Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                        CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                        RETURN
                      END IF
                    END SELECT

                  END SELECT

                END DO

              CASE( "Sediment Physical Properties" )
                DO WHILE( .NOT. EOF(FILE_PRJ_ID) )
                  CALL MR_ACCESS_REC( FILE_PRJ_ID , REC , ERROR , ERRMSG )
                  REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID
                  IF( ERROR < 0 ) THEN
                    ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                    //"when initializing Constants and Reference Parameters\"   &
                    //"Sediment Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                    CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                    RETURN
                  ELSE IF( REC == "" ) THEN
                    CYCLE
                  END IF

                  CALL MR_AWAYOUT_LABEL( REC , LABEL , ERROR , ERRMSG )
                  IF( ERROR < 0 ) THEN
                    ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                    //"when initializing Constants and Reference Parameters\"   &
                    //"Sediment Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                    CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                    RETURN
                  END IF

                  SELECT CASE( TRIM(LABEL) )
                  CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "IJK" , "NKS" , "TAB" , "TAC" )
                    BACKSPACE( FILE_PRJ_ID )
                    REC_ID = REC_ID - 1
                    EXIT
                  CASE( "VAR" )
                    CALL MR_AWAYOUT_ALIAS( REC , ALIAS , ERROR , ERRMSG )
                    IF( ERROR < 0 ) THEN
                      ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                      //"when initializing Constants and Reference Parameters\"   &
                      //"Sediment Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                      CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                      RETURN
                    END IF

                    SELECT CASE( TRIM(ALIAS) )
                    CASE( "Sediment specific gravity" )
                      CALL MR_READ_PAR( REC , SS , ERROR , ERRMSG )
                      IF( ERROR < 0 ) THEN
                        ERRMSG = TRIM(ERRMSG)//" ""Sediment specific gravity"" "   &
                        //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                        //"when initializing Constants and Reference Parameters\"   &
                        //"Sediment Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                        CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                        RETURN
                      END IF
                    CASE( "Sediment porosity" )
                      CALL MR_READ_PAR( REC , PS , ERROR , ERRMSG )
                      IF( ERROR < 0 ) THEN
                        ERRMSG = TRIM(ERRMSG)//" ""Sediment porosity"" "   &
                        //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                        //"when initializing Constants and Reference Parameters\"   &
                        //"Sediment Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                        CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                        RETURN
                      END IF
                    END SELECT

                  END SELECT

                END DO

              CASE( "Physical Constants" )
                DO WHILE( .NOT. EOF(FILE_PRJ_ID) )
                  CALL MR_ACCESS_REC( FILE_PRJ_ID , REC , ERROR , ERRMSG )
                  REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID
                  IF( ERROR < 0 ) THEN
                    ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                    //"when initializing Constants and Reference Parameters\"   &
                    //"Physical Constants from file "//TRIM(FILE_PRJ_NAME)
                    CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                    RETURN
                  ELSE IF( REC == "" ) THEN
                    CYCLE
                  END IF

                  CALL MR_AWAYOUT_LABEL( REC , LABEL , ERROR , ERRMSG )
                  IF( ERROR < 0 ) THEN
                    ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                    //"when initializing Constants and Reference Parameters\"   &
                    //"Physical Constants from file "//TRIM(FILE_PRJ_NAME)
                    CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                    RETURN
                  END IF

                  SELECT CASE( TRIM(LABEL) )
                  CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "IJK" , "NKS" , "TAB" , "TAC" )
                    BACKSPACE( FILE_PRJ_ID )
                    REC_ID = REC_ID - 1
                    EXIT
                  CASE( "VAR" )
                    CALL MR_AWAYOUT_ALIAS( REC , ALIAS , ERROR , ERRMSG )
                    IF( ERROR < 0 ) THEN
                      ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                      //"when initializing Constants and Reference Parameters\"   &
                      //"Physical Constants from file "//TRIM(FILE_PRJ_NAME)
                      CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                      RETURN
                    END IF

                    SELECT CASE( TRIM(ALIAS) )
                    CASE( "Von Karman constant" )
                      CALL MR_READ_PAR( REC , KAR , ERROR , ERRMSG )
                      IF( ERROR < 0 ) THEN
                        ERRMSG = TRIM(ERRMSG)//" ""Von Karman constant"" "   &
                        //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                        //"when initializing Constants and Reference Parameters\"   &
                        //"Physical Constants from file "//TRIM(FILE_PRJ_NAME)
                        CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                        RETURN
                      END IF
                    CASE( "Coriolis frequency" )
                      CALL MR_READ_PAR( REC , COR , ERROR , ERRMSG )
                      IF( ERROR < 0 ) THEN
                        ERRMSG = TRIM(ERRMSG)//" ""Coriolis frequency"" "   &
                        //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                        //"when initializing Constants and Reference Parameters\"   &
                        //"Physical Constants from file "//TRIM(FILE_PRJ_NAME)
                        CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                        RETURN
                      END IF
                    CASE( "Gravitational acceleration" )
                      CALL MR_READ_PAR( REC , GR , ERROR , ERRMSG )
                      IF( ERROR < 0 ) THEN
                        ERRMSG = TRIM(ERRMSG)//" ""Gravitational acceleration"" "   &
                        //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                        //"when initializing Constants and Reference Parameters\"   &
                        //"Physical Constants from file "//TRIM(FILE_PRJ_NAME)
                        CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                        RETURN
                      END IF
                    END SELECT

                  END SELECT

                END DO

              CASE( "Reference Parameters" )
                DO WHILE( .NOT. EOF(FILE_PRJ_ID) )
                  CALL MR_ACCESS_REC( FILE_PRJ_ID , REC , ERROR , ERRMSG )
                  REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID
                  IF( ERROR < 0 ) THEN
                    ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                    //"when initializing Constants and Reference Parameters\"   &
                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                    CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                    RETURN
                  ELSE IF( REC == "" ) THEN
                    CYCLE
                  END IF

                  CALL MR_AWAYOUT_LABEL( REC , LABEL , ERROR , ERRMSG )
                  IF( ERROR < 0 ) THEN
                    ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                    //"when initializing Constants and Reference Parameters\"   &
                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                    CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                    RETURN
                  END IF

                  SELECT CASE( TRIM(LABEL) )
                  CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "IJK" , "NKS" , "TAB" , "TAC" )
                    BACKSPACE( FILE_PRJ_ID )
                    REC_ID = REC_ID - 1
                    EXIT
                  CASE( "VAR" )
                    CALL MR_AWAYOUT_ALIAS( REC , ALIAS , ERROR , ERRMSG )
                    IF( ERROR < 0 ) THEN
                      ERRMSG = TRIM(ERRMSG)//" no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                      //"when initializing Constants and Reference Parameters\"   &
                      //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                      CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                      RETURN
                    END IF

                    SELECT CASE( TRIM(ALIAS) )
                    CASE( "Reference horizontal dimension" )
                      CALL MR_READ_PAR( REC , XYR , ERROR , ERRMSG )
                      IF( ERROR < 0 ) THEN
                        ERRMSG = TRIM(ERRMSG)//" ""Reference horizontal dimension"" "   &
                        //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                        //"when initializing Constants and Reference Parameters\"   &
                        //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                        CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                        RETURN
                      END IF
                    CASE( "Reference vertical dimension" )
                      CALL MR_READ_PAR( REC , ZR , ERROR , ERRMSG )
                      IF( ERROR < 0 ) THEN
                        ERRMSG = TRIM(ERRMSG)//" ""Reference vertical dimension"" "   &
                        //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                        //"when initializing Constants and Reference Parameters\"   &
                        //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                        CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                        RETURN
                      END IF
                    CASE( "Reference horizontal velocity" )
                      CALL MR_READ_PAR( REC , UVR , ERROR , ERRMSG )
                      IF( ERROR < 0 ) THEN
                        ERRMSG = TRIM(ERRMSG)//" ""Reference horizontal velocity"" "   &
                        //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                        //"when initializing Constants and Reference Parameters\"   &
                        //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                        CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                        RETURN
                      END IF
                    CASE( "Reference horizontal eddy kinematic viscosity" )
                      CALL MR_READ_PAR( REC , VXYR , ERROR , ERRMSG )
                      IF( ERROR < 0 ) THEN
                        ERRMSG = TRIM(ERRMSG)//" ""Reference horizontal eddy kinematic viscosity"" "   &
                        //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                        //"when initializing Constants and Reference Parameters\"   &
                        //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                        CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                        RETURN
                      END IF
                    CASE( "Reference vertical eddy kinematic viscosity" )
                      CALL MR_READ_PAR( REC , VZR , ERROR , ERRMSG )
                      IF( ERROR < 0 ) THEN
                        ERRMSG = TRIM(ERRMSG)//" ""Reference vertical eddy kinematic viscosity"" "   &
                        //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                        //"when initializing Constants and Reference Parameters\"   &
                        //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                        CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                        RETURN
                      END IF
                    CASE( "Reference water-sediment mixture density" )
                      CALL MR_READ_PAR( REC , RR , ERROR , ERRMSG )
                      IF( ERROR < 0 ) THEN
                        ERRMSG = TRIM(ERRMSG)//" ""Reference water-sediment mixture density"" "   &
                        //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                        //"when initializing Constants and Reference Parameters\"   &
                        //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                        CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                        RETURN
                      END IF
                    END SELECT

                  END SELECT

                END DO

              END SELECT

            END SELECT

          END DO

        ! CALCULATE REFERENCE PARAMETERS
          WR = UVR / XYR * ZR
          SR = COR * UVR * XYR / GR
          TUVR = R0 * COR * UVR * ZR
          QUVR = COR * XYR * ZR
          KIR = COR * UVR * ZR
          DIR = COR * UVR * UVR
          DXYR = VXYR
          DZR = VZR

        ! CALCULATE DIMENSIONLESS COMBINATIONS
          RB = UVR / ( COR * XYR )
          RBT = UVR / ( COR * ZR )
          EKXY = VXYR / ( COR * XYR * XYR )
          EKZ = VZR / ( COR * ZR * ZR )
          SCXY = DXYR / ( COR * XYR * XYR )
          SCZ = DZR / ( COR * ZR * ZR )
          FR2 = UVR * UVR / ( GR * ZR )
          FRD2 = FR2 * R0 / ( RR - R0 )
          ALPAR = FR2 / RB
          BPAR = RB / ALPAR

        END SELECT

      END SELECT

    END DO

    CALL MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_PRJ_NAME)
      RETURN
    END IF

  ! ALLOCATE MEMORY FOR PROJECT
    CALL MR_MALLOC_GRID_SYS( NI , NJ )
    CALL MR_MALLOC_CURVED_GEOS( NI , NJ )
    CALL MR_MALLOC_FIELD_VARS( NI , NJ , NK )
    CALL MR_MALLOC_ACTIVITY( NI , NJ )

  ! NONDIMENSIONALIZE DT
    DT = DT * COR

  END SUBROUTINE MR_INIT_PRJ

  END MODULE MR_MOD_INIT_PRJ