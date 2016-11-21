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

    USE MR_MOD_CREATE_OPEN_N_CLOSE_FILE_DEFAULT

    USE MR_MOD_FUNC_TCRS

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

    INTEGER                          :: REC_ID
    CHARACTER( 2**03 )               :: REC_ID_CHAR

    CHARACTER( 2**05 )               :: LABEL
    CHARACTER( 2**08 )               :: ALIAS

    INTEGER(KKID_KIND)               :: K

    ERRMSG = ""

    REC_ID = 0

    CALL MR_OPEN_FILE_DEFAULT( FILE_PRJ_NAME , "READ" , FILE_PRJ_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_PRJ_NAME)
      RETURN
    END IF

    DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

      REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

      READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
      IF( ERROR > 0 ) THEN
        ERROR = - ERROR
        ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
        //"when initializing Project from file "//TRIM(FILE_PRJ_NAME)
        CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
        RETURN
      ELSE

        SELECT CASE( TRIM(LABEL) )
        CASE( "PRJMETADATA" )

          DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

            REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

            READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
            IF( ERROR > 0 ) THEN
              ERROR = - ERROR
              ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
              //"when initializing Project from file "//TRIM(FILE_PRJ_NAME)
              CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
              RETURN
            ELSE

              SELECT CASE( TRIM(LABEL) )
              CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "IJK" , "NKS" , "TAB" )
                BACKSPACE( FILE_PRJ_ID )
                REC_ID = REC_ID - 1
                EXIT
              CASE( "TITLE" )
                BACKSPACE( FILE_PRJ_ID )

                READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , PRJ_TITLE
                IF( ERROR > 0 ) THEN
                  ERROR = - ERROR
                  ERRMSG = "Error in reading Project Title from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                  //"when initializing Project Metadata from file "//TRIM(FILE_PRJ_NAME)
                  CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                  RETURN
                END IF

              CASE( "ABSTRACT" )
                BACKSPACE( FILE_PRJ_ID )

                READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , PRJ_ABSTRACT
                IF( ERROR > 0 ) THEN
                  ERROR = - ERROR
                  ERRMSG = "Error in reading Project Abstract from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                  //"when initializing Project Metadata from file "//TRIM(FILE_PRJ_NAME)
                  CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                  RETURN
                END IF

              END SELECT

            END IF

          END DO

        CASE( "IJK" )
          BACKSPACE( FILE_PRJ_ID )

          READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , NI , NJ , NK
          IF( ERROR > 0 ) THEN
            ERROR = - ERROR
            ERRMSG = "Error in reading numbers of columns (NI), "   &
            //"rows (NJ) and layers (NK) from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
            //"when initializing Grid Ranks from file "//TRIM(FILE_PRJ_NAME)
            CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
            RETURN
          END IF

        ! ALLOCATE MEMORIES FOR LAYER CLASSFIED PARAMETERS
          CALL MR_MALLOC_KK_CONSTS_N_REF_PARS( NK )

        CASE( "DKS" )
          BACKSPACE( FILE_PRJ_ID )

          READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , D0
          IF( ERROR > 0 ) THEN
            ERROR = - ERROR
            ERRMSG = "Error in reading grain size from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
            //"when initializing Grain Size from file "//TRIM(FILE_PRJ_NAME)
            CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
            RETURN
          END IF

         !BLOCK
        ! CONVERT UNIT FROM MILLIMETERS TO METERS
          D0 = D0 / 1000.0
         !END BLOCK

        CASE( "TAB" )
          BACKSPACE( FILE_PRJ_ID )

          READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS
          IF( ERROR > 0 ) THEN
            ERROR = - ERROR
            ERRMSG = "Error in acquiring the alias of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
            //"when initializing Project Groups from file "//TRIM(FILE_PRJ_NAME)
            CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
            RETURN
          ELSE

            SELECT CASE( TRIM(ALIAS) )
            CASE( "Timing" )

              DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

                REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

                READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
                IF( ERROR > 0 ) THEN
                  ERROR = - ERROR
                  ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                  //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                  CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                  RETURN
                ELSE

                  SELECT CASE( TRIM(LABEL) )
                  CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "IJK" , "NKS" , "TAB" )
                    BACKSPACE( FILE_PRJ_ID )
                    REC_ID = REC_ID - 1
                    EXIT
                  CASE( "VAR" )
                    BACKSPACE( FILE_PRJ_ID )

                    READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS
                    IF( ERROR > 0 ) THEN
                      ERROR = - ERROR
                      ERRMSG = "Error in acquiring the alias of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                      //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                      CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                      RETURN
                    ELSE

                      SELECT CASE( TRIM(ALIAS) )
                      CASE( "Computational time interval" )
                        BACKSPACE( FILE_PRJ_ID )

                        READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , DT
                        IF( ERROR > 0 ) THEN
                          ERROR = - ERROR
                          ERRMSG = "Error in reading ""Computational time interval"" "   &
                          //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                          //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                          CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                          RETURN
                        END IF

                      CASE( "Starting time" )
                        BACKSPACE( FILE_PRJ_ID )

                        READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , T_START
                        IF( ERROR > 0 ) THEN
                          ERROR = - ERROR
                          ERRMSG = "Error in reading ""Starting time"" "   &
                          //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                          //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                          CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                          RETURN
                        END IF

                      CASE( "Total number of time steps for computing" )
                        BACKSPACE( FILE_PRJ_ID )

                        READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , NTSS
                        IF( ERROR > 0 ) THEN
                          ERROR = - ERROR
                          ERRMSG = "Error in reading ""Total number of time steps for computing"" "   &
                          //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                          //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                          CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                          RETURN
                        END IF

                      CASE( "Inteval of number of time steps for outputting" )
                        BACKSPACE( FILE_PRJ_ID )

                        READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , ITS_OUTPUT
                        IF( ERROR > 0 ) THEN
                          ERROR = - ERROR
                          ERRMSG = "Error in reading ""Inteval of number of time steps for outputting"" "   &
                          //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                          //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                          CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                          RETURN
                        END IF

                      CASE( "Time relaxation factor" )
                        BACKSPACE( FILE_PRJ_ID )

                        READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , PHI
                        IF( ERROR > 0 ) THEN
                          ERROR = - ERROR
                          ERRMSG = "Error in reading ""Time relaxation factor"" "   &
                          //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                          //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                          CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                          RETURN
                        END IF

                      END SELECT

                    END IF

                  END SELECT

                END IF

              END DO

            CASE( "Flow Plane Slope" )

              DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

                REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

                READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
                IF( ERROR > 0 ) THEN
                  ERROR = - ERROR
                  ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                  //"when initializing Flow Plane Slope from file "//TRIM(FILE_PRJ_NAME)
                  CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                  RETURN
                ELSE

                  SELECT CASE( TRIM(LABEL) )
                  CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "IJK" , "NKS" , "TAB" )
                    BACKSPACE( FILE_PRJ_ID )
                    REC_ID = REC_ID - 1
                    EXIT
                  CASE( "VAR" )
                    BACKSPACE( FILE_PRJ_ID )

                    READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS
                    IF( ERROR > 0 ) THEN
                      ERROR = - ERROR
                      ERRMSG = "Error in acquiring the alias of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                      //"when initializing Flow Plane Slope from file "//TRIM(FILE_PRJ_NAME)
                      CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                      RETURN
                    ELSE

                      SELECT CASE( TRIM(ALIAS) )
                      CASE( "Flow plane slope" )
                        BACKSPACE( FILE_PRJ_ID )

                        READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , SLOPE
                        IF( ERROR > 0 ) THEN
                          ERROR = - ERROR
                          ERRMSG = "Error in reading ""Flow plane slope"" "   &
                          //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                          //"when initializing Flow Plane Slope from file "//TRIM(FILE_PRJ_NAME)
                          CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                          RETURN
                        END IF

                      END SELECT

                    END IF

                  END SELECT

                END IF

              END DO

            CASE( "Constants and Reference Parameters" )

              DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

                REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

                READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
                IF( ERROR > 0 ) THEN
                  ERROR = - ERROR
                  ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                  //"when initializing Constants and Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                  CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                  RETURN
                ELSE

                  SELECT CASE( TRIM(LABEL) )
                  CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "IJK" , "NKS" , "TAB" )
                    BACKSPACE( FILE_PRJ_ID )
                    REC_ID = REC_ID - 1
                    EXIT
                  CASE( "TAC" )
                    BACKSPACE( FILE_PRJ_ID )

                    READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS
                    IF( ERROR > 0 ) THEN
                      ERROR = - ERROR
                      ERRMSG = "Error in acquiring the alias of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                      //"when initializing Constants and Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                      CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                      RETURN
                    ELSE

                      SELECT CASE( TRIM(ALIAS) )
                      CASE( "Water Physical Properties" )

                        DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

                          REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

                          READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
                          IF( ERROR > 0 ) THEN
                            ERROR = - ERROR
                            ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                            //"when initializing Constants and Reference Parameters\"   &
                            //"Water Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                            CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                            RETURN
                          ELSE

                            SELECT CASE( TRIM(LABEL) )
                            CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "IJK" , "NKS" , "TAB" , "TAC" )
                              BACKSPACE( FILE_PRJ_ID )
                              REC_ID = REC_ID - 1
                              EXIT
                            CASE( "VAR" )
                              BACKSPACE( FILE_PRJ_ID )

                              READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS
                              IF( ERROR > 0 ) THEN
                                ERROR = - ERROR
                                ERRMSG = "Error in acquiring the alias of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                //"when initializing Constants and Reference Parameters\"   &
                                //"Water Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                                CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                RETURN
                              ELSE

                                SELECT CASE( TRIM(ALIAS) )
                                CASE( "Water density" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , R0
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading ""Water density"" "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Water Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "Water kinematic viscosity" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , V0
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading ""Water kinematic viscosity"" "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Water Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                END SELECT

                              END IF

                            END SELECT

                          END IF

                        END DO

                      CASE( "Sediment Physical Properties" )

                        DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

                          REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

                          READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
                          IF( ERROR > 0 ) THEN
                            ERROR = - ERROR
                            ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                            //"when initializing Constants and Reference Parameters\"   &
                            //"Sediment Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                            CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                            RETURN
                          ELSE

                            SELECT CASE( TRIM(LABEL) )
                            CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "IJK" , "NKS" , "TAB" , "TAC" )
                              BACKSPACE( FILE_PRJ_ID )
                              REC_ID = REC_ID - 1
                              EXIT
                            CASE( "VAR" )
                              BACKSPACE( FILE_PRJ_ID )

                              READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS
                              IF( ERROR > 0 ) THEN
                                ERROR = - ERROR
                                ERRMSG = "Error in acquiring the alias of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                //"when initializing Constants and Reference Parameters\"   &
                                //"Sediment Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                                CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                RETURN
                              ELSE

                                SELECT CASE( TRIM(ALIAS) )
                                CASE( "Sediment specific gravity" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , SS
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading ""Sediment specific gravity"" "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Sediment Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "Sediment porosity" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , PS
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading ""Sediment porosity"" "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Sediment Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                END SELECT

                              END IF

                            END SELECT

                          END IF

                        END DO

                      CASE( "Physical Constants" )

                        DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

                          REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

                          READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
                          IF( ERROR > 0 ) THEN
                            ERROR = - ERROR
                            ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                            //"when initializing Constants and Reference Parameters\"   &
                            //"Physical Constants from file "//TRIM(FILE_PRJ_NAME)
                            CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                            RETURN
                          ELSE

                            SELECT CASE( TRIM(LABEL) )
                            CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "IJK" , "NKS" , "TAB" , "TAC" )
                              BACKSPACE( FILE_PRJ_ID )
                              REC_ID = REC_ID - 1
                              EXIT
                            CASE( "VAR" )
                              BACKSPACE( FILE_PRJ_ID )

                              READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS
                              IF( ERROR > 0 ) THEN
                                ERROR = - ERROR
                                ERRMSG = "Error in acquiring the alias of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                //"when initializing Constants and Reference Parameters\"   &
                                //"Physical Constants from file "//TRIM(FILE_PRJ_NAME)
                                CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                RETURN
                              ELSE

                                SELECT CASE( TRIM(ALIAS) )
                                CASE( "Von Karman constant" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , KAR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading ""Von Karman constant"" "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Physical Constants from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "Coriolis frequency" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , COR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading ""Coriolis frequency"" "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Physical Constants from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "Gravitational acceleration" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , GR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading ""Gravitational acceleration"" "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Physical Constants from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                END SELECT

                              END IF

                            END SELECT

                          END IF

                        END DO

                      CASE( "Reference Parameters" )

                        DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

                          REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

                          READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
                          IF( ERROR > 0 ) THEN
                            ERROR = - ERROR
                            ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                            //"when initializing Constants and Reference Parameters\"   &
                            //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                            CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                            RETURN
                          ELSE

                            SELECT CASE( TRIM(LABEL) )
                            CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "IJK" , "NKS" , "TAB" , "TAC" )
                              BACKSPACE( FILE_PRJ_ID )
                              REC_ID = REC_ID - 1
                              EXIT
                            CASE( "VAR" )
                              BACKSPACE( FILE_PRJ_ID )

                              READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS
                              IF( ERROR > 0 ) THEN
                                ERROR = - ERROR
                                ERRMSG = "Error in acquiring the alias of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                //"when initializing Constants and Reference Parameters\"   &
                                //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                RETURN
                              ELSE

                                SELECT CASE( TRIM(ALIAS) )
                                CASE( "Reference horizontal dimension" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , XYR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading ""Reference horizontal dimension"" "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "Reference vertical dimension" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , ZR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading ""Reference vertical dimension"" "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "Reference horizontal velocity" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , UVR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading ""Reference horizontal velocity"" "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "Reference horizontal eddy kinematic viscosity" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , VXYR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading ""Reference horizontal eddy kinematic viscosity"" "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "Reference vertical eddy kinematic viscosity" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , VZR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading ""Reference vertical eddy kinematic viscosity"" "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "Reference water-sediment mixture density" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , RR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading ""Reference water-sediment mixture density"" "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                END SELECT

                              END IF

                            END SELECT

                          END IF

                        END DO

                      END SELECT

                    END IF

                  END SELECT

                END IF

              END DO

            END SELECT

          END IF

        END SELECT

      END IF

    END DO

    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_PRJ_NAME)
      RETURN
    END IF

  ! ALLOCATE MEMORIES FOR PROJECT
    CALL MR_MALLOC_GRID_SYS( NI , NJ )
    CALL MR_MALLOC_CURVED_GEOS( NI , NJ )
    CALL MR_MALLOC_FIELD_VARS( NI , NJ , NK )
    CALL MR_MALLOC_ACTIVITY( NI , NJ )

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
    RE = UVR * ZR / V0
    RET = RE / SQRT( RBT )
    FR2 = UVR * UVR / ( GR * ZR )
    FRD2 = FR2 * R0 / ( RR - R0 )
    ALPAR = FR2 / RB
    BPAR = RB / ALPAR

   !BLOCK
  ! CALCULATE DSIGMA
    DSIGMA = 1.0 / NK
  ! CALCULATE SIGMA COORDINATES
    SIGMA(NK) = - 0.5 * DSIGMA
    DO K = NK-1 , 1 , -1
      SIGMA( K ) = SIGMA(K+1) - DSIGMA
    END DO
   !END BLOCK

   !BLOCK
  ! CALCULATE DS, TCRS, WS & RBS
    DS = D0 * ( GR * (SS-1.0) / (V0*V0) )**(1.0/3.0)
    TCRS = MR_FUNC_TCRS( D0 , DS )
   !END BLOCK

  ! NONDIMENSIONALIZE DT
    DT = DT * COR

  END SUBROUTINE MR_INIT_PRJ

  END MODULE MR_MOD_INIT_PRJ