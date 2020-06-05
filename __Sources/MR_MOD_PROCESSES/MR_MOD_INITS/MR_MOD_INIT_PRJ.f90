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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_INIT_PRJ( FILE_PRJ_NAME , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_DEFAULT

    USE MR_MOD_OPERATOR_CHAR_STRING

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

    REC_ID = 0

    CALL MR_OPEN_FILE_DEFAULT( FILE_PRJ_NAME , FILE_PRJ_ID , ERROR , ERRMSG , READONLY=.TRUE. )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_PRJ_NAME)
      RETURN
    END IF

    DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

      REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

      LABEL = "" ; ALIAS = ""

      READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
      IF( ERROR > 0 ) THEN
        ERROR = - ERROR
        ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
        //"when initializing Project from file "//TRIM(FILE_PRJ_NAME)
        CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
        RETURN
      ELSE

        SELECT CASE( .MRCHARUPPER.(TRIM(LABEL)) )
        CASE( "PRJMETADATA" )

          DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

            REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

            LABEL = "" ; ALIAS = ""

            READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
            IF( ERROR > 0 ) THEN
              ERROR = - ERROR
              ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
              //"when initializing Project from file "//TRIM(FILE_PRJ_NAME)
              CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
              RETURN
            ELSE

              SELECT CASE( .MRCHARUPPER.(TRIM(LABEL)) )
              CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "NKK" , "NKS" , "TAB" )
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

        CASE( "NKK" )
          BACKSPACE( FILE_PRJ_ID )

          READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , NK
          IF( ERROR > 0 ) THEN
            ERROR = - ERROR
            ERRMSG = "Error in reading Number of Layers from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
            //"when initializing it from file "//TRIM(FILE_PRJ_NAME)
            CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
            RETURN
          ELSE IF( NK <= 0 ) THEN
            ERROR = - 1
            ERRMSG = "Illegal value for Number of Layers from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
            //"when initializing it from file "//TRIM(FILE_PRJ_NAME)
            CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
            RETURN
          END IF

        CASE( "NKS" )
          BACKSPACE( FILE_PRJ_ID )

          READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , NKS
          IF( ERROR > 0 ) THEN
            ERROR = - ERROR
            ERRMSG = "Error in reading Number of Sediment Sizes from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
            //"when initializing it from file "//TRIM(FILE_PRJ_NAME)
            CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
            RETURN
          ELSE IF( NKS <= 0 ) THEN
            ERROR = - 1
            ERRMSG = "Illegal value for Number of Sediment Sizes from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
            //"when initializing it from file "//TRIM(FILE_PRJ_NAME)
            CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
            RETURN
          ELSE

            DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

              REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

              LABEL = "" ; ALIAS = ""

              READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
              IF( ERROR > 0 ) THEN
                ERROR = - ERROR
                ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                //"when initializing Sediment Sizes from file "//TRIM(FILE_PRJ_NAME)
                CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                RETURN
              ELSE
                SELECT CASE( .MRCHARUPPER.(TRIM(LABEL)) )
                CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "NKK" , "NKS" , "TAB" )
                  BACKSPACE( FILE_PRJ_ID )
                  REC_ID = REC_ID - 1
                  EXIT
                CASE( "DKS" )
                  BACKSPACE( FILE_PRJ_ID )

                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , D0
                  IF( ERROR > 0 ) THEN
                    ERROR = - ERROR
                    ERRMSG = "Error in reading value of Sediment Sizes from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                    //"when initializing it from file "//TRIM(FILE_PRJ_NAME)
                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                    RETURN
                  ELSE IF( D0 <= 0.0 ) THEN
                    ERROR = - 1
                    ERRMSG = "Illegal value for Sediment Sizes from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                    //"when initializing it from file "//TRIM(FILE_PRJ_NAME)
                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                    RETURN
                  END IF

                  D0 = D0 / 1000.0

                END SELECT

              END IF

            END DO

          END IF

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

            SELECT CASE( .MRCHARUPPER.(TRIM(ALIAS)) )
            CASE( "TIMING" )

              DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

                REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

                LABEL = "" ; ALIAS = ""

                READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
                IF( ERROR > 0 ) THEN
                  ERROR = - ERROR
                  ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                  //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                  CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                  RETURN
                ELSE

                  SELECT CASE( .MRCHARUPPER.(TRIM(LABEL)) )
                  CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "NKK" , "NKS" , "TAB" )
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

                      SELECT CASE( .MRCHARUPPER.(TRIM(ALIAS)) )
                      CASE( "TIME INTERVAL" )
                        BACKSPACE( FILE_PRJ_ID )

                        READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , DT
                        IF( ERROR > 0 ) THEN
                          ERROR = - ERROR
                          ERRMSG = "Error in reading Time Interval "   &
                          //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                          //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                          CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                          RETURN
                        ELSE IF( DT <= 0.0 ) THEN
                          ERROR = - 1
                          ERRMSG = "Illegal value for Time Interval "   &
                          //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                          //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                          CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                          RETURN
                        END IF

                      CASE( "TIME RELAXATION FACTOR" )
                        BACKSPACE( FILE_PRJ_ID )

                        READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , PHI
                        IF( ERROR > 0 ) THEN
                          ERROR = - ERROR
                          ERRMSG = "Error in reading Time Relaxation Factor "   &
                          //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                          //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                          CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                          RETURN
                        ELSE IF( PHI < 0.0 ) THEN
                          ERROR = - 1
                          ERRMSG = "Illegal value for Time Relaxation Factor "   &
                          //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                          //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                          CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                          RETURN
                        END IF

                      CASE( "TOTAL NUMBER OF TIMESTEPS COMPUTED" )
                        BACKSPACE( FILE_PRJ_ID )

                        READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , NTSS
                        IF( ERROR > 0 ) THEN
                          ERROR = - ERROR
                          ERRMSG = "Error in reading Total Number of Timesteps Computed "   &
                          //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                          //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                          CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                          RETURN
                        ELSE IF( NTSS < 0 ) THEN
                          ERROR = - 1
                          ERRMSG = "Illegal value for Total Number of Timesteps Computed "   &
                          //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                          //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                          CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                          RETURN
                        END IF

                      CASE( "NUMBER OF TIMESTEPS BETWEEN TWO OUTPUTS" )
                        BACKSPACE( FILE_PRJ_ID )

                        READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , NTSS_OUTPUT
                        IF( ERROR > 0 ) THEN
                          ERROR = - ERROR
                          ERRMSG = "Error in reading Number of Timesteps Between Two Outputs "   &
                          //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                          //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                          CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                          RETURN
                        ELSE IF( NTSS_OUTPUT <= 0 ) THEN
                          ERROR = - 1
                          ERRMSG = "Illegal value for Number of Timesteps Between Two Outputs "   &
                          //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                          //"when initializing Timing from file "//TRIM(FILE_PRJ_NAME)
                          CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                          RETURN
                        END IF

                      CASE( "STARTING TIME" )
                        BACKSPACE( FILE_PRJ_ID )

                        READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , T_START
                        IF( ERROR > 0 ) THEN
                          ERROR = - ERROR
                          ERRMSG = "Error in reading Starting Time "   &
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

            CASE( "SLOPE" )

              DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

                REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

                LABEL = "" ; ALIAS = ""

                READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
                IF( ERROR > 0 ) THEN
                  ERROR = - ERROR
                  ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                  //"when initializing Slope from file "//TRIM(FILE_PRJ_NAME)
                  CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                  RETURN
                ELSE

                  SELECT CASE( .MRCHARUPPER.(TRIM(LABEL)) )
                  CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "NKK" , "NKS" , "TAB" )
                    BACKSPACE( FILE_PRJ_ID )
                    REC_ID = REC_ID - 1
                    EXIT
                  CASE( "VAR" )
                    BACKSPACE( FILE_PRJ_ID )

                    READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS
                    IF( ERROR > 0 ) THEN
                      ERROR = - ERROR
                      ERRMSG = "Error in acquiring the alias of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                      //"when initializing Slope from file "//TRIM(FILE_PRJ_NAME)
                      CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                      RETURN
                    ELSE

                      SELECT CASE( .MRCHARUPPER.(TRIM(ALIAS)) )
                      CASE( "SLOPE" )
                        BACKSPACE( FILE_PRJ_ID )

                        READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , SLOPE
                        IF( ERROR > 0 ) THEN
                          ERROR = - ERROR
                          ERRMSG = "Error in reading Slope "   &
                          //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                          //"when initializing Slope from file "//TRIM(FILE_PRJ_NAME)
                          CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                          RETURN
                        END IF

                      END SELECT

                    END IF

                  END SELECT

                END IF

              END DO

            CASE( "CONSTANTS AND REFERENCE PARAMETERS" )

              DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

                REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

                LABEL = "" ; ALIAS = ""

                READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
                IF( ERROR > 0 ) THEN
                  ERROR = - ERROR
                  ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                  //"when initializing Constants and Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                  CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                  RETURN
                ELSE

                  SELECT CASE( .MRCHARUPPER.(TRIM(LABEL)) )
                  CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "NKK" , "NKS" , "TAB" )
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

                      SELECT CASE( .MRCHARUPPER.(TRIM(ALIAS)) )
                      CASE( "WATER PHYSICAL PROPERTIES" )

                        DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

                          REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

                          LABEL = "" ; ALIAS = ""

                          READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
                          IF( ERROR > 0 ) THEN
                            ERROR = - ERROR
                            ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                            //"when initializing Constants and Reference Parameters\"   &
                            //"Water Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                            CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                            RETURN
                          ELSE

                            SELECT CASE( .MRCHARUPPER.(TRIM(LABEL)) )
                            CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "NKK" , "NKS" , "TAB" , "TAC" )
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

                                SELECT CASE( .MRCHARUPPER.(TRIM(ALIAS)) )
                                CASE( "WATER DENSITY" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , R0
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading Water Density "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Water Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  ELSE IF( R0 <= 0.0 ) THEN
                                    ERROR = - 1
                                    ERRMSG = "Illegal value for Water Density "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Water Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "WATER KINEMATIC VISCOSITY" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , V0
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading Water Kinematic Viscosity "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Water Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  ELSE IF( V0 <= 0.0 ) THEN
                                    ERROR = - 1
                                    ERRMSG = "Illegal value for Water Kinematic Viscosity "   &
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

                      CASE( "SEDIMENT PHYSICAL PROPERTIES" )

                        DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

                          REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

                          LABEL = "" ; ALIAS = ""

                          READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
                          IF( ERROR > 0 ) THEN
                            ERROR = - ERROR
                            ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                            //"when initializing Constants and Reference Parameters\"   &
                            //"Sediment Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                            CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                            RETURN
                          ELSE

                            SELECT CASE( .MRCHARUPPER.(TRIM(LABEL)) )
                            CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "NKK" , "NKS" , "TAB" , "TAC" )
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

                                SELECT CASE( .MRCHARUPPER.(TRIM(ALIAS)) )
                                CASE( "SEDIMENT SPECIFIC GRAVITY" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , SS
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading Sediment Specific Gravity "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Sediment Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  ELSE IF( SS <= 1.0 ) THEN
                                    ERROR = - 1
                                    ERRMSG = "Illegal value for Sediment Specific Gravity "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Sediment Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "SEDIMENT POROSITY" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , PS
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading Sediment Porosity "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Sediment Physical Properties from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  ELSE IF( PS < 0.0 .OR. PS >= 1.0 ) THEN
                                    ERROR = - 1
                                    ERRMSG = "Illegal value for Sediment Porosity "   &
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

                      CASE( "PHYSICAL CONSTANTS" )

                        DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

                          REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

                          LABEL = "" ; ALIAS = ""

                          READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
                          IF( ERROR > 0 ) THEN
                            ERROR = - ERROR
                            ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                            //"when initializing Constants and Reference Parameters\"   &
                            //"Physical Constants from file "//TRIM(FILE_PRJ_NAME)
                            CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                            RETURN
                          ELSE

                            SELECT CASE( .MRCHARUPPER.(TRIM(LABEL)) )
                            CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "NKK" , "NKS" , "TAB" , "TAC" )
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

                                SELECT CASE( .MRCHARUPPER.(TRIM(ALIAS)) )
                                CASE( "VON KARMAN CONSTANT" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , KAR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading Von Karman Constant "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Physical Constants from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  ELSE IF( KAR <= 0.0 ) THEN
                                    ERROR = - 1
                                    ERRMSG = "Illegal value for Von Karman Constant "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Physical Constants from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "CORIOLIS FREQUENCY" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , COR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading Coriolis Frequency "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Physical Constants from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  ELSE IF( COR <= 0.0 ) THEN
                                    ERROR = - 1
                                    ERRMSG = "Illegal value for Coriolis Frequency "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Physical Constants from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "GRAVITATIONAL ACCELERATION" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , GR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading Gravitational Acceleration "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Physical Constants from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  ELSE IF( GR <= 0.0 ) THEN
                                    ERROR = - 1
                                    ERRMSG = "Illegal value for Gravitational Acceleration "   &
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

                      CASE( "REFERENCE PARAMETERS" )

                        DO WHILE( .NOT. EOF(FILE_PRJ_ID) )

                          REC_ID = REC_ID + 1 ; WRITE(REC_ID_CHAR,'(I<LEN(REC_ID_CHAR)>)') REC_ID

                          LABEL = "" ; ALIAS = ""

                          READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL
                          IF( ERROR > 0 ) THEN
                            ERROR = - ERROR
                            ERRMSG = "Error in acquiring the label of record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                            //"when initializing Constants and Reference Parameters\"   &
                            //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                            CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                            RETURN
                          ELSE

                            SELECT CASE( .MRCHARUPPER.(TRIM(LABEL)) )
                            CASE( "PRJMETADATA" , "ENDPRJMETADATA" , "NKK" , "NKS" , "TAB" , "TAC" )
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

                                SELECT CASE( .MRCHARUPPER.(TRIM(ALIAS)) )
                                CASE( "REFERENCE HORIZONTAL DIMENSION" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , XYR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading Reference Horizontal Dimension "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  ELSE IF( XYR <= 0.0 ) THEN
                                    ERROR = - 1
                                    ERRMSG = "Illegal value for Reference Horizontal Dimension "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "REFERENCE VERTICAL DIMENSION" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , ZR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading Reference Vertical Dimension "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  ELSE IF( ZR <= 0.0 ) THEN
                                    ERROR = - 1
                                    ERRMSG = "Illegal value for Reference Vertical Dimension "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "REFERENCE HORIZONTAL VELOCITY" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , UVR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading Reference Horizontal Velocity "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  ELSE IF( UVR <= 0.0 ) THEN
                                    ERROR = - 1
                                    ERRMSG = "Illegal value for Reference Horizontal Velocity "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "REFERENCE EDDY KINEMATIC VISCOSITY" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , VZR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading Reference Eddy Kinematic Viscosity "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  ELSE IF( VZR <= 0.0 ) THEN
                                    ERROR = - 1
                                    ERRMSG = "Illegal value for Reference Eddy Kinematic Viscosity "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "REFERENCE EDDY KINEMATIC DIFFUSIVITY" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , DZR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading Reference Eddy Kinematic Diffusivity "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  ELSE IF( DZR <= 0.0 ) THEN
                                    ERROR = - 1
                                    ERRMSG = "Illegal value for Reference Eddy Kinematic Diffusivity "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  END IF

                                CASE( "REFERENCE WATER-SEDIMENT MIXTURE DENSITY" )
                                  BACKSPACE( FILE_PRJ_ID )

                                  READ( FILE_PRJ_ID , * , IOSTAT=ERROR ) LABEL , ALIAS , RR
                                  IF( ERROR > 0 ) THEN
                                    ERROR = - ERROR
                                    ERRMSG = "Error in reading Reference Water-Sediment Mixture Density "   &
                                    //"from record no."//TRIM(ADJUSTL(REC_ID_CHAR))//" "   &
                                    //"when initializing Constants and Reference Parameters\"   &
                                    //"Reference Parameters from file "//TRIM(FILE_PRJ_NAME)
                                    CALL MR_CLOSE_FILE_DEFAULT( FILE_PRJ_ID , ERROR_DUMMY , ERRMSG_DUMMY )
                                    RETURN
                                  ELSE IF( RR <= 0.0 ) THEN
                                    ERROR = - 1
                                    ERRMSG = "Illegal value for Reference Water-Sediment Mixture Density "   &
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

  END SUBROUTINE MR_INIT_PRJ

  END MODULE MR_MOD_INIT_PRJ