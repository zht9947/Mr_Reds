#INCLUDE 'MR_H_ALIGN_PADDING.H'
!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_RW_EXTEND_FIELD_VARS_N_ACTIVITY

    USE XMDF

    USE MR_DEF_GRID_SYS
    USE MR_DEF_GRID_SYS_EXTEND

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_RW_EXTEND_UV
    PUBLIC :: MR_RW_EXTEND_SS

!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_RW_EXTEND_UV( MULTI_DSETS_ID , MULTI_DSETS_ID_EXTEND , ITS , T ,   &
  & NND , NND2 , NEM , NEM2 , NLOOPS , NI , NJ , ERROR , ERRMSG )

    USE MR_MOD_READ2_FIELD_VARS_N_ACTIVITY
    USE MR_MOD_WRITE2_FIELD_VARS_N_ACTIVITY
    USE MR_MOD_GET_DSET_UNIT
    USE MR_MOD_CREATE_DSET

    USE MR_MOD_EXTEND

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID
    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID_EXTEND

    INTEGER(TSID_KIND) , INTENT(IN ) :: ITS

    REAL   (TMRD_KIND) , INTENT(IN ) :: T

    INTEGER(NDID_KIND) , INTENT(IN ) :: NND , NND2
    INTEGER(EMID_KIND) , INTENT(IN ) :: NEM , NEM2
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER            , INTENT(IN ) :: NLOOPS

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ,1:2) :: UU
    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ,1:2) :: VV
    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ,1:2) :: UVO
    INTEGER(ACID_KIND) , DIMENSION(1:NI1(NI,ACID_KIND),1:NJ    ) :: ACTIVITY

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI*NLOOPS,FDRD_KIND),1:NJ,1:2) :: EXTEND_UV
    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI*NLOOPS,FDRD_KIND),1:NJ,1:2) :: EXTEND_UU
    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI*NLOOPS,FDRD_KIND),0:NJ,1:2) :: EXTEND_VV
    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI*NLOOPS,FDRD_KIND),0:NJ,1:2) :: EXTEND_UVO
    INTEGER(ACID_KIND) , DIMENSION(1:NI1(NI*NLOOPS,ACID_KIND),1:NJ    ) :: EXTEND_ACTIVITY

    CHARACTER          , DIMENSION(:) , ALLOCATABLE :: PATH_UV_CHAR_ARRAY
    CHARACTER( 2**08 )               :: PATH_UV
    CHARACTER( 2**04 )               :: UNIT_UV

    INTEGER                          :: N_UV_DSETS , N_MAX_PATH_UV_LENGTH

    INTEGER                          :: IUV_DSET , ICR

    INTEGER                          :: DSET_UV_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    LOGICAL            , SAVE        :: FIRST_CALL = .TRUE.

    ERRMSG = ""
    CALL XF_GET_VECTOR_DATASETS_INFO( MULTI_DSETS_ID , N_UV_DSETS , N_MAX_PATH_UV_LENGTH , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in getting vector datasets information from the source file between"
    ELSE
      ALLOCATE( PATH_UV_CHAR_ARRAY( N_UV_DSETS * N_MAX_PATH_UV_LENGTH ) )
        CALL XF_GET_VECTOR_DATASET_PATHS( MULTI_DSETS_ID , N_UV_DSETS , N_MAX_PATH_UV_LENGTH , PATH_UV_CHAR_ARRAY , ERROR )
        IF( ERROR < 0 ) THEN
          ERRMSG = "Error in getting each vector dataset's path in multiple datasets from the source file between"
        ELSE
          DO IUV_DSET = 1 , N_UV_DSETS
            DO ICR = 1 , N_MAX_PATH_UV_LENGTH-1
              PATH_UV(ICR:ICR) = PATH_UV_CHAR_ARRAY( (IUV_DSET-1)*N_MAX_PATH_UV_LENGTH + ICR )
            END DO

          ! READ FROM SOURCE
            CALL MR_READ2_UV( MULTI_DSETS_ID , PATH_UV(1:N_MAX_PATH_UV_LENGTH-1) , ITS ,   &
            & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
            & UV , UU , VV , UVO ,   &
            & ACTIVITY , ERROR , ERRMSG )
            IF( ERROR < 0 ) THEN
              ERRMSG = TRIM(ERRMSG)//" in the source file between"
              RETURN
            END IF

          ! EXTEND
            CALL MR_EXTEND_UV( NLOOPS , NI , NJ ,   &
            & UV , UU , VV , UVO , ACTIVITY ,   &
            & EXTEND_UV , EXTEND_UU , EXTEND_VV , EXTEND_UVO , EXTEND_ACTIVITY )

          ! CREATE CATALOG IN THE TARGET
            IF( FIRST_CALL ) THEN
              CALL MR_GET_DSET_UNIT( MULTI_DSETS_ID , PATH_UV(1:N_MAX_PATH_UV_LENGTH-1) , UNIT_UV , ERROR , ERRMSG )
              IF( ERROR < 0 ) THEN
                ERRMSG = TRIM(ERRMSG)//" in the source file between"
                RETURN
              END IF
              CALL MR_CREATE_DSET_UV( MULTI_DSETS_ID_EXTEND , PATH_UV(1:N_MAX_PATH_UV_LENGTH-1) , UNIT_UV , ERROR , ERRMSG )
              IF( ERROR < 0 ) THEN
                ERRMSG = TRIM(ERRMSG)//" in the target file between"
                RETURN
              END IF
            END IF

          ! WRITE TO THE TARGET
            CALL MR_WRITE2_UV( MULTI_DSETS_ID_EXTEND , PATH_UV(1:N_MAX_PATH_UV_LENGTH-1) , T ,   &
            & NND2 , NEM2 , NI*NLOOPS , NJ , EMIDW2 , NDIDW2 , NDIDU2 , NDIDV2 , NDIDO2 ,   &
            & EXTEND_UV , EXTEND_UU , EXTEND_VV , EXTEND_UVO ,   &
            & EXTEND_ACTIVITY , ERROR , ERRMSG )
            IF( ERROR < 0 ) THEN
              ERRMSG = TRIM(ERRMSG)//" in the target file between"
            END IF

          END DO

          FIRST_CALL = .FALSE.

        END IF

      DEALLOCATE( PATH_UV_CHAR_ARRAY )

    END IF

  END SUBROUTINE MR_RW_EXTEND_UV

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_RW_EXTEND_SS( MULTI_DSETS_ID , MULTI_DSETS_ID_EXTEND , ITS , T ,   &
  & NND , NND2 , NEM , NEM2 , NLOOPS , NI , NJ , ERROR , ERRMSG )

    USE MR_MOD_READ2_FIELD_VARS_N_ACTIVITY
    USE MR_MOD_WRITE2_FIELD_VARS_N_ACTIVITY
    USE MR_MOD_GET_DSET_UNIT
    USE MR_MOD_CREATE_DSET

    USE MR_MOD_EXTEND

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID
    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID_EXTEND

    INTEGER(TSID_KIND) , INTENT(IN ) :: ITS

    REAL   (TMRD_KIND) , INTENT(IN ) :: T

    INTEGER(NDID_KIND) , INTENT(IN ) :: NND , NND2
    INTEGER(EMID_KIND) , INTENT(IN ) :: NEM , NEM2
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER            , INTENT(IN ) :: NLOOPS

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: SS
    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: SU
    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: SV
    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: SO
    INTEGER(ACID_KIND) , DIMENSION(1:NI1(NI,ACID_KIND),1:NJ) :: ACTIVITY

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI*NLOOPS,FDRD_KIND),1:NJ) :: EXTEND_SS
    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI*NLOOPS,FDRD_KIND),1:NJ) :: EXTEND_SU
    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI*NLOOPS,FDRD_KIND),0:NJ) :: EXTEND_SV
    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI*NLOOPS,FDRD_KIND),0:NJ) :: EXTEND_SO
    INTEGER(ACID_KIND) , DIMENSION(1:NI1(NI*NLOOPS,ACID_KIND),1:NJ) :: EXTEND_ACTIVITY

    CHARACTER          , DIMENSION(:) , ALLOCATABLE :: PATH_SS_CHAR_ARRAY
    CHARACTER( 2**08 )               :: PATH_SS
    CHARACTER( 2**04 )               :: UNIT_SS

    INTEGER                          :: N_SS_DSETS , N_MAX_PATH_SS_LENGTH

    INTEGER                          :: ISS_DSET , ICR

    INTEGER                          :: DSET_SS_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    LOGICAL            , SAVE        :: FIRST_CALL = .TRUE.

    ERRMSG = ""
    CALL XF_GET_SCALAR_DATASETS_INFO( MULTI_DSETS_ID , N_SS_DSETS , N_MAX_PATH_SS_LENGTH , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in getting vector datasets information from the source file between"
    ELSE
      ALLOCATE( PATH_SS_CHAR_ARRAY( N_SS_DSETS * N_MAX_PATH_SS_LENGTH ) )
        CALL XF_GET_SCALAR_DATASET_PATHS( MULTI_DSETS_ID , N_SS_DSETS , N_MAX_PATH_SS_LENGTH , PATH_SS_CHAR_ARRAY , ERROR )
        IF( ERROR < 0 ) THEN
          ERRMSG = "Error in getting each scalar dataset's path in multiple datasets from the source file between"
        ELSE
          DO ISS_DSET = 1 , N_SS_DSETS
            DO ICR = 1 , N_MAX_PATH_SS_LENGTH-1
              PATH_SS(ICR:ICR) = PATH_SS_CHAR_ARRAY( (ISS_DSET-1)*N_MAX_PATH_SS_LENGTH + ICR )
            END DO

          ! READ FROM SOURCE
            CALL MR_READ2_SS( MULTI_DSETS_ID , PATH_SS(1:N_MAX_PATH_SS_LENGTH-1) , ITS ,   &
            & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
            & SS , SU, SV , SO ,   &
            & ACTIVITY , ERROR , ERRMSG )
            IF( ERROR < 0 ) THEN
              ERRMSG = TRIM(ERRMSG)//" in the source file between"
              RETURN
            END IF

          ! EXTEND
            CALL MR_EXTEND_SS( NLOOPS , NI , NJ ,   &
            & SS , SU , SV , SO , ACTIVITY ,   &
            & EXTEND_SS , EXTEND_SU , EXTEND_SV , EXTEND_SO , EXTEND_ACTIVITY )

          ! CREATE CATALOG IN THE TARGET
            IF( FIRST_CALL ) THEN
              CALL MR_GET_DSET_UNIT( MULTI_DSETS_ID , PATH_SS(1:N_MAX_PATH_SS_LENGTH-1) , UNIT_SS , ERROR , ERRMSG )
              IF( ERROR < 0 ) THEN
                ERRMSG = TRIM(ERRMSG)//" in the source file between"
                RETURN
              END IF
              CALL MR_CREATE_DSET_SS( MULTI_DSETS_ID_EXTEND , PATH_SS(1:N_MAX_PATH_SS_LENGTH-1) , UNIT_SS , ERROR , ERRMSG )
              IF( ERROR < 0 ) THEN
                ERRMSG = TRIM(ERRMSG)//" in the target file between"
                RETURN
              END IF
            END IF

          ! WRITE TO THE TARGET
            CALL MR_WRITE2_SS( MULTI_DSETS_ID_EXTEND , PATH_SS(1:N_MAX_PATH_SS_LENGTH-1) , T ,   &
            & NND2 , NEM2 , NI*NLOOPS , NJ , EMIDW2 , NDIDW2 , NDIDU2 , NDIDV2 , NDIDO2 ,   &
            & EXTEND_SS , EXTEND_SU , EXTEND_SV , EXTEND_SO ,   &
            & EXTEND_ACTIVITY , ERROR , ERRMSG )
            IF( ERROR < 0 ) THEN
              ERRMSG = TRIM(ERRMSG)//" in the target file between"
            END IF

          END DO

          FIRST_CALL = .FALSE.

        END IF

      DEALLOCATE( PATH_SS_CHAR_ARRAY )

    END IF

  END SUBROUTINE MR_RW_EXTEND_SS

  END MODULE MR_MOD_RW_EXTEND_FIELD_VARS_N_ACTIVITY