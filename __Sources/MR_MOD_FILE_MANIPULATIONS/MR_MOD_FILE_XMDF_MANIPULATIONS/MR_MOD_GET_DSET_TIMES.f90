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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_GET_DSET_TIMES

    USE XMDF

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_GET_DSET_NTSS
    PUBLIC :: MR_GET_DSET_T_ITS

    PUBLIC :: MR_GET_DSET_NTSS_N_T_NTSS

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_GET_DSET_NTSS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , NTSS , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    CHARACTER(   *   ) , INTENT(IN ) :: PATH_DSET_IN_MULTI_DSETS

    INTEGER                          :: DSET_ID

    INTEGER                          :: NTIMES
    INTEGER(TSID_KIND) , INTENT(OUT) :: NTSS

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    ERRMSG = ""
    CALL XF_OPEN_GROUP( MULTI_DSETS_ID , TRIM(PATH_DSET_IN_MULTI_DSETS) , DSET_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in openning dataset group"
    ELSE

      CALL XF_GET_DATASET_NUM_TIMES( DSET_ID , NTIMES , ERROR )
      IF( ERROR < 0 ) THEN
        ERRMSG = "Error in getting the number of timesteps from dataset group"
      ELSE

        NTSS = NTIMES - 1

      END IF

      CALL XF_CLOSE_GROUP( DSET_ID , ERROR_DUMMY )
      IF( ERROR_DUMMY < 0 .AND. ERROR >= 0 ) THEN
        ERROR = ERROR_DUMMY
        ERRMSG = "Error in closing dataset group"
      END IF

    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(PATH_DSET_IN_MULTI_DSETS)//" in multiple datasets"
      RETURN
    END IF

  END SUBROUTINE MR_GET_DSET_NTSS

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_GET_DSET_T_ITS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , NTSS , ITS , T_ITS , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    CHARACTER(   *   ) , INTENT(IN ) :: PATH_DSET_IN_MULTI_DSETS

    INTEGER                          :: DSET_ID

    INTEGER(TSID_KIND) , INTENT(IN ) :: NTSS
    INTEGER(TSID_KIND) , INTENT(IN ) :: ITS

    REAL   (8)         , ALLOCATABLE , DIMENSION(:) :: T_ARRAY

    REAL   (TMRD_KIND) , INTENT(OUT) :: T_ITS

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    ERRMSG = ""
    CALL XF_OPEN_GROUP( MULTI_DSETS_ID , TRIM(PATH_DSET_IN_MULTI_DSETS) , DSET_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in openning dataset group"
    ELSE

      ALLOCATE( T_ARRAY(0:NTSS) )
        CALL XF_GET_DATASET_TIMES( DSET_ID , SIZE(T_ARRAY) , T_ARRAY , ERROR )
        IF( ERROR < 0 ) THEN
          ERRMSG = "Error in getting time's array from dataset group"
        ELSE

          T_ITS = T_ARRAY( ITS )

        END IF

      DEALLOCATE( T_ARRAY )

      CALL XF_CLOSE_GROUP( DSET_ID , ERROR_DUMMY )
      IF( ERROR_DUMMY < 0 .AND. ERROR >= 0 ) THEN
        ERROR = ERROR_DUMMY
        ERRMSG = "Error in closing dataset group"
      END IF

    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(PATH_DSET_IN_MULTI_DSETS)//" in multiple datasets"
      RETURN
    END IF

  END SUBROUTINE MR_GET_DSET_T_ITS

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_GET_DSET_NTSS_N_T_NTSS( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , NTSS , T_NTSS , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    CHARACTER(   *   ) , INTENT(IN ) :: PATH_DSET_IN_MULTI_DSETS

    INTEGER                          :: DSET_ID

    INTEGER                          :: NTIMES
    INTEGER(TSID_KIND) , INTENT(OUT) :: NTSS

    REAL   (8)         , ALLOCATABLE , DIMENSION(:) :: T_ARRAY

    REAL   (TMRD_KIND) , INTENT(OUT) :: T_NTSS

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    ERRMSG = ""
    CALL XF_OPEN_GROUP( MULTI_DSETS_ID , TRIM(PATH_DSET_IN_MULTI_DSETS) , DSET_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in openning dataset group"
    ELSE

      CALL XF_GET_DATASET_NUM_TIMES( DSET_ID , NTIMES , ERROR )
      IF( ERROR < 0 ) THEN
        ERRMSG = "Error in getting the number of timesteps from dataset group"
      ELSE

        NTSS = NTIMES - 1

        ALLOCATE( T_ARRAY(0:NTSS) )
          CALL XF_GET_DATASET_TIMES( DSET_ID , SIZE(T_ARRAY) , T_ARRAY , ERROR )
          IF( ERROR < 0 ) THEN
            ERRMSG = "Error in getting time's array from dataset group"
          ELSE

            T_NTSS = T_ARRAY( NTSS )

          END IF

        DEALLOCATE( T_ARRAY )

      END IF

      CALL XF_CLOSE_GROUP( DSET_ID , ERROR_DUMMY )
      IF( ERROR_DUMMY < 0 .AND. ERROR >= 0 ) THEN
        ERROR = ERROR_DUMMY
        ERRMSG = "Error in closing dataset group"
      END IF

    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(PATH_DSET_IN_MULTI_DSETS)//" in multiple datasets"
      RETURN
    END IF

  END SUBROUTINE MR_GET_DSET_NTSS_N_T_NTSS

  END MODULE MR_MOD_GET_DSET_TIMES