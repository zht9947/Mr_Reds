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
  MODULE MR_MOD_WRITE_RAW_FIELD_VARS_N_ACTIVITY

    USE XMDF

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_WRITE_RAW_UV
    PUBLIC :: MR_WRITE_RAW_SS

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
  SUBROUTINE MR_WRITE_RAW_ACTIVITY( DSET_ACTIVITY_ID , NEM , NI , NJ , EMIDW , ACTIVITY , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: DSET_ACTIVITY_ID

    INTEGER(EMID_KIND) , INTENT(IN ) :: NEM
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(EMID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,EMID_KIND),1:NJ) :: EMIDW

    INTEGER(ACID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,ACID_KIND),1:NJ) :: ACTIVITY
    INTEGER(1)         , DIMENSION(1:NEM) :: ACTIVITY_ARRAY

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

   !DIR$ FORCEINLINE
    CALL MR_WRITE_RAW_ACTIVITY_PACK_FOR_ELEMS
   !END$ FORCEINLINE

    ERRMSG = ""
    CALL XF_WRITE_ACTIVITY_TIMESTEP( DSET_ACTIVITY_ID , NEM , ACTIVITY_ARRAY , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in writing activity"
      RETURN
    END IF

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
  SUBROUTINE MR_WRITE_RAW_ACTIVITY_PACK_FOR_ELEMS

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED, ALWAYS
      DO I = 1 , NI
        ACTIVITY_ARRAY( EMIDW( I , J ) ) = ACTIVITY( I , J )
      END DO
    END DO

  END SUBROUTINE MR_WRITE_RAW_ACTIVITY_PACK_FOR_ELEMS

  END SUBROUTINE MR_WRITE_RAW_ACTIVITY

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
  SUBROUTINE MR_WRITE_RAW_UV( MULTI_DSETS_ID , PATH_UV_IN_MULTI_DSETS , T ,   &
  & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO , UV , UU , VV , UVO ,   &
  & ACTIVITY , ERROR , ERRMSG , OVERWRITE )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    CHARACTER(   *   ) , INTENT(IN ) :: PATH_UV_IN_MULTI_DSETS

    REAL   (TMRD_KIND) , INTENT(IN ) :: T

    INTEGER(NDID_KIND) , INTENT(IN ) :: NND
    INTEGER(EMID_KIND) , INTENT(IN ) :: NEM
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(EMID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,EMID_KIND),1:NJ    ) :: EMIDW
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,NDID_KIND),1:NJ    ) :: NDIDW
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,NDID_KIND),1:NJ    ) :: NDIDU
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,NDID_KIND),0:NJ    ) :: NDIDV
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,NDID_KIND),0:NJ    ) :: NDIDO

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ,1:2) :: UU
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ,1:2) :: VV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ,1:2) :: UVO

    INTEGER(ACID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,ACID_KIND),1:NJ    ) , OPTIONAL :: ACTIVITY

    REAL   (4)         , DIMENSION(1:2,1:NND) :: UV_ARRAY
    REAL   (4)         , DIMENSION(    1:NND) :: UV_MOD_ARRAY

    INTEGER                          :: DSET_UV_ID

    INTEGER                          :: NTIMES

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    LOGICAL            , INTENT(IN ) , OPTIONAL :: OVERWRITE

   !DIR$ FORCEINLINE
    CALL MR_WRITE_RAW_UV_PACK_FOR_W_NODES
   !DIR$ FORCEINLINE
    CALL MR_WRITE_RAW_UV_PACK_FOR_U_NODES
   !DIR$ FORCEINLINE
    CALL MR_WRITE_RAW_UV_PACK_FOR_V_NODES
   !DIR$ FORCEINLINE
    CALL MR_WRITE_RAW_UV_PACK_FOR_O_NODES
   !END$ FORCEINLINE

    ERRMSG = ""
    CALL XF_OPEN_GROUP( MULTI_DSETS_ID , TRIM(PATH_UV_IN_MULTI_DSETS) , DSET_UV_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in openning vector dataset group"
    ELSE

      IF( PRESENT(OVERWRITE) .AND. OVERWRITE ) THEN
        CALL XF_GET_DATASET_NUM_TIMES( DSET_UV_ID , NTIMES , ERROR )
        IF( ERROR < 0 ) THEN
          ERRMSG = "Error in getting the total number of timesteps from vector dataset group"
        ELSE
          NTIMES = NTIMES - 1
          CALL XF_SET_DATASET_NUM_TIMES( DSET_UV_ID , NTIMES , ERROR )
          IF( ERROR < 0 ) THEN
            ERRMSG = "Error in droping the last timestep out of vector dataset group"
          END IF
        END IF
      END IF

      IF( ERROR < 0 ) THEN
        CONTINUE
      ELSE

        CALL XF_WRITE_VECTOR_TIMESTEP( DSET_UV_ID , REAL(T,8) , NND , 2 , UV_ARRAY , ERROR )
        IF( ERROR < 0 ) THEN
          ERRMSG = "Error in writing vector values into dataset group"
        ELSE

          CALL XF_GET_DATASET_NUM_TIMES( DSET_UV_ID , NTIMES , ERROR )
          IF( ERROR < 0 ) THEN
            ERRMSG = "Error in getting the current number of timesteps from vector dataset group"
          ELSE

            UV_MOD_ARRAY(:) = SQRT( UV_ARRAY(1,:) * UV_ARRAY(1,:) + UV_ARRAY(2,:) * UV_ARRAY(2,:) )

            CALL XF_SET_DATASET_TIMESTEP_MIN_MAX( DSET_UV_ID , NTIMES , MINVAL(UV_MOD_ARRAY) , MAXVAL(UV_MOD_ARRAY) , ERROR )
            IF( ERROR < 0 ) THEN
              ERRMSG = "Error in setting minimum and maximum vector values into dataset group"
            ELSE

              IF( PRESENT( ACTIVITY ) ) THEN
                CALL MR_WRITE_RAW_ACTIVITY( DSET_UV_ID , NEM , NI , NJ , EMIDW , ACTIVITY , ERROR , ERRMSG )
                IF( ERROR < 0 ) THEN
                  ERRMSG = TRIM(ERRMSG)//" into vector dataset group"
                END IF
              END IF

            END IF

          END IF

        END IF

      END IF

      CALL XF_CLOSE_GROUP( DSET_UV_ID , ERROR_DUMMY )
      IF( ERROR_DUMMY < 0 .AND. ERROR >= 0 ) THEN
        ERROR = ERROR_DUMMY
        ERRMSG = "Error in closing vector dataset group"
      END IF

    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(PATH_UV_IN_MULTI_DSETS)//" in multiple datasets"
      RETURN
    END IF

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
  SUBROUTINE MR_WRITE_RAW_UV_PACK_FOR_W_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 1 , NI
          UV_ARRAY(DIM, NDIDW( I , J ) ) = UV( I , J ,DIM)
        END DO
      END DO

    END DO

  END SUBROUTINE MR_WRITE_RAW_UV_PACK_FOR_W_NODES

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
  SUBROUTINE MR_WRITE_RAW_UV_PACK_FOR_U_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 0 , NI
          UV_ARRAY(DIM, NDIDU( I , J ) ) = UU( I , J ,DIM)
        END DO
      END DO

    END DO

  END SUBROUTINE MR_WRITE_RAW_UV_PACK_FOR_U_NODES

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
  SUBROUTINE MR_WRITE_RAW_UV_PACK_FOR_V_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 1 , NI
          UV_ARRAY(DIM, NDIDV( I , J ) ) = VV( I , J ,DIM)
        END DO
      END DO

    END DO

  END SUBROUTINE MR_WRITE_RAW_UV_PACK_FOR_V_NODES

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
  SUBROUTINE MR_WRITE_RAW_UV_PACK_FOR_O_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 0 , NI
          UV_ARRAY(DIM, NDIDO( I , J ) ) = UVO( I , J ,DIM)
        END DO
      END DO

    END DO

  END SUBROUTINE MR_WRITE_RAW_UV_PACK_FOR_O_NODES

  END SUBROUTINE MR_WRITE_RAW_UV

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
  SUBROUTINE MR_WRITE_RAW_SS( MULTI_DSETS_ID , PATH_SS_IN_MULTI_DSETS , T ,   &
  & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO , SS , SU , SV , SO ,   &
  & ACTIVITY , ERROR , ERRMSG , OVERWRITE )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    CHARACTER(   *   ) , INTENT(IN ) :: PATH_SS_IN_MULTI_DSETS

    REAL   (TMRD_KIND) , INTENT(IN ) :: T

    INTEGER(NDID_KIND) , INTENT(IN ) :: NND
    INTEGER(EMID_KIND) , INTENT(IN ) :: NEM
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(EMID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,EMID_KIND),1:NJ) :: EMIDW
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,NDID_KIND),1:NJ) :: NDIDW
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,NDID_KIND),1:NJ) :: NDIDU
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,NDID_KIND),0:NJ) :: NDIDV
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,NDID_KIND),0:NJ) :: NDIDO

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: SS
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: SU
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: SV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: SO

    INTEGER(ACID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,ACID_KIND),1:NJ) , OPTIONAL :: ACTIVITY

    REAL   (4)         , DIMENSION(1:NND) :: SS_ARRAY

    INTEGER                          :: DSET_SS_ID

    INTEGER                          :: NTIMES

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    LOGICAL            , INTENT(IN ) , OPTIONAL :: OVERWRITE

   !DIR$ FORCEINLINE
    CALL MR_WRITE_RAW_SS_PACK_FOR_W_NODES
   !DIR$ FORCEINLINE
    CALL MR_WRITE_RAW_SS_PACK_FOR_U_NODES
   !DIR$ FORCEINLINE
    CALL MR_WRITE_RAW_SS_PACK_FOR_V_NODES
   !DIR$ FORCEINLINE
    CALL MR_WRITE_RAW_SS_PACK_FOR_O_NODES
   !END$ FORCEINLINE

    ERRMSG = ""
    CALL XF_OPEN_GROUP( MULTI_DSETS_ID , TRIM(PATH_SS_IN_MULTI_DSETS) , DSET_SS_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in openning scalar dataset group"
    ELSE

      IF( PRESENT(OVERWRITE) .AND. OVERWRITE ) THEN
        CALL XF_GET_DATASET_NUM_TIMES( DSET_SS_ID , NTIMES , ERROR )
        IF( ERROR < 0 ) THEN
          ERRMSG = "Error in getting the total number of timesteps from scalar dataset group"
        ELSE
          NTIMES = NTIMES - 1
          CALL XF_SET_DATASET_NUM_TIMES( DSET_SS_ID , NTIMES , ERROR )
          IF( ERROR < 0 ) THEN
            ERRMSG = "Error in droping the last timestep out of scalar dataset group"
          END IF
        END IF
      END IF

      IF( ERROR < 0 ) THEN
        CONTINUE
      ELSE

        CALL XF_WRITE_SCALAR_TIMESTEP( DSET_SS_ID , REAL(T,8) , NND , SS_ARRAY , ERROR )
        IF( ERROR < 0 ) THEN
          ERRMSG = "Error in writing scalar values into dataset group"
        ELSE

          CALL XF_GET_DATASET_NUM_TIMES( DSET_SS_ID , NTIMES , ERROR )
          IF( ERROR < 0 ) THEN
            ERRMSG = "Error in getting the current number of timesteps from scalar dataset group"
          ELSE

            CALL XF_SET_DATASET_TIMESTEP_MIN_MAX( DSET_SS_ID , NTIMES , MINVAL(SS_ARRAY) , MAXVAL(SS_ARRAY) , ERROR )
            IF( ERROR < 0 ) THEN
              ERRMSG = "Error in setting minimum and maximum scalar values into dataset group"
            ELSE

              IF( PRESENT( ACTIVITY ) ) THEN
                CALL MR_WRITE_RAW_ACTIVITY( DSET_SS_ID , NEM , NI , NJ , EMIDW , ACTIVITY , ERROR , ERRMSG )
                IF( ERROR < 0 ) THEN
                  ERRMSG = TRIM(ERRMSG)//" into scalar dataset group"
                END IF
              END IF

            END IF

          END IF

        END IF

      END IF

      CALL XF_CLOSE_GROUP( DSET_SS_ID , ERROR_DUMMY )
      IF( ERROR_DUMMY < 0 .AND. ERROR >= 0 ) THEN
        ERROR = ERROR_DUMMY
        ERRMSG = "Error in closing scalar dataset group"
      END IF

    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(PATH_SS_IN_MULTI_DSETS)//" in multiple datasets"
      RETURN
    END IF

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
  SUBROUTINE MR_WRITE_RAW_SS_PACK_FOR_W_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED, ALWAYS
      DO I = 1 , NI
        SS_ARRAY( NDIDW( I , J ) ) = SS( I , J )
      END DO
    END DO

  END SUBROUTINE MR_WRITE_RAW_SS_PACK_FOR_W_NODES

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
  SUBROUTINE MR_WRITE_RAW_SS_PACK_FOR_U_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED, ALWAYS
      DO I = 0 , NI
        SS_ARRAY( NDIDU( I , J ) ) = SU( I , J )
      END DO
    END DO

  END SUBROUTINE MR_WRITE_RAW_SS_PACK_FOR_U_NODES

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
  SUBROUTINE MR_WRITE_RAW_SS_PACK_FOR_V_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    DO J = 0 , NJ
     !DIR$ VECTOR ALIGNED, ALWAYS
      DO I = 1 , NI
        SS_ARRAY( NDIDV( I , J ) ) = SV( I , J )
      END DO
    END DO

  END SUBROUTINE MR_WRITE_RAW_SS_PACK_FOR_V_NODES

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
  SUBROUTINE MR_WRITE_RAW_SS_PACK_FOR_O_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    DO J = 0 , NJ
     !DIR$ VECTOR ALIGNED, ALWAYS
      DO I = 0 , NI
        SS_ARRAY( NDIDO( I , J ) ) = SO( I , J )
      END DO
    END DO

  END SUBROUTINE MR_WRITE_RAW_SS_PACK_FOR_O_NODES

  END SUBROUTINE MR_WRITE_RAW_SS

  END MODULE MR_MOD_WRITE_RAW_FIELD_VARS_N_ACTIVITY