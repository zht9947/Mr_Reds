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
  MODULE MR_MOD_WRITE_FIELD_VARS_N_ACTIVITY

    USE XMDF

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_WRITE_UV
    PUBLIC :: MR_WRITE_SS

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
  SUBROUTINE MR_WRITE_ACTIVITY( DSET_ACTIVITY_ID , NEM , NI , NJ , EMIDW , ACTIVITY , ERROR , ERRMSG )

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
    CALL MR_WRITE_ACTIVITY_PACK_FOR_ELEMS
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
  SUBROUTINE MR_WRITE_ACTIVITY_PACK_FOR_ELEMS

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED, ALWAYS
      DO I = 1 , NI
        ACTIVITY_ARRAY( EMIDW( I , J ) ) = ACTIVITY( I , J )
      END DO
    END DO

  END SUBROUTINE MR_WRITE_ACTIVITY_PACK_FOR_ELEMS

  END SUBROUTINE MR_WRITE_ACTIVITY

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
  SUBROUTINE MR_WRITE_UV( MULTI_DSETS_ID , PATH_UV_IN_MULTI_DSETS , T ,   &
  & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO , JUV , JUU , JVV , JOO ,   &
  & UV_BASE , UV_REF , UV , U , UU , V , VV ,   &
  & ACTIVITY , ERROR , ERRMSG , OVERWRITE )

    USE MR_MOD_OPERATOR_UV
    USE MR_MOD_INTERP_XY

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    CHARACTER(   *   ) , INTENT(IN ) :: PATH_UV_IN_MULTI_DSETS

    REAL   (TMRD_KIND) , INTENT(IN ) :: T

    INTEGER(NDID_KIND) , INTENT(IN ) :: NND
    INTEGER(EMID_KIND) , INTENT(IN ) :: NEM
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(EMID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,EMID_KIND),1:NJ        ) :: EMIDW

    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,NDID_KIND),1:NJ        ) :: NDIDW
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,NDID_KIND),1:NJ        ) :: NDIDU
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,NDID_KIND),0:NJ        ) :: NDIDV
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,NDID_KIND),0:NJ        ) :: NDIDO

    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,GJRD_KIND),1:NJ,1:2,1:2) :: JUV
    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,GJRD_KIND),1:NJ,1:2,1:2) :: JUU
    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,GJRD_KIND),0:NJ,1:2,1:2) :: JVV
    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,GJRD_KIND),0:NJ,1:2,1:2) :: JOO

    REAL   (PARD_KIND) , INTENT(IN ) :: UV_BASE
    REAL   (PARD_KIND) , INTENT(IN ) :: UV_REF

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2    ) , OPTIONAL :: UV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ        ) , OPTIONAL :: U
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ,1:2    ) , OPTIONAL :: UU
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ        ) , OPTIONAL :: V
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ,1:2    ) , OPTIONAL :: VV

    INTEGER(ACID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,ACID_KIND),1:NJ        ) , OPTIONAL :: ACTIVITY

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION( : , : ) :: U_AT_U
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION( : , : ) :: V_AT_V

    REAL   (4)         , DIMENSION(1:2,1:NND) :: UV_ARRAY
    REAL   (4)         , DIMENSION(    1:NND) :: UV_MOD_ARRAY

    INTEGER                          :: DSET_UV_ID

    INTEGER                          :: NTIMES

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    LOGICAL            , INTENT(IN ) , OPTIONAL :: OVERWRITE

   !DIR$ FORCEINLINE
    CALL MR_WRITE_UV_PACK_FOR_W_NODES
   !DIR$ FORCEINLINE
    CALL MR_WRITE_UV_PACK_FOR_U_NODES
   !DIR$ FORCEINLINE
    CALL MR_WRITE_UV_PACK_FOR_V_NODES
   !DIR$ FORCEINLINE
    CALL MR_WRITE_UV_PACK_FOR_O_NODES
   !END$ FORCEINLINE

    UV_ARRAY = UV_ARRAY * ( UV_REF - UV_BASE ) + UV_BASE

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
                CALL MR_WRITE_ACTIVITY( DSET_UV_ID , NEM , NI , NJ , EMIDW , ACTIVITY , ERROR , ERRMSG )
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
  SUBROUTINE MR_WRITE_UV_PACK_FOR_W_NODES

    IMPLICIT NONE

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV_AT_W

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    IF( PRESENT( UV ) ) THEN

      DO DIM = 1 , 2

        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            UV_AT_W( I , J ,DIM) = UV( I , J ,DIM)
          END DO
        END DO

      END DO

    ELSE

      DO DIM = 1 , 2

        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            UV_AT_W( I , J ,DIM) = 0.0
          END DO
        END DO

      END DO

    END IF

    UV_AT_W = JUV .MRUVTFM. UV_AT_W

    DO DIM = 1 , 2

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 1 , NI
          UV_ARRAY(DIM, NDIDW( I , J ) ) = UV_AT_W( I , J ,DIM)
        END DO
      END DO

    END DO

  END SUBROUTINE MR_WRITE_UV_PACK_FOR_W_NODES

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
  SUBROUTINE MR_WRITE_UV_PACK_FOR_U_NODES

    IMPLICIT NONE

    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ,1:2) :: UV_AT_U

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    IF( PRESENT( U ) ) THEN
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          UV_AT_U( I , J ,1) = U( I , J )
        END DO
      END DO
    ELSE IF( PRESENT( UU ) ) THEN
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          UV_AT_U( I , J ,1) = UU( I , J ,1)
        END DO
      END DO
    ELSE IF( PRESENT( UV ) ) THEN
      CALL MR_INTERP_XY_UV_U_BY_LINEAR( NI , NJ , UV , UV_AT_U(:,:,1) )
      ALLOCATE( U_AT_U(0:NI0(NI,FDRD_KIND),1:NJ) )
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          U_AT_U( I , J ) = UV_AT_U( I , J ,1)
        END DO
      END DO
    ELSE
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          UV_AT_U( I , J ,1) = 0.0
        END DO
      END DO
    END IF

    IF( PRESENT( UU ) ) THEN
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          UV_AT_U( I , J ,2) = UU( I , J ,2)
        END DO
      END DO
    ELSE IF( PRESENT( UV ) ) THEN
      CALL MR_INTERP_XY_UV_U_TO_GET_V_AT_U( NI , NJ , UV , UV_AT_U(:,:,2) )
    ELSE
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          UV_AT_U( I , J ,2) = 0.0
        END DO
      END DO
    END IF

    UV_AT_U = JUU .MRUVTFM. UV_AT_U

    DO DIM = 1 , 2

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 0 , NI
          UV_ARRAY(DIM, NDIDU( I , J ) ) = UV_AT_U( I , J ,DIM)
        END DO
      END DO

    END DO

  END SUBROUTINE MR_WRITE_UV_PACK_FOR_U_NODES

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
  SUBROUTINE MR_WRITE_UV_PACK_FOR_V_NODES

    IMPLICIT NONE

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ,1:2) :: UV_AT_V

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    IF( PRESENT( VV ) ) THEN
      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          UV_AT_V( I , J ,1) = VV( I , J ,1)
        END DO
      END DO
    ELSE IF( PRESENT( UV ) ) THEN
      CALL MR_INTERP_XY_UV_V_TO_GET_U_AT_V( NI , NJ , UV , UV_AT_V(:,:,1) )
    ELSE
      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          UV_AT_V( I , J ,1) = 0.0
        END DO
      END DO
    END IF

    IF( PRESENT( V ) ) THEN
      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          UV_AT_V( I , J ,2) = V( I , J )
        END DO
      END DO
    ELSE IF( PRESENT( VV ) ) THEN
      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          UV_AT_V( I , J ,2) = VV( I , J ,2)
        END DO
      END DO
    ELSE IF( PRESENT( UV ) ) THEN
      CALL MR_INTERP_XY_UV_V_BY_LINEAR( NI , NJ , UV , UV_AT_V(:,:,2) )
      ALLOCATE( V_AT_V(1:NI1(NI,FDRD_KIND),0:NJ) )
      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          V_AT_V( I , J ) = UV_AT_V( I , J ,2)
        END DO
      END DO
    ELSE
      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          UV_AT_V( I , J ,2) = 0.0
        END DO
      END DO
    END IF

    UV_AT_V = JVV .MRUVTFM. UV_AT_V

    DO DIM = 1 , 2

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 1 , NI
          UV_ARRAY(DIM, NDIDV( I , J ) ) = UV_AT_V( I , J ,DIM)
        END DO
      END DO

    END DO

  END SUBROUTINE MR_WRITE_UV_PACK_FOR_V_NODES

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
  SUBROUTINE MR_WRITE_UV_PACK_FOR_O_NODES

    IMPLICIT NONE

    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ,1:2) :: UV_AT_O

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    IF( PRESENT( U ) ) THEN
      CALL MR_INTERP_XY_UV_O_U( NI , NJ , U , UV_AT_O(:,:,1) )
    ELSE IF( PRESENT( UU ) ) THEN
      CALL MR_INTERP_XY_UV_O_U( NI , NJ , UU(:,:,1) , UV_AT_O(:,:,1) )
    ELSE IF( ALLOCATED( U_AT_U ) ) THEN
      CALL MR_INTERP_XY_UV_O_U( NI , NJ , U_AT_U , UV_AT_O(:,:,1) )
      DEALLOCATE( U_AT_U )
    ELSE
      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          UV_AT_O( I , J ,1) = 0.0
        END DO
      END DO
    END IF

    IF( PRESENT( V ) ) THEN
      CALL MR_INTERP_XY_UV_O_V( NI , NJ , V , UV_AT_O(:,:,2) )
    ELSE IF( PRESENT( VV ) ) THEN
      CALL MR_INTERP_XY_UV_O_V( NI , NJ , VV(:,:,2) , UV_AT_O(:,:,2) )
    ELSE IF( ALLOCATED( V_AT_V ) ) THEN
      CALL MR_INTERP_XY_UV_O_V( NI , NJ , V_AT_V , UV_AT_O(:,:,2) )
      DEALLOCATE( V_AT_V )
    ELSE
      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          UV_AT_O( I , J ,2) = 0.0
        END DO
      END DO
    END IF

    UV_AT_O = JOO .MRUVTFM. UV_AT_O

    DO DIM = 1 , 2

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 0 , NI
          UV_ARRAY(DIM, NDIDO( I , J ) ) = UV_AT_O( I , J ,DIM)
        END DO
      END DO

    END DO

  END SUBROUTINE MR_WRITE_UV_PACK_FOR_O_NODES

  END SUBROUTINE MR_WRITE_UV

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
  SUBROUTINE MR_WRITE_SS( MULTI_DSETS_ID , PATH_SS_IN_MULTI_DSETS , T ,   &
  & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
  & SS_BASE , SS_REF , SS , SU , SV , SO ,   &
  & ACTIVITY , ERROR , ERRMSG , OVERWRITE )

    USE MR_MOD_INTERP_XY

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

    REAL   (PARD_KIND) , INTENT(IN ) :: SS_BASE
    REAL   (PARD_KIND) , INTENT(IN ) :: SS_REF

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) , OPTIONAL :: SS
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) , OPTIONAL :: SU
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) , OPTIONAL :: SV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) , OPTIONAL :: SO

    INTEGER(ACID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,ACID_KIND),1:NJ) , OPTIONAL :: ACTIVITY

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION( : , : ) :: SU_AT_O
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION( : , : ) :: SV_AT_O

    REAL   (4)         , DIMENSION(1:NND) :: SS_ARRAY

    INTEGER                          :: DSET_SS_ID

    INTEGER                          :: NTIMES

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    LOGICAL            , INTENT(IN ) , OPTIONAL :: OVERWRITE

   !DIR$ FORCEINLINE
    CALL MR_WRITE_SS_PACK_FOR_W_NODES
   !DIR$ FORCEINLINE
    CALL MR_WRITE_SS_PACK_FOR_U_NODES
   !DIR$ FORCEINLINE
    CALL MR_WRITE_SS_PACK_FOR_V_NODES
   !DIR$ FORCEINLINE
    CALL MR_WRITE_SS_PACK_FOR_O_NODES
   !END$ FORCEINLINE

    SS_ARRAY = SS_ARRAY * ( SS_REF - SS_BASE ) + SS_BASE

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
                CALL MR_WRITE_ACTIVITY( DSET_SS_ID , NEM , NI , NJ , EMIDW , ACTIVITY , ERROR , ERRMSG )
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
  SUBROUTINE MR_WRITE_SS_PACK_FOR_W_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    IF( PRESENT( SS ) ) THEN

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 1 , NI
          SS_ARRAY( NDIDW( I , J ) ) = SS( I , J )
        END DO
      END DO

    ELSE

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 1 , NI
          SS_ARRAY( NDIDW( I , J ) ) = 0.0
        END DO
      END DO

    END IF

  END SUBROUTINE MR_WRITE_SS_PACK_FOR_W_NODES

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
  SUBROUTINE MR_WRITE_SS_PACK_FOR_U_NODES

    IMPLICIT NONE

    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: SS_AT_U

    INTEGER(IJID_KIND) :: I , J

    IF( PRESENT( SU ) ) THEN

      IF( .NOT. PRESENT( SO ) ) THEN
        ALLOCATE( SU_AT_O(0:NI0(NI,FDRD_KIND),0:NJ) )
        CALL MR_INTERP_XY_SS_O_U( NI , NJ , SU , SU_AT_O )
      END IF

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 0 , NI
          SS_ARRAY( NDIDU( I , J ) ) = SU( I , J )
        END DO
      END DO

    ELSE IF( PRESENT( SS ) ) THEN

      CALL MR_INTERP_XY_SS_U( NI , NJ , SS , SS_AT_U )

      IF( .NOT. PRESENT( SO ) ) THEN
        ALLOCATE( SU_AT_O(0:NI0(NI,FDRD_KIND),0:NJ) )
        CALL MR_INTERP_XY_SS_O_U( NI , NJ , SS_AT_U , SU_AT_O )
      END IF

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 0 , NI
          SS_ARRAY( NDIDU( I , J ) ) = SS_AT_U( I , J )
        END DO
      END DO

    ELSE

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 0 , NI
          SS_ARRAY( NDIDU( I , J ) ) = 0.0
        END DO
      END DO

    END IF

  END SUBROUTINE MR_WRITE_SS_PACK_FOR_U_NODES

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
  SUBROUTINE MR_WRITE_SS_PACK_FOR_V_NODES

    IMPLICIT NONE

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: SS_AT_V

    INTEGER(IJID_KIND) :: I , J

    IF( PRESENT( SV ) ) THEN

      IF( .NOT. PRESENT( SO ) ) THEN
        ALLOCATE( SV_AT_O(0:NI0(NI,FDRD_KIND),0:NJ) )
        CALL MR_INTERP_XY_SS_O_V( NI , NJ , SV , SV_AT_O )
      END IF

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 1 , NI
          SS_ARRAY( NDIDV( I , J ) ) = SV( I , J )
        END DO
      END DO

    ELSE IF( PRESENT( SS ) ) THEN

      CALL MR_INTERP_XY_SS_V( NI , NJ , SS , SS_AT_V )

      IF( .NOT. PRESENT( SO ) ) THEN
        ALLOCATE( SV_AT_O(0:NI0(NI,FDRD_KIND),0:NJ) )
        CALL MR_INTERP_XY_SS_O_V( NI , NJ , SS_AT_V , SV_AT_O )
      END IF

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 1 , NI
          SS_ARRAY( NDIDV( I , J ) ) = SS_AT_V( I , J )
        END DO
      END DO

    ELSE

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 1 , NI
          SS_ARRAY( NDIDV( I , J ) ) = 0.0
        END DO
      END DO

    END IF

  END SUBROUTINE MR_WRITE_SS_PACK_FOR_V_NODES

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
  SUBROUTINE MR_WRITE_SS_PACK_FOR_O_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    IF( PRESENT( SO ) ) THEN

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 0 , NI
          SS_ARRAY( NDIDO( I , J ) ) = SO( I , J )
        END DO
      END DO

    ELSE IF( ALLOCATED( SU_AT_O ) .AND. ALLOCATED( SV_AT_O ) ) THEN

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 0 , NI
          SS_ARRAY( NDIDO( I , J ) ) = 0.5 * ( SU_AT_O( I , J ) + SV_AT_O( I , J ) )
        END DO
      END DO

      DEALLOCATE( SU_AT_O , SV_AT_O )

    ELSE IF( ALLOCATED( SU_AT_O ) ) THEN

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 0 , NI
          SS_ARRAY( NDIDO( I , J ) ) = SU_AT_O( I , J )
        END DO
      END DO

      DEALLOCATE( SU_AT_O )

    ELSE IF( ALLOCATED( SV_AT_O ) ) THEN

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 0 , NI
          SS_ARRAY( NDIDO( I , J ) ) = SV_AT_O( I , J )
        END DO
      END DO

      DEALLOCATE( SV_AT_O )

    ELSE

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 0 , NI
          SS_ARRAY( NDIDO( I , J ) ) = 0.0
        END DO
      END DO

    END IF

  END SUBROUTINE MR_WRITE_SS_PACK_FOR_O_NODES

  END SUBROUTINE MR_WRITE_SS

  END MODULE MR_MOD_WRITE_FIELD_VARS_N_ACTIVITY