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
  MODULE MR_MOD_READ_FIELD_VARS_N_ACTIVITY

    USE XMDF

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_READ_UV
    PUBLIC :: MR_READ_SS

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
  SUBROUTINE MR_READ_ACTIVITY( DSET_ACTIVITY_ID , ITS , NEM , NI , NJ , EMIDW , ACTIVITY , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: DSET_ACTIVITY_ID

    INTEGER(TSID_KIND) , INTENT(IN ) :: ITS

    INTEGER(EMID_KIND) , INTENT(IN ) :: NEM
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(EMID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,EMID_KIND),1:NJ) :: EMIDW

    INTEGER(ACID_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,ACID_KIND),1:NJ) :: ACTIVITY
    INTEGER            , DIMENSION(1:NEM) :: ACTIVITY_ARRAY

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    CALL XF_READ_ACTIVITY_TIMESTEP( DSET_ACTIVITY_ID , INT(ITS+1,4) , NEM , ACTIVITY_ARRAY(1:NEM) , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in reading activity"
      RETURN
    END IF

   !DIR$ FORCEINLINE
    CALL MR_READ_ACTIVITY_UNPACK_FOR_ELEMS
   !END$ FORCEINLINE

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
  SUBROUTINE MR_READ_ACTIVITY_UNPACK_FOR_ELEMS

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED, ALWAYS
      DO I = 1 , NI
        ACTIVITY( I , J ) = ACTIVITY_ARRAY( EMIDW( I , J ) )
      END DO
    END DO

  END SUBROUTINE MR_READ_ACTIVITY_UNPACK_FOR_ELEMS

  END SUBROUTINE MR_READ_ACTIVITY

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
  SUBROUTINE MR_READ_UV( MULTI_DSETS_ID , PATH_UV_IN_MULTI_DSETS , ITS ,   &
  & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO , IUV , IUU , IVV , IOO ,   &
  & UV_BASE , UV_REF , UV , U , UU , V , VV , UVO ,   &
  & ACTIVITY , ERROR , ERRMSG )

    USE MR_MOD_OPERATOR_UV

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    CHARACTER(   *   ) , INTENT(IN ) :: PATH_UV_IN_MULTI_DSETS

    INTEGER(TSID_KIND) , INTENT(IN ) :: ITS

    INTEGER(NDID_KIND) , INTENT(IN ) :: NND
    INTEGER(EMID_KIND) , INTENT(IN ) :: NEM
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(EMID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,EMID_KIND),1:NJ        ) :: EMIDW

    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,NDID_KIND),1:NJ        ) :: NDIDW
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,NDID_KIND),1:NJ        ) :: NDIDU
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,NDID_KIND),0:NJ        ) :: NDIDV
    INTEGER(NDID_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,NDID_KIND),0:NJ        ) :: NDIDO

    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,GJRD_KIND),1:NJ,1:2,1:2) :: IUV
    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,GJRD_KIND),1:NJ,1:2,1:2) :: IUU
    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,GJRD_KIND),0:NJ,1:2,1:2) :: IVV
    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,GJRD_KIND),0:NJ,1:2,1:2) :: IOO

    REAL   (PARD_KIND) , INTENT(IN ) :: UV_BASE
    REAL   (PARD_KIND) , INTENT(IN ) :: UV_REF

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2    ) , OPTIONAL :: UV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ        ) , OPTIONAL :: U
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ,1:2    ) , OPTIONAL :: UU
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ        ) , OPTIONAL :: V
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ,1:2    ) , OPTIONAL :: VV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ,1:2    ) , OPTIONAL :: UVO

    INTEGER(ACID_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,ACID_KIND),1:NJ        ) , OPTIONAL :: ACTIVITY

    REAL   (4)         , DIMENSION(1:2,1:NND) :: UV_ARRAY

    INTEGER                          :: DSET_UV_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    ERRMSG = ""
    CALL XF_OPEN_GROUP( MULTI_DSETS_ID , TRIM(PATH_UV_IN_MULTI_DSETS) , DSET_UV_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in openning vector dataset group"
    ELSE

      CALL XF_READ_VECTOR_VALUES_TIMESTEP( DSET_UV_ID , INT(ITS+1,4) , NND , 2 , UV_ARRAY(1:2,1:NND) , ERROR )
      IF( ERROR < 0 ) THEN
        ERRMSG = "Error in reading vector values from dataset group"
      ELSE

        IF( PRESENT( ACTIVITY ) ) THEN
          CALL MR_READ_ACTIVITY( DSET_UV_ID , ITS , NEM , NI , NJ , EMIDW , ACTIVITY , ERROR , ERRMSG )
          IF( ERROR < 0 ) THEN
            ERRMSG = TRIM(ERRMSG)//" from vector dataset group"
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

    UV_ARRAY(1:2,1:NND) = ( UV_ARRAY(1:2,1:NND) - UV_BASE ) / ( UV_REF - UV_BASE )

   !DIR$ FORCEINLINE
    CALL MR_READ_UV_UNPACK_FOR_W_NODES
   !DIR$ FORCEINLINE
    CALL MR_READ_UV_UNPACK_FOR_U_NODES
   !DIR$ FORCEINLINE
    CALL MR_READ_UV_UNPACK_FOR_V_NODES
   !DIR$ FORCEINLINE
    CALL MR_READ_UV_UNPACK_FOR_O_NODES
   !END$ FORCEINLINE

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
  SUBROUTINE MR_READ_UV_UNPACK_FOR_W_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    IF( PRESENT( UV ) ) THEN

      DO DIM = 1 , 2

        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED, ALWAYS
          DO I = 1 , NI
            UV( I , J ,DIM) = UV_ARRAY(DIM, NDIDW( I , J ) )
          END DO
        END DO

      END DO

      CALL MR_COPY_UV( UV , IUV .MRUVTFM. UV )

    END IF

  END SUBROUTINE MR_READ_UV_UNPACK_FOR_W_NODES

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
  SUBROUTINE MR_READ_UV_UNPACK_FOR_U_NODES

    IMPLICIT NONE

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION( :  , :  , : ) :: UT

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    IF( PRESENT( UU ) ) THEN

      DO DIM = 1 , 2

        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED, ALWAYS
          DO I = 0 , NI
            UU( I , J ,DIM) = UV_ARRAY(DIM, NDIDU( I , J ) )
          END DO
        END DO

      END DO

      CALL MR_COPY_UV( UU , IUU .MRUVTFM. UU )

      IF( PRESENT( U ) ) THEN
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 0 , NI
            U( I , J ) = UU( I , J ,1)
          END DO
        END DO
      END IF

    ELSE IF( PRESENT( U ) ) THEN

      ALLOCATE( UT(0:NI0(NI,FDRD_KIND),1:NJ,1:2) )

        DO DIM = 1 , 2

          DO J = 1 , NJ
           !DIR$ VECTOR ALIGNED, ALWAYS
            DO I = 0 , NI
              UT( I , J ,DIM) = UV_ARRAY(DIM, NDIDU( I , J ) )
            END DO
          END DO

        END DO

        CALL MR_COPY_UV( UT , IUU .MRUVTFM. UT )

        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 0 , NI
            U( I , J ) = UT( I , J ,1)
          END DO
        END DO

      DEALLOCATE( UT )

    END IF

  END SUBROUTINE MR_READ_UV_UNPACK_FOR_U_NODES

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
  SUBROUTINE MR_READ_UV_UNPACK_FOR_V_NODES

    IMPLICIT NONE

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION( :  , :  , : ) :: VT

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    IF( PRESENT( VV ) ) THEN

      DO DIM = 1 , 2

        DO J = 0 , NJ
         !DIR$ VECTOR ALIGNED, ALWAYS
          DO I = 1 , NI
            VV( I , J ,DIM) = UV_ARRAY(DIM, NDIDV( I , J ) )
          END DO
        END DO

      END DO

      CALL MR_COPY_UV( VV , IVV .MRUVTFM. VV )

      IF( PRESENT( V ) ) THEN
        DO J = 0 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            V( I , J ) = VV( I , J ,2)
          END DO
        END DO
      END IF

    ELSE IF( PRESENT( V ) ) THEN

      ALLOCATE( VT(1:NI1(NI,FDRD_KIND),0:NJ,1:2) )

        DO DIM = 1 , 2

          DO J = 0 , NJ
           !DIR$ VECTOR ALIGNED, ALWAYS
            DO I = 1 , NI
              VT( I , J ,DIM) = UV_ARRAY(DIM, NDIDV( I , J ) )
            END DO
          END DO

        END DO

        CALL MR_COPY_UV( VT , IVV .MRUVTFM. VT )

        DO J = 0 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            V( I , J ) = VT( I , J ,2)
          END DO
        END DO

      DEALLOCATE( VT )

    END IF

  END SUBROUTINE MR_READ_UV_UNPACK_FOR_V_NODES

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
  SUBROUTINE MR_READ_UV_UNPACK_FOR_O_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    IF( PRESENT( UVO ) ) THEN

      DO DIM = 1 , 2

        DO J = 0 , NJ
         !DIR$ VECTOR ALIGNED, ALWAYS
          DO I = 0 , NI
            UVO( I , J ,DIM) = UV_ARRAY(DIM, NDIDO( I , J ) )
          END DO
        END DO

      END DO

      CALL MR_COPY_UV( UVO , IOO .MRUVTFM. UVO )

    END IF

  END SUBROUTINE MR_READ_UV_UNPACK_FOR_O_NODES

  END SUBROUTINE MR_READ_UV

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
  SUBROUTINE MR_READ_SS( MULTI_DSETS_ID , PATH_SS_IN_MULTI_DSETS , ITS ,   &
  & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
  & SS_BASE , SS_REF , SS , SU , SV , SO ,   &
  & ACTIVITY , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    CHARACTER(   *   ) , INTENT(IN ) :: PATH_SS_IN_MULTI_DSETS

    INTEGER(TSID_KIND) , INTENT(IN ) :: ITS

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

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) , OPTIONAL :: SS
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) , OPTIONAL :: SU
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) , OPTIONAL :: SV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) , OPTIONAL :: SO

    INTEGER(ACID_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,ACID_KIND),1:NJ) , OPTIONAL :: ACTIVITY

    REAL   (4)         , DIMENSION(1:NND) :: SS_ARRAY

    INTEGER                          :: DSET_SS_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    ERRMSG = ""
    CALL XF_OPEN_GROUP( MULTI_DSETS_ID , TRIM(PATH_SS_IN_MULTI_DSETS) , DSET_SS_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in openning scalar dataset group"
    ELSE

      CALL XF_READ_SCALAR_VALUES_TIMESTEP( DSET_SS_ID , INT(ITS+1,4) , NND , SS_ARRAY(1:NND) , ERROR )
      IF( ERROR < 0 ) THEN
        ERRMSG = "Error in reading scalar values from dataset group"
      ELSE

        IF( PRESENT( ACTIVITY ) ) THEN
          CALL MR_READ_ACTIVITY( DSET_SS_ID , ITS , NEM , NI , NJ , EMIDW , ACTIVITY , ERROR , ERRMSG )
          IF( ERROR < 0 ) THEN
            ERRMSG = TRIM(ERRMSG)//" from scalar dataset group"
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

    SS_ARRAY(1:NND) = ( SS_ARRAY(1:NND) - SS_BASE ) / ( SS_REF - SS_BASE )

   !DIR$ FORCEINLINE
    CALL MR_READ_SS_UNPACK_FOR_W_NODES
   !DIR$ FORCEINLINE
    CALL MR_READ_SS_UNPACK_FOR_U_NODES
   !DIR$ FORCEINLINE
    CALL MR_READ_SS_UNPACK_FOR_V_NODES
   !DIR$ FORCEINLINE
    CALL MR_READ_SS_UNPACK_FOR_O_NODES
   !END$ FORCEINLINE

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
  SUBROUTINE MR_READ_SS_UNPACK_FOR_W_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    IF( PRESENT( SS ) ) THEN
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 1 , NI
          SS( I , J ) = SS_ARRAY( NDIDW( I , J ) )
        END DO
      END DO
    END IF

  END SUBROUTINE MR_READ_SS_UNPACK_FOR_W_NODES

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
  SUBROUTINE MR_READ_SS_UNPACK_FOR_U_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    IF( PRESENT( SU ) ) THEN
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 0 , NI
          SU( I , J ) = SS_ARRAY( NDIDU( I , J ) )
        END DO
      END DO
    END IF

  END SUBROUTINE MR_READ_SS_UNPACK_FOR_U_NODES

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
  SUBROUTINE MR_READ_SS_UNPACK_FOR_V_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    IF( PRESENT( SV ) ) THEN
      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 1 , NI
          SV( I , J ) = SS_ARRAY( NDIDV( I , J ) )
        END DO
      END DO
    END IF

  END SUBROUTINE MR_READ_SS_UNPACK_FOR_V_NODES

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
  SUBROUTINE MR_READ_SS_UNPACK_FOR_O_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    IF( PRESENT( SO ) ) THEN
      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED, ALWAYS
        DO I = 0 , NI
          SO( I , J ) = SS_ARRAY( NDIDO( I , J ) )
        END DO
      END DO
    END IF

  END SUBROUTINE MR_READ_SS_UNPACK_FOR_O_NODES

  END SUBROUTINE MR_READ_SS

  END MODULE MR_MOD_READ_FIELD_VARS_N_ACTIVITY