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
  MODULE MR_MOD_READ_FIELD_VARS_N_ACTIVITY

    USE XMDF

    USE MR_KINDS

    USE MR_DEF_GRID_SYS
    USE MR_DEF_CURVED_GEOS

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
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_READ_ACTIVITY( DSET_ACTIVITY_ID , ITS , NEM , NI , NJ , ACTIVITY , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: DSET_ACTIVITY_ID

    INTEGER(TSID_KIND) , INTENT(IN ) :: ITS

    INTEGER(EMID_KIND) , INTENT(IN ) :: NEM
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(ACID_KIND) , INTENT(OUT) , DIMENSION(1:NI1(ACID_KIND),1:NJ) :: ACTIVITY
    INTEGER            , DIMENSION(1:NEM) :: ACTIVITY_ARRAY

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    CALL XF_READ_ACTIVITY_TIMESTEP( DSET_ACTIVITY_ID , ITS , NEM , ACTIVITY_ARRAY , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in reading activity"
      RETURN
    END IF

    CALL MR_READ_ACTIVITY_UNPACK_FOR_ELEMS

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
  SUBROUTINE MR_READ_ACTIVITY_UNPACK_FOR_ELEMS

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
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
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_READ_UV( MULTI_DSETS_ID , PATH_UV_IN_MULTI_DSETS , ITS ,   &
  & NND , NEM , NI , NJ , UV_BASE , UV_REF , UV , U , UU , V , VV , UVO ,   &
  & ACTIVITY , ERROR , ERRMSG )

    USE MR_MOD_OPERATOR_UV

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    CHARACTER(   *   ) , INTENT(IN ) :: PATH_UV_IN_MULTI_DSETS

    INTEGER(TSID_KIND) , INTENT(IN ) :: ITS

    INTEGER(NDID_KIND) , INTENT(IN ) :: NND
    INTEGER(EMID_KIND) , INTENT(IN ) :: NEM
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (PARD_KIND) , INTENT(IN ) :: UV_BASE
    REAL   (PARD_KIND) , INTENT(IN ) :: UV_REF

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2) , OPTIONAL :: UV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(FDRD_KIND),1:NJ    ) , OPTIONAL :: U
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(FDRD_KIND),1:NJ,1:2) , OPTIONAL :: UU
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),0:NJ    ) , OPTIONAL :: V
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),0:NJ,1:2) , OPTIONAL :: VV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(FDRD_KIND),0:NJ,1:2) , OPTIONAL :: UVO

    INTEGER(ACID_KIND) , INTENT(OUT) , DIMENSION(1:NI1(ACID_KIND),1:NJ    ) , OPTIONAL :: ACTIVITY

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
    
      CALL XF_READ_VECTOR_VALUES_TIMESTEP( DSET_UV_ID , ITS , NND , 2 , UV_ARRAY , ERROR )
      IF( ERROR < 0 ) THEN
        ERRMSG = "Error in reading vector values from dataset group"
      ELSE
      
        IF( PRESENT( ACTIVITY ) ) THEN
          CALL MR_READ_ACTIVITY( DSET_UV_ID , ITS , NEM , NI , NJ , ACTIVITY , ERROR , ERRMSG )
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

    UV_ARRAY = ( UV_ARRAY - UV_BASE ) / ( UV_REF - UV_BASE )

    CALL MR_READ_UV_UNPACK_FOR_W_NODES
    CALL MR_READ_UV_UNPACK_FOR_U_NODES
    CALL MR_READ_UV_UNPACK_FOR_V_NODES
    CALL MR_READ_UV_UNPACK_FOR_O_NODES

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
  SUBROUTINE MR_READ_UV_UNPACK_FOR_W_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    IF( PRESENT( UV ) ) THEN

      DO DIM = 1 , 2

        DO J = 1 , NJ
          DO I = 1 , NI
            UV( I , J ,DIM) = UV_ARRAY(DIM, NDIDW( I , J ) )
          END DO
        END DO

      END DO

      UV = IUV .MRUVTFM. UV

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
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
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
          DO I = 0 , NI
            UU( I , J ,DIM) = UV_ARRAY(DIM, NDIDU( I , J ) )
          END DO
        END DO

      END DO

      UU = IUU .MRUVTFM. UU

      IF( PRESENT( U ) ) THEN
        DO J = 1 , NJ
          DO I = 0 , NI
            U( I , J ) = UU( I , J ,1)
          END DO
        END DO
      END IF

    ELSE IF( PRESENT( U ) ) THEN

      ALLOCATE( UT(0:NI0(FDRD_KIND),1:NJ,1:2) )

        DO DIM = 1 , 2

          DO J = 1 , NJ
            DO I = 0 , NI
              UT( I , J ,DIM) = UV_ARRAY(DIM, NDIDU( I , J ) )
            END DO
          END DO

        END DO

        UT = IUU .MRUVTFM. UT

        DO J = 1 , NJ
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
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
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
          DO I = 1 , NI
            VV( I , J ,DIM) = UV_ARRAY(DIM, NDIDV( I , J ) )
          END DO
        END DO

      END DO

      VV = IVV .MRUVTFM. VV

      IF( PRESENT( V ) ) THEN
        DO J = 0 , NJ
          DO I = 1 , NI
            V( I , J ) = VV( I , J ,2)
          END DO
        END DO
      END IF

    ELSE IF( PRESENT( V ) ) THEN

      ALLOCATE( VT(1:NI1(FDRD_KIND),0:NJ,1:2) )

        DO DIM = 1 , 2

          DO J = 0 , NJ
            DO I = 1 , NI
              VT( I , J ,DIM) = UV_ARRAY(DIM, NDIDV( I , J ) )
            END DO
          END DO

        END DO

        VT = IVV .MRUVTFM. VT

        DO J = 0 , NJ
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
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_READ_UV_UNPACK_FOR_O_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    IF( PRESENT( UVO ) ) THEN

      DO DIM = 1 , 2

        DO J = 0 , NJ
          DO I = 0 , NI
            UVO( I , J ,DIM) = UV_ARRAY(DIM, NDIDO( I , J ) )
          END DO
        END DO

      END DO

      UVO = IOO .MRUVTFM. UVO

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
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_READ_SS( MULTI_DSETS_ID , PATH_SS_IN_MULTI_DSETS , ITS ,   &
  & NND , NEM , NI , NJ , SS_BASE , SS_REF , SS , SU , SV , SO ,   &
  & ACTIVITY , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    CHARACTER(   *   ) , INTENT(IN ) :: PATH_SS_IN_MULTI_DSETS

    INTEGER(TSID_KIND) , INTENT(IN ) :: ITS

    INTEGER(NDID_KIND) , INTENT(IN ) :: NND
    INTEGER(EMID_KIND) , INTENT(IN ) :: NEM
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (PARD_KIND) , INTENT(IN ) :: SS_BASE
    REAL   (PARD_KIND) , INTENT(IN ) :: SS_REF

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),1:NJ) , OPTIONAL :: SS
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(FDRD_KIND),1:NJ) , OPTIONAL :: SU
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),0:NJ) , OPTIONAL :: SV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(FDRD_KIND),0:NJ) , OPTIONAL :: SO

    INTEGER(ACID_KIND) , INTENT(OUT) , DIMENSION(1:NI1(ACID_KIND),1:NJ) , OPTIONAL :: ACTIVITY

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

      CALL XF_READ_SCALAR_VALUES_TIMESTEP( DSET_SS_ID , ITS , NND , SS_ARRAY , ERROR )
      IF( ERROR < 0 ) THEN
        ERRMSG = "Error in reading scalar values from dataset group"
      ELSE

        IF( PRESENT( ACTIVITY ) ) THEN
          CALL MR_READ_ACTIVITY( DSET_SS_ID , ITS , NEM , NI , NJ , ACTIVITY , ERROR , ERRMSG )
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

    SS_ARRAY = ( SS_ARRAY - SS_BASE ) / ( SS_REF - SS_BASE )

    CALL MR_READ_SS_UNPACK_FOR_W_NODES
    CALL MR_READ_SS_UNPACK_FOR_U_NODES
    CALL MR_READ_SS_UNPACK_FOR_V_NODES
    CALL MR_READ_SS_UNPACK_FOR_O_NODES

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
  SUBROUTINE MR_READ_SS_UNPACK_FOR_W_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    IF( PRESENT( SS ) ) THEN
      DO J = 1 , NJ
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
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_READ_SS_UNPACK_FOR_U_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    IF( PRESENT( SU ) ) THEN
      DO J = 1 , NJ
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
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_READ_SS_UNPACK_FOR_V_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    IF( PRESENT( SV ) ) THEN
      DO J = 0 , NJ
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
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_READ_SS_UNPACK_FOR_O_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    IF( PRESENT( SO ) ) THEN
      DO J = 0 , NJ
        DO I = 0 , NI
          SO( I , J ) = SS_ARRAY( NDIDO( I , J ) )
        END DO
      END DO
    END IF

  END SUBROUTINE MR_READ_SS_UNPACK_FOR_O_NODES

  END SUBROUTINE MR_READ_SS

  END MODULE MR_MOD_READ_FIELD_VARS_N_ACTIVITY