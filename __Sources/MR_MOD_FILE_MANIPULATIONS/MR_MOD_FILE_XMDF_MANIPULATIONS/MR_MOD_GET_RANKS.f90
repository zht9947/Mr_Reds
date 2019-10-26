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
  MODULE MR_MOD_GET_RANKS

    USE XMDF

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_GET_RANKS
    PUBLIC :: MR_GET_NK
    PUBLIC :: MR_GET_NKS

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
  SUBROUTINE MR_GET_RANKS( MESH_IN_XMDF_ID , NND , NEM , NI , NJ , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MESH_IN_XMDF_ID

    INTEGER(NDID_KIND) , INTENT(OUT) :: NND
    INTEGER(EMID_KIND) , INTENT(OUT) :: NEM
    INTEGER(EMID_KIND) , INTENT(OUT) :: NI , NJ

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    REAL   (CARD_KIND)               :: CTMP1 , CTMP2

    ERRMSG = ""

  ! GET FROM MESH NND AND NEM
    CALL XF_GET_NUMBER_OF_NODES( MESH_IN_XMDF_ID , NND , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in getting the number of nodes from mesh"
      RETURN
    END IF
    CALL XF_GET_NUMBER_OF_ELEMENTS( MESH_IN_XMDF_ID , NEM , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in getting the number of elements from mesh"
      RETURN
    END IF

  ! CALCULATE NI AND NJ
    CTMP1 = 0.25*(NND-1) - NEM
    CTMP2 = SQRT( CTMP1*CTMP1 - NEM )
  ! ASSUME NI IS ALWAYS GREATER THAN OR EQUAL TO NJ
    NI = NINT( CTMP1 + CTMP2 )
    NJ = NINT( CTMP1 - CTMP2 )

  ! CHECK CONSISTENCY
    IF( NND /= ((2*NI+1)*(2*NJ+1)) .AND. NEM /= (NI*NJ) ) THEN
      ERROR = - 1
      ERRMSG = "Inconsistent both number of nodes (NND) and number of elements (NEM) gotten from mesh"
      RETURN
    ELSE IF( NND /= ((2*NI+1)*(2*NJ+1)) ) THEN
      ERROR = - 1
      ERRMSG = "Inconsistent number of nodes (NND) gotten from mesh"
      RETURN
    ELSE IF( NEM /= (NI*NJ) ) THEN
      ERROR = - 1
      ERRMSG = "Inconsistent number of elements (NEM) gotten from mesh"
      RETURN
    END IF

  END SUBROUTINE MR_GET_RANKS

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
  SUBROUTINE MR_GET_NK( MULTI_DSETS_ID , NK , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    INTEGER                          :: PROP_ID , GROUP_ID

    INTEGER(KKID_KIND) , INTENT(OUT) :: NK
    INTEGER                          :: NK_ARRAY(1:1)

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    ERRMSG = ""

    CALL XF_OPEN_PROPERTY_GROUP( MULTI_DSETS_ID , PROP_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in opening /PROPERTIES"
    ELSE

      CALL XF_OPEN_GROUP( PROP_ID , "By Layers" , GROUP_ID , ERROR )
      IF( ERROR < 0 ) THEN
        ERRMSG = "Error in opening /PROPERTIES/By Layers"
      ELSE

        CALL XF_READ_PROPERTY_INT( GROUP_ID , "Num Layers" , 1 , NK_ARRAY , ERROR )
        IF( ERROR < 0 ) THEN
          ERRMSG = "Error in getting the number of layers from /PROPERTIES/By Layers"
        ELSE
          NK = NK_ARRAY(1)
        END IF

        CALL XF_CLOSE_GROUP( GROUP_ID , ERROR_DUMMY )
        IF( ERROR_DUMMY < 0 .AND. ERROR >= 0 ) THEN
          ERROR = ERROR_DUMMY
          ERRMSG = "Error in closing /PROPERTIES/By Layers"
        END IF

      END IF

      CALL XF_CLOSE_GROUP( PROP_ID , ERROR_DUMMY )
      IF( ERROR_DUMMY < 0 .AND. ERROR >= 0 ) THEN
        ERROR = ERROR_DUMMY
        ERRMSG = "Error in closing /PROPERTIES"
      END IF

    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" in multiple datasets"
      RETURN
    END IF

  END SUBROUTINE MR_GET_NK

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
  SUBROUTINE MR_GET_NKS( MULTI_DSETS_ID , NKS , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    INTEGER                          :: PROP_ID , GROUP_ID

    INTEGER(KSID_KIND) , INTENT(OUT) :: NKS
    INTEGER                          :: NKS_ARRAY(1:1)

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    ERRMSG = ""

    CALL XF_OPEN_PROPERTY_GROUP( MULTI_DSETS_ID , PROP_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in opening /PROPERTIES"
    ELSE

      CALL XF_OPEN_GROUP( PROP_ID , "By Sediment Sizes" , GROUP_ID , ERROR )
      IF( ERROR < 0 ) THEN
        ERRMSG = "Error in opening /PROPERTIES/By Sediment Sizes"
      ELSE

        CALL XF_READ_PROPERTY_INT( GROUP_ID , "Num Sediment Sizes" , 1 , NKS_ARRAY , ERROR )
        IF( ERROR < 0 ) THEN
          ERRMSG = "Error in getting the number of sediment sizes from /PROPERTIES/By Sediment Sizes"
        ELSE
          NKS = NKS_ARRAY(1)
        END IF

        CALL XF_CLOSE_GROUP( GROUP_ID , ERROR_DUMMY )
        IF( ERROR_DUMMY < 0 .AND. ERROR >= 0 ) THEN
          ERROR = ERROR_DUMMY
          ERRMSG = "Error in closing /PROPERTIES/By Sediment Sizes"
        END IF

      END IF

      CALL XF_CLOSE_GROUP( PROP_ID , ERROR_DUMMY )
      IF( ERROR_DUMMY < 0 .AND. ERROR >= 0 ) THEN
        ERROR = ERROR_DUMMY
        ERRMSG = "Error in closing /PROPERTIES"
      END IF

    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" in multiple datasets"
      RETURN
    END IF

  END SUBROUTINE MR_GET_NKS

  END MODULE MR_MOD_GET_RANKS