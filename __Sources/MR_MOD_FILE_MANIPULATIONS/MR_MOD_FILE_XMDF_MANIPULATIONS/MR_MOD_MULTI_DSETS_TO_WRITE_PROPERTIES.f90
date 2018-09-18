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
  MODULE MR_MOD_WRITE_PROPERTIES

    USE XMDF

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_WRITE_NK
    !PUBLIC :: MR_WRITE_NKS , MR_WRITE_D0

    PUBLIC :: MR_WRITE_SLOPE

    INTEGER            , PARAMETER :: COMPRESSION = NONE   ! NO COMPRESSION

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
  SUBROUTINE MR_WRITE_NK( MULTI_DSETS_ID , NK , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    INTEGER                          :: PROP_ID , GROUP_ID

    INTEGER(KKID_KIND) , INTENT(IN ) :: NK
    INTEGER                          :: NK_ARRAY(1:1)

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    ERRMSG = ""

    CALL XF_OPEN_PROPERTY_GROUP( MULTI_DSETS_ID , PROP_ID , ERROR )
    IF( ERROR < 0 ) THEN
      CALL XF_CREATE_PROPERTY_GROUP( MULTI_DSETS_ID , PROP_ID , ERROR )
      IF( ERROR < 0 ) THEN
        ERRMSG = "Error in opening /PROPERTIES"
      END IF
    END IF

    IF( ERROR >= 0 ) THEN

      CALL XF_OPEN_GROUP( PROP_ID , "By Layers" , GROUP_ID , ERROR )
      IF( ERROR < 0 ) THEN
        CALL XF_CREATE_GENERIC_GROUP( PROP_ID , "By Layers" , GROUP_ID , ERROR )
        IF( ERROR < 0 ) THEN
          ERRMSG = "Error in opening /PROPERTIES/By Layers"
        END IF
      END IF

      IF( ERROR >= 0 ) THEN

        NK_ARRAY(1) = NK

        CALL XF_WRITE_PROPERTY_INT( GROUP_ID , "Num Layers" , 1 , NK_ARRAY , COMPRESSION , ERROR )
        IF( ERROR < 0 ) THEN
          ERRMSG = "Error in writing the number of layers into /PROPERTIES/By Layers"
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

  END SUBROUTINE MR_WRITE_NK

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
  SUBROUTINE MR_WRITE_SLOPE( MULTI_DSETS_ID , SLOPE , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    INTEGER                          :: PROP_ID

    REAL   (SPRD_KIND) , INTENT(IN ) :: SLOPE
    REAL   (8)                       :: SLOPE_ARRAY(1:1)

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    ERRMSG = ""

    CALL XF_OPEN_PROPERTY_GROUP( MULTI_DSETS_ID , PROP_ID , ERROR )
    IF( ERROR < 0 ) THEN
      CALL XF_CREATE_PROPERTY_GROUP( MULTI_DSETS_ID , PROP_ID , ERROR )
      IF( ERROR < 0 ) THEN
        ERRMSG = "Error in opening /PROPERTIES"
      END IF
    END IF

    IF( ERROR >= 0 ) THEN

      SLOPE_ARRAY(1) = SLOPE

      CALL XF_WRITE_PROPERTY_DOUBLE( PROP_ID , "Slope" , 1 , SLOPE_ARRAY , COMPRESSION , ERROR )
      IF( ERROR < 0 ) THEN
        ERRMSG = "Error in writing the value of slope into /PROPERTIES"
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

  END SUBROUTINE MR_WRITE_SLOPE

  END MODULE MR_MOD_WRITE_PROPERTIES