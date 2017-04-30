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
  MODULE MR_MOD_SEARCH_DSET_UNIT

    USE MR_KINDS

    USE MR_DEF_FIELD_VARS_DSET_NAMES
    USE MR_DEF_FIELD_VARS_UNITS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_SEARCH_DSET_UNIT

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
  SUBROUTINE MR_SEARCH_DSET_UNIT( DSET_PATH , DSET_UNIT )

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: DSET_PATH
    CHARACTER(   *   ) , INTENT(OUT) :: DSET_UNIT

    IF(      INDEX( DSET_PATH , TRIM(DSET_NAME_TBFUV) ) > 0 ) THEN
      DSET_UNIT = UNIT_TBFUV
    ELSE IF( INDEX( DSET_PATH , TRIM(DSET_NAME_TBUV ) ) > 0 ) THEN
      DSET_UNIT = UNIT_TBUV
    ELSE IF( INDEX( DSET_PATH , TRIM(DSET_NAME_CSS  ) ) > 0 ) THEN
      DSET_UNIT = UNIT_CSS
    ELSE IF( INDEX( DSET_PATH , TRIM(DSET_NAME_QSBUV) ) > 0 ) THEN
      DSET_UNIT = UNIT_QSBUV
    ELSE IF( INDEX( DSET_PATH , TRIM(DSET_NAME_KI   ) ) > 0 ) THEN
      DSET_UNIT = UNIT_KI
    ELSE IF( INDEX( DSET_PATH , TRIM(DSET_NAME_DI   ) ) > 0 ) THEN
      DSET_UNIT = UNIT_DI
    ELSE IF( INDEX( DSET_PATH , TRIM(DSET_NAME_ZB   ) ) > 0 ) THEN
      DSET_UNIT = UNIT_ZB
    ELSE IF( INDEX( DSET_PATH , TRIM(DSET_NAME_ZS   ) ) > 0 ) THEN
      DSET_UNIT = UNIT_ZS
    ELSE IF( INDEX( DSET_PATH , TRIM(DSET_NAME_UVA  ) ) > 0 ) THEN
      DSET_UNIT = UNIT_UVA
    ELSE IF( INDEX( DSET_PATH , TRIM(DSET_NAME_H    ) ) > 0 ) THEN
      DSET_UNIT = UNIT_H
    ELSE IF( INDEX( DSET_PATH , TRIM(DSET_NAME_UV   ) ) > 0 ) THEN
      DSET_UNIT = UNIT_UV
    ELSE IF( INDEX( DSET_PATH , TRIM(DSET_NAME_WW   ) ) > 0 ) THEN
      DSET_UNIT = UNIT_WW
    ELSE IF( INDEX( DSET_PATH , TRIM(DSET_NAME_R    ) ) > 0 ) THEN
      DSET_UNIT = UNIT_R
    ELSE IF( INDEX( DSET_PATH , TRIM(DSET_NAME_VXYUV) ) > 0 ) THEN
      DSET_UNIT = UNIT_VXYUV
    ELSE IF( INDEX( DSET_PATH , TRIM(DSET_NAME_VZWW ) ) > 0 ) THEN
      DSET_UNIT = UNIT_VZWW
    ELSE IF( INDEX( DSET_PATH , TRIM(DSET_NAME_DXYUV) ) > 0 ) THEN
      DSET_UNIT = UNIT_DXYUV
    ELSE IF( INDEX( DSET_PATH , TRIM(DSET_NAME_DZWW ) ) > 0 ) THEN
      DSET_UNIT = UNIT_DZWW
    ELSE
      DSET_UNIT = "DEFAULT"
    END IF

  END SUBROUTINE MR_SEARCH_DSET_UNIT

  END MODULE MR_MOD_SEARCH_DSET_UNIT