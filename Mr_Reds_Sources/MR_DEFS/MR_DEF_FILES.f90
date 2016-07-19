!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MR_DEF_FILES
!
! PURPOSE:
!
!   TO DEFINE THE FILE NAMES AS CHARACTER VARIABLES.
!
! DEFINITION OF VARIABLES:
!
!                         FILE_PRJ   :    PROJECT FILE'S NAME
!
!                        FILE_XMDF   :    EXTENSIBLE MODEL DATA FORMAT (XMDF) FILE'S NAME
!
!                        FILE_MESH   :    MESH FILE'S NAME
!
!                 FILE_MULTI_DSETS   :    MULTIPLE DATASETS FILE'S NAME
!                FILE_TEMPEQI_DSET   :    EQUIBRILIUM TEMPERATURE DATASET FILE'S NAME
!                   FILE_WIND_DSET   :    WIND DATASET FILE'S NAME
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_DEF_FILES

    USE MR_KINDS

    IMPLICIT NONE

    CHARACTER( 2**08 ) :: FILE_PRJ

    CHARACTER( 2**08 ) :: FILE_XMDF

  ! CHARACTER( 2**08 ) :: FILE_MESH

  ! CHARACTER( 2**08 ) :: FILE_MULTI_DSETS
  ! CHARACTER( 2**08 ) :: FILE_TEMPEQI_DSET
  ! CHARACTER( 2**08 ) :: FILE_WIND_DSET
  ! ......

  END MODULE MR_DEF_FILES