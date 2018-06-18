!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MR_DEF_GRID_SYS
!
! PURPOSE:
!
!   TO DEFINE THE GRID SYSTEM, WHICH IS CONSIDERED AS A 2-D MAPPING OF THE 1-D MESH. UNLIKE THE MESH WHICH USES ELEMENTS TO MANAGE
!   NODES, THE GRID STORES NODE IDS IN 2-D ARRAYS WITH THE CENTRAL, THE FACIAL AND THE CORNER NODES STRICTLY DISTINGUISHED. MOREOVER,
!   TO MAKE THE MAPPING COMPLETE, THE ELEMENT IDS ARE ALSO STORED IN A 2-D ARRAY WITH THE SAME RANKS OF THE ARRAY OF THE CENTRAL
!   NODE IDS FOR AN ELEMENT OR A CELL CONTAINS ONLY ONE CENTRAL NODE.
!
! DEFINITION OF VARIABLES:
!
!                           EMIDW    :    ELEMENT ID ARRAY
!
!                           NDIDW    :    CENTROID ID ARRAY
!                    NDIDU, NDIDV    :    U- AND V-, RESPECTIVELY, FACIAL NODE ID ARRAYS
!                           NDIDO    :    CORNER NODE ID ARRAY
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-03-27    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_DEF_GRID_SYS

    USE MR_KINDS

    IMPLICIT NONE

    INTEGER(EMID_KIND) , ALLOCATABLE , DIMENSION(:,:) :: EMIDW

    INTEGER(NDID_KIND) , ALLOCATABLE , DIMENSION(:,:) :: NDIDW
    INTEGER(NDID_KIND) , ALLOCATABLE , DIMENSION(:,:) :: NDIDU , NDIDV
    INTEGER(NDID_KIND) , ALLOCATABLE , DIMENSION(:,:) :: NDIDO

  END MODULE MR_DEF_GRID_SYS