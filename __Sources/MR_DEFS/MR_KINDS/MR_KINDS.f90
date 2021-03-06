!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MR_KINDS
!
! PURPOSE:
!
!   TO DEFINE THE KIND PARAMETERS.
!
! DEFINITION OF VARIABLES:
!
!                       TSID_KIND    :    KIND OF TIMESTEP ID      , MARKED AS 'ITS'
!                       EMID_KIND    :    KIND OF ELEMENT ID       , MARKED AS 'EMID'
!                       NDID_KIND    :    KIND OF NODE ID          , MARKED AS 'NDID'
!                       IJID_KIND    :    KIND OF CELL ID          , MARKED AS 'I' FOR COLUMN ID & 'J' FOR ROW ID
!                                    :    KIND OF DIMENSION ID     , MARKED AS 'DIM'
!                       KKID_KIND    :    KIND OF LAYER ID         , MARKED AS 'K'
!                       KSID_KIND    :    KIND OF SEDIMENT SIZE ID , MARKED AS 'KS'
!
!                                      \\ ABOVE ARE FOR RANKS AND LOOP VARIABLES
!
!                       NRID_KIND    :    KIND OF ENUMERATORS
!
!                                      \\ ABOVE ARE FOR ENUMERATOR DATA
!
!                       ACID_KIND    :    KIND OF ACTIVITY
!
!                                      \\ ABOVE ARE FOR INTEGER DATA
!
!                       TMRD_KIND    :    KIND OF TIME
!                       PARD_KIND    :    KIND OF CONSTANTS AND REFERENCE PARAMETERS
!                       XYRD_KIND    :    KIND OF COORDINATES
!                       GJRD_KIND    :    KIND OF CURVED GEOMETRY
!                       FDRD_KIND    :    KIND OF FIELD VARIABLES
!                       SPRD_KIND    :    KIND OF SLOPE
!
!                                      \\ ABOVE ARE FOR REAL DATA
!
!                       CARD_KIND    :    KIND OF CALCULATIONS
!
!                                      \\ ABOVE ARE FOR CALCULATIONS
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_KINDS

    IMPLICIT NONE

    INTEGER , PARAMETER :: TSID_KIND = 4
    INTEGER , PARAMETER :: EMID_KIND = 4
    INTEGER , PARAMETER :: NDID_KIND = 4
    INTEGER , PARAMETER :: IJID_KIND = 4
    INTEGER , PARAMETER :: KKID_KIND = 4
    INTEGER , PARAMETER :: KSID_KIND = 4

    INTEGER , PARAMETER :: NRID_KIND = 1

    INTEGER , PARAMETER :: ACID_KIND = 1

    INTEGER , PARAMETER :: TMRD_KIND = 8
    INTEGER , PARAMETER :: PARD_KIND = 8
    INTEGER , PARAMETER :: XYRD_KIND = 8
    INTEGER , PARAMETER :: GJRD_KIND = 8
    INTEGER , PARAMETER :: FDRD_KIND = 4
    INTEGER , PARAMETER :: SPRD_KIND = 8

    INTEGER , PARAMETER :: CARD_KIND = 8

  END MODULE MR_KINDS