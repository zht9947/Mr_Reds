#INCLUDE 'MR_H_PROGRAM_METADATA.H'
#INCLUDE 'MR_H_ALIGN_PADDING.H'
!***********************************************************************************************************************************
! UNIT:
!
!  (PROGRAM)
!
! PURPOSE:
!
!   TO
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
  PROGRAM INIBATHYGEN

    USE MR_KINDS

    USE MR_MAC_PI

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_GRID_SYS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_ACTIVITY

    USE MR_MOD_GEN_INI_ZB
    
    USE MR_MOD_INIT_GRID_SYS
    USE MR_MOD_INIT_CURVED_GEOS

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MULTI_DSETS
    
    USE MR_MOD_CREATE_DSET
    
    USE MR_MOD_WRITE_FIELD_VARS_N_ACTIVITY
    
    USE MR_MOD_MALLOC_GRID_SYS
    USE MR_MOD_MALLOC_CURVED_GEOS
    USE MR_MOD_MALLOC_ACTIVITY

    IMPLICIT NONE

    REAL   (PARD_KIND) :: THETA0 = 30.0*PI/180.0
    REAL   (PARD_KIND) :: BTH = 1.0
    REAL   (PARD_KIND) :: LTH = 6.737086 , HTH = 1.0
    REAL   (PARD_KIND) :: DZB_BK_MIN = -4.0 , DZB_BK_MAX = +3.0
    REAL   (PARD_KIND) :: XI0 = 0.223 , XXIM = 0.15

    INTEGER            :: NBENDS = 1

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:) :: ZB

    INTEGER            , ALLOCATABLE , DIMENSION(:) :: ERROR_1D_ARRAY
    INTEGER :: ERROR
    CHARACTER( 2**10 ) :: ERRMSG
    
    INTEGER :: FILE_XMDF_ID , MULTI_DSETS_ID

    NI = 33 ; NJ = 10
    NND = 1407 ; NEM = 330
    
    XYR = 1.0

    ALLOCATE( ZB(1:NI1(NI,FDRD_KIND),1:NJ) )
    ALLOCATE( ERROR_1D_ARRAY(1:NI1(NI,KIND(ERROR))) )

    CALL MR_GEN_INI_ZB( THETA0 , BTH , LTH , HTH , DZB_BK_MIN , DZB_BK_MAX , XI0 , XXIM , NBENDS , NI , NJ , ZB , ERROR_1D_ARRAY )

    CALL MR_MALLOC_GRID_SYS
    CALL MR_INIT_GRID_SYS( "R:\Case.h5" , ERROR , ERRMSG )
    CALL MR_MALLOC_CURVED_GEOS
    CALL MR_INIT_CURVED_GEOS( "R:\Case.h5" , ERROR , ERRMSG )
    CALL MR_MALLOC_ACTIVITY
    ACTIVITY = BEACTIVE
    
    CALL MR_OPEN_FILE_XMDF( "R:\Case.h5" , "WRITE" , FILE_XMDF_ID , ERROR , ERRMSG )
    PRINT*,1,ERROR
    CALL MR_OPEN_MULTI_DSETS( FILE_XMDF_ID , MULTI_DSETS_ID , ERROR , ERRMSG )
    PRINT*,2,ERROR
    
    CALL MR_CREATE_DSET_SS( MULTI_DSETS_ID , "Mr.Reds/Bathymetry" , "m" , ERROR , ERRMSG )
    PRINT*,3,ERROR
    
    CALL MR_WRITE_SS( MULTI_DSETS_ID , "Mr.Reds/Bathymetry" , 0.0_8 ,   &
    & NND , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
    & SS_BASE=0.0_PARD_KIND , SS_REF=1.0_PARD_KIND , SS=ZB ,   &
    & activity=activity ,   &
    & ERROR=ERROR , ERRMSG=ERRMSG )
    PRINT*,4,ERROR

    CALL MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR , ERRMSG )
    CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR , ERRMSG )


  END PROGRAM INIBATHYGEN