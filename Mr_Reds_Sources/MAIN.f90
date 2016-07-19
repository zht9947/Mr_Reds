!#include 'MR_H_ALIGN_PADDING.h'
!program main
!
!  USE MR_KINDS
!  
!  USE MR_DEF_PRJ_METADATA
!    
!  USE MR_DEF_RANKS
!  USE MR_DEF_CURVED_GEOS
!  USE MR_DEF_CONSTS_N_REF_PARS
!  USE MR_DEF_FIELD_VARS
!  USE MR_DEF_FIELD_VARS_DSET_NAMES
!  USE MR_DEF_FIELD_VARS_UNITS
!  USE MR_DEF_ACTIVITY
!  USE MR_DEF_TIMING
!  USE MR_DEF_FILES
!  
!  USE MR_MOD_OPERATOR_SS
!  USE MR_MOD_OPERATOR_UV
!  USE MR_MOD_OPERATOR_XUV
!  
!  USE MR_MOD_CALC_GRAD_XY
!  USE MR_MOD_CALC_GRAD_Z
!  USE MR_MOD_CALC_QADV_N_QDIF_XY
!  USE MR_MOD_CALC_QADV_N_QDIF_Z
!  USE MR_MOD_INTERP_XY
!  USE MR_MOD_INTERP_Z
!  USE MR_MOD_TDMA
!  USE MR_MOD_TDMA3
! 
!  USE MR_MOD_CALC_HYD_COR_XY
!  USE MR_MOD_CALC_HYD_BAR_XY
!  USE MR_MOD_CALC_HYD_ADV_N_DIF_XY
!  USE MR_MOD_CALC_HYD_ADV_N_DIF_Z
!  USE MR_MOD_CALC_HYD_A3_B3_C3_D3
!  USE MR_MOD_CALC_ALFA_N_BETA
!  USE MR_MOD_CALC_A_B_C_D
!  
!  USE MR_MOD_UPDT_TBUV
!  USE MR_MOD_UPDT_HYD
!  USE MR_MOD_UPDT_H
!  USE MR_MOD_UPDT_VZW
!  
!  USE MR_MOD_INIT_PRJ
!  USE MR_MOD_INIT_GRID_SYS
!  USE MR_MOD_INIT_CURVED_GEOS
!  USE MR_MOD_INIT_FIELD_VARS_N_ACTIVITY
!  USE MR_MOD_INIT_OUTPUT
!  
!  USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
!  USE MR_MOD_OPEN_N_CLOSE_MESH_IN_XMDF
!  USE MR_MOD_OPEN_N_CLOSE_MULTI_DSETS
!  USE MR_MOD_GET_RANKS2
!  
!  USE MR_MOD_OUTPUT
!  
!  USE MR_MOD_ECHO_PRJ
!  
!  implicit none
!  
!  integer(ijid_kind) :: ni111 , ni222
!  
!  integer(ijid_kind) :: i , j
!  integer(kkid_kind) :: k
!  
!  integer :: dim
!
!  real   (fdrd_kind) , allocatable , dimension(:,:      ) :: h0
!  real   (fdrd_kind) , allocatable , dimension(:,:,:    ) :: uva0
!  real   (fdrd_kind) , allocatable , dimension(:,:,:,:  ) :: uv0
!  
!  real   (card_kind) , allocatable , dimension(:,:,:    ) :: alfa , beta
!    
!  real   (card_kind) , allocatable , dimension(:,:      ) :: a1 , b1 , c1
!  real   (card_kind) , allocatable , dimension(:,:      ) :: d1
!    
!  real   (card_kind) , allocatable , dimension(:,:      ) :: a2 , b2 , c2
!  real   (card_kind) , allocatable , dimension(:,:      ) :: d2
!    
!  real   (card_kind) , allocatable , dimension(:,:,  :  ) :: hyd_a3 , hyd_b3 , hyd_c3
!  real   (card_kind) , allocatable , dimension(:,:,:,:  ) :: hyd_d3
!  
!  real   (fdrd_kind) , allocatable , dimension(:,:,:) :: uvt
!  real   (fdrd_kind) , allocatable , dimension(:,:,:) :: uut
!  real   (fdrd_kind) , allocatable , dimension(:,:,:) :: vvt
!    
!  real   (fdrd_kind) , allocatable , dimension(:,:,:) :: qadv_xy_uv_u , qdif_xy_uv_u
!  real   (fdrd_kind) , allocatable , dimension(:,:,:) :: qadv_xy_uv_v , qdif_xy_uv_v
!  
!  real   (fdrd_kind) , allocatable , dimension(:,:,:) :: tsuv
!  real   (fdrd_kind) , allocatable , dimension(:,:,:,:) :: qadv_z_uv_w , qdif_z_uv_w
!  
!  INTEGER(TSID_KIND) :: ITS
!  
!  REAL   (TMRD_KIND) :: T
!    
!  INTEGER            :: ERROR
!  CHARACTER( 2**10 ) :: ERRMSG
!  
!  FILE_PRJ = "C:\Users\ZHT9947\Desktop\Case.txt"
!  FILE_XMDF = "R:\Case.h5"
!  
!  WRITE(*,'("Initialize project... ", $ )')
!  CALL MR_INIT_PRJ( TRIM(FILE_PRJ) , ERROR , ERRMSG )
!  IF( ERROR < 0 ) THEN
!    WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
!    STOP
!  ELSE
!    WRITE(*,'("Done! ")')
!  END IF
!  
!  CALL MR_ECHO_PRJ
!  
!  ni111=ni+1 ; ni222=ni-2
!  
!  ALLOCATE(    H0(1:NI,1:NJ           ) )
!  ALLOCATE(  UVA0(1:NI,1:NJ,1:2       ) )
!  ALLOCATE(   UV0(1:NI,1:NJ,1:2,1:NK  ) )
!  
!  ALLOCATE(  ALFA(1:NI,1:NJ,1:2       ) ,   BETA(1:NI,1:NJ,1:2       ) )
!    
!  ALLOCATE(    A1(2:NI,1:NJ           ) ,     B1(1:NI,1:NJ           ) ,     C1(1:NI-1,1:NJ         ) )
!  ALLOCATE(    D1(1:NI,1:NJ           ) )
!    
!  ALLOCATE(    A2(1:NI,2:NJ           ) ,     B2(1:NI,1:NJ           ) ,     C2(1:NI,1:NJ-1         ) )
!  ALLOCATE(    D2(1:NI,1:NJ           ) )
!    
!  ALLOCATE(HYD_A3(1:NI,1:NJ,    2:NK  ) , HYD_B3(1:NI,1:NJ,    1:NK  ) , HYD_C3(1:NI,1:NJ,    1:NK-1) )
!  ALLOCATE(HYD_D3(1:NI,1:NJ,1:2,1:NK  ) )
!  
!  ALLOCATE( UVT(1:NI,1:NJ,1:2) )
!  ALLOCATE( UUT(0:NI,1:NJ,1:2) )
!  ALLOCATE( VVT(1:NI,0:NJ,1:2) )
!    
!  ALLOCATE( QADV_XY_UV_U(0:NI,1:NJ,1:2) , QDIF_XY_UV_U(0:NI,1:NJ,1:2) )
!  ALLOCATE( QADV_XY_UV_V(1:NI,0:NJ,1:2) , QDIF_XY_UV_V(1:NI,0:NJ,1:2) )
!    
!  ALLOCATE( TSUV(1:NI,1:NJ,1:2) ) ; TSUV = 0.0
!    
!  ALLOCATE( QADV_Z_UV_W(1:NI,1:NJ,1:2,0:NK) , QDIF_Z_UV_W(1:NI,1:NJ,1:2,0:NK) )
!    
!    
!  WRITE(*,'( )')
!    
!  WRITE(*,'("Initialize grid system... ", $ )')
!  CALL MR_INIT_GRID_SYS( TRIM(FILE_XMDF) , ERROR , ERRMSG )
!  IF( ERROR < 0 ) THEN
!    WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
!    STOP
!  ELSE
!    WRITE(*,'("Done! ")')
!  END IF
!    
!  WRITE(*,'("Initialize curved geometries... ", $ )')
!  CALL MR_INIT_CURVED_GEOS( TRIM(FILE_XMDF) , ERROR , ERRMSG )
!  IF( ERROR < 0 ) THEN
!    WRITE(*,'(/,2X, A ,"!")') TRIM(ERRMSG)
!    STOP
!  ELSE
!    WRITE(*,'("Done! ")')
!  END IF
!  
! ! PRINT'("J11 : ")'
! ! do j = nj, 0, -1
! !   print'( <ni>(35x," ",f10.5," ",10X," ",:,1x) )', ( jvv(i,j,1,1), i=1,ni )
! !   if(j/=0) then
! !     print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', juu(0,j,1,1), ( (juv(i,j,1,1), juu(i,j,1,1)), i=1,ni )
! !   end if
! ! end do
! ! PRINT'("J12 : ")'
! ! do j = nj, 0, -1
! !   print'( <ni>(35x," ",f10.5," ",10X," ",:,1x) )', ( jvv(i,j,1,2), i=1,ni )
! !   if(j/=0) then
! !     print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', juu(0,j,1,2), ( (juv(i,j,1,2), juu(i,j,1,2)), i=1,ni )
! !   end if
! ! end do
! ! PRINT'("J21 : ")'
! ! do j = nj, 0, -1
! !   print'( <ni>(35x," ",f10.5," ",10X," ",:,1x) )', ( jvv(i,j,2,1), i=1,ni )
! !   if(j/=0) then
! !     print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', juu(0,j,2,1), ( (juv(i,j,2,1), juu(i,j,2,1)), i=1,ni )
! !   end if
! ! end do
! ! PRINT'("J22 : ")'
! ! do j = nj, 0, -1
! !   print'( <ni>(35x," ",f10.5," ",10X," ",:,1x) )', ( jvv(i,j,2,2), i=1,ni )
! !   if(j/=0) then
! !     print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', juu(0,j,2,2), ( (juv(i,j,2,2), juu(i,j,2,2)), i=1,ni )
! !   end if
! ! end do
! !!***
!  !PRINT'("J11 : ")'
!  !do j = nj, 0, -1
!  !  print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', joo(0,j,1,1), ( (jvv(i,j,1,1), joo(i,j,1,1)), i=1,ni )
!  !  if(j/=0) then
!  !    print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', juu(0,j,1,1), ( (juv(i,j,1,1), juu(i,j,1,1)), i=1,ni )
!  !  end if
!  !end do
!  !PRINT'("J12 : ")'
!  !do j = nj, 0, -1
!  !  print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', joo(0,j,1,2), ( (jvv(i,j,1,2), joo(i,j,1,2)), i=1,ni )
!  !  if(j/=0) then
!  !    print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', juu(0,j,1,2), ( (juv(i,j,1,2), juu(i,j,1,2)), i=1,ni )
!  !  end if
!  !end do
!  !PRINT'("J21 : ")'
!  !do j = nj, 0, -1
!  !  print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', joo(0,j,2,1), ( (jvv(i,j,2,1), joo(i,j,2,1)), i=1,ni )
!  !  if(j/=0) then
!  !    print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', juu(0,j,2,1), ( (juv(i,j,2,1), juu(i,j,2,1)), i=1,ni )
!  !  end if
!  !end do
!  !PRINT'("J22 : ")'
!  !do j = nj, 0, -1
!  !  print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', joo(0,j,2,2), ( (jvv(i,j,2,2), joo(i,j,2,2)), i=1,ni )
!  !  if(j/=0) then
!  !    print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', juu(0,j,2,2), ( (juv(i,j,2,2), juu(i,j,2,2)), i=1,ni )
!  !  end if
!  !end do
!  
! ! PRINT'("I11 : ")'
! ! do j = nj, 0, -1
! !   print'( <ni>(35x," ",f10.5," ",10X," ",:,1x) )', ( ivv(i,j,1,1), i=1,ni )
! !   if(j/=0) then
! !     print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', iuu(0,j,1,1), ( (iuv(i,j,1,1), iuu(i,j,1,1)), i=1,ni )
! !   end if
! ! end do
! ! PRINT'("I12 : ")'
! ! do j = nj, 0, -1
! !   print'( <ni>(35x," ",f10.5," ",10X," ",:,1x) )', ( ivv(i,j,1,2), i=1,ni )
! !   if(j/=0) then
! !     print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', iuu(0,j,1,2), ( (iuv(i,j,1,2), iuu(i,j,1,2)), i=1,ni )
! !   end if
! ! end do
! ! PRINT'("I21 : ")'
! ! do j = nj, 0, -1
! !   print'( <ni>(35x," ",f10.5," ",10X," ",:,1x) )', ( ivv(i,j,2,1), i=1,ni )
! !   if(j/=0) then
! !     print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', iuu(0,j,2,1), ( (iuv(i,j,2,1), iuu(i,j,2,1)), i=1,ni )
! !   end if
! ! end do
! ! PRINT'("I22 : ")'
! ! do j = nj, 0, -1
! !   print'( <ni>(35x," ",f10.5," ",10X," ",:,1x) )', ( ivv(i,j,2,2), i=1,ni )
! !   if(j/=0) then
! !     print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', iuu(0,j,2,2), ( (iuv(i,j,2,2), iuu(i,j,2,2)), i=1,ni )
! !   end if
! ! end do
! !!***
!  !PRINT'("I11 : ")'
!  !do j = nj, 0, -1
!  !  print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', ioo(0,j,1,1), ( (ivv(i,j,1,1), ioo(i,j,1,1)), i=1,ni )
!  !  if(j/=0) then
!  !    print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', iuu(0,j,1,1), ( (iuv(i,j,1,1), iuu(i,j,1,1)), i=1,ni )
!  !  end if
!  !end do
!  !PRINT'("I12 : ")'
!  !do j = nj, 0, -1
!  !  print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', ioo(0,j,1,2), ( (ivv(i,j,1,2), ioo(i,j,1,2)), i=1,ni )
!  !  if(j/=0) then
!  !    print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', iuu(0,j,1,2), ( (iuv(i,j,1,2), iuu(i,j,1,2)), i=1,ni )
!  !  end if
!  !end do
!  !PRINT'("I21 : ")'
!  !do j = nj, 0, -1
!  !  print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', ioo(0,j,2,1), ( (ivv(i,j,2,1), ioo(i,j,2,1)), i=1,ni )
!  !  if(j/=0) then
!  !    print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', iuu(0,j,2,1), ( (iuv(i,j,2,1), iuu(i,j,2,1)), i=1,ni )
!  !  end if
!  !end do
!  !PRINT'("I22 : ")'
!  !do j = nj, 0, -1
!  !  print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', ioo(0,j,2,2), ( (ivv(i,j,2,2), ioo(i,j,2,2)), i=1,ni )
!  !  if(j/=0) then
!  !    print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', iuu(0,j,2,2), ( (iuv(i,j,2,2), iuu(i,j,2,2)), i=1,ni )
!  !  end if
!  !end do
!  
!  pause
!  
!  CALL MR_INIT_FIELD_VARS_N_ACTIVITY
!  
!  
!  
!  WRITE(*,'( )')
!  
!  print'("Start computing:")'
!  
!  where( activity == noactive )
!    zb = huge(zb)
!     h = epsilon(h)
!  end where
!  
!  t = 0.0
!  
!  do its = 1 , ntss
!  
!    h0 = h
!    uva0 = uva
!    
!    print'("***********************************")'
!    
!    print'("its=",i5)', its
!  
!    call mr_updt_tbuv
!    print'("TBUV : ")'
!    do j = nj, 1, -1
!      print'(<ni>(35X,"(",es10.3,",",es10.3,")",:,1X))', ( TUVR*tbuv(i,j,1:2) , i=1,ni )
!    end do
!  
!  !! QADV and QDIF xy
!  !  do k = 1, nk
!  !    
!  !    uvt = juv .mruvtfm. uv(:,:,1:2, k )
!  !    
!  !    call mr_interp_xy_uv_v_to_get_u_at_v( ni , nj , uv(:,:,1:2, k ) , vvt(:,:,1) )
!  !    call mr_interp_xy_uv_v_by_linear( ni , nj , uv(:,:,1:2, k ) , vvt(:,:,2) )
!  !    vvt = jvv .mruvtfm. vvt
!  !    call mr_calc_qadv_n_qdif_xy_uv_u( ni , nj , uvt , vvt , rb , u(:,:, k ) , qadv_xy_uv_u , ekxy , vxyu(:,:, k ) , qdif_xy_uv_u )
!  !    
!  !    call mr_interp_xy_uv_u_by_linear( ni , nj , uv(:,:,1:2, k ) , uut(:,:,1) )
!  !    call mr_interp_xy_uv_u_to_get_v_at_u( ni , nj , uv(:,:,1:2, k ) , uut(:,:,2) )
!  !    uut = juu .mruvtfm. uut
!  !    call mr_calc_qadv_n_qdif_xy_uv_v( ni , nj , uvt , uut , rb , v(:,:, k ) , qadv_xy_uv_v , ekxy , vxyv(:,:, k ) , qdif_xy_uv_v )
!  !    
!  !    print'("k=",i3)', k
!  !    print'("QADV : ")'
!  !    do j = nj, 0, -1
!  !      print'( <ni>(35X,"(",es10.3,",",es10.3,")",:,1X) )', ( qadv_xy_uv_v(i,j,1:2), i=1,ni )
!  !      if(j/=0) then
!  !        print'( <ni111>(10X,"(",es10.3,",",es10.3,")",25X,:,1X) )', ( qadv_xy_uv_u(i,j,1:2), i=0,ni )
!  !      end if
!  !    end do
!  !    print'("QDIF : ")'
!  !    do j = nj, 0, -1
!  !      print'( <ni>(35X,"(",f10.5,",",f10.5,")",:,1X) )', ( qdif_xy_uv_v(i,j,1:2), i=1,ni )
!  !      if(j/=0) then
!  !        print'( <ni111>(10X,"(",f10.5,",",f10.5,")",25X,:,1X) )', ( qdif_xy_uv_u(i,j,1:2), i=0,ni )
!  !      end if
!  !    end do
!  !    
!  !  end do
!    
!  !! QADV and QDIF z
!  !  call mr_calc_qadv_n_qdif_z_uv_w( ni , nj , nk , uv , rb , w , qadv_z_uv_w , ekz , vzw , qdif_z_uv_w , tbuv , tsuv )
!  !  do k = 0, nk
!  !    print'("k=",i3)',k
!  !    print'("QADV : ")'
!  !    do j = nj, 1, -1
!  !      print'( <ni>(35X,"(",f10.5,",",f10.5,")",:,1X) )', ( qadv_z_uv_w(i,j,1:2, k ), i=1,ni )
!  !    end do
!  !    print'("QDIF : ")'
!  !    do j = nj, 1, -1
!  !      print'( <ni>(35X,"(",f10.5,",",f10.5,")",:,1X) )', ( qdif_z_uv_w(i,j,1:2, k ), i=1,ni )
!  !    end do
!  !  end do
!    
!  !! ALFA and BETA
!  !! block
!  !  call mr_calc_raw_hyd_d3( ni , nj , nk , hyd_d3 )
!  !  !print'("Rough HYD_D3 : ")'
!  !  !do k = 1, nk
!  !  !  print'("k=",i3)', k
!  !  !  do j = nj, 1, -1
!  !  !    print'( <ni>(35X,"(",f10.5,",",f10.5,")",:,1X) )', ( hyd_d3(i,j,1:2,k), i=1,ni )
!  !  !  end do
!  !  !end do
!  !  
!  !  call mr_calc_alfa_n_beta( ni , nj , nk , hyd_d3 , alfa , beta )
!  !  !print'("HYD_D3 : ")'
!  !  !do k = 1, nk
!  !  !  print'("k=",i3)', k
!  !  !  do j = nj, 1, -1
!  !  !    print'( <ni>(35X,"(",f10.5,",",f10.5,")",:,1X) )', ( hyd_d3(i,j,1:2,k), i=1,ni )
!  !  !  end do
!  !  !end do
!  !  print'("===================================")'
!  !  print'("ALFA : ")'
!  !  do j = nj, 1, -1
!  !    print'( <ni>(35X,"(",es10.3,",",es10.3,")",:,1X) )', ( alfa(i,j,1:2), i=1,ni )
!  !  end do
!  !  print'("BETA : ")'
!  !  do j = nj, 1, -1
!  !    print'( <ni>(35X,"(",es10.3,",",es10.3,")",:,1X) )', ( beta(i,j,1:2), i=1,ni )
!  !  end do
!  !  print'("===================================")'
!  !  
!  !  call mr_calc_d1( ni , nj , zs , alfa , beta , d1 )
!  !  call mr_calc_a1_b1_c1( ni , nj , alfa , a1 , b1 , c1 )
!  !  zs = mr_tdma1( ni , nj , a1 , b1 , c1 , d1 )
!  !  do j = nj, 1, -1
!  !    print'(  ( A10 ,",",f10.5,",",f10.5," : "," ",es10.3," ",10X," ",:,1X),   &
!  !    & <ni222>(f10.5,",",f10.5,",",f10.5," : "," ",es10.3," ",10X," ",:,1X),   &
!  !    &        (f10.5,",",f10.5,",", A10 ," : "," ",es10.3," ",10X," ",:,1X) )',   &
!  !      "-------",  b1( 1,j),  c1( 1,j), d1( 1,j),   &
!  !    ( ( a1( i,j), b1( i,j),  c1( i,j), d1( i,j)), i=2,ni-1 ),   &
!  !        a1(ni,j), b1(ni,j), "-------", d1(ni,j)
!  !  end do
!  !  print'("ZS : ")'
!  !  do j = nj, 1, -1
!  !    print'( <ni>(35X," ",es10.3," ",10X," ",:,1X) )', ( zs(i,j), i=1,ni )
!  !  end do
!  !    
!  !  print*
!  !    
!  !  call mr_calc_d2( ni , nj , zs , beta , d2 )
!  !  call mr_calc_a2_b2_c2( ni , nj , alfa , a2 , b2 , c2 )
!  !  zs = mr_tdma2( ni , nj , a2 , b2 , c2 , d2 )
!  !  do j = nj, 1, -1
!  !    if(j==1) then
!  !      print'( <ni>( A10 ,",",f10.5,",",f10.5," : "," ",es10.3," ",10X," ",:,1X) )', ( ( "-------", b2(i, 1),  c2(i, 1), d2(i, 1) ), i=1,ni )
!  !    else if(j<nj) then
!  !      print'( <ni>(f10.5,",",f10.5,",",f10.5," : "," ",es10.3," ",10X," ",:,1X) )', ( (  a2(i, j), b2(i, j),  c2(i, j), d2(i, j) ), i=1,ni )
!  !    else
!  !      print'( <ni>(f10.5,",",f10.5,",", A10 ," : "," ",es10.3," ",10X," ",:,1X) )', ( (  a2(i,nj), b2(i,nj), "-------", d2(i,nj) ), i=1,ni )
!  !    end if
!  !  end do
!  !  print'("ZS : ")'
!  !  do j = nj, 1, -1
!  !    print'( <ni>(35X," ",es10.3," ",10X," ",:,1X) )', ( zs(i,j), i=1,ni )
!  !  end do
!  !  
!  !  print*
!  !  print*,"Sum ZS = ",sum(zs(1:ni,1:nj))
!  !  print*
!  !
!  !  
!  !  call mr_interp_xy_zs_u( ni , nj , zs , zsu )
!  !  call mr_interp_xy_zs_v( ni , nj , zs , zsv )
!  !    
!  !  PRINT'("ZS, ZSU & ZSV : ")'
!  !  do j = nj, 0, -1
!  !    print'( <ni>(35x," ",f10.5," ",10X," ",:,1x) )', ( zsv(i,j), i=1,ni )
!  !    if(j/=0) then
!  !      print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', zsu(0,j), ( (zs(i,j), zsu(i,j)), i=1,ni )
!  !    end if
!  !  end do
!  !  
!  !  call mr_calc_uva( ni , nj , zsU , zsv , alfa , beta , uva )
!  !    
!  !  call mr_interp_xy_uv_u_by_rcprac( ni , nj , zs , alfa , beta , ua )
!  !  call mr_interp_xy_uv_v_by_rcprac( ni , nj , zs , alfa , beta , va )
!  !  print'("UVA, UA & VA : ")'
!  !  do j = nj, 0, -1
!  !    print'( <ni>(35x," ",10x," ",f10.5," ",:,1x) )', ( va(i,j), i=1,ni )
!  !    if(j/=0) then
!  !      print'( (10x," ",f10.5," ",10x,"   "), <ni>("(",f10.5,",",f10.5,")",1x,10x," ",f10.5," ",10x,"   ") )', ua(0,j), ( (uva(i,j,1:2), ua(i,j)), i=1,ni )
!  !    end if
!  !  end do
!  !  
!  !  !print'("===================================")'
!  !  
!  !  call mr_calc_hyd_a3_b3_c3( ni , nj , nk , hyd_a3 , hyd_b3 , hyd_c3 )
!  !  !do k = 1, nk
!  !  !  print'("k=",i3)', k
!  !  !  do j = nj, 1, -1
!  !  !    if(k==1) then
!  !  !      print'( <ni>(  A10 ,",",es10.3,",",es10.3," : ","(",es10.3,",",es10.3,")",:,1X) )', (      "-------", hyd_b3(i,j, 1), hyd_c3(i,j, 1), hyd_d3(i,j,1:2, 1), i=1,ni )
!  !  !    else if(k<nk) then
!  !  !      print'( <ni>(es10.3,",",es10.3,",",es10.3," : ","(",es10.3,",",es10.3,")",:,1X) )', ( hyd_a3(i,j, k), hyd_b3(i,j, k), hyd_c3(i,j, k), hyd_d3(i,j,1:2, k), i=1,ni )
!  !  !    else
!  !  !      print'( <ni>(es10.3,",",es10.3,",",  A10 ," : ","(",es10.3,",",es10.3,")",:,1X) )', ( hyd_a3(i,j,nk), hyd_b3(i,j,nk),      "-------", hyd_d3(i,j,1:2,nk), i=1,ni )
!  !  !    end if
!  !  !  end do
!  !  !end do
!  !  
!  !  !print*
!  !  
!  !  uv = mr_tdma3_uv( ni , nj , nk , hyd_a3 , hyd_b3 , hyd_c3 , hyd_d3 )
!  !  !print'("UV Deviation : ")'
!  !  !do k = 1, nk
!  !  !  print'("k=",i3)', k
!  !  !  do j = nj, 1, -1
!  !  !    print'( <ni>(35X,"(",es10.3,",",es10.3,")",:,1X) )', ( uv(i,j,1:2,k), i=1,ni )
!  !  !  end do
!  !  !end do
!  !  !
!  !  !print*
!  !  
!  !  do k = 1 , nk
!  !    call mr_interp_xy_uv_u_by_linear( ni , nj , uv(:,:,1:2, k ) , u(:,:, k ) )
!  !    call mr_interp_xy_uv_v_by_linear( ni , nj , uv(:,:,1:2, k ) , v(:,:, k ) )
!  !  end do
!  !  call mr_calc_w( ni , nj , nk , u , v , w )
!  !  !print'("W : ")'
!  !  !do k = 0, nk
!  !  !  print'("k=",i3)', k
!  !  !  do j = nj, 1, -1
!  !  !    print'( <ni>(35X," ",es10.3," ",10X" ",:,1X) )', ( w(i,j,k), i=1,ni )
!  !  !  end do
!  !  !end do
!  !  
!  !  do k = 1 , nk
!  !    u(:,:, k ) = u(:,:, k ) + ua(:,:)
!  !    v(:,:, k ) = v(:,:, k ) + va(:,:)
!  !    
!  !    uv(:,:,1:2, k ) = uv(:,:,1:2, k ) + uva(:,:,1:2)
!  !    
!  !  end do
!  !  !print'("UV, U & V : ")'
!  !  !do k = 1, nk
!  !  !  print'("k=",i3)', k
!  !  !  do j = nj, 0, -1
!  !  !    print'( <ni>(35x," ",10x," ",f10.5," ",:,1x) )', ( v(i,j,k), i=1,ni )
!  !  !    if(j/=0) then
!  !  !      print'( (10x," ",f10.5," ",10x,"   "), <ni>("(",f10.5,",",f10.5,")",1x,10x," ",f10.5," ",10x,"   ") )', u(0,j,k), ( (uv(i,j,1:2,k), u(i,j,k)), i=1,ni )
!  !  !    end if
!  !  !  end do
!  !  !end do
!  !  
!  !! end block
!  
!    call mr_updt_hyd
!    !if( mod(its,its_output) == 0 ) then
!    !  call mr_updt_ww
!    !end if
!    
!    call mr_updt_h
!    print'("H, HU & HV : ")'
!    do j = nj, 0, -1
!      print'( <ni>(35x," ",f10.5," ",10X," ",:,1x) )', ( hv(i,j), i=1,ni )
!      if(j/=0) then
!        print'( (10x," ",f10.5," ",10x,"   "), <ni>(" ",f10.5," ",10X," ",1x,10x," ",f10.5," ",10x,"   ") )', hu(0,j), ( (h(i,j), hu(i,j)), i=1,ni )
!      end if
!    end do
!    
!    call mr_updt_vzw
!    !print'("vzw : ")'
!    !do k = 0, nk
!    !  print'("k=",i3)',k
!    !  do j = nj, 1, -1
!    !    print'( <ni>(35X," ",es10.3," ",10X," ",:,1X) )', ( vzw(i,j,k), i=1,ni )
!    !  end do
!    !end do
!    
!    print*
!    
!    
!    t = t + dt
!    if( mod(its,its_output) == 0 ) then
!      continue
!    end if
!    
!    
!    
!    if( its>10 .and. maxval(abs(uva(1:ni,1:nj,1:2)-uva0(1:ni,1:nj,1:2)))<epsilon(uva) .and. maxval(abs(h(1:ni,1:nj)-h0(1:ni,1:nj)))<epsilon(h) ) then
!      exit
!    end if
!    
!    !if( maxval(abs(h-h0))<epsilon(h) ) then
!    !  exit
!    !end if
!    
!    !if( maxval(abs(uva))<epsilon(uva)) then
!    !  exit
!    !end if
!    
!    !pause
!    
!  end do
!  
!  !print'(f10.8)',epsilon(h)
!  
!end program main