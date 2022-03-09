module mod_io
  use mpi_f08
  use mod_common, only: rp,MPI_REAL_RP,ierr
  private
  public load, load_scalar 
contains
  subroutine load(io,filename,comm,myid,ng,nh,lo,hi,u,v,w,p,vof,us,vs,ws,tmp,sca,time,istep,pth,dpthdt_n)
    !
    ! reads/writes a restart file
    !
    implicit none
    character(len=1), intent(in) :: io
    character(len=*), intent(in) :: filename
    type(MPI_COMM)  , intent(in) :: comm
    integer         , intent(in) :: myid
    integer , intent(in), dimension(3) :: ng,nh,lo,hi
    real(rp), intent(inout), dimension(lo(1)-nh(1):,lo(2)-nh(2):,lo(3)-nh(3):) :: u,v,w,p,vof, &
                                                                                  us,vs,ws,tmp,sca
    real(rp), intent(inout) :: time,pth,dpthdt_n!,dt
    integer , intent(inout) :: istep
    !real(rp), intent(out)    :: time,dt,pth,dpthdt_n
    !integer, intent(out)     :: istep
    !real(rp), dimension(5)  :: fldinfo 
    type(MPI_FILE) :: fh
    !integer :: nreals_myid
    integer(kind=MPI_OFFSET_KIND) :: filesize,disp,good
    select case(io)
    case('r')
      call MPI_FILE_OPEN(comm, filename, &
           MPI_MODE_RDONLY, MPI_INFO_NULL,fh,ierr)
      !
      ! check file size first
      !
      call MPI_FILE_GET_SIZE(fh,filesize,ierr)
      good = (product(int(ng(:),MPI_OFFSET_KIND)))*(storage_size(1._rp)/8)
      if(filesize /= good) then
        if(myid == 0) print*, ''
        if(myid == 0) print*, '*** Simulation aborted due a checkpoint file with incorrect size ***'
        if(myid == 0) print*, '    file: ', filename, ' | expected size: ', good, '| actual size: ', filesize
        call MPI_FINALIZE(ierr)
        error stop
      end if
      !
      ! read
      !
      disp = 0_MPI_OFFSET_KIND
      if     (filename == 'data/fldu.bin')   then
      call io_field('r',fh,ng,nh,lo,hi,disp,u)
      if(myid == 0) print*, 'file:', filename, 'read'


      elseif (filename == 'data/fldv.bin')   then
      call io_field('r',fh,ng,nh,lo,hi,disp,v)
      if(myid == 0) print*, 'file:', filename, 'read'


      elseif (filename == 'data/fldw.bin')   then
      call io_field('r',fh,ng,nh,lo,hi,disp,w)
      if(myid == 0) print*, 'file:', filename, 'read'


      elseif (filename == 'data/fldp.bin')   then
      call io_field('r',fh,ng,nh,lo,hi,disp,p)
      if(myid == 0) print*, 'file:', filename, 'read'


      elseif (filename == 'data/fldvof.bin') then
      call io_field('r',fh,ng,nh,lo,hi,disp,vof)
      if(myid == 0) print*, 'file:', filename, 'read'


      elseif (filename == 'data/fldug.bin')  then
      call io_field('r',fh,ng,nh,lo,hi,disp,us)
      if(myid == 0) print*, 'file:', filename, 'read'


      elseif (filename == 'data/fldvg.bin')  then
      call io_field('r',fh,ng,nh,lo,hi,disp,vs)
      if(myid == 0) print*, 'file:', filename, 'read'


      elseif (filename == 'data/fldwg.bin')  then
      call io_field('r',fh,ng,nh,lo,hi,disp,ws)
      if(myid == 0) print*, 'file:', filename,  'read'


      elseif (filename == 'data/fldtmp.bin') then
      call io_field('r',fh,ng,nh,lo,hi,disp,tmp)
      if(myid == 0) print*, 'file:', filename,  'read'


      elseif (filename == 'data/fldsca.bin') then
      call io_field('r',fh,ng,nh,lo,hi,disp,sca)     
      if(myid == 0) print*, 'file:', filename,  'read'


      endif

      call MPI_FILE_SET_VIEW(fh,disp,MPI_REAL_RP,MPI_REAL_RP,'native',MPI_INFO_NULL,ierr)
      !nreals_myid = 0
      !if(myid == 0) nreals_myid = 2
      !call MPI_FILE_READ(fh,fldinfo,nreals_myid,MPI_REAL_RP,MPI_STATUS_IGNORE,ierr)
      call MPI_FILE_CLOSE(fh,ierr)
      !call MPI_BCAST(fldinfo,2,MPI_REAL_RP,0,comm,ierr)
      !call MPI_BCAST(fldinfo,2,MPI_REAL_RP,0,comm,ierr)
      !time  = fldinfo(1)
      !istep = nint(fldinfo(2))
      !dt    = fldinfo(3)
      !pth   = fldinfo(4)
      !dpthdt_n = fldinfo(5)
    case('w')
      !
      ! write
      !
      call MPI_FILE_OPEN(comm, filename                 , &
           MPI_MODE_CREATE+MPI_MODE_WRONLY, MPI_INFO_NULL,fh,ierr)
      filesize = 0_MPI_OFFSET_KIND
      call MPI_FILE_SET_SIZE(fh,filesize,ierr)
      disp = 0_MPI_OFFSET_KIND

      disp = 0_MPI_OFFSET_KIND                           
      
 
      if     (filename == 'data/fldu_o.bin')   then
      call io_field('w',fh,ng,nh,lo,hi,disp,u)
      if(myid == 0) print*, 'file:', filename, 'write'
                                                
                                                
      elseif (filename == 'data/fldv_o.bin')   then
      call io_field('w',fh,ng,nh,lo,hi,disp,v)
      if(myid == 0) print*, 'file:', filename, 'write'
                                                
                                                
      elseif (filename == 'data/fldw_o.bin')   then
      call io_field('w',fh,ng,nh,lo,hi,disp,w)
      if(myid == 0) print*, 'file:', filename, 'write'
                                                
                                                
      elseif (filename == 'data/fldp_o.bin')   then
      call io_field('w',fh,ng,nh,lo,hi,disp,p)
      if(myid == 0) print*, 'file:', filename, 'write'
                                                
                                                
      elseif (filename == 'data/fldvof_o.bin') then
      call io_field('w',fh,ng,nh,lo,hi,disp,vof)
      if(myid == 0) print*, 'file:', filename, 'write'
                                                
                                                
      elseif (filename == 'data/fldug_o.bin')  then
      call io_field('w',fh,ng,nh,lo,hi,disp,us)
      if(myid == 0) print*, 'file:', filename, 'write'
                                                
                                                      
      elseif (filename == 'data/fldvg_o.bin')  then          
      call io_field('w',fh,ng,nh,lo,hi,disp,vs)             
      if(myid == 0) print*, 'file:', filename, 'write'        
                                                      
                                                      
      elseif (filename == 'data/fldwg_o.bin')  then          
      call io_field('w',fh,ng,nh,lo,hi,disp,ws)             
      if(myid == 0) print*, 'file:', filename,  'write'       
                                                      
                                                      
      elseif (filename == 'data/fldtmp_o.bin') then
      call io_field('w',fh,ng,nh,lo,hi,disp,tmp)
      if(myid == 0) print*, 'file:', filename,  'write'
                                                
                                                
      elseif (filename == 'data/fldsca_o.bin') then
      call io_field('w',fh,ng,nh,lo,hi,disp,sca)     
      if(myid == 0) print*, 'file:', filename,  'write'

      
      endif      
              
      call MPI_FILE_SET_VIEW(fh,disp,MPI_REAL_RP,MPI_REAL_RP,'native',MPI_INFO_NULL,ierr)
      !fldinfo = [time,1._rp*istep,dt,pth,dpthdt_n]
      !nreals_myid = 0
      !if(myid == 0) nreals_myid = 2
      !call MPI_FILE_WRITE(fh,fldinfo,nreals_myid,MPI_REAL_RP,MPI_STATUS_IGNORE,ierr)
      call MPI_FILE_CLOSE(fh,ierr)
    end select
  end subroutine load

  subroutine load_scalar(io,filename,myid,pth,dpthdt_n,time,istep)
    !
    implicit none
    integer         , intent(in)    :: myid
    character(len=1), intent(in   ) :: io
    character(len=*), intent(in   ) :: filename
    real(8)         , intent(inout) :: pth,dpthdt_n
    integer         , intent(inout) :: istep
    real(8)         , intent(inout) :: time
    integer :: fh
    ! integer(kind=MPI_OFFSET_KIND) :: filesize,disp,good
    ! integer(8), dimension(3) :: ng
    ! integer(8) :: lenr
    !
    select case(io)
    case('r')
      open(88,file=filename,status='old',action='read')
      read(88,*) pth,dpthdt_n,time,istep
      close(88)
    case('w')
      if(myid.eq.0) then
        open(88,file=filename)
        !write(88,'(3E15.7, 1I9.8)') pth,dpthdt_n,time,istep
        write(88,'(3E15.7, 1I12.11)') pth,dpthdt_n,time,istep
        close(88)
      endif
    end select
    !
    return
  end subroutine load_scalar

  subroutine io_field(io,fh,ng,nh,lo,hi,disp,var)
    implicit none
    character(len=1), intent(in)                 :: io
    type(MPI_FILE)  , intent(in)                 :: fh
    integer         , intent(in), dimension(3)   :: ng,nh,lo,hi
    integer(kind=MPI_OFFSET_KIND), intent(inout) :: disp
    real(rp), intent(inout), dimension(lo(1)-nh(1):,lo(2)-nh(2):,lo(3)-nh(3):) :: var
    integer , dimension(3) :: n
    integer , dimension(3) :: sizes,subsizes,starts
    type(MPI_DATATYPE) :: type_glob,type_loc
    !
    n(:)        = hi(:)-lo(:)+1
    sizes(:)    = ng(:)
    subsizes(:) = n(:)
    starts(:)   = lo(:) - 1 ! starts from 0
    call MPI_TYPE_CREATE_SUBARRAY(3,sizes,subsizes,starts,MPI_ORDER_FORTRAN,MPI_REAL_RP,type_glob,ierr)
    call MPI_TYPE_COMMIT(type_glob,ierr)
    sizes(:)    = n(:) + 2*nh(:)
    subsizes(:) = n(:)
    starts(:)   = 0 + nh(:)
    call MPI_TYPE_CREATE_SUBARRAY(3,sizes,subsizes,starts,MPI_ORDER_FORTRAN,MPI_REAL_RP,type_loc ,ierr)
    call MPI_TYPE_COMMIT(type_loc,ierr)
    select case(io)
    case('r')
      call MPI_FILE_SET_VIEW(fh,disp,MPI_REAL_RP,type_glob,'native',MPI_INFO_NULL,ierr)
      call MPI_FILE_READ_ALL(fh,var,1,type_loc,MPI_STATUS_IGNORE,ierr)
    case('w')
      call MPI_FILE_SET_VIEW(fh,disp,MPI_REAL_RP,type_glob,'native',MPI_INFO_NULL,ierr)
      call MPI_FILE_WRITE_ALL(fh,var,1,type_loc,MPI_STATUS_IGNORE,ierr)
    end select
    disp = disp+product(int(ng(:),MPI_OFFSET_KIND))*(storage_size(1._rp)/8)
    call MPI_TYPE_FREE(type_glob,ierr)
    call MPI_TYPE_FREE(type_loc ,ierr)
  end subroutine io_field
end module mod_io
