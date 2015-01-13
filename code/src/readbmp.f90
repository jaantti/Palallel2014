subroutine READBMP(image, iheight, iwidth, filename)
    implicit none
    integer Maxwidth,Maxheight,irec,iwidth,iheight,i,j,ipad
    parameter(Maxwidth=92,Maxheight=112)
    character header(54),ch
    character(len = 100) :: filename
    real(kind=8) image(Maxheight,Maxwidth)
    
    !print *, filename
    !open(1,file='img.bmp',form='unformatted',access='direct',recl=1)
    open(1,file=filename,form='unformatted',access='direct',recl=1)

    do irec=1,54
        read(1,rec=irec) header(irec)
    end do

    if(ichar(header(11)).ne.54.or.ichar(header(29)).ne.24.or.ichar(header(31)).ne.0) then
        print*,'sorry, can not handle this file'
    end if

    ! get image height and width
    iheight=ichar(header(23))+256*(ichar(header(24))+256*(ichar(header(25))+256*ichar(header(26))))
    iwidth=ichar(header(19))+256*(ichar(header(20))+256*(ichar(header(21))+256*ichar(header(22))))
    !print*,'iheight,iwidth=',iheight,iwidth

    ipad=(Maxwidth-iwidth)*3-((Maxwidth-iwidth)*3/4)*4
    irec=54
    do i=1,iheight
        do j=1,iwidth
            irec=irec+1
            read(1,rec=irec) ch
            !image(i,j,3)=ichar(ch)
            irec=irec+1
            read(1,rec=irec) ch
            !image(i,j,2)=ichar(ch)
            irec=irec+1
            read(1,rec=irec) ch
            image(i,j)=ichar(ch)
        end do
        irec=irec+ipad
    end do
    !print*, 'readbmp'
    !print*, image
end subroutine