c Code from John Woodhouse to calculate surface wave sensitivity kernels 
c for ABSOLUTE perturbations in velocity, based on either PREM or AK135
      parameter (mndim=222)
      real lcon,ncon
      common/modl1/n,nic,noc,moho,nsl,ifanis,r(mndim)
     1          ,rho(mndim),qrho(3,mndim),g(mndim),ell(mndim),eta(mndim)
      common/modl2/acon(mndim),qacon(3,mndim),ccon(mndim),qccon(3,mndim)
     1            ,lcon(mndim),qlcon(3,mndim),ncon(mndim),qncon(3,mndim)
     2            ,fcon(mndim),qfcon(3,mndim)
      common/modl3/qshear(mndim),qkappa(mndim)
      common/nond/rn,wn,vn,gn,rhobar
c
      common/tape/rtap(mndim),indsfr(330),indtor(300)
     1    ,kntsfr(330),knttor(300),knts,kntt,lu1,nbytes,lmaxs,lmaxt
     2            ,numbs,numbt
c
      common/mode/nord,jcom,lord,wcom,qbar,cgp,avert,ahor,phis
     1          ,eif(mndim,6)
      dimension u(mndim),up(mndim),v(mndim),vp(mndim),p(mndim),pp(mndim)
      equivalence (eif(1,1),u(1)),(eif(1,2),up(1)),(eif(1,3),v(1))
     1           ,(eif(1,4),vp(1)),(eif(1,5),p(1)),(eif(1,6),pp(1))
      character*1 mtyp,mtypold,ans
      dimension plot(mndim,6),plot1(mndim,3)
      character*80 file
c
      write(*,*) 'do you want ak135 model ? y [n]'
      read(*,'(a)') ans
      if(ans.eq.'y') then
      open(15,file='ak135f.222')
      lu1=1
      call openfl(lu1,'AK135F222.BIN'
     #,1,0,0,istat,5364)
      else
      open(15,file='foanis05.222')
      lu1=1
      call openfl(lu1,'PREM222.BIN'
     #,1,0,0,istat,5364)
      endif
      call modl(15,1110,rn,wn,vn,gn,rhobar)
      write(*,*) rn,wn,vn,gn,rhobar
      call setup
      write(6,1008) numbs,lmaxs,numbt,lmaxt
 1008 format(1x,i6,' spheroidal modes.  lmax =',i4/
     1       1x,i6,' toroidal modes.    lmax =',i4)
c

  10  write(*,*) 'do you want anisotropic plots ? y [n]'
      read(*,'(a)') ans
      iso=0
      if(ans.eq.'y') iso=1
      idiff=0
      if(iso.eq.0) then
      write(*,*) 'do you want differential plots ? y [n]'
      read(*,'(a)') ans
      if(ans.eq.'y') idiff=1
      endif

      write(6,"('Type mode id'/'*** x ***')")
      read(5,"(i3,1x,a1,1x,i3)") nordi,mtyp,lordi
      if(mtyp.eq.'t'.or.mtyp.eq.'T') then
        irec=indtor(lordi)+nordi
        mtyp='T'
      else if(mtyp.eq.'s'.or.mtyp.eq.'S') then
        irec=indsfr(lordi+1)+nordi
        mtyp='S'
      else
        stop
      endif
      call bffi(1,1,nord,nbytes,j,m,irec)
      if(n.ne.mndim) call spread(n)
      write(6,*) nord,jcom,lord,wcom
      if(nord.ne.nordi.or.lord.ne.lordi) goto 10
      sfl3=sqrt(float(lord*(lord+1)))
 
      write(*,*) 'eigenfunctions ---> 1, kernels ---> 2'
      read(*,*) iptyp
      if(mtyp.eq.'S') then
        if(iptyp.eq.1) then
          do i=1,n
            plot(i,1)=u(i)
            plot(i,2)=sfl3*v(i)
          enddo
          nstart=1
          nplot=2
        else if(iptyp.eq.2) then
          if(iso.eq.0) then
          call kernels(iso,1,plot(1,1))
          call kernels(iso,2,plot(1,2))
          call kernels(iso,3,plot(1,3))
          nstart=1
          nplot=3
          else
          call kernels(iso,1,plot(1,1))
          call kernels(iso,2,plot(1,2))
          call kernels(iso,3,plot(1,3))
          call kernels(iso,4,plot(1,4))
          call kernels(iso,5,plot(1,5))
          call kernels(iso,6,plot(1,6))
          nstart=1
          nplot=6
          endif
        endif
      else if(mtyp.eq.'T') then
        if(iptyp.eq.1) then
          do i=1,n
            plot(i,2)=sfl3*v(i)
          enddo
          nstart=2
          nplot=1
        else if(iptyp.eq.2) then
          if(iso.eq.0) then
          call kernels(iso,2,plot(1,2))
          call kernels(iso,3,plot(1,3))
          nstart=2
          nplot=2
          else
          call kernels(iso,3,plot(1,3))
          call kernels(iso,4,plot(1,4))
          call kernels(iso,5,plot(1,5))
          nstart=3
          nplot=3
          endif
        endif
      endif

      per=2.*3.1415926/wcom
      write(6,*) 'period and Q',per,1./qbar
      pvel=6371.*wcom/(lord+.5)
      write(*,*) 'rho ',rhobar
      write(*,*) 'group and phase velocity',cgp,pvel
      ff=1.
c kernels returns 2*dw/w for radius between 0 and 1
c the following normalization gives the derivatives for dc/c and radius between 0 and a:
      if(pvel.ne.0..and.cgp.ne.0.) ff=pvel/cgp/2./6371.
c
cb      pi=4.*atan(1.)
cb      ff=wcom*1.e6/4./pi  ! for df in microHz as in Resovsky and radius between 0 and 1 (because f=w/2pi)

      if(idiff.eq.1) then
    
      mtypold=mtyp
      write(6,"('Type mode id'/'*** x *** for second mode')")
      read(5,"(i3,1x,a1,1x,i3)") nordi,mtyp,lordi
      if(mtyp.eq.'t'.or.mtyp.eq.'T') then
        irec=indtor(lordi)+nordi
        mtyp='T'
      else if(mtyp.eq.'s'.or.mtyp.eq.'S') then
        irec=indsfr(lordi+1)+nordi
        mtyp='S'
      else
        stop
      endif
      if(mtyp.ne.mtypold) stop 'different mode types'
      call bffi(1,1,nord,nbytes,j,m,irec)
      if(n.ne.mndim) call spread(n)
      write(6,*) nord,jcom,lord,wcom
      if(nord.ne.nordi.or.lord.ne.lordi) goto 10
      sfl3=sqrt(float(lord*(lord+1)))
 
c      write(*,*) 'eigenfunctions ---> 1, kernels ---> 2'
c      read(*,*) iptyp
      if(mtyp.eq.'S') then
        if(iptyp.eq.1) then
          do i=1,n
            plot1(i,1)=u(i)
            plot1(i,2)=sfl3*v(i)
          enddo
          nstart=1
          nplot=2
        else if(iptyp.eq.2) then
          call kernels(iso,1,plot1(1,1))
          call kernels(iso,2,plot1(1,2))
          call kernels(iso,3,plot1(1,3))
          nstart=1
          nplot=3
        endif
      else if(mtyp.eq.'T') then
        if(iptyp.eq.1) then
          do i=1,n
            plot1(i,2)=sfl3*v(i)
          enddo
          nstart=2
          nplot=1
        else if(iptyp.eq.2) then
          call kernels(iso,2,plot1(1,2))
          call kernels(iso,3,plot1(1,3))
          nstart=2
          nplot=2
        endif
      endif

      per=2.*3.1415926/wcom
      write(6,*) 'period and Q',per,1./qbar
      pvel=6371.*wcom/(lord+.5)
      ff1=1.
      if(pvel.ne.0..and.cgp.ne.0.) ff1=pvel/cgp/2./6371.
      write(*,*) 'group and phase velocity',cgp,pvel

      endif

      write(*,*) 'maximum depth for ploting'
      read(*,*) dmax

      if(idiff.eq.1) then
      s1=0.
      s=0.
      m=0
      do i=1,n
      ray=6371.*r(i)
      d=6371.-ray
      if(d.lt.410.) then
      s=(ff*plot(i,2))**2+s
      s1=(ff1*plot1(i,2))**2+s1
      m=m+1
      endif
      enddo
      smean=sqrt(s/float(m))
      s1mean=sqrt(s1/float(m))
      sr=smean/s1mean
      write(*,*) m,smean,s1mean
      write(*,*) 'optimum scaling to reduce 410 upper km ',sr
      endif
	write(*,*)'sfl3',sfl3

c      open(10,file='modeplot.out')
      write(*,*) 'output filename'
      read(*,'(a)') file
      open(10,file=file)
      if(idiff.eq.0) write(10,'(1a,3f10.5)') '#',per,cgp,pvel
c      do i=1,nsl
      do i=1,n
      ray=6371.*r(i)
      d=6371.-ray
      if(d.le.dmax) then
      if(idiff.eq.0) then
      write(10,101) d,(ff*plot(i,j),j=nstart,nstart+nplot-1)
      else
      write(10,101) d,(ff*plot(i,j)-(ff1*sr*plot1(i,j))
     #,j=nstart,nstart+nplot-1)
      endif
      endif
      enddo
      close(10)
 101  format(f8.3,6e13.5)

c      xmax=-1.e30
c      xmin=1.e30
c      do i=nstart,nstart+nplot-1
c        call vextrm(plot(1,i),1,nsl,1.,xmax,xmin)
c      enddo
c      write(6,*) 'xmin,xmax',ff*xmin,ff*xmax

      goto 10
      end

c-----------------------------------------------------
      subroutine vextrm(x,inc,n,fac,xmax,xmin)
      dimension x(inc,*)
      do i=1,n
        xmax=amax1(fac*x(1,i),xmax)
        xmin=amin1(fac*x(1,i),xmin)
      enddo
      return
      end
c------------------------------------------------------
      subroutine kernels(iso,isort,vker)

c returns kernel for 2.*dw/w or q(w)

c iso = 0 isort=1 ---> vp
c         isort=2 ---> vs
c         isort=3 ---> rho
c iso = 1 isort=1 ---> vph
c         isort=2 ---> vpv
c         isort=3 ---> vsh
c         isort=4 ---> vsv
c         isort=5 ---> rho
c         isort=6 ---> eta
c iso = 2 isort=1 ---> qmu
c         isort=2 ---> qkappa

      dimension vker(*)

      parameter (mndim=222)
      real lcon,ncon
      common/modl1/n,nic,noc,moho,nsl,ifanis,r(mndim)
     1          ,rho(mndim),qrho(3,mndim),g(mndim),ell(mndim),eta(mndim)
      common/modl2/acon(mndim),qacon(3,mndim),ccon(mndim),qccon(3,mndim)
     1            ,lcon(mndim),qlcon(3,mndim),ncon(mndim),qncon(3,mndim)
     2            ,fcon(mndim),qfcon(3,mndim)
      common/modl3/qshear(mndim),qkappa(mndim)
      common/nond/rn,wn,vn,gn,rhobar
c
      common/tape/rtap(mndim),indsfr(330),indtor(300)
     1    ,kntsfr(330),knttor(300),knts,kntt,lu1,nbytes,lmaxs,lmaxt
     2            ,numbs,numbt
c
      common/mode/nord,jcom,lord,wcom,qbar,cgp,avert,ahor,phis
     1          ,eif(mndim,6)
      dimension u(mndim),up(mndim),v(mndim),vp(mndim),p(mndim),pp(mndim)
      equivalence (eif(1,1),u(1)),(eif(1,2),up(1)),(eif(1,3),v(1))
     1           ,(eif(1,4),vp(1)),(eif(1,5),p(1)),(eif(1,6),pp(1))

      dimension q(3),qp(3)
      equivalence (q(1),ur),(q(2),vr),(q(3),pr)
     1          ,(qp(1),upr),(qp(2),vpr),(qp(3),ppr)

      dimension radj(mndim),rg2(mndim),qrg2(3,mndim),wk(3,mndim)


      real ll,nn
      fl=float(lord)
      fl3=fl*(fl+1.)
      sfl3=sqrt(fl3)
      con1=fl3-3.
      con2=fl3*(fl3-2.)+3.*(4.*fl3-8.)
      con3=fl3*(fl3-2.)
      con4=3.*(4.*fl3-8.)
      omnd=wcom/wn
      omn2=omnd**2
      j1=1
      j2=3
      if(jcom.eq.2) j1=2
      if(jcom.eq.2) j2=2
      vker(1)=0.
      if(jcom.ne.2) then
        do iq=1,n
         f=2.*u(iq)-fl3*v(iq)
         if(iq.eq.1) then
           fp=2.*up(iq)-fl3*vp(iq)
           rg2(iq)=-2.*rho(iq)*(up(iq)*f+u(iq)*fp)
         else
           rg2(iq)=-2.*rho(iq)*u(iq)*f/r(iq)
         endif
        enddo
        call rspln(1,n,r(1),rg2(1),qrg2(1,1),wk(1,1))
        radj(n)=0.
        do iq=n-1,1,-1
          t=r(iq+1)-r(iq)
          radj(iq)=radj(iq+1)+4.*(
     1 t*(rg2(iq)+t*(qrg2(1,iq)/2.+
     1 t*(qrg2(2,iq)/3.+t*(qrg2(3,iq)/4.))))  )
        enddo

      else
        do iq=1,n
          radj(iq)=0.0
        enddo
      endif

      do iq=2,n
        xac=1.
        xf=1.
        xln=1.
        call corfac(iq,wcom,jcom,xac,xf,xln)
        aa=xac*acon(iq)
cb        write(*,*)r(iq)*6371.,aa
        cc=xac*ccon(iq)
        ff=xf*fcon(iq)
        ll=xln*lcon(iq)
        nn=xln*ncon(iq)
        rrho=rho(iq)
        gr=g(iq)
        etan=ff/(aa-2.*ll)
        delv2=(ll+nn)/rrho   ! this is 2Vs**2
        delvp2=(aa+cc)/rrho  ! this is 2Vp**2
        vph2=aa/rrho
        vpv2=cc/rrho
        vsh2=nn/rrho
        vsv2=ll/rrho
cb        write(77,*)r(iq)*6371.,sqrt(vsv2),sqrt(vsh2),rrho
c add jt
        xkap=(4.*aa+cc+4.*ff-4.*nn)/9.
        xmu=(aa+cc-2.*ff+5.*nn+6.*ll)/15.
c        svel2=xmu/rrho
c        pvel2=(xkap+4./3.*xmu)/rrho
c        write(*,*) svel2,delv2/2.,pvel2,delvp2/2.
c equivalence tested

        rr=r(iq)
        do i=1,3
         q(i)=0.
         qp(i)=0.
        enddo
        do i=j1,j2
          i1=2*i-1
          i2=i1+1
          q(i)=eif(iq,i1)
          qp(i)=eif(iq,i2)*rr
        enddo


        f=2.*ur-fl3*vr
        if(jcom.eq.2) f=0.
        xx=vpr-vr+ur
        xx2=xx*xx
        vr2=vr*vr
        ur2=ur*ur
        rka0=f*f
        rkc0=upr*upr
        rkf0=2.*upr*f
        rkl0=fl3*xx2
        rkn0=-rka0+con3*vr2
        t1=(-omn2*vr2*rr+vr*(2.*pr+gr*ur))*rr
        rkr0=((8.*rrho-omn2)*ur2*rr-gr*ur*(f+2.*ur)+2.*ur*ppr)*rr+fl3*t1
     1    +radj(iq)*rr*rr

        if(iso.eq.0) then

        if(isort.eq.1) then
cb           vker(iq)=rrho*delvp2*(rka0+rkc0+etan*rkf0)   !for dVp/Vp
           vker(iq)=rrho*sqrt(delvp2)*(rka0+rkc0+etan*rkf0)/vn   !for dVp
c
        else if(isort.eq.2) then
cb           vker(iq)=rrho*delv2*(rkn0+rkl0-2.*etan*rkf0) !for dVs/Vs
           vker(iq)=rrho*sqrt(delv2)*(rkn0+rkl0-2.*etan*rkf0)/vn !for dVs
c
        else if(isort.eq.3) then
cb          vker(iq)=rrho*(rkr0
cb     1     +.5*(delvp2*(rka0+rkc0+etan*rkf0)
cb     1      +delv2*(rkl0+rkn0-2.*etan*rkf0)  )    )   !for drho/rho
          vker(iq)=(rkr0
     1     +.5*(delvp2*(rka0+rkc0+etan*rkf0)
     1      +delv2*(rkl0+rkn0-2.*etan*rkf0)  )    )/rhobar   !for drho

        endif

        else if(iso.eq.1) then

        if(isort.eq.1) then
cb           vker(iq)=2.*rrho*vph2*(rka0+etan*rkf0)         !for dVph/Vph
           vker(iq)=2.*rrho*sqrt(vph2)*(rka0+etan*rkf0)/vn  !for dVph
c
        else if(isort.eq.2) then
cb           vker(iq)=2.*rrho*vpv2*(rkc0)               !for dVpv/Vpv
           vker(iq)=2.*rrho*sqrt(vpv2)*(rkc0)/vn        !for dVpv
c
        else if(isort.eq.3) then
cb           vker(iq)=2.*rrho*vsh2*(rkn0)               !for dVsh/Vsh
           vker(iq)=2.*rrho*sqrt(vsh2)*(rkn0)/vn        !for dVsh
c
        else if(isort.eq.4) then
cb           vker(iq)=2.*rrho*vsv2*(rkl0-2.*etan*rkf0)  !for dVsv/Vsv
           vker(iq)=2.*rrho*sqrt(vsv2)*(rkl0-2.*etan*rkf0)/vn   !for dVsv
c
        else if(isort.eq.5) then
cc          vker(iq)=rkr0
cb          vker(iq)=rrho*(rkr0
cb     1     +(vpv2*rkc0+vph2*(rka0+etan*rkf0)
cb     1      +vsh2*rkn0+vsv2*(rkl0-2.*etan*rkf0)  )    )   !for drho/rho
          vker(iq)=(rkr0
     1     +(vpv2*rkc0+vph2*(rka0+etan*rkf0)
     1      +vsh2*rkn0+vsv2*(rkl0-2.*etan*rkf0)  )    )/rhobar  !for drho
c
        else if(isort.eq.6) then
cb          vker(iq)=etan*rrho*rkf0*(vph2-2.*vsv2)      !for deta/eta
          vker(iq)=rrho*rkf0*(vph2-2.*vsv2)             !for deta
        endif
      
        else if(iso.eq.2) then
        
        if(isort.eq.1) then
           rkk0=rka0+rkc0+rkf0
cb           vker(iq)=xmu*(rkl0+rkn0-2./3.*rkk0)  !for dmu/mu
           vker(iq)=xmu*(rkl0+rkn0-2./3.*rkk0)/rhobar/vn/vn  !for dmu
c
        else if(isort.eq.2) then
cb           vker(iq)=xkap*(rka0+rkc0+rkf0)  !for dkappa/kappa
           vker(iq)=xkap*(rka0+rkc0+rkf0)/rhobar/vn/vn  !for dkappa
        endif

        endif
      enddo
      return
      end

c----------------------------------------------------------------------
      subroutine corfac(iq,wcom,jcom,xac,xf,xln)
      save
      real lcon,ncon
      common/modl1/n,nic,noc,moho,nsl,ifanis,r(222)
     1            ,rho(222),qrho(3,222),g(222),ell(222),eta(222)
      common/modl2/acon(222),qacon(3,222),ccon(222),qccon(3,222)
     1            ,lcon(222),qlcon(3,222),ncon(222),qncon(3,222)
     2            ,fcon(222),qfcon(3,222)
      common/modl3/qshear(222),qkappa(222)
      common/nond/rn,wn,vn,gn,rhobar
c
      data pi/3.14159265358979/
      data fot/1.33333333333333333333/
c
      fct=2.*alog(.5*wcom/pi)/pi
      xmu=qshear(iq)*fct
      xln=1.+xmu
      if(jcom.eq.2) return
      xka=qkappa(iq)*fct
      rt=fot*(acon(iq)+ccon(iq)-2.*fcon(iq)+5.*ncon(iq)+6.*lcon(iq))
     1   /(8.*acon(iq)+3.*ccon(iq)+4.*fcon(iq)+8.*lcon(iq))
      xf=1.+((1.-rt)*xka-.5*rt*xmu)/(1.-1.5*rt)
      xac=1.+(1.-rt)*xka+rt*xmu
      return
      end

c----------------------------------------------------------------------
      subroutine spread(n)
      save
      common/mode/nord,jcom,lord,wcom,qbar,cgp,avert,ahor,phis
     1          ,eif(222,6)
      dimension eifs(1)
      equivalence (eif(1,1),eifs(1))
      do 10 i=1,6
      icol=7-i
      do 10 j=1,n
      irow=1+n-j
   10 eif(irow,icol)=eifs(irow+n*(icol-1))
      return
      end

c----------------------------------------------------------------------
      subroutine setup
      save
c   this subroutine calculates the record numbers of
c   records containing data for fundamental modes (spheroidal
c   and toroidal, separately) and number of overtones
c   (including fundamental) for each s(l) and t(l).
c   lu1 is the logical unit number of input tape (john's
c   tape with eigenfunctions stored on big0 in a file
c   eig1066b.bin/200).
c
      common/dddu/ltot,mmax,nr,nslr,ninf,recd(222)
      common/tape/ r(222),indsfr(330),indtor(300)
     1    ,kntsfr(330),knttor(300),knts,kntt,lu1,nbytes,lmaxs,lmaxt
     2            ,numbs,numbt
      common/modl1/n,nic,noc,moho,nsl,ifanis,rmod(222)
     1            ,rho(222),qrho(3,222),g(222),ell(222),eta(222)
      integer*2 iaddr,izero
      common novert(700),iaddr(700)
      equivalence (iaddr4,iaddr(1))
      izero=0
cc    rewind lu1
cc    read(lu1) ltot,mmax,nr,nslr,ninf,(r(i),i=1,n)
      call bffi(lu1,1,ltot,(222+5)*4,jstat,nread,1)
      do i=1,n
      r(i)=recd(i)
      enddo

      nbytes=2*ninf
      nfir=1
      nlas=min0(ninf,ltot)
cc 10 read(lu1) (iaddr(i),i=nfir,nlas)
   10 call bffi(lu1,1,iaddr4,(nlas-nfir+1)*2,jstat,nread,0)
      nfir=1+nlas
      nlas=min0(nlas+ninf,ltot)
      if(nfir.le.ltot) goto 10
      knts=0
      lmaxs=-1
      numbs=0
      do 1 i=1,ltot
      if(i.lt.ltot) i1=iaddr(i+1)
      if(i.eq.ltot) i1=iaddr(i)
      if(i.eq.ltot) i1=iabs(i1)+1
      i2=iaddr(i)
      novert(i)=iabs(i1)-iabs(i2)
      if(iaddr(i).lt.izero) go to 1
      knts=knts+1
      lmaxs=1+lmaxs
      numbs=numbs+novert(i)
      indsfr(knts)=iaddr(i)
      kntsfr(knts)=novert(i)
    1 continue
      kntt=0
      lmaxt=0
      numbt=0
      do 2 i=1,ltot
      if(iaddr(i).gt.izero) go to 2
      kntt=kntt+1
      lmaxt=1+lmaxt
      indtor(kntt)=-iaddr(i)
      knttor(kntt)=novert(i)
      numbt=numbt+novert(i)
    2 continue
      return
      end

      subroutine modl(lumod,lupr,rn,wn,vn,gn,rhobar)
      parameter (NKNTS=222)
      parameter (MXDSC=30)
      real lcon,ncon
      common/modl1/n,nic,noc,moho,nsl,ifanis,r(NKNTS)
     1          ,rho(NKNTS),qrho(3,NKNTS),g(NKNTS),ell(NKNTS),eta(NKNTS)
      common/modl2/acon(NKNTS),qacon(3,NKNTS),ccon(NKNTS),qccon(3,NKNTS)
     1            ,lcon(NKNTS),qlcon(3,NKNTS),ncon(NKNTS),qncon(3,NKNTS)
     2            ,fcon(NKNTS),qfcon(3,NKNTS)
      common/modl3/qshear(NKNTS),qkappa(NKNTS)
      common/modl4/ndisc,ndsc(MXDSC)
c      include 'modl.h'
      common/work/wk(3,nknts),coef(5),y(3),y1(3),y2(3),y3(3),ydot(3)
c
      data bigg,capom/6.6723e-11,7.292115e-05/
      data pi,tau,rhon/3.1415926,1000.0,5514.3/
c
      read(lumod,7001) ifdeck,ifanis
 7001 format(49x,i1,8x,i1)
      backspace lumod
      if(ifdeck.eq.0) goto 200
      read(lumod,1000) n,nic,noc,moho
 1000 format(/,4i5)
c
      if(ifanis.eq.0) read(lumod,1001)(r(i),rho(i)
     1     ,ccon(i),lcon(i),qshear(i),qkappa(i),i=1,n)
 1001 format(6f10.2)
      if(ifanis.ne.0) read(lumod,1002)(r(i),rho(i)
     1     ,ccon(i),lcon(i),qshear(i),qkappa(i),acon(i)
     2     ,ncon(i),fcon(i),i=1,n)
              
 1002 format(f8.0,8f9.2)
      goto 300
c
  200 read(lumod,1005) nreg,nic,noc,rx,moho
 1005 format(3i5,e15.8,i5)
      knt=0
      knt1=0
      if(ifanis.eq.0) npar=5
      if(ifanis.ne.0) npar=8
c
      do 10 nn=1,nreg
      read(lumod,1101) nlay,ifs,r1,r2
 1101 format(2i5,2e15.8)
      if(nn.ne.1) r(knt)=r1*tau
      dr=(r2-r1)/float(nlay-1)
      do 2 i=1,nlay
      knt=1+knt
    2 r(knt)=r1+dr*float(i-1)
      do 1 j=1,npar
      read(lumod,3002) (coef(i),i=1,5)
 3002 format(5e16.5)
      do 3 i=1,nlay
      ind=knt1+i
      rt=r(ind)/rx
      if(j.eq.npar) r(ind)=r(ind)*tau
      val=coef(5)
      do 4 ii=1,4
    4 val=val*rt+coef(5-ii)
      if(j.eq.1) rho(ind)=val*tau
      if(j.eq.2) ccon(ind)=val*tau
      if(j.eq.3) lcon(ind)=val*tau
      if(j.eq.4) qshear(ind)=val
      if(j.eq.5) qkappa(ind)=val
      if(j.eq.6) acon(ind)=val*tau
      if(j.eq.7) ncon(ind)=val*tau
      if(j.eq.8) fcon(ind)=val
       if(j.eq.4)  write(*,*)'# ',r(i)/1000.,qshear(i)
    3 continue
    1 continue
   10 knt1=knt1+nlay
      n=knt
c
  300 rn=r(n)
      if(ifanis.ne.0) goto 400
      do 25 i=1,n
      acon(i)=ccon(i)
      ncon(i)=lcon(i)
   25 fcon(i)=1.
c
  400 nsl=n
  420 if(lcon(nsl).ne.0.) goto 410
      nsl=nsl-1
      goto 420
c
  410 do 26 i=1,n
      r(i)=r(i)/rn
   26 rho(i)=rho(i)/rhon
      call rspln(1,n,r,rho,qrho,wk)
c
      y(1)=1.
      y(2)=0.
      y(3)=rho(1)
      rr=1.e-4
      do 100 i=2,n
      rstep=r(i)-r(i-1)
      if(abs(rstep).gt.1.e-4) goto 110
      ell(i)=ell(i-1)
      eta(i)=eta(i-1)
      g(i)=g(i-1)
      goto 100
c
  110 do 101 ii=1,3
  101 y1(ii)=y(ii)
      rr1=rr
      if(rr+rstep.gt.r(i)) rstep=r(i)-rr
      iback=1
      goto 1100
c
 1201 do 201 ii=1,3
  201 y2(ii)=y(ii)
      rr2=rr
  901 rstep=rstep*.5
      do 321 ii=1,3
  321 y(ii)=y1(ii)
      rr=rr1
      iback=2
      goto 1100
c
 1202 do 401 ii=1,3
  401 y3(ii)=y(ii)
      rr3=rr
      iback=3
      goto 1100
c
 1203 do 501 ii=1,3
      if(abs(y(ii)-y2(ii)).gt..5e-4) goto 601
  501 continue
      goto 701
c
  601 do 801 ii=1,3
  801 y2(ii)=y3(ii)
      rr2=rr3
      goto 901
  701 if(abs(rr-r(i)).lt.1.e-5) goto 1300
      rstep=4.*rstep
      goto 110
c
 1300 ell(i)=y(1)
      eta(i)=rr*y(2)/y(1)
      g(i)=4.*rr*y(3)/3.
      goto 100
c
 1100 k=krunge(3,y,ydot,rr,rstep)
      if(k.ne.1) goto (1201,1202,1203),iback
c
      ydot(1)=y(2)
      t=rr-r(i-1)
      im1=i-1
      rhot=rho(im1)+t*(qrho(1,im1)+t*(qrho(2,im1)+t*qrho(3,im1)))
      ydot(3)=3.*(rhot-y(3))/rr
      ydot(2)=-2.*(ydot(3)*y(1)+3.*rhot*y(2))/(y(3)*rr)
      goto 1100
c
  100 continue
c
      goto 1400
c
c
 1400 factr=.75*g(n)/r(n)
      rhobar=rhon*factr
      wn=(pi*bigg*rhobar)**.5
      vn=rn*wn
      gn=rn*wn**2
      fac=2.5*(capom/wn)**2/(4.*ell(n)*(eta(n)+2.)/3.)
      g(1)=0.
      ell(1)=1.
      do 1401 i=1,n
      rho(i)=rho(i)/factr
      g(i)=g(i)/factr
      ell(i)=ell(i)*fac
      do 1401 j=1,3
 1401 qrho(j,i)=qrho(j,i)/factr
c
c
c
      do 7 i=1,n
      ccon(i)=rho(i)*(ccon(i)/vn)**2
      lcon(i)=rho(i)*(lcon(i)/vn)**2
      acon(i)=rho(i)*(acon(i)/vn)**2
      ncon(i)=rho(i)*(ncon(i)/vn)**2
    7 fcon(i)=fcon(i)*(acon(i)-2.*lcon(i))
c
c
      call rspln(1,n,r,ccon,qccon,wk)
      call rspln(1,n,r,lcon,qlcon,wk)
      call rspln(1,n,r,acon,qacon,wk)
      call rspln(1,n,r,ncon,qncon,wk)
      call rspln(1,n,r,fcon,qfcon,wk)
c
      ndisc=0.
      do 56 i=2,n
      if(abs(r(i)-r(i-1)).gt.1.e-4) goto 56
      ndisc=1+ndisc
      ndsc(ndisc)=i-1
   56 continue
      ndisc=1+ndisc
      ndsc(ndisc)=n
c
      if(lupr.eq.0) goto 99
      write(lupr,909) rhobar,1./ell(n),nic,noc,moho,nsl,n
  909 format(///' mean density =',f10.3,'     surface ellipticity ='
     1   ,' 1/',f7.3/' nic =',i4,'   noc =',i4,'   moho =',i4
     2   ,'   nsl =',i4,'   n =',i4///)
      i=0
      rlas=-10000.
      lines=9
  903 write(lupr,902)
  902 format(1x,'level',
     1 3x,'radius',7x,'rho',8x,'vpv',8x,'vph',7x,'vsv',
     2 7x,'vsh',6x,'eta',7x,'g',7x,'1/qm',5x,'1/qk'
     3  ,'      1/ell    etaell'/)
  906 i=1+i
      if(i.gt.n) goto 99
      rr=r(i)*rn
      rrho=rho(i)*rhobar
      vpv=sqrt(ccon(i)/rho(i))*vn
      vph=sqrt(acon(i)/rho(i))*vn
      vsv=sqrt(lcon(i)/rho(i))*vn
      vsh=sqrt(ncon(i)/rho(i))*vn
      eeta=fcon(i)/(acon(i)-2.*lcon(i))
      gg=g(i)*gn
      if(abs(rlas-rr).lt.500.) write(lupr,912)
  912 format(1x,132('-'))
      if(rr.eq.rlas) lines=1+lines
      rlas=rr
      write(lupr,9021) i,rr,rrho,vpv,vph,vsv,vsh,eeta,gg
     1   ,qshear(i),qkappa(i),ell(i),eta(i)
 9021 format(2x,i3,f11.1,f10.3,2f11.3,2f10.3,4f9.5,f11.7,f9.5)
      lines=1+lines
      if(lines.lt.57) goto 906
      lines=1
      write(lupr,911)
  911 format(1h1)
      goto 903
   99 return
      end


      SUBROUTINE RSPLN(I1,I2,X,Y,Q,F)
C
C C$C$C$C$C$ CALLS ONLY LIBRARY ROUTINES C$C$C$C$C$
C
C   SUBROUTINE RSPLN COMPUTES CUBIC SPLINE INTERPOLATION COEFFICIENTS
C   FOR Y(X) BETWEEN GRID POINTS I1 AND I2 SAVING THEM IN Q.  THE
C   INTERPOLATION IS CONTINUOUS WITH CONTINUOUS FIRST AND SECOND
C   DERIVITIVES.  IT AGREES EXACTLY WITH Y AT GRID POINTS AND WITH THE
C   THREE POINT FIRST DERIVITIVES AT BOTH END POINTS (I1 AND I2).
C   X MUST BE MONOTONIC BUT IF TWO SUCCESSIVE VALUES OF X ARE EQUAL
C   A DISCONTINUITY IS ASSUMED AND SEPERATE INTERPOLATION IS DONE ON
C   EACH STRICTLY MONOTONIC SEGMENT.  THE ARRAYS MUST BE DIMENSIONED AT
C   LEAST - X(I2), Y(I2), Q(3,I2), AND F(3,I2).  F IS WORKING STORAGE
C   FOR RSPLN.
C                                                     -RPB
C
      DIMENSION X(*),Y(*),Q(3,*),F(3,*),YY(3)
      EQUIVALENCE (YY(1),Y0)
      DATA SMALL/1.E-5/,YY/0.,0.,0./
      J1=I1+1
      Y0=0.
C   BAIL OUT IF THERE ARE LESS THAN TWO POINTS TOTAL.
      IF(I2-I1)13,17,8
 8    A0=X(J1-1)
C   SEARCH FOR DISCONTINUITIES.
      DO 3 I=J1,I2
      B0=A0
      A0=X(I)
      IF(ABS((A0-B0)/AMAX1(A0,B0)).LT.SMALL) GO TO 4
 3    CONTINUE
 17   J1=J1-1
      J2=I2-2
      GO TO 5
 4    J1=J1-1
      J2=I-3
C   SEE IF THERE ARE ENOUGH POINTS TO INTERPOLATE (AT LEAST THREE).
 5    IF(J2+1-J1)9,10,11
C   ONLY TWO POINTS.  USE LINEAR INTERPOLATION.
 10   J2=J2+2
      Y0=(Y(J2)-Y(J1))/(X(J2)-X(J1))
      DO 15 J=1,3
      Q(J,J1)=YY(J)
 15   Q(J,J2)=YY(J)
      GO TO 12
C   MORE THAN TWO POINTS.  DO SPLINE INTERPOLATION.
 11   A0=0.
      H=X(J1+1)-X(J1)
      H2=X(J1+2)-X(J1)
      Y0=H*H2*(H2-H)
      H=H*H
      H2=H2*H2
C   CALCULATE DERIVITIVE AT NEAR END.
      B0=(Y(J1)*(H-H2)+Y(J1+1)*H2-Y(J1+2)*H)/Y0
      B1=B0
C   EXPLICITLY REDUCE BANDED MATRIX TO AN UPPER BANDED MATRIX.
      DO 1 I=J1,J2
      H=X(I+1)-X(I)
      Y0=Y(I+1)-Y(I)
      H2=H*H
      HA=H-A0
      H2A=H-2.*A0
      H3A=2.*H-3.*A0
      H2B=H2*B0
      Q(1,I)=H2/HA
      Q(2,I)=-HA/(H2A*H2)
      Q(3,I)=-H*H2A/H3A
      F(1,I)=(Y0-H*B0)/(H*HA)
      F(2,I)=(H2B-Y0*(2.*H-A0))/(H*H2*H2A)
      F(3,I)=-(H2B-3.*Y0*HA)/(H*H3A)
      A0=Q(3,I)
 1    B0=F(3,I)
C   TAKE CARE OF LAST TWO ROWS.
      I=J2+1
      H=X(I+1)-X(I)
      Y0=Y(I+1)-Y(I)
      H2=H*H
      HA=H-A0
      H2A=H*HA
      H2B=H2*B0-Y0*(2.*H-A0)
      Q(1,I)=H2/HA
      F(1,I)=(Y0-H*B0)/H2A
      HA=X(J2)-X(I+1)
      Y0=-H*HA*(HA+H)
      HA=HA*HA
C   CALCULATE DERIVITIVE AT FAR END.
      Y0=(Y(I+1)*(H2-HA)+Y(I)*HA-Y(J2)*H2)/Y0
      Q(3,I)=(Y0*H2A+H2B)/(H*H2*(H-2.*A0))
      Q(2,I)=F(1,I)-Q(1,I)*Q(3,I)
C   SOLVE UPPER BANDED MATRIX BY REVERSE ITERATION.
      DO 2 J=J1,J2
      K=I-1
      Q(1,I)=F(3,K)-Q(3,K)*Q(2,I)
      Q(3,K)=F(2,K)-Q(2,K)*Q(1,I)
      Q(2,K)=F(1,K)-Q(1,K)*Q(3,K)
 2    I=K
      Q(1,I)=B1
C   FILL IN THE LAST POINT WITH A LINEAR EXTRAPOLATION.
 9    J2=J2+2
      DO 14 J=1,3
 14   Q(J,J2)=YY(J)
C   SEE IF THIS DISCONTINUITY IS THE LAST.
 12   IF(J2-I2)6,13,13
C   NO.  GO BACK FOR MORE.
 6    J1=J2+2
      IF(J1-I2)8,8,7
C   THERE IS ONLY ONE POINT LEFT AFTER THE LATEST DISCONTINUITY.
 7    DO 16 J=1,3
 16   Q(J,I2)=YY(J)
C   FINI.
 13   RETURN
      END

      function krunge(n,y,f,x,h)
      dimension phi(6),savey(6),y(6),f(6)
      data m/0/
      m=m+1
      goto(1,2,3,4,5), m
1     krunge=1
      return
2     do 22 j=1,n
      savey(j)=y(j)
      phi(j)=f(j)
22    y(j)=savey(j)+0.5d0*h*f(j)
      x=x+0.5d0*h
      krunge=1
      return
3     do 33 j=1,n
      phi(j)=phi(j)+2.d0*f(j)
33    y(j)=savey(j)+0.5d0*h*f(j)
      krunge=1
      return
4     do 44 j=1,n
      phi(j)=phi(j)+2.0d0*f(j)
44    y(j)=savey(j)+h*f(j)
      x=x+0.5d0*h
      krunge=1
      return
5     do 55 j=1,n
55    y(j)=savey(j)+(phi(j)+f(j))*h/6.d0
      m=0
      krunge=0
      return
      end
