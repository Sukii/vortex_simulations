PROGRAM COMP
USE OMP_LIB

REAL, DIMENSION(100000,3) :: VORTICES   !-- array holds vortices
REAL, DIMENSION(100000,3) :: vor        !-- temporary array for vortices
COMPLEX :: Z, Z0, Z0C, IMG, VELOCITY
REAL :: D, PI, A, B, W, delta, mu, NR
character(len=40) :: filename
character(len=5) :: s
integer :: io, iox, IT, I, J, IJ, NMU, NIT

io = 0
iox = 100000001
D = 10.0
NR = 2.0
B = D*NR
N = 5
ND = FLOOR(N*D)
NB = FLOOR(N*B)
M = NB*ND
delta = 1.0/N
h = 0.2
sigma = delta/N
WW = 1.0*sigma
READ(*,*) NIT
WRITE(s,'(I0.5)') NIT
filename = 'outc/vortices_'//s//'.dat'
!PRINT *, filename
DT = 0.01

IF(NIT == 0) THEN
   open(unit=NIT, file=filename, status="old", position="append", action="write")
   !--Initializing vortices--
   DO I=1,NB*ND
      CALL RANDOM_NUMBER(f)
      X = (B/2)+(1-2*f)*B/20
      CALL RANDOM_NUMBER(g)
      Y = (D/2)+(1-2*g)*D/20
      CALL RANDOM_NUMBER(h)
      W = WW*(1.0-2.0*h)/20
      VORTICES(I,1) = X
      VORTICES(I,2) = Y
      VORTICES(I,3) = W
      WRITE(io, *) I, X, Y, W
   END DO
   close(NIT)
ELSE
   !--Loading vortices from previous end position--
   open(unit=NIT, file=filename, status="old", action="read")
100 CONTINUE
   read(unit=NIT, fmt=*, end=200) j, xj, yj, wj
   vortices(j,1) = xj
   vortices(j,2) = yj
   vortices(j,3) = wj
   GO TO 100
200 CONTINUE
   close(NIT)
END IF


DO it=NIT+1,250
   !$OMP PARALLEL
   !--Update vortices to temporary vor-- 
   call chvelocity(B,D,vortices,M,DT,sigma,vor)
   !$OMP END PARALLEL
   !--Update vortices from vor--
   vortices = vor
   !--Write updated vortices to file--
   write(s,'(i0.5)') it
   filename = 'outc/vortices_' // s // '.dat'
   open(it, file=filename, status="old", position="append", action="write")
   DO i=1,M
      write(it,*) i, vortices(i,1), vortices(i,2), vortices(i,3)
   END DO
   close(it)
END DO

END PROGRAM COMP

REAL function gexp(rs,sigma)
  REAL :: rs,sigma
  gexp = 1.0-exp(-1.0*rs/sigma)
end function gexp

character(len=20) function str(k)
  !"Convert an integer to string."
  integer, intent(in) :: k
  write (str, *) k
  str = adjustl(str)
end function str

REAL function walldamper(x,y,D,sigmabl)
  REAL :: sigmabl
  walldamper = 1-exp(-ABS(y-D)**2/sigmabl)
  IF(y < D/2) THEN
     walldamper = 1-exp(-ABS(y)**2/sigmabl)
  END IF
end function walldamper

REAL function rho(f,h)
  REAL :: f, h
  rho = 0.5 + (2*f - 1)*h
end function rho


subroutine cvelocity(B,D,Z,Z0,FZ)
  COMPLEX :: Z,Z0, FZ, Z0C, ZL, ZLC, ZZL, ZZLC, Z_L, Z_LC, CONE, A
  INTEGER MZ
  eps = 0.000001
  MZ = 5
  PI = ASIN(1.0)*2
  A = CMPLX(PI/(2*D),0.0)
  CONE = CMPLX(1.0,0.0)
  FZ = CMPLX(0.0,0.0)
  DO L=-MZ,MZ
     ZL = Z0 + CMPLX(L*B,0)
     ZZL = Z - ZL 
     ZLC = CONJG(ZL)
     ZZLC = Z - ZLC
     IF(L == 0 .AND. ABS(Z-Z0) < eps) THEN
        ZZL = CMPLX(eps,0)
     END IF
     FZ = FZ + CONE/TANH((A*(ZZL))) - CONE/TANH((A*(ZZLC)))
  END DO
end subroutine cvelocity

subroutine chvelocity(B,D,vortices,M,DT,sigma,vor)
  real, intent(in) :: B,D,DT,sigma
  real, DIMENSION(100000,3), intent(in) :: vortices
  real, DIMENSION(100000,3) :: vor
  integer, intent(in) :: M
  COMPLEX :: Z, Z0, ZZ0, IMG, VELOCITY, FZ
  REAL :: w0, w, u, v, AZZ0
  IMG =CMPLX(0.0,1.0)
  !$OMP DO
  DO i=1,M
     x = vortices(i,1)
     y = vortices(i,2)
     Z = CMPLX(x,y)
     w0 = vortices(i,3)
     dx = 0
     dy = 0
     DO j=1,M
        IF(.not. i == j) THEN
           qx = vortices(j,1)
           qy = vortices(j,2)
           qw = vortices(j,3)
           Z0 = CMPLX(qx,qy)
           ZZ0 = Z-Z0
           AZZ0 = ABS(ZZ0)
           !PRINT *, "params:", i, j, A,B,Z,Z0,MZ,FZ
           CALL cvelocity(B,D,Z,Z0,FZ)
           !PRINT *, "FZ:", FZ, gexp(AZZ0,sigma)
           VELOCITY = IMG*gexp(AZZ0,sigma)*(qw/D)*FZ
           u = REAL(VELOCITY)
           v = AIMAG(VELOCITY)
           dx = dx + u*DT
           dy = dy + v*DT
        END IF
     END DO
     vor(i,1) = x+dx
     vor(i,2) = y+dy
     vor(i,3) = w0
  END DO
  !$OMP END DO
end subroutine chvelocity
