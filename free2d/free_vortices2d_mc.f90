PROGRAM Parallel_Vortices
  
REAL*4, DIMENSION(100000,3) :: vortices   !-- array holds 2D vortices
REAL*4, DIMENSION(100000,3) :: vor   !-- temporary array holds 2D vortices

character(len=40) :: filename
character(len=5) :: s
integer :: io, NIT, i, j
LOGICAL :: exist

READ(*,*) NIT
WRITE(s,'(I0.5)') NIT

filename = 'out/vortices_' // s // '.dat'
M = 100000  !-- number of vortices
WW = 1.0
eps = 0.000001
PI = ASIN(1.0)*2
sigma = PI
BB = 40.0
DD = 10.0
R1 = 0.3
R2 = 0.4
THETA1 = 0.0
THETA2 = PI/6
XX = 1000.0/2-300
YY = 800.0/2
XT = 500.0
X0 = X - 50.0
Y0 = Y + 200.0
T = 0.0
N = 0
MU = 1.0
NU = 0.999999
DT = 0.01

io = 0
IF(NIT == 0) THEN
   open(NIT, file=filename, status="old", position="append", action="write")
   !--Initializing vortices--
   DO i = 1,M
      CALL RANDOM_NUMBER(f)
      a = BB*f
      CALL RANDOM_NUMBER(g)
      b = DD*g
      x = XX + a
      y = YY + b
      CALL RANDOM_NUMBER(h)
      w = WW*(1-2*h)
      vortices(i,1) = x
      vortices(i,2) = y
      vortices(i,3) = w
      write(NIT, *) i, x, y, w
   END DO
   close(NIT)
ELSE
   !--Looading from previosu end position--
   open(NIT, file=filename, status="old", action="read")
100 CONTINUE
   read(UNIT=NIT, FMT=*, END=200) j, xj, yj, wj
   vortices(j,1) = xj
   vortices(j,2) = yj
   vortices(j,3) = wj
   GO TO 100
200 CONTINUE
   close(NIT)
END IF


DO it=NIT+1,250
   !$OMP PARALLEL
   !--Updating vortices to temporary vor--
   call rvelocity(vortices,M,DD,BB,DT,sigma,vor)
   !$OMP END PARALLEL
   !--Updating vortices --
   vortices = vor
   !--Writing vortices to file--
   write(s,'(i0.5)') it
   filename = 'out/vortices_' // s // '.dat'
   open(it, file=filename, status="old", position="append", action="write")
   DO i=1,M
      write(it,*) i, vortices(i,1), vortices(i,2), vortices(i,3)
   END DO
   close(it)
END DO

END PROGRAM Parallel_Vortices



REAL*4 function gexp(rs,sigma)
  REAL*4 :: rs,sigma
  gexp = 1-exp(-1.0*rs/sigma)
end function gexp

subroutine rvelocity(vortices,M,DD,BB,DT,sigma,vor)
  USE OMP_LIB
  REAL*4, intent(in) :: DT,sigma
  REAL*4, DIMENSION(100000,3), intent(in) :: vortices
  REAL*4, DIMENSION(100000,3) :: vor
  eps = 0.000001
  !$OMP DO
  DO i=1,M
     x = vortices(i,1)
     y = vortices(i,2)
     w = vortices(i,3)
     dx = 0.0
     dy = 0.0
     DO j=1,M
        IF(.not. i == j) THEN
           qx = vortices(j,1)
           qy = vortices(j,2)
           qw = vortices(j,3)
           pqx = x-qx
           pqy = y-qy
           rpq = pqx**2+pqy**2
           IF(SQRT(rpq) < DD/5) THEN 
              dx = dx - gexp(rpq,sigma)*pqy*qw*DT/(rpq+eps)
              dy = dy + gexp(rpq,sigma)*pqx*qw*DT/(rpq+eps)
           END IF
        END IF
     END DO
     vor(i,1) = x+dx
     vor(i,2) = y+dy
     vor(i,3) = w
  END DO
  !$OMP END DO
end subroutine rvelocity
