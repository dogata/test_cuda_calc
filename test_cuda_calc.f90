!
!  Program to test time consumption on GPU device for calculating mathematical operations
!

MODULE PARAMS
      IMPLICIT NONE

      INTEGER, PUBLIC, PARAMETER :: ntimes = 20
      INTEGER, PUBLIC, PARAMETER :: nxreal = 256, nyreal = nxreal
      REAL, PUBLIC, PARAMETER :: pi = 4.d0*ATAN(1.d0), Lnc = 0.8

END MODULE PARAMS          

PROGRAM test_cuda_calc

      USE PARAMS
      IMPLICIT NONE
      
      INTEGER :: n,m,p
      REAL :: rd_avg, rd_std
      REAL, DIMENSION(nxreal,nyreal) :: ra,rb,rc,rd
      REAL, DEVICE, DIMENSION(nxreal,nyreal) :: ra_d,rb_d

      ! timing variables
      INTEGER(KIND=8) :: clock_start,clock_end,clock_rate
      REAL :: dt,tavg,tstd
      REAL, DIMENSION(ntimes) :: tcycle

      CALL SYSTEM_CLOCK(COUNT_RATE=clock_rate) ! Find the rate

      ! initialize the array
      DO m = 1, nyreal
         DO n = 1, nxreal
            ra(n,m) = COS(2.d0*pi*(REAL(n)/REAL(nxreal) + REAL(m)/REAL(nyreal)))
         END DO
      END DO

      ! calculate something on the CPU
      DO n = 1, ntimes
         CALL SYSTEM_CLOCK(COUNT=clock_start) ! Start timing
         DO m = 1, 100
            CALL calc_something(ra,rb,nxreal,nyreal)
         END DO
         CALL SYSTEM_CLOCK(COUNT=clock_end) ! Stop timing

         dt = REAL((clock_end-clock_start))/REAL(clock_rate)
!	  write(*,*) "Time taken: ", dt
         tcycle(n) = dt
      END DO

      tavg = SUM(tcycle)/REAL(ntimes)
      tstd = SQRT( SUM( (/ ((tcycle(n)-tavg)**2,n=1,ntimes) /) )/(ntimes-1) )

      WRITE(*,*) "CPU: Average time taken: ", tavg, " pm ", tstd

      ! calculate something on the GPU
      DO n = 1, ntimes
         CALL SYSTEM_CLOCK(COUNT=clock_start) ! Start timing
         DO m = 1, 100
            CALL calc_something_d(ra,rc,nxreal,nyreal)
         END DO
         CALL SYSTEM_CLOCK(COUNT=clock_end) ! Stop timing

         dt = REAL((clock_end-clock_start))/REAL(clock_rate)
!	  write(*,*) "Time taken: ", dt
         tcycle(n) = dt
      END DO
 
      tavg = SUM(tcycle)/REAL(ntimes)
      tstd = SQRT( SUM( (/ ((tcycle(n)-tavg)**2,n=1,ntimes) /) )/(ntimes-1) )

      WRITE(*,*) "GPU: Average time taken: ", tavg, " pm ", tstd

      ! compare the difference as a check
      rd = ABS(rb - rc)
      rd_avg = SUM(rd)/REAL(nxreal*nyreal)
      rd_std = SQRT( SUM( (rd - rd_avg)**2 )/REAL(nxreal*nyreal - 1) )
      
      WRITE(*,*) "rd_avg = ",rd_avg," rd_std = ",rd_std

END PROGRAM test_cuda_calc


SUBROUTINE calc_something(ra,rb,nx,ny,Lnc)
!
!  subroutine to calculate something
!
      USE PARAMS
      IMPLICIT NONE
      
      INTEGER :: n,m
      INTEGER, INTENT(IN) :: nx,ny
      REAL, INTENT(IN) :: Lnc
      REAL, DIMENSION(nx,ny), INTENT(IN) :: ra
      REAL, DIMENSION(nx,ny), INTENT(OUT) :: rb
      REAL :: fact,swtanh

      DO m = 1, ny
         DO n = 1, nx
            fact = ra(n,m)
            swtanh = 1.d0 + 0.5d0*TANH(fact - Lnc) - 0.5d0*TANH(fact + Lnc)
            rb(n,m) = swtanh
         END DO
      END DO
      
END SUBROUTINE calc_something

SUBROUTINE calc_something_d(ra,rb,nx,ny,Lnc)
!
!  subroutine to calculate something
!
      USE PARAMS
      IMPLICIT NONE
      
      INTEGER :: n,m
      INTEGER, INTENT(IN) :: nx,ny
      REAL, INTENT(IN) :: Lnc
      REAL, DIMENSION(nx,ny), INTENT(IN) :: ra
      REAL, DIMENSION(nx,ny), INTENT(OUT) :: rb

      REAL, DEVICE, DIMENSION(nx,ny) :: ra_d,rb_d
      REAL, DEVICE :: fact_d,swtanh_d
      
      ra_d = ra

      !$cuf kernel do <<< *,* >>>
      DO m = 1, ny
         DO n = 1, nx
            fact_d = ra_d(n,m)
            swtanh_d = 1.d0 + 0.5d0*TANH(fact_d - Lnc) - 0.5d0*TANH(fact_d + Lnc)
            rb_d(n,m) = swtanh_d
         END DO
      END DO

      rb = rb_d
      
END SUBROUTINE calc_something_d
