PROGRAM numprecision

!Note as in the compilation must be used the flag -fno-range-check
!Otherwise it would return an error

IMPLICIT NONE

!Declaration part
INTEGER*2 :: a2, b2
INTEGER*4 :: a4, b4

REAL*4 :: a_real4, b_real4
REAL*8 :: a_real8, b_real8

!Define Pi as
REAL*4 :: PI4 = 16*ATAN(1./5.) - 4*ATAN(1./239.)
REAL*8 :: PI8 = 16*ATAN(1./5.) - 4*ATAN(1./239.)

a2 = 2e7
b2 = 1

a4 = 2e7
b4 = 1

a_real4 = PI4*1e32
b_real4 = SQRT(2.)*1e21

a_real8 = PI8*1e32
b_real8 = SQRT(2.)*1e21

!Executable statements
PRINT*, "Result of 2e7+1 using 2 bytes", a2+b2
PRINT*, "Overflow encountered! Result is actually wrong"

PRINT*, "Result of 2e7 + 1 using 4 bytes", a4+b4
PRINT*, "Now result is correct!"

PRINT*,
PRINT*,


PRINT*, "Representation of pi*1e32 using 4 bytes", a_real4
PRINT*, "Representation of sqrt(2)*e21 using 4 bytes", b_real4


PRINT*, "Result of pi*1e32 + sqrt(2)*e21 using single precision"
PRINT*,  a_real4+b_real4
PRINT*, "Actually it is equal to the first term. Precision is too low!"

PRINT*

PRINT*, "Representation of pi*1e32 using 8 bytes", a_real8
PRINT*, "Representation of sqrt(2)*e21 using 8 bytes", b_real8

PRINT*, "Result of pi*1e32 + sqrt(2)*e21 using double precision"
PRINT*, a_real8+b_real8
PRINT*, "Result is now correct!"

END PROGRAM 

