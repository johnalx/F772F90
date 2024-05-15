MODULE implicit
! Module to set and reset implicit variable types for use by to_f90.

IMPLICIT NONE
INTEGER, SAVE :: var_type(26) = (/  &
                 1,1,1,1,1,1,1,1,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1 /)
!                a b c d e f g h i j k l m n o p q r s t u v w x y z
CHARACTER (LEN=24), SAVE :: vt(0:7) = (/ 'NO TYPE                 ', &
                                         'REAL                    ', &
                                         'INTEGER                 ', &
                                         'DOUBLE PRECISION        ', &
                                         'LOGICAL                 ', &
                                         'COMPLEX                 ', &
                                         'CHARACTER               ', &
                                         'OTHER TYPE              ' /)

CONTAINS


SUBROUTINE reset_defaults()

var_type(1:8) = 1            ! REAL (A-H)
var_type(9:14) = 2           ! INTEGER (I-N)
var_type(15:26) = 1          ! REAL (O-Z)

RETURN
END SUBROUTINE reset_defaults



SUBROUTINE set_implicit_types(text)
! Read in implicit statement and interpret.

CHARACTER (LEN=*), INTENT(IN OUT) :: text

! Local variables
INTEGER :: ivt, length, start, i, j, pos, left, right
LOGICAL :: first

i = INDEX(text, 'IMPLICIT')
IF (i > 0) text = text(i+8:)
text = ADJUSTL(text)

DO
  IF (text(1:4) == 'NONE') THEN
    var_type = 0
    RETURN
  ELSE IF (text(1:4) == 'REAL') THEN
    ivt = 1
  ELSE IF (text(1:7) == 'INTEGER') THEN
    ivt = 2
  ELSE IF (text(1:24) == 'DOUBLE PRECISION COMPLEX') THEN
    ivt = 7
    vt(7) = 'DOUBLE PRECISION COMPLEX'
  ELSE IF (text(1:16) == 'DOUBLE PRECISION') THEN
    ivt = 3
  ELSE IF (text(1:7) == 'LOGICAL') THEN
    ivt = 4
  ELSE IF (text(1:7) == 'COMPLEX') THEN
    ivt = 5
  ELSE IF (text(1:9) == 'CHARACTER') THEN
    ivt = 6
  ELSE
    ivt = 7
    i = INDEX(text, ' ')
    vt(7) = text(1:i-1)
  END IF

! Interpret the part in brackets, e.g. (a - h, o - z)

  length = LEN_TRIM(text)
  start = 5
  left = INDEX(text(start:length), '(') + start - 1
  IF (left < start) RETURN
  right = INDEX(text(start:length), ')') + start - 1
  IF (right < left) RETURN
                                       ! Interpret text(left+1:right-1)
  first = .TRUE.
  DO pos = left+1, right
    SELECT CASE (text(pos:pos))
      CASE (' ')
        CYCLE
      CASE ('-')
        first = .FALSE.
      CASE (',', ')')
        IF (first) THEN
          var_type(i) = ivt
        ELSE
          var_type(i:j) = ivt
          first = .TRUE.
        END IF
      CASE DEFAULT
        IF (first) THEN
          i = ICHAR(text(pos:pos)) - ICHAR('a') + 1
          IF (i < 1) THEN
            i = ICHAR(text(pos:pos)) - ICHAR('A') + 1
          END IF
        ELSE
          j = ICHAR(text(pos:pos)) - ICHAR('a') + 1
          IF (j < 1) THEN
            j = ICHAR(text(pos:pos)) - ICHAR('A') + 1
          END IF
        END IF
    END SELECT
  END DO

  start = right + 1
  IF (start >= length) RETURN
  text = text(start:length)
  DO
    IF (text(1:1) == ',' .OR. text(1:1) == ' ') THEN
      text = text(2:)
    ELSE
      EXIT
    END IF
  END DO
END DO

RETURN
END SUBROUTINE set_implicit_types



FUNCTION implicit_type(ch) RESULT(vtype)
! Return the variable type given the first character of its name.
! The first character is expected to be lower case, but just in case ..

CHARACTER (LEN=1), INTENT(IN) :: ch
CHARACTER (LEN=24)            :: vtype

! Local variable
INTEGER  :: i, j

i = ICHAR(ch) - ICHAR('a') + 1
IF (i >= 1 .AND. i <= 26) THEN
  j = var_type(i)
  vtype = vt(j)
ELSE
  i = ICHAR(ch) - ICHAR('A') + 1
  IF (i >= 1 .AND. i <= 26) THEN
    j = var_type(i)
    vtype = vt(j)
  ELSE
    vtype = ' '
  END IF
END IF

RETURN
END FUNCTION implicit_type

END MODULE implicit

