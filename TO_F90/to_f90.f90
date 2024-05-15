! Takes Fortran 77 code in standard format and makes some changes to produce
! free-format Fortran 90 code.
! N.B. It expects STANDARD F77 code.   Non-standard extensions such as
! DO .. END DO (i.e. no label) or in-line comments may cause havoc!

! Changes included are:
!     C or c in column 1 replaced with !
!     Continuation denoted by a character in column 6 replaced with & at the
!         end of the previous line.
!     Indenting of code for DO-loops and IF blocks.
!     END of program unit replaced by END SUBROUTINE (/PROGRAM/FUNCTION) name
!     Fortran `keywords' are in upper case, all other words other than those
!         in character strings are converted to lower case.
!     .LT., .EQ., etc. replaced with <, ==, etc.
!     Labels removed from DO loops; all of which will end with END DO.
!         If labels are not referenced, they are removed.
!     Short continued lines are adjoined to the previous line.
!     ENDIF, ELSEIF & GOTO split into separate words.
!     3-way arithmetic IF constructs are converted to IF .. ELSE IF form.
!     Embedded blanks are removed from numbers in DATA statements.
!     INTENT declarations are added for dummy arguments.
!     Some GO TOs are converted to CYCLE or EXIT.
!     Converts CHARACTER * to CHARACTER (LEN=xx) ::.
!     Converts computed GO TOs to SELECT CASE.

! To be done:
!     DATA statements to be replaced by assignments on the declaration line.
!     IMPLICIT NONE statements to be included.
!     Declaration of types of unlisted variables.
!     Functions to be converted to ELF90 form, i.e. REAL FUNCTION XYZ(arg)
!         converted to FUNCTION xyz(arg) RESULT(fn_val).

! Known problems
!     Cannot handle character strings or names broken at the end of lines.
!     No attempt to convert BLOCKDATA, COMMON or EQUIVALENCE.
!     Does not convert Hollerith strings, e.g. 31HTHIS IS A COMMENT ...
!     May do the wrong thing if variable names start with IF or end with DO.
!     INTENTs are sometimes wrong.  In particular, INTENT(IN) arguments are
!         often shown as INTENT(IN OUT).
!     Cannot handle comment lines in the middle of continued instructions.
!     Can handle 'character*(*) str' but not 'character str*(*)'.

! The default extension for the name of the input file is `for'; this can be
! over-ruled by giving the full name (e.g. myprog.f77).   The output file name
! will be the input name (and directory) with extension `.f90'.

! Added conversion of `enddo' to END DO - 13 March 1997
! Corrected bug which occurred when an arithmetic IF within a DO-loop involved
!     a jump to the end of the DO-loop - 17 August 1997.

! ELSEIF, ENDIF & ELSEIF were being split into 2 separate words, and then the
!     last letter converted back to lower case - corrected 17 August 1997.
! Corrected bug which occurred when .LT. (or other comparison) had a blank
!     before and/or after, followed on the same line by a text string, followed
!     by a Fortran word such as THEN or GO TO - 8 December 1997.
! Added (LEN=1) after CHARACTER if length not specified - 9 December 1997.
! Embedded blanks are removed from numerical constants in DATA statements.
!     Added 9 December 1997.
! Added INTENTs and TYPE declarations for dummy arguments - 23 December 1997.
! Corrected problem when DO statement contains a comma immediately after DO,
!     and improved the detection of INTENTs when a dummy argument appears in an
!     IF-expression.  Added extra indentation on continuation lines to improve
!     readability - 13 January 1998
! Corrected a bug which could occur when the last type declaration was matched
!     to a dummy variable and the line deleted - 5 June 1998
! Corrected jumps out of inner nested DO loops, and replaced GO TOs, out of
!     DO loops to the next executable line, with EXIT - 8 June 1998
! Added conversion of CHARACTER * to CHARACTER (LEN=xx) ::
!     including CHARACTER*10 a, d, c*50, d   - 21 June 1998.
! Corrected for case of final command of a DO loop which is not CONTINUE and
!     which flows onto the next line - 29 June 1998.
! Added conversion of computed GO TOs to SELECT CASE form, and
!     fixed problem when a CHARACTER dummy variable had '*' as its last
!     dimension - 26 November 1998.
! Fixed problems when the dimensions of a dummy variable involved another
!     dummy variable, e.g. wk(nrow*(ncols+1)) - 25 December 1998
! Added date & time stamp - 27 January 1999
! Finally fixed the problems with CYCLE & EXIT, I hope! - 2 February 1999
! Fixed a problem when a type declaration was continued and the next line
!     declared the type(s) of dummy arguments - 3 February 1999
! Added conversion of PARAMETER statements from PARAMETER (name1=v1, .. )
!     to TYPE1, PARAMETER :: name1=v1  - 8 February 1999
! Added EQV to the list of FORTRAN `words' - 11 February 1999
! Partially corrected problems with the construct:
!     IF (condition) GO TO (10, 20, ..
!       ..., 99), next
!     i.e. with IF & computed GOTO in the same statement (without a THEN), and
!     continued onto the next line.
!     Also changed a DATA statement to a PARAMETER statement to make the code
!     compatible with ELF90 (Thanks to David Ormerod) - 20 May 1999
! Added test for existence of source file.  Program crashed previously if
!     the file was not found - 3 August 1999
! Corrected SUBROUTINE fix_3way_IF so that it does not interpret IFIX (or
!     similar) as an IF - 23 January 2000.
! At last fixed up strings in quotes which flowed from one line to the next
!     - 24 January 2000
! Fixed an error which sometimes caused GOTOs to be wrongly converted to CYCLE
!     - 21 March 2000

! Latest revision - 21 March 2000
! Author - Alan Miller  (amiller @ bigpond.net.au)
! WWW-page: http://users.bigpond.net.au/amiller/


PROGRAM to_f90
USE DFLIB
USE implicit
USE prog

IMPLICIT NONE

CHARACTER (LEN=$MAXPATH) :: buf
CHARACTER (LEN=256)      :: src_path, f77_name, f90_name, ext, fname
CHARACTER (LEN=3)        :: drive
INTEGER                  :: n_drive, n_fname, n_ext, n_src_path, n_args
LOGICAL                  :: finished = .FALSE.
INTEGER(2)               :: i_args, arg_status

i_args = 0
n_args = NARGS()-1

DO WHILE(.not. finished)

    IF( n_args == 0 ) THEN

        WRITE(*, '(a)', ADVANCE='NO')' Enter name of Fortran source file: '
        READ(*, '(a)', IOSTAT=iostatus) f77_name
        IF (iostatus < 0) STOP               ! Halts gracefully when the names are
                                             ! read from a file and the end is reached\
    ELSE
        i_args = i_args + 1
        CALL GETARG(i_args,f77_name,STATUS=arg_status)
    END IF                                  
    IF (LEN_TRIM( f77_name ) == 0) THEN
        finished = .true.
        CYCLE
    END IF

    path_len = FULLPATHQQ(f77_name, buf)
    n_src_path = SPLITPATHQQ(buf, drive, src_path, fname, ext)  
    n_fname = LEN_TRIM(fname)
    n_drive = LEN_TRIM(drive)
    
    f77_name = drive(1:n_drive)//src_path(1:n_src_path)//fname(1:n_fname)//ext
    if( ext == '.inc' ) then        
        f90_name = drive(1:n_drive)//src_path(1:n_src_path)//fname(1:n_fname)//'.fi'                
    else    
        f90_name = drive(1:n_drive)//src_path(1:n_src_path)//fname(1:n_fname)//'.f90'    
    end if

    CALL process_file(f77_name, f90_name)

    if( n_args>0 .and. i_args>n_args ) THEN
        finished = .true.
    END IF
END DO

STOP


END PROGRAM to_f90

