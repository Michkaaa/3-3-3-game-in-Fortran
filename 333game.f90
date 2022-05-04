!gfortran [PATH]\333game.f90 -o 333game
!  *** GRID ( LINES , COLUMNS) ***
FUNCTION INPUT(lincol) RESULT(out) 
    IMPLICIT NONE
    LOGICAL :: lincol !TRUE = LINE   FALSE = COLUMN
    INTEGER :: out
    INTEGER :: temp
    temp = 0
    DO WHILE (.TRUE.)
        IF (lincol) THEN
            PRINT *, "LINE = "
        ELSE
            PRINT *, "COLUMN = "
        END IF 
        READ (*,*) temp
        IF ((temp < 1).OR.(temp > 3)) THEN
            PRINT *, "/!\ INVALID COORDINATES /!\"
        ELSE 
            EXIT 
        END IF 
    END DO 
    out = temp
END FUNCTION INPUT

SUBROUTINE ADDMARK(grid,turn)
    CHARACTER, DIMENSION(3,3) :: grid 
    CHARACTER :: turn
    INTEGER :: lin, col 
    INTEGER :: INPUT

    DO WHILE (.TRUE.)
        lin = INPUT(.TRUE.)
        col = INPUT(.FALSE.)
        IF (grid(lin,col) == " ") THEN
            EXIT 
        ELSE 
            PRINT *, "/!\ INVALID COORDINATES /!\"
        END IF 
    END DO 
    grid(lin,col) = turn
END SUBROUTINE ADDMARK

FUNCTION WINLIN(grid, turn) RESULT(winner)
    IMPLICIT NONE
    CHARACTER, DIMENSION(3,3), INTENT(IN) :: grid
    CHARACTER, INTENT(IN) :: turn
    LOGICAL :: winner
    LOGICAL :: temp
    INTEGER :: i, j, k
    temp = .FALSE.

    DO i = 1, 3
        k = 0
        DO j = 1, 3
            IF (grid(i,j) == turn) THEN
                K = k+1
            END IF 
        END DO 
        IF (k == 3) THEN
            temp = .TRUE.
        END IF 
    END DO
    winner = temp
END FUNCTION WINLIN

FUNCTION WINCOL(grid, turn) RESULT(winner)
    IMPLICIT NONE
    CHARACTER, DIMENSION(3,3), INTENT(IN) :: grid
    CHARACTER, INTENT(IN) :: turn
    LOGICAL :: winner
    LOGICAL :: temp
    INTEGER :: i, j, k
    temp = .FALSE.

    DO i = 1, 3
        k = 0
        DO j = 1, 3
            IF (grid(j,i) == turn) THEN
                K = k+1
            END IF 
        END DO 
        IF (k == 3) THEN
            temp = .TRUE.
        END IF 
    END DO
    winner = temp
END FUNCTION WINCOL

FUNCTION WINDIAG(grid, turn) RESULT(winner)
    IMPLICIT NONE
    CHARACTER, DIMENSION(3,3), INTENT(IN) :: grid
    CHARACTER, INTENT(IN) :: turn
    LOGICAL :: winner
    LOGICAL :: temp
    INTEGER :: i, k
    temp = .FALSE.

    k = 0
    DO i = 1, 3
        IF (grid(i,i) == turn) THEN
            k = k+1
        END IF 
    END DO 
    IF (k == 3) THEN 
        temp = .TRUE. 
    END IF 
    k = 0
    DO i = 1, 3
        IF (grid(i,4-i) == turn) THEN
            k = k+1
        END IF 
    END DO 
    IF (k == 3) THEN 
        temp = .TRUE. 
    END IF 
    winner = temp
END FUNCTION WINDIAG 

FUNCTION WIN(grid, turn) RESULT(winner)
    CHARACTER, DIMENSION(3,3), INTENT(IN) :: grid
    CHARACTER, INTENT(IN) :: turn
    LOGICAL :: winner

    LOGICAL :: WINLIN 
    LOGICAL :: WINCOL 
    LOGICAL :: WINDIAG 

    winner = (WINLIN(grid, turn)).OR.(WINCOL(grid, turn)).OR.(WINDIAG(grid, turn))
END FUNCTION WIN

SUBROUTINE PRINTGRID(grid)
    IMPLICIT NONE
    CHARACTER, DIMENSION(3,3), INTENT(OUT) :: grid
    INTEGER :: i 
    PRINT *, "             1 2 3"
    DO i = 1, 3
        PRINT *, "            +-+-+-+"
        PRINT *, i, "|", grid(i,1), "|", grid(i,2), "|", grid(i,3), "|"
    END DO
    PRINT *, "            +-+-+-+"
END SUBROUTINE PRINTGRID

SUBROUTINE OUTPUT(grid, tour)
    IMPLICIT NONE
    CHARACTER, DIMENSION(3,3), INTENT(OUT) :: grid
    CHARACTER, INTENT(OUT) :: tour
    CALL PRINTGRID(grid)
    PRINT *, "IT'S THE TURN OF " , tour
END SUBROUTINE OUTPUT

PROGRAM MORPION 
    IMPLICIT NONE
    CHARACTER, DIMENSION(3,3) :: grid
    LOGICAL :: WIN
    CHARACTER :: player
    LOGICAL :: matchNull
    INTEGER :: i 
    i = 0 

    grid(:,:) = " "
    player = "X"
    matchNull = .TRUE.

    DO WHILE (.TRUE.)
        i = i+1
        CALL OUTPUT(grid,player)
        CALL ADDMARK(grid,player)
        IF (WIN(grid, player)) THEN
            matchNull = .FALSE.
            EXIT 
        ELSE IF (i == 9) THEN
            EXIT
        END IF 
        IF (player == "X") THEN
            player = "O"
        ELSE 
            player = "X"
        END IF 
    END DO 
    CALL PRINTGRID(grid)
    IF (matchNull) THEN
        PRINT *, "TIE"
    ELSE
        PRINT *, player, " WON"
    END IF
END PROGRAM MORPION 
