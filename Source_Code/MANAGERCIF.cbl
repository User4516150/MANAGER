       ID DIVISION.
       PROGRAM-ID. MANAGERCIF.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77 ind-arr PIC 9(5) VALUE 1.

       77 ind-wheel-mixed PIC 9(5) VALUE 1.
       77 ind-num PIC 9(2) VALUE 1.

       77 wscif-key PIC X(20) VALUE ALL SPACES.

       77 num-key REDEFINES wscif-key PIC 9 COMP OCCURS 20.

       77 num-start PIC 9(5).
       77 num-round PIC X.

       77 num-add PIC 9(5) VALUE 1.
       77 res-subtraction PIC 9(5) VALUE 1.

       77 answer PIC A VALUE SPACE.

       LINKAGE SECTION.
       01 ws-arr-wheel.
         02 arr-wheel PIC X OCCURS 85248.

       01 ws-key PIC X(20).

       01 ws-wheel-mixed.
         02 wheel-mixed PIC X OCCURS 85248.

       PROCEDURE DIVISION USING ws-arr-wheel, ws-key,
                                ws-wheel-mixed.

           MOVE ws-key TO wscif-key.

       GEN-KEY.
           IF ind-num GREATER 20
              COMPUTE num-add = num-add * ind-num
              GO TO VERIFY-NUMADD.


           IF num-key(ind-num) = 0
              ADD 2 TO num-key(ind-num).

           COMPUTE num-add = num-add + num-key(ind-num).

           ADD 1 TO ind-num.
           GO TO GEN-KEY.

       L-MOVE.
           MOVE arr-wheel(ind-arr)
                          TO wheel-mixed(ind-wheel-mixed).

           ADD 1 TO ind-arr, ind-wheel-mixed.

           IF ind-arr GREATER 85248
              MOVE 1 TO ind-arr.

           IF ind-wheel-mixed GREATER 85248 AND num-round = '1'
              MOVE 1 TO ind-wheel-mixed
              MOVE '2' TO num-round
              GO TO L-MOVE.

           IF num-round = '2' AND ind-wheel-mixed = num-start
              MOVE 1 TO ind-wheel-mixed
              GOBACK.

           GO TO L-MOVE.

       VERIFY-NUMADD.
           IF num-add GREATER 85248
                 PERFORM SUBTRACTION UNTIL num-add LESS 85249
                 MOVE num-add TO num-start
                 MOVE '1' TO num-round
                 MOVE num-add TO ind-arr
                 GO TO L-MOVE.

           MOVE num-add TO num-start.
           MOVE '1' TO num-round.
           MOVE num-add TO ind-arr.
           GO TO L-MOVE.

       SUBTRACTION.
           COMPUTE num-add =  num-add - 3.
