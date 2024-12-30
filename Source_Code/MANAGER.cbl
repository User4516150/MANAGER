      ******************************************************************
      * Author:
      * Date:
      * Purpose: Securely store and manage personal data.
      * Tectonics: GNU-COBOL.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MANAGER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *File WHEEL contains 85248 bytes.
           SELECT wheel ASSIGN TO 'WHEEL'.
           SELECT settings ASSIGN TO 'SETTINGS'
                                         FILE STATUS IS fs-settings.
           SELECT informations ASSIGN TO 'INFORMATIONS'
                                         ORGANIZATION INDEXED
                                         ACCESS DYNAMIC
                                         RECORD KEY IS site-name
                                         FILE STATUS IS fs.

       DATA DIVISION.
       FILE SECTION.
       FD wheel.
       01 rec-wheel PIC X.

       FD settings.
       01 rec-imp.
         02 psw PIC 9(5) COMP OCCURS 20.

       FD informations.
       01 rec.
         02 site-name PIC X(40).
         02 comp-informations PIC 9(5) COMP OCCURS 200.


       WORKING-STORAGE SECTION.
       01 ws-wheel-mixed.
         02 wheel-mixed PIC X OCCURS 85248 VALUE SPACES.

       77 fs PIC X(2).
       77 answer PIC A VALUE SPACE.
       77 ins-answer PIC X VALUE SPACE.
       77 mod-answer PIC X VALUE SPACE.
       77 res-answer PIC X VALUE SPACE.
       77 del-answer PIC X VALUE SPACE.
       77 list-answer PIC X VALUE SPACE.

       77 ws-line PIC 99 VALUE 9.
       77 ws-column PIC 999 VALUE 1.

       77 ind-rec PIC 999 VALUE 1.
       01 ws-recarr.
         02 rec-arr PIC X OCCURS 200 VALUE SPACES.

       01 ws-rec.
         02 ws-name PIC X(20) VALUE SPACES.
         02 ws-mail PIC X(30) VALUE SPACES.
         02 ws-pasw PIC X(30) VALUE SPACES.
         02 desc1 PIC X(60) VALUE SPACES.
         02 desc2 PIC X(60) VALUE SPACES.

       77 ind-arr PIC 9(5) VALUE 1.
       01 ws-arr-wheel.
         02 arr-wheel PIC X OCCURS 85248 VALUE SPACES.


       77 rd-wheel PIC X.

       77 ws-verify-key PIC X VALUE SPACE.

       77 fs-settings PIC X(2).

       77 ws-key PIC X(20) VALUE SPACES.

       77 num-start PIC 9(5) VALUE 1.

       77 ind-psw PIC 9(2) VALUE 1.
       01 ws-arr-psw.
         02 arr-psw PIC X OCCURS 20.
       01 verf.
         02 verf-psw-arr PIC X OCCURS 20.

       SCREEN SECTION.
       COPY MANAGER_SCREEN.

       PROCEDURE DIVISION.

       VERIFICA-FS-IMPOSTAZIONI.
           OPEN INPUT settings.

           IF fs-settings NOT = '00'
               DISPLAY scr
               DISPLAY sc-beginning
               ACCEPT sc-beginning
               GO TO VERIFY-KEY.

           DISPLAY scr, sc-cifgen.

       READ-SETTINGS.
           READ settings.

           OPEN INPUT wheel.

       READ-WHEEL.
           READ wheel.
           MOVE rec-wheel TO arr-wheel(ind-arr).
           ADD 1 TO ind-arr.

           IF ind-arr GREATER 85248 CLOSE wheel
                                    MOVE 1 TO ind-arr
                                    GO TO ASK-PSW.

           GO TO READ-WHEEL.

       VERIFY-FS.
           OPEN INPUT informations.

           IF fs NOT = "00"
              CLOSE informations
              OPEN OUTPUT informations.

           CLOSE informations.
           OPEN I-O informations.

       ASK-PSW.
           IF fs-settings NOT = '00'
               DISPLAY scr
               DISPLAY sc-cifgen
               PERFORM CIF-GEN 1 TIMES
               CLOSE settings
               OPEN OUTPUT settings
               GO TO MOD-SETTINGS.

           DISPLAY scr.
           DISPLAY sc-psw.
           ACCEPT sc-psw.


               MOVE ws-key TO ws-arr-psw.
           PERFORM CIF-GEN 1 TIMES.


       READ-PSW.
           IF ind-psw GREATER 20
               MOVE 1 TO ind-psw, ind-arr
               CLOSE settings
               GO TO VERIFY-PSW.

           MOVE psw(ind-psw) TO ind-arr.
           MOVE wheel-mixed(ind-arr) TO verf-psw-arr(ind-psw).
           ADD 1 TO ind-psw.

           GO TO READ-PSW.

       VERIFY-PSW.
           IF ws-arr-psw NOT = verf
               DISPLAY scr
               DISPLAY sc-psw-wrong
               ACCEPT OMITTED
               GO TO END-PROGRAM.

           PERFORM VERIFY-FS 1 TIMES.
           GO TO BEGINNING.


       MOD-SETTINGS.
           IF ind-arr GREATER 85248
              MOVE 1 TO ind-arr.

           IF ind-psw GREATER 20
               WRITE rec-imp
               MOVE 1 TO ind-psw
               MOVE SPACES TO ws-key, ws-arr-psw
               CLOSE settings
               OPEN INPUT settings
               READ settings
               DISPLAY scr
               DISPLAY sc-psw
               ACCEPT sc-psw
               MOVE ws-key TO ws-arr-psw
               GO TO READ-PSW.

           IF arr-psw(ind-psw) = wheel-mixed(ind-arr)
               MOVE ind-arr TO psw(ind-psw)
               ADD 1 TO ind-psw, ind-arr
               GO TO MOD-SETTINGS.

           ADD 1 TO ind-arr.

           GO TO MOD-SETTINGS.


       BEGINNING.
           MOVE SPACES TO site-name.
           DISPLAY scr.
           DISPLAY version.
           DISPLAY cornice.
           ACCEPT sc-answer.

           IF answer = 'i' OR 'I'
              MOVE SPACE TO answer
              DISPLAY scr
              MOVE '1' TO ins-answer
              GO TO INSERTION.

           IF answer = 'l' OR 'L'
              MOVE SPACE TO answer
              MOVE '1' TO list-answer
              CLOSE informations
              OPEN INPUT informations
              GO TO LIST.

           IF answer = 'r' OR 'R'
               MOVE SPACE TO answer
               MOVE '1' TO res-answer
               GO TO BEGIN-RESEARCH.


           IF answer = 'm' OR 'M'
               MOVE SPACE TO answer
               MOVE '1' TO mod-answer
               GO TO BEGIN-RESEARCH.

           IF answer = 'd' OR 'D'
               MOVE SPACE TO answer
               MOVE '1' TO del-answer
               GO TO BEGIN-RESEARCH.

           IF answer = SPACES OR 'n' OR 'N'
              CLOSE informations
              GO TO END-PROGRAM.

           MOVE SPACE TO ANSWER.
           GO TO BEGINNING.


       INSERTION.
           MOVE 9 TO ws-line.
           MOVE 1 TO ws-column.
           DISPLAY scr.
           DISPLAY row-high.
           PERFORM ADD-ROW-LEFT 13 TIMES.
           DISPLAY row-low.
           MOVE 9 TO ws-line.
           MOVE 120 TO ws-column.
           PERFORM ADD-ROW-RIGHT 13 TIMES.

           DISPLAY sc-insertion.
           ACCEPT sc-insertion.

           MOVE ws-rec TO ws-recarr.
           GO TO CIPHER.


       BEGIN-RESEARCH.
           DISPLAY scr.
           DISPLAY sc-research.
           ACCEPT sc-research.

           GO TO FIND.

       L-MODIFY.
           MOVE 9 TO ws-line.
           MOVE 1 TO ws-column.
           DISPLAY scr.
           DISPLAY row-high.
           PERFORM ADD-ROW-LEFT 13 TIMES.
           DISPLAY row-low.
           MOVE 9 TO ws-line.
           MOVE 120 TO ws-column.
           PERFORM ADD-ROW-RIGHT 13 TIMES.
           DISPLAY sc-modify.
           ACCEPT sc-modify.

           MOVE ws-rec TO ws-recarr.
           GO TO CIPHER.

       FIND.
           READ informations INVALID KEY
                DISPLAY scr
                DISPLAY sc-notfound
                ACCEPT OMITTED
                MOVE SPACES TO site-name
                GO TO BEGINNING.

           GO TO DECIPHER.


       LIST.
           READ informations AT END
                DISPLAY scr
                DISPLAY sc-eof
                ACCEPT OMITTED
                MOVE SPACE TO list-answer, answer, ws-rec, ws-recarr
                CLOSE informations, OPEN I-O informations
                GO TO BEGINNING.

           GO TO DECIPHER.

       SEE-RESEARCH.
           MOVE 9 TO ws-line.
           MOVE 1 TO ws-column.
           DISPLAY scr.
           DISPLAY row-high.
           PERFORM ADD-ROW-LEFT 14 TIMES.
           DISPLAY row-low.
           MOVE 9 TO ws-line.
           MOVE 120 TO ws-column.
           PERFORM ADD-ROW-RIGHT 14 TIMES.
           DISPLAY sc-seerec.
           ACCEPT sc-seerec.

           IF res-answer = '1'
               MOVE SPACES TO res-answer, answer, site-name
               GO TO BEGINNING.

           IF list-answer = '1' AND answer = 'n' OR SPACE
               GO TO LIST.
           IF list-answer = '1' AND answer = 's'
               CLOSE informations, OPEN I-O informations
               MOVE '0' TO list-answer
               MOVE SPACES TO answer, site-name, ws-rec, ws-recarr
               GO TO BEGINNING.

           IF del-answer = '1' AND answer = 's'
               MOVE SPACE TO answer
               GO TO L-DELETE
           ELSE
              DISPLAY " OPERATION CANCELLED! "
              ACCEPT OMITTED
              MOVE '0' TO del-answer
              MOVE SPACES TO answer, ws-rec, ws-recarr
              GO TO BEGINNING.


           GO TO BEGINNING.

       READ-NEXT.
           READ informations NEXT AT END
                DISPLAY scr
                DISPLAY sc-eof
                ACCEPT OMITTED
                MOVE SPACE TO list-answer, ws-rec, ws-recarr
                GO TO BEGINNING.

           GO TO DECIPHER.


       L-DELETE.
           DELETE informations INVALID KEY
                  DISPLAY " Error!? "
                  ACCEPT OMITTED
                 MOVE SPACES TO del-answer, site-name, ws-rec, ws-recarr
                  GO TO BEGINNING.

           MOVE '0' TO del-answer.
           MOVE SPACES TO site-name, ws-rec, ws-recarr.
           GO TO BEGINNING.

       L-REWRITE.
           REWRITE rec INVALID KEY
                       DISPLAY ' ERROR!?'
                       ACCEPT OMITTED
                       GO TO BEGINNING.
           MOVE SPACES TO ws-rec, ws-recarr.
           MOVE SPACE TO mod-answer.
           GO TO BEGINNING.

       CIF-GEN.
           CALL "MANAGERCIF"
           USING ws-arr-wheel, ws-key, ws-wheel-mixed.
           EXIT.

       CIPHER.
           IF ind-rec GREATER THAN 200 AND ins-answer = '1'
               WRITE rec
               MOVE 1 TO ind-rec, ind-arr
               MOVE SPACES TO ws-rec, ws-recarr
               MOVE SPACE TO ins-answer
               GO TO BEGINNING.
           IF ind-rec GREATER THAN 200 AND mod-answer = '1'
               MOVE 1 TO ind-rec, ind-arr
               MOVE SPACES TO ws-rec, ws-recarr
               MOVE SPACE TO mod-answer
               GO TO L-REWRITE.

           IF ind-arr GREATER THAN 85248
               MOVE 1 TO ind-arr.

           IF rec-arr(ind-rec) = wheel-mixed(ind-arr)
               MOVE ind-arr TO comp-informations(ind-rec)
               ADD 1 TO ind-rec
               GO TO CIPHER.

           ADD 1 TO ind-arr.
           GO TO CIPHER.

       DECIPHER.
           IF ind-rec GREATER THAN 200 AND list-answer = '1'
               MOVE ws-recarr TO ws-rec
               MOVE 1 TO ind-rec
               GO TO SEE-RESEARCH.
           IF ind-rec GREATER THAN 200 AND res-answer = '1'
               MOVE ws-recarr TO ws-rec
               MOVE 1 TO ind-rec
               GO TO SEE-RESEARCH.
           IF ind-rec GREATER THAN 200 AND mod-answer = '1'
               MOVE ws-recarr TO ws-rec
               MOVE 1 TO ind-rec
               GO TO L-MODIFY.
           IF ind-rec GREATER THAN 200 AND del-answer = '1'
               MOVE ws-recarr TO ws-rec
               MOVE 1 TO ind-rec
               GO TO SEE-RESEARCH.

           MOVE comp-informations(ind-rec) TO ind-arr.
           MOVE wheel-mixed(ind-arr) TO rec-arr(ind-rec).
           ADD 1 TO ind-rec.
           GO TO DECIPHER.


       VERIFY-KEY.
           IF ws-key = SPACE OR SPACES
               DISPLAY scr
               DISPLAY sc-beginning
               ACCEPT sc-beginning
               GO TO VERIFY-KEY.

           DISPLAY scr, sc-cifgen.
           MOVE ws-key TO ws-arr-psw.
           OPEN INPUT wheel.
           GO TO READ-WHEEL.


       ADD-ROW-LEFT.
           ADD 1 TO ws-line.
           DISPLAY row-left.
       ADD-ROW-RIGHT.
           ADD 1 TO ws-line.
           DISPLAY row-right.


       END-PROGRAM.
           STOP RUN.
       END PROGRAM MANAGER.
