      * SCREEN SECTION.
       01 scr BLANK SCREEN BACKGROUND-COLOR 6 FOREGROUND-COLOR 6.

       01 sc-beginning BACKGROUND-COLOR 6 FOREGROUND-COLOR 4.
         02 FILLER LINE 15 COL 38 VALUE 'KEY TO CIPHER: '.
         02 FILLER LINE 15 COL 62 USING ws-key.

       01 sc-psw BACKGROUND-COLOR 6 FOREGROUND-COLOR 4.
         02 FILLER LINE 15 COL 38 VALUE 'PASSWORD: '.
         02 FILLER LINE 15 COL 48 USING ws-key.

       01 sc-psw-wrong FOREGROUND-COLOR 4 LINE 15 COL 48 VALUE
                                                       'PSW WRONG!'.
       01 sc-elaborating FOREGROUND-COLOR 4 LINE 15 COL 40 VALUE
                                                     'ELABORATING! '.

       01 version BACKGROUND-COLOR 6 FOREGROUND-COLOR 4 LINE 9 COL 56
                                                      VALUE ' V 1.0.0 '.
       01 cornice BACKGROUND-COLOR 4 FOREGROUND-COLOR 4.
         02 lin-high PIC X(23) LINE 10 COL 49.
         02 lin-low PIC X(23) LINE 24 COL 49.
         02 row1-left PIC X LINE 11 COL 49.
         02 row2-left PIC X LINE 12 COL 49.
         02 txt1 LINE 12 COL 50 BACKGROUND-COLOR 6 FOREGROUND-COLOR 4
                                VALUE ' TYPE:'.
         02 row3-left PIC X LINE 13 COL 49.
         02 row4-left PIC X LINE 14 COL 49.
         02 txt2 LINE 14 COL 50 BACKGROUND-COLOR 6 FOREGROUND-COLOR 4
                                VALUE ' i TO INSERT'.
         02 row5-left PIC X LINE 15 COL 49.
         02 row6-left PIC X LINE 16 COL 49.
         02 txt3 LINE 16 COL 50 BACKGROUND-COLOR 6 FOREGROUND-COLOR 4
                                VALUE ' l TO LIST'.
         02 row7-left PIC X LINE 17 COL 49.
         02 row8-left PIC X LINE 18 COL 49.
         02 txt4 LINE 18 COL 50 BACKGROUND-COLOR 6 FOREGROUND-COLOR 4
                                VALUE ' r TO RESEARCH'.
         02 row9-left PIC X LINE 19 COL 49.
         02 row10-left PIC X LINE 20 COL 49.
         02 txt5 LINE 20 COL 50 BACKGROUND-COLOR 6 FOREGROUND-COLOR 4
                                VALUE ' m TO MODIFY'.
         02 row11-left PIC X LINE 21 COL 49.
         02 row12-left PIC X LINE 22 COL 49.
         02 txt6 LINE 22 COL 50 BACKGROUND-COLOR 6 FOREGROUND-COLOR 4
                                VALUE ' d TO DELETE'.
         02 row13-left PIC X LINE 23 COL 49.
         02 row14-left PIC X LINE 24 COL 49.

         02 row1-right PIC X LINE 11 COL 71.
         02 row2-right PIC X LINE 12 COL 71.
         02 row3-right PIC X LINE 13 COL 71.
         02 row4-right PIC X LINE 14 COL 71.
         02 row5-right PIC X LINE 15 COL 71.
         02 row6-right PIC X LINE 16 COL 71.
         02 row7-right PIC X LINE 17 COL 71.
         02 row8-right PIC X LINE 18 COL 71.
         02 row9-right PIC X LINE 19 COL 71.
         02 row10-right PIC X LINE 20 COL 71.
         02 row11-right PIC X LINE 21 COL 71.
         02 row12-right PIC X LINE 22 COL 71.
         02 row13-right PIC X LINE 23 COL 71.
         02 row14-right PIC X LINE 24 COL 71.
       01 sc-answer PIC A LINE 30 COL 1
           USING answer BACKGROUND-COLOR 7 FOREGROUND-COLOR 4.

       01 cornice-dynamic BACKGROUND-COLOR 4.
         02 row-high PIC X(120) LINE ws-line COL ws-column.
         02 row-low PIC X(120) LINE ws-line COL ws-column.
         02 row-left PIC X LINE ws-line COL ws-column.
         02 row-right PIC X LINE ws-line COL ws-column.

       01 sc-insertion BACKGROUND-COLOR 6 FOREGROUND-COLOR 4.
         02 FILLER LINE 2 COL 55 VALUE ' INSERTION '.

         02 FILLER LINE 11 COL 2 VALUE ' SITE NAME: '.
         02 FILLER PIC X(40) LINE 11 COL 21 USING site-name.

         02 FILLER LINE 13 COL 2 VALUE ' NAME: '.
         02 FILLER PIC X(20) LINE 13 COL 16 USING ws-name.

         02 FILLER LINE 15 COL 2 VALUE ' EMAIL: '.
         02 FILLER PIC X(30) LINE 15 COL 20 USING ws-mail.

         02 FILLER LINE 17 COL 2 VALUE ' PASSWORD: '.
         02 FILLER PIC X(30) LINE 17 COL 23 USING ws-pasw.

         02 FILLER LINE 19 COL 2 VALUE ' DESCRIPTION: '.
         02 FILLER PIC X(60) LINE 19 COL 26 USING desc1.
         02 FILLER PIC X(60) LINE 21 COL 26 USING desc2.

       01 sc-modify BACKGROUND-COLOR 6 FOREGROUND-COLOR 4.
         02 FILLER LINE 2 COL 55 VALUE ' MODIFY: '.

         02 FILLER LINE 11 COL 2 VALUE ' SITE NAME: '.
         02 FILLER PIC X(40) LINE 11 COL 21 USING site-name.

         02 FILLER LINE 13 COL 2 VALUE ' NAME: '.
         02 FILLER PIC X(20) LINE 13 COL 16 USING ws-name.

         02 FILLER LINE 15 COL 2 VALUE ' EMAIL: '.
         02 FILLER PIC X(30) LINE 15 COL 20 USING ws-mail.

         02 FILLER LINE 17 COL 2 VALUE ' PASSWORD: '.
         02 FILLER PIC X(30) LINE 17 COL 23 USING ws-pasw.

         02 FILLER LINE 19 COL 2 VALUE ' DESCRIPTION: '.
         02 FILLER PIC X(60) LINE 19 COL 27 USING desc1.
         02 FILLER PIC X(60) LINE 21 COL 27 USING desc2.


       01 sc-seerec BACKGROUND-COLOR 6 FOREGROUND-COLOR 4.
         02 FILLER LINE 2 COL 51 VALUE ' SEE RESEARCH '.

         02 FILLER LINE 11 COL 3 VALUE 'SITE NAME: '.
         02 FILLER PIC X(40) LINE 11 COL 14 FROM site-name.

         02 FILLER LINE 13 COL 3 VALUE 'NAME: '.
         02 FILLER PIC X(20) LINE 13 COL 10 FROM ws-name.

         02 FILLER LINE 15 COL 3 VALUE 'EMAIL: '.
         02 FILLER PIC X(30) LINE 15 COL 11 FROM ws-mail.

         02 FILLER LINE 17 COL 3 VALUE 'PASSWORD: '.
         02 FILLER PIC X(30) LINE 17 COL 13 FROM ws-pasw.

         02 FILLER LINE 19 COL 3 VALUE 'DESCRIPTION: '.
         02 FILLER PIC X(60) LINE 19 COL 15 FROM desc1.
         02 FILLER PIC X(60) LINE 21 COL 15 FROM desc2.

         02 FILLER LINE 23 COL 32 VALUE
                                  ' THIS? y/n OR f TO FINISH:'.
         02 FILLER BACKGROUND-COLOR 7 PIC A LINE 23 COL 69 USING answer.


       01 sc-research BACKGROUND-COLOR 6 FOREGROUND-COLOR 4.
         02 FILLER LINE 15 COL 30 VALUE ' RESEARCH: '.
         02 FILLER PIC X(40) LINE 15 COL 52 USING site-name.


       01 sc-notfound BACKGROUND-COLOR 6 FOREGROUND-COLOR 4.
         02 FILLER LINE 15 COL 40 VALUE ' NOTHING FOUND! '.


       01 sc-eof BACKGROUND-COLOR 6 FOREGROUND-COLOR 4.
         02 FILLER LINE 15 COL 45 VALUE ' END OF FILE! '.


       01 sc-cifgen BACKGROUND-COLOR 6 FOREGROUND-COLOR 4.
         02 FILLER LINE 15 COL 45 VALUE " WAIT PLEASE... ".
