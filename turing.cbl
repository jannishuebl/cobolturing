       IDENTIFICATION DIVISION.
       PROGRAM-ID. Turingmaschine.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 zustandcounter     pic 99.
       01 coll     pic 99.
       01 coll2     pic 99.
       01 row     pic 99.
       01 temp     pic 99.
       01 counter     pic 9999.
       01 counter2     pic 9999.
       01 chartomove pic X.
       01 zustandtoedit pic 99.
       01 startword pic X(20).
       01 bandlinks.
           05 kopfLinks pic X.
           05 rumpfLinks.
               15 zeichenlinks pic X occurs 10000 times.
       01 bandrechts.
           05 kopfRechts pic X.
           05 rumpfRechts.
               15 zeichenrechts pic X occurs 10000 times.
       01 letitrun pic 9 value 1.
           88 ende value 0.
           88 weiter value 1.
       01 comando       PIC X       Value SPACE.
           88 quit                  value "q".
           88 acceptAlpabet         value "a".
           88 acceptStartword         value "w".
           88 editZustaende         value "z".

       01 editZustaendecomando  PIC X       Value SPACE.
           88 editzustaendeready            value "r".
           88 addzustand                    value "a".
           88 editzustand                    value "e".
       01 menu          PIC X(60)   Value 
           "Quit(q) Alphabet(a) Startword(w) Start(s) Zustands Menu(z)".
       01 zustandstabelle.
           05 zustaende OCCURS 8 times.
               10 prefix.
                   15 Filler pic XXX value space.
                   15 Filler pic X value "Z".
                   15 ZNumber pic 99.
                   15 Filler pic XXX value space.
               10 zustandCol occurs 6 times.
                   15 writeChar pic X.
                   15 Filler pic X value "Z".
                   15 gotoZ pic 99.
                   15 moveto pic X.
                   15 Filler pic XX value "  ".
       01 alp.
           05 char     pic X value  space occurs 5 times.



       01 KopfzeileZustandstabelle.
           05 Filler pic X(8) value "Zustand".
           05 KopfAlpabet occurs 6 times.
               10   Filler pic XXX value space.
               10   kopfChar   pic X value space.
               10   Filler pic XXX value space.
           
       01 zeileZustandstabelle.
           05 Filler pic XX value space.
           05 zZustand pic X(8) value "Zustand".
           05 Filler pic XXX value space.
           05 zeilealp1   pic X value space.
           05 Filler pic XXX value space.
           05 zeilealp2   pic X value space.
           05 Filler pic XXX value space.
           05 zeilealp3   pic X value space.
           05 Filler pic XXX value space.
           05 zeilealp4   pic X value space.
           05 Filler pic XXX value space.
           05 zeilealp5   pic X value space.
       PROCEDURE DIVISION.
       PROGRAM-STEUERUNGS SECTION.
       PR-1000.
           perform init.
           perform show-display.
           perform select-command with test after
                   until quit.
       PR-9999.
           STOP RUN.

       init section.
       init-1000.
           move "0" to char(1).
           move "1" to char(2).
       init-9999.
       select-command section.
       sel-1000.
           ACCEPT  comando AT 2316.
           if acceptAlpabet 
               perform accept-alphabet.
           if editZustaende         
               perform zustaendeMenuSection.
           if acceptStartword         
               perform accept-startword.
       sel-9999.
           
       show-display section.
       sho-1000.
           DISPLAY "###  COBOL TURINGMASCHINE ###" AT 0205.
           DISPLAY "ALPABET(max. 5 ZEICHEN):" AT 0405.
           DISPLAY alp     AT 0430.
           DISPLAY "Startword:" AT 0505.
           DISPLAY startword     AT 0530.
           DISPLAY "Zustandsuebergangstabelle:" AT 0445.
           DISPLAY  menu AT 2205.
           DISPLAY "Commando:" AT 2305.
           perform print-zustaede. 
       sho-9999.

       accept-alphabet section.
       alp-1000.
           ACCEPT alp at 0430.
           perform print-zustaede. 
       alp-9999.

       print-band section.
       ast-1000.
           move 1 to counter.
           perform print-band-left until counter = 15.
           move 1 to counter.
           perform print-band-right until counter = 15.
       ast-9999.

       print-band-left section.
       pbl-1000.
           compute temp = 40 + counter.
           display zeichenlinks(counter) at line number 18 col number 
           temp.
           compute counter = counter + 1.
       pbl-9999.

       print-band-right section.
       pbr-1000.
           compute temp = 39 - counter.
           display zeichenrechts(counter) at line number 18 col number 
           temp.
           compute counter = counter + 1.
       pbr-9999.

       accept-startword section.
       ast-1000.
           ACCEPT startword at 0530.
           move "_" to bandlinks.
           move startword to bandlinks.
           perform print-band.
       ast-9999.

       zustaendeMenuSection section.
       ezu-1000.
           DISPLAY "Add Zustand(a) edit Zustand(e) delete Zustand (e) ready(r)" AT 2205.
           perform select-command-edit-zustaende with test after 
           until editzustaendeready.
           DISPLAY  menu AT 2205.
       ezu-9999.

       select-command-edit-zustaende section.
       sel-1000.
           ACCEPT  editZustaendecomando AT 2316.
           if addzustand 
               perform addzustandsection.
           if editzustand                    
               perform askzustand.
       sel-9999.

       askzustand section.
       akz-1000.
           display "enter zustand to edit:" at 2305.
           display "Z" at 2324.
           ACCEPT  zustandcounter AT 2325.
           compute row = 6 + zustandcounter.
           move 1 to counter.
           perform acceptZustaende until counter = 6 or
           kopfChar(counter) = space.
           DISPLAY "Commando:                 " AT 2305.
       akz-9999.

       addzustandsection section.
       azs-1000.
           compute zustandcounter = zustandcounter + 1.
           perform print-zustaede.
           compute row = 6 + zustandcounter.
           move 1 to counter.
           perform acceptZustaende until counter = 6 or
           kopfChar(counter) = space.
       azs-9999.

       acceptZustaende section.
       mck-1000.
           compute coll = 39 + 7 * (counter - 1).  
           accept writeChar(zustandcounter, counter)  at line number row 
           col number coll.
           compute coll2 = coll + 2.  
           accept gotoZ(zustandcounter, counter)  at line number row 
           col number coll2.
           compute coll2 = coll + 4.  
           accept moveto(zustandcounter, counter)  at line number row 
           col number coll2.
           compute counter = counter + 1.
       mck-9999.

       print-zustaede section.
       zus-1000.
           move 1 to counter.

           move char(1) to chartomove. 
           display counter at 0101.
           perform movechartoKopf until counter = 6 or chartomove = 
           space.
           move "_" to kopfChar(counter).
           display counter at 0202.

           display KopfzeileZustandstabelle at 0630.
           move 1 to counter.
           compute temp = zustandcounter + 1.
           perform printTabelline until counter = temp. 
       zus-9999.
       printTabelline section.

       ptl-1000.
           compute row = 6 + counter.
           move counter to ZNumber(counter).
           display prefix(counter) at line number row col number 30.
           move 1 to counter2.
           perform printZustandCom until counter2 = 6 or
           kopfChar(counter2) = space.
           compute counter = 1 + counter.
       ptl-9999.

       printZustandCom section.
       pzc-1000.
           compute coll = 39 + 7 * (counter2 - 1).  
           display zustandCol(counter, counter2) at line number row col number coll.
           compute counter2 = counter2 + 1.
       pzc-9999.

       movechartoKopf section.
       mck-1000.
           move chartomove to kopfChar(counter).
           compute counter = counter + 1.
           display counter at 0303.
           move char(counter) to chartomove. 
       mck-9999.
