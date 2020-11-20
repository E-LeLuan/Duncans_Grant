Date sent:        Fri, 08 Aug 1997 14:38:38 -0500
From:             Gary Raney <geraney@uic.edu>
Subject:          software
To:               Chuck Clifton <cec@psych.umass.edu>

PROGRAM REVAMP;       {Program to calculate subject and item means for }
USES CRT,DOS;         {studies using segmented passages. The program   }
                      { reads the .IXS (item-by-subject) files produced}
                      {by eyedry. Files are produced in the format of  }
                      {eyedry's subject files and item files.          }
                      {Program requires two condition specification    }
                      {files, one that specifies the condition of each }
                      {critical region (word), and one that specifies  }
                      {the item number of each critical region. Regions}
                      {labelled as condition 0 are ignored.            }
                      {Gary Raney, 7-2-97 }

CONST
    maxitem = 100;      {maximum number of items}
    maxtrialcond = 16;  {maximum number of conditions}
    maxreg = 12;        {maximum number of regions in item-by-subject file}

TYPE
    STR25     =       STRING[25];

VAR
   path               :STR25;
   ixsfile,wcdnfile   :STR25;
   ncdnfile           :STR25;
   sname,iname        :STR25;
   quit,check         :CHAR;
   wcdn,ncdn          :TEXT;
   ixs,itm,sub        :TEXT;
   c,i,r,s,w,x        :INTEGER;
   wc,tc              :INTEGER;
   item,trial,subj    :INTEGER;
   numcond,trialcond  :INTEGER;
   wordcond,wordnum   :INTEGER;
   itemadjust,tempsub :INTEGER;
   numtrialcond       :INTEGER;
   numreg,numword     :INTEGER;
   fixdur             :ARRAY [0..maxtrialcond] OF INTEGER;
   fixsum_s,fixnum_s  :ARRAY [0..maxitem] OF ARRAY [0..maxtrialcond] OF INTEGER;
   fixsum_w,fixnum_w  :ARRAY [0..maxitem] OF ARRAY [0..maxtrialcond] OF INTEGER;
   wcon               :ARRAY [0..maxitem] OF ARRAY [0..maxtrialcond] OF ARRAY [0..maxreg] OF BYTE;
   wnum               :ARRAY [0..maxitem] OF ARRAY [0..maxtrialcond] OF ARRAY [0..maxreg] OF BYTE;
   {wcon               :ARRAY [0..maxitem] OF ARRAY [0..maxtrialcond] OF ARRAY [0..maxreg] OF INTEGER;}
   {wnum               :ARRAY [0..maxitem] OF ARRAY [0..maxtrialcond] OF ARRAY [0..maxreg] OF INTEGER ABSOLUTE $8000:0;}

{ ************************************************** }
PROCEDURE Header;

BEGIN
  CLRSCR;
  WRITELN('Welcome to REVAMP.');
  WRITELN('Developed by Gary Raney at the University of Illinois at Chicago.');
  WRITELN;
  WRITELN('This program calculates means by subjects and by items.');
  WRITELN('It''s meant to be used with experiments that presents passages in segments.');
  WRITELN('It calculates mean fixation duration and number of fixations for each');
  WRITELN('subcondition.');
  WRITELN;
  WRITELN('Maximum number of unique trial conditions in a stimulus file = ',maxtrialcond,'.');
  WRITELN('Maximum number of unique target words in a stimulus file = ',maxitem,'.');
  WRITELN('Maximum number of regions in an item-by-subject data file = ',maxreg,'.');
  WRITELN;
  WRITELN('Output format for subject means is:');
  WRITELN(' subject, word condition, fix dur for trial cond 1, N for trial cond 1.');
  WRITELN(' Number of columns of fixation durations equals number of regions.');
  WRITELN;
  WRITELN('Output format for item means is:');
  WRITELN(' word, word condition, fix dur for trial cond 1, N for trial cond 1.');
  WRITELN(' Number of columns of fixation durations equals number of regions.');
  WRITELN;
  WRITE('Press ENTER to begin.');
  READLN;
  CLRSCR;
END;

{ ************************************ }
PROCEDURE Getfiles;            {Get file names and open input files.}

BEGIN
  WRITELN;
  WRITE('File name for item-by-subject data? (include path) -> ');
  READLN(ixsfile);
    ASSIGN(ixs,ixsfile);
    {$I-}
    RESET(ixs);
    {$I+}
  WRITE('Adjust item numbers in item-by-subject file by how much? (enter number) -> ');
  READLN(itemadjust);
  WRITELN;
  WRITE('File name for word CONDITION data? (include path) -> ');
  READLN(wcdnfile);
    ASSIGN(wcdn,wcdnfile);
    {$I-}
    RESET(wcdn);
    {$I+}
  WRITE('File name for word NUMBER data? (include path) -> ');
  READLN(ncdnfile);
    ASSIGN(ncdn,ncdnfile);
    {$I-}
    RESET(ncdn);
    {$I+}
  WRITE('Do you want to check each data line and condition labels? (Y/N) -> ');
  READLN(check);
  check:=UPCASE(check);
  WRITELN;
  {
  sname:='test1.sub';
  iname:='test1.itm';
  }
  WRITE('Output file name for Subject means? (include path) -> ');
  READLN(sname);
    ASSIGN(sub,sname);
    REWRITE(sub);
  WRITE('Output file name for Item (word) means? (include path) -> ');
  READLN(iname);
    ASSIGN(itm,iname);
    REWRITE(itm);
  WRITELN;

END;

{ ************************************************** }
PROCEDURE Zerobuffsub;

BEGIN
  FOR i:=0 to maxitem DO
    BEGIN
      FOR tc:=0 to maxtrialcond DO
        BEGIN
          fixsum_s[i,tc]:=0;
          fixnum_s[i,tc]:=0;
        END;
    END;
END;

{ ************************************************** }

PROCEDURE Zerobuffitem;

BEGIN
  FOR i:=0 to maxitem DO
    BEGIN
      FOR tc:=0 to maxtrialcond DO
        BEGIN
          fixsum_w[i,tc]:=0;
          fixnum_w[i,tc]:=0;
          FOR r:=0 to maxreg DO
            BEGIN
              wcon[i,tc,r]:=0;
              wnum[i,tc,r]:=0;
              fixdur[r]:=0;
            END;
        END;
    END;
END;

{ ************************************************** }
{
PROCEDURE Zerobuff;

BEGIN
  FOR i:=0 to maxitem DO
    BEGIN
      FOR tc:=0 to maxtrialcond DO
        BEGIN
          fixsum_w[i,tc]:=0;
          fixnum_w[i,tc]:=0;
          FOR r:=0 to maxreg DO
            BEGIN
              wcon[i,tc,r]:=0;
              wnum[i,tc,r]:=0;
              fixdur[r]:=0;
            END;
        END;
    END;
  FOR s:=0 to maxsub DO
    BEGIN
      FOR tc:=0 to maxtrialcond DO
        BEGIN
          fixsum_s[s,tc]:=0;
          fixnum_s[s,tc]:=0;
        END;
    END;
END;
}
{ ************************************************** }
PROCEDURE Readcdn;      {Reads word and item CDN files.}

BEGIN
  numcond:=0;
  numword:=0;
  numtrialcond:=0;
  REPEAT
    READ(wcdn,item,trialcond,numreg);  {Read first three numbers - Word condition.}
    IF (trialcond>numtrialcond) THEN numtrialcond:=trialcond;
    FOR r:=1 TO numreg DO
      BEGIN
        READ(wcdn,wcon[item,trialcond,r]);
        IF (wcon[item,trialcond,r]>numcond) THEN numcond:=wcon[item,trialcond,r];
      END;
    READLN(wcdn);

    READ(ncdn,item,trialcond,numreg);  {Read first three numbers - Word number.}
    FOR r:=1 TO numreg DO
      BEGIN
        READ(ncdn,wnum[item,trialcond,r]);
        IF (wnum[item,trialcond,r]>numword) THEN numword:=wnum[item,trialcond,r];
      END;
    READLN(ncdn);
    {
    write('Word condition for item=',item,' trialcond=',trialcond,' numreg=',numreg);
    for r:=1 to numreg DO write(wcon[item,trialcond,r]:4);
    writeln;
    write('Word number for item=',item,' trialcond=',trialcond,' numreg=',numreg);
    for r:=1 to numreg DO write(wnum[item,trialcond,r]:4);
    writeln;
    readln;
    }
  UNTIL EOF(wcdn);

  WRITELN('Largest number of regions in item-by-subject file = ',numreg);
  WRITELN('Largest trial condition = ',numtrialcond);
  WRITELN('Largest word condition = ',numcond);
  WRITELN('Largest word number = ',numword);
  WRITELN('If this is not correct, then the output will be meaningless.');
  WRITELN;
  WRITELN('Press ENTER to continue.');
  READLN;
  CLOSE(wcdn);
  CLOSE(ncdn);
  {Print out CDN to check.}
  {
  i:=0;
  writeln('word number data before reading data files.');
  FOR w:=1 TO numword DO
    BEGIN
      i:=i+1;
      WRITE('fixsum word ',w:3);
      FOR tc:=0 TO numtrialcond DO WRITE(' ',tc,'=',fixsum_w[w,tc]:4);
      WRITELN;
      WRITE('fixnum word ',w:3);
      FOR tc:=0 TO numtrialcond DO WRITE(' ',tc,'=',fixnum_w[w,tc]:4);
      WRITELN;
      IF (i=10) THEN
        BEGIN
          i:=0;
          READLN;
        END;
    END;
    READLN;
  }
END;

{ ************************************************** }

BEGIN                                    {MAIN BODY OF PROGRAM}
Header;                                         {Procedure}
REPEAT
  CLRSCR;
  path:='';
  WRITE('Press RETURN to continue or Q to Quit -> ');
  READLN(path);
  IF (path<>'Q') AND (path<>'q') THEN
  BEGIN
    Zerobuffsub;                             {Procedure}
    Zerobuffitem;                            {Procedure}
    Getfiles;                                {Procedure}
    Readcdn;                                 {Procedure}
    tempsub:=1;
    WRITELN('Working on subject ',tempsub);
      REPEAT                                 {Repeat until EOF .IXS}
        READ(ixs,subj,item,trialcond);       {Read info columns.}
        item:=item+itemadjust;
        FOR r:=1 TO maxreg DO fixdur[r]:=0;           {Zero fixation buffer.}
        FOR r:=1 TO numreg DO READ(ixs,fixdur[r]);    {Read fixations.}
        READLN(ixs);
        {Note. An oddity of pascal is that I can have more reads than}
        {number of regions on a line because READ doesn't advance to}
        {a new line.}

        IF (tempsub<>subj) THEN
          BEGIN
            {***Write Subject by Condition file for prior subject.}
            FOR wc:=1 TO numcond DO
              BEGIN
                WRITE(sub,tempsub:3,wc:3);
                FOR tc:=1 TO numtrialcond DO
                IF (fixnum_s[wc,tc] > 0) THEN
                  WRITE(sub,(fixsum_s[wc,tc]/fixnum_s[wc,tc]):5:0,fixnum_s[wc,tc]:4)
                ELSE
                  WRITE(sub,0:5,fixnum_s[wc,tc]:4);
                WRITELN(sub);
              END;
            WRITELN('Working on subject ',subj);
            tempsub:=subj;
            DELAY(1500);
            Zerobuffsub;               {Clear subject buffer before sorting.}
          END;

        IF (check = 'Y') THEN
          BEGIN
            WRITE('subj = ',subj:2,' item = ',item,' trialcond = ',trialcond);
            FOR r:=1 TO numreg DO write(fixdur[r]:4); writeln;
            write('                 word conditions ');
            for r:=1 to numreg DO write(wcon[item,trialcond,r]:4); writeln;
            write('                    word numbers ');
            for r:=1 to numreg DO write(wnum[item,trialcond,r]:4); writeln;
            WRITELN('Press Enter.');
            readln;
          END;

        {***Begin sorting fixations.***}
        IF (trialcond>0) THEN                    {Determine if OK data line.}
          BEGIN
            FOR r:=1 TO numreg DO
              BEGIN
                wordcond:=wcon[item,trialcond,r];
                wordnum:=wnum[item,trialcond,r];
                IF (fixdur[r] > 0) THEN
                  BEGIN
                    {*Sum fixations by WORD CONDITION and TRIAL condition.}
                    fixsum_s[wordcond,trialcond]:=fixsum_s[wordcond,trialcond]+fixdur[r];
                    fixnum_s[wordcond,trialcond]:=fixnum_s[wordcond,trialcond]+1;
                    {*Sum fixations by WORD NUMBER and TRIAL condition.}
                    fixsum_w[wordnum,trialcond]:=fixsum_w[wordnum,trialcond]+fixdur[r];
                    fixnum_w[wordnum,trialcond]:=fixnum_w[wordnum,trialcond]+1;
                    {
                    IF (item > 30) AND (item < 38) THEN
                    BEGIN
                      WRITELN('wordcond=',wordcond,'  trialcond=',trialcond,'  r=',r,'  fixdur=',fixdur[r]);
                      WRITELN('subject =',subj,'  fixsum_s[',wordcond,',',trialcond,']=',fixsum_s[wordcond,trialcond],
                      '  fixnum_s[',wordcond,',',trialcond,']=',fixnum_s[wordcond,trialcond]);
                      WRITELN('word number = ',wordnum,'  fixsum_w[',wordnum,',',trialcond,']=',fixsum_w[wordnum,trialcond],
                      '  fixnum_w[',wordnum,',',trialcond,']=',fixnum_w[wordnum,trialcond]);
                      delay(10);READLN;
                    END;
                    }
                  END;
              END;
              {
              WRITELN;
              FOR w:=1 TO numcond DO
                BEGIN
                  WRITE('fixsum subj ',subj:3);
                  FOR tc:=0 TO numtrialcond DO WRITE(' ',tc,'=',fixsum_s[w,tc]:4);
                  WRITELN;
                  WRITE('fixnum subj ',subj:3);
                  FOR tc:=0 TO numtrialcond DO WRITE(' ',tc,'=',fixnum_s[w,tc]:4);
                  WRITELN;
                END;
              READLN;
              }
              {
              i:=0;
              FOR w:=1 TO numword DO
                BEGIN
                  i:=i+1;
                  WRITE('fixSum word ',w:3);
                  FOR tc:=0 TO numtrialcond DO WRITE(' ',tc,'=',fixsum_w[w,tc]:4);
                  WRITELN;
                  WRITE('fixNum word ',w:3);
                  FOR tc:=0 TO numtrialcond DO WRITE(' ',tc,'=',fixnum_w[w,tc]:4);
                  WRITELN;
                  IF (i=10) THEN
                    BEGIN
                      i:=0;
                      READLN;
                    END;
                END;
              READLN;
              }
          END;  {do nothing if condition = 0}

      UNTIL EOF(ixs);   {keep reading file until EOF}

      {***Write Subject by Condition file for final subject. Needed because }
      {***data for subject N is written after reading data for subject N+1.}
      FOR wc:=1 TO numcond DO
        BEGIN
          WRITE(sub,tempsub:3,wc:3);
          FOR tc:=1 TO numtrialcond DO
            IF (fixnum_s[wc,tc] > 0) THEN
              WRITE(sub,(fixsum_s[wc,tc]/fixnum_s[wc,tc]):5:0,fixnum_s[wc,tc]:4)
            ELSE
              WRITE(sub,0:5,fixnum_s[wc,tc]:4);
          WRITELN(sub);
        END;

      {***Write Word by Condition file for all words.}
      FOR w:=1 TO numword DO
        BEGIN
          WRITE(itm,w:3);
          FOR tc:=1 TO numtrialcond DO
            IF (fixnum_w[w,tc] > 0) THEN
              WRITE(itm,(fixsum_w[w,tc]/fixnum_w[w,tc]):5:0,fixnum_w[w,tc]:4)
            ELSE
              WRITE(itm,0:5,fixnum_w[w,tc]:4);
          WRITELN(itm);
        END;

      CLOSE(ixs);
      CLOSE(sub);
      CLOSE(itm);
  END;
UNTIL (path = 'q') OR (path = 'Q');
END.

