
PROGRAM EYERINSE;       {Program to read eyetrack data from *.DAT files }
USES CRT,DOS;           {and perform preliminary adjustments to the     }
                        {y-values. Data is output as "filename.DAX".    }

CONST
    maxsub = 30;
    rcheck1 = 13;  {value for checking full return sweep }
    rcheck2 = 22;  {value for checking for short return sweep }
    maxcond = 15;  {maximum number of conditions}
    maxitems = 220; {maximum number of items}
    shortfix = 20; {y-values of fixations <= shortfix are not used when }
                   {determining if x position has exceeded xcheck.      }
                   {X and Y values of short fixations are set equal to  }
                   {the prior fixations as long as the short fixation   }
                   {follows a forward saccade.                          }

TYPE
    STR25     =       STRING[25];

VAR
   oname,iname        :STR25;
   sname,dname        :STR25;
   path,subnum,ext    :STR25;
   posfile,opath      :STR25;
   quit,poschoice     :CHAR;
   iii,ooo,ddd,spf    :TEXT;
   i,j,k,s,yoffset    :INTEGER;
   dum1,dum2          :SHORTINT;
   item,trial         :SHORTINT;
   cond,lines         :SHORTINT;
   answer,nsubs       :SHORTINT;
   count,temp         :SHORTINT;
   xdum,ydum,oldx     :SHORTINT;
   tfixed             :SHORTINT; 
   numtrials          :INTEGER;
   numfix,words       :INTEGER;
   unknown,duptime    :LONGINT;
   trialtime          :LONGINT;
   xflag              :BOOLEAN;
   xcheck             :REAL;     {percent value for checking forward x position before return sweep }
   x,y,newy           :ARRAY [0..300] OF SHORTINT;
   startf,endf        :ARRAY [0..300] of LONGINT;
   subj               :ARRAY [0..maxsub] OF STRING[12];
   numlines           :ARRAY [1..MAXCOND] OF ARRAY [1..MAXITEMS] OF SHORTINT;
   linelength         :ARRAY [1..MAXCOND] OF ARRAY [1..MAXITEMS] OF ARRAY [0..14] OF SHORTINT;

{ ************************************ }
PROCEDURE Displayparam;      {Display program parameters.}

BEGIN
  WRITELN('Welcome to EYERINSE');
  WRITELN('Developed by Gary Raney at the University of Massachuestts.');
  WRITELN;
  WRITELN(' Maximum number of subjects = ',maxsub,'.');
  WRITELN(' Maximum number of conditions = ',maxcond,'.');
  WRITELN(' Maximum number of items = ',maxitems,'.');
  WRITELN(' Value for checking for full return sweeps = ',rcheck1,'.');
  WRITELN(' Value for checking for short return sweeps = ',rcheck2,'.');
  WRITELN(' Fixations less than ',shortfix,' ms are not used when adjusting y-values.');
 { WRITELN(' Fixations less than ',shortfix,' are set to y-value = 99.');}
END;

{ ************************************ }
PROCEDURE Getsubs;           {Get subject names for data files.}

BEGIN
  WRITELN;
  WRITE('Enter path for output files (e.g., d:\subdata\). -> ');
  READLN(opath);
  WRITELN;
  WRITE('How many SUBJECTS? -> ');
  READLN(nsubs);
  FOR s:= 0 TO nsubs-1 DO
    BEGIN
      WRITE(' Enter FILE NAME for subject ',s+1,' (omitt the extension) -> ');
      READLN(subj[s]);
    END;
  WRITELN;
  WRITE('Enter extention for data files (omitt the ''.''). -> ');
  READLN(ext);
  WRITELN;
  WRITE('Enter Y offset. -> ');
  READLN(yoffset);
  WRITE('Enter percentage of line length to check for reading of entire line (e.g., 66). -> ');
  READLN(xcheck);
  xcheck:=xcheck/100;
END;

{ ************************************************** }
PROCEDURE Makefilename;

BEGIN
  delay(100);
  iname:=path+subj[s]+'.'+ext;
  WRITELN;
  WRITELN('input file name = ',iname);
  ASSIGN(iii,iname);
  {$I-}
  RESET(iii);
  {$I+}
  dname:=opath+subj[s]+'.DAX';
  WRITELN('Data file name = ',dname);
  delay(100);
  ASSIGN(ooo,dname);
  REWRITE(ooo);
  WRITELN;
  WRITELN('     Here we go...');
  WRITELN;
END;

{ ************************************************** }
PROCEDURE Readsenpos;                 {Reads .SEN file to determine how many }
                                      {lines of material are presented for   }
BEGIN                                 {each trial.                           }
  WRITELN;
  WRITELN('WARNING: If you do not have a position file, then all trials');
  WRITELN('  must have the same number of lines.');
  WRITE('Do you have a position file? (Y/N) -> ');
  READLN(poschoice);
  poschoice:=UPCASE(poschoice);
  IF poschoice = 'N' THEN
    BEGIN
      WRITE('Maximum number of lines in a trial? -> ');
      READLN(lines);
      lines:=lines-1;
      FOR i:=1 TO MAXCOND DO
        FOR j:=1 TO MAXITEMS DO
          BEGIN
            numlines[i,j]:=lines;
            FOR k:=0 TO 14 DO linelength[i,j,k]:=60;
          END;
    END
  ELSE
    BEGIN
      WRITE('Name of POSITION FILE? (Include path. e.g., d:\eye\) -> ');
      READLN(posfile);
      {posfile:=path+posfile;}
      WRITELN;
      WRITELN('Reading position file: ',posfile);
      ASSIGN(spf,posfile);
      {$I-}
      RESET(spf);
      {$I+}
      REPEAT
        READ(spf,cond,item,words);       {Read first 3 numbers.}
        temp:=yoffset;
        oldx:=0;
        FOR i:=1 TO words DO
        BEGIN
          READ(spf,xdum,ydum);               {Read positions.}
          IF ydum > temp THEN
            BEGIN
              linelength[cond,item,ydum-1]:=oldx;
              temp:=ydum;
              {
              writeln('x = ',xdum,'   y = ',ydum,'   oldx = ',oldx);
              writeln('linelength[',cond,',',item,',',ydum-1,'] = ',linelength[cond,item,ydum-1]);
              readln;
              }
            END;
          oldx:=xdum;
        END;
        READLN(spf);
        numlines[cond,item]:=ydum;
        linelength[cond,item,ydum]:=oldx;
        {
        writeln('x = ',xdum,'   y = ',ydum,'   oldx = ',oldx);
        writeln('linelength[',cond,',',item,',',ydum,'] = ',linelength[cond,item,ydum]);
        writeln('numlines[',cond,',',item,'] = ',numlines[cond,item]);
        readln;
        }
      UNTIL EOF(spf);
      CLOSE(spf);
    END;
END;

{ ************************************************** }

BEGIN                                    {MAIN BODY OF PROGRAM}
REPEAT
 CLRSCR;
 Displayparam;                                   {Procedure}
 WRITELN;
 WRITE('Enter data file path (e.g., d:\subdata\) or Q to Quit -> ');
 READLN(path);
 IF (path<>'Q') AND (path<>'q') THEN
 BEGIN
   Getsubs;                                      {Procedure}
   Readsenpos;                                   {Procedure}
   FOR s:=0 TO nsubs-1 DO                        {Loop for number of subjects.}
    BEGIN
      Makefilename;                              {Procedure}
      tfixed:=0;

      WHILE not EOF(iii) DO
      BEGIN
        {$I-}
        READ(iii,trial,cond,item);               {Read first three numbers.}
        {$I+}

        IF IORESULT<>0 THEN cond:=-1;            {Doublecheck for EOF.}

        IF (cond>0) and (cond<100) THEN          {Determine if question.}
          BEGIN
            READ(iii,trialtime,unknown,duptime,answer,numfix);
            FOR i:=0 TO numfix-1 DO READ(iii,x[i],y[i],startf[i],endf[i]);
            READLN(iii);                         {Read fixations.}
            writeln('Condition = ',cond:2,'    Item = ',item:2);
            {
            writeln('trial = ',trial);
            writeln('condition = ',cond);
            writeln('item = ',item);
            writeln('trialtime = ',trialtime);
            writeln('unknown = ',unknown);
            writeln('duptime = ',duptime);
            writeln('answer = ',answer);
            writeln('numfix = ',numfix);
            readln;
            }

            {***Correct for rollover of reading times during data collection.***}
            FOR i:=1 TO numfix-1 DO
              BEGIN
                IF (startf[i] < startf[i-1]) THEN
                  BEGIN
                    If (startf[i-1] < 33000) THEN startf[i]:=startf[i]+32767
                    ELSE startf[i]:=startf[i]+65535;
                  END;
                IF (endf[i] < endf[i-1]) THEN
                  BEGIN
                    If (endf[i-1] < 33000) THEN endf[i]:=endf[i]+32767
                    ELSE endf[i]:=endf[i]+65535;
                  END;
              END;

            {***Adjustment of y-values.***}
            xflag:=false;
            newy[0]:=yoffset;
            FOR i:=1 TO numfix-1 DO
              BEGIN
                IF ((endf[i]-startf[i]) > shortfix) AND (x[i] > (linelength[cond,item,newy[i-1]]*xcheck))
                    THEN xflag:=true;
                    {Check if x position has exceeded xcheck.}
                    {Don't set flag based on very short fixations.}
                {
                WRITE('xcheck[',i,'] = ',(linelength[cond,item,newy[i-1]]*xcheck):3:2,'   newy[i-1] = ',newy[i-1]);
                READLN;
                }
                IF x[i] >= x[i-1] THEN
                  {***Forward saccade***}
                  BEGIN
                    newy[i]:=newy[i-1];
                    {IF ((endf[i]-startf[i]) <= shortfix)  THEN newy[i]:=99;}
                      {Fix for very short fixations.}
                  END
                ELSE
                  {***Regression or return sweep***}
                  BEGIN
                    IF xflag  THEN {Check if x has been > xcheck }
                      BEGIN
                        IF (x[i] <= rcheck1) THEN    {if regression past }
                          BEGIN                      { rcheck1, then assume }
                            newy[i]:=(newy[i-1]+1);  { a full return sweep }
                            xflag:=false;
                          END
                        ELSE                {regression not past rcheck1}
                          BEGIN
                            IF (x[i] <= rcheck2) AND (x[i+1] <= x[i]) THEN
                              {If regression is past rcheck2 and next fixation }
                              {is a further regression, then assume return }
                              {sweep simply fell short }
                              BEGIN
                                newy[i]:=(newy[i-1]+1);
                                xflag:=false;
                              END
                            ELSE newy[i]:=newy[i-1];
                              {assume large regression on current line }
                          END;
                       END
                    ELSE               {if x position never > xcheck (66%)}
                      BEGIN
                        newy[i]:=newy[i-1]; {assume regression on current line}
                      END;
                  END;
              END;

            {***Change last fixation***}
            IF y[numfix-1]=0 THEN newy[numfix-1]:=y[numfix-1]
              ELSE newy[numfix-1]:=newy[numfix-1];

            {***Write fixations to screen.***}
            {
            count:=0;
            for i:=0 TO numfix-1 DO
              BEGIN
                writeln(i,'  x = ',x[i],'  y = ',y[i],'  newy = ',newy[i],'  startf = ',startf[i],
                '  endf = ',endf[i]);
                count:=count+1;
                IF count=20 then
                  begin
                    readln;
                    count:=0;
                  end;
              END;
            readln;
            }

            {***Write non-question trial information to *.DAX file.***}
            WRITE(ooo,' ',trial,' ',cond,' ',item,' ',trialtime,' ',unknown,' ',
                  duptime,' ',answer,'        ',numfix,'  ');
            IF newy[i-1] = numlines[cond,item] THEN
              BEGIN
                {Use newy if correct number of lines after adjustment.}
                FOR i:=0 TO numfix-1 DO WRITE(ooo,x[i],' ',newy[i],' ',startf[i],' ',endf[i],' ');
                tfixed:=tfixed+1;
              END
            ELSE
              {Use original y values if wrong number of lines after adjustment.}
              FOR i:=0 TO numfix-1 DO WRITE(ooo,x[i],' ',y[i],' ',startf[i],' ',endf[i],' ');
            WRITELN(OOO);
          END

        {***Question trial***}
        ELSE IF cond>=100 THEN         {read and write question information }
          BEGIN                        {do nothing if EOF }
            READLN(iii,trialtime,answer,dum1,dum2);
            WRITELN('Question trial');
            {
            writeln(trial,'  ',cond,'  ',item,'  ',trialtime,'  ',answer,'  ',dum1,'  ',dum2);
            }
            WRITE(ooo,' ',trial,' ',cond,' ',item,' ',trialtime,' ',answer,' ',dum1,' ',dum2);
            WRITELN(ooo);
          END;

      END;
      CLOSE(iii);
      CLOSE(ooo);
      WRITELN('Number of trials successfully rinsed = ',tfixed,'.');
      WRITELN('Closing files for subject ',subj[s]);
      DELAY(2500);
    END;
 END;
UNTIL (path = 'q') OR (path = 'Q');
END.

