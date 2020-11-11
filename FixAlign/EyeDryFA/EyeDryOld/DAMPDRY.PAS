PROGRAM DAMPDRY;        {Program to read .DA1 files and calculate summary }
USES CRT,DOS;           {data such as average fixation duration, average  }
                        {saccade length, and total reading time. }
                        {Programed by Gary Raney. Updated 6-20-2000.}

CONST
    maxsub = 20;    {maximum number of subjects }
    maxitem = 250;  {maximum number of items }
    maxcond = 20;   {maximum number of conditions }
    maxfix = 350;

TYPE
    STR25     =       STRING[25];

VAR
   oname,iname        :STR25;
   posfile,ext,path   :STR25;
   subnum,cntfile     :STR25;
   subfile            :STR25;
   quit,poschoice     :CHAR;
   incorrect,examine  :CHAR;
   printtrial,input   :CHAR;
   change,complete    :CHAR;
   iii,ooo,spf,cnt,sss:TEXT;
   c,i,j,s,t          :INTEGER;
   dum1,dum2,rsb      :INTEGER;
   numcond,cond       :SHORTINT;
   answer,nsubs       :SHORTINT;
   shortcount         :SHORTINT;
   miny,maxy,ycheck   :SHORTINT;
   item,trial         :INTEGER;
   fixdur,sacdur      :INTEGER;
   count,trialcount   :INTEGER;
   words,lines        :INTEGER;
   numfix,tempcount   :INTEGER;
   shortfix,longfix   :INTEGER;
   shortsac,longsac   :INTEGER;
   numsen,wordcount   :INTEGER;
   charline,region    :INTEGER;
   numreg             :INTEGER;
   duptime            :LONGINT;
   trialtime          :LONGINT;
   timeavg,avgwords   :REAL;
   wpm,temp           :REAL;
   realnumwords       :REAL;
   numlines           :ARRAY [1..MAXCOND] OF ARRAY [1..MAXITEM] OF SHORTINT;
   numwords           :ARRAY [1..MAXCOND] OF ARRAY [1..MAXITEM] OF INTEGER;
   rstart,rend        :ARRAY [1..MAXCOND] OF ARRAY [1..MAXITEM] OF INTEGER;
   reg                :ARRAY [1..maxcond] OF INTEGER;
   {Individual trial sums and counters}
   ffixsumx,fsacsumx     :LONGINT;
   rfixsumx,rsacsumx     :LONGINT;
   ffixcountx,rfixcountx :INTEGER;
   fsaccountx,rsaccountx :INTEGER;
   ffixavgx,fsacavgx     :SINGLE;
   rfixavgx,rsacavgx     :SINGLE;
   sweeptimex,sweepnumx  :SINGLE;
   sactimex,sacnumx      :SINGLE;
   omitt,readtime        :LONGINT;
   {Across trials and subjects sums and counters}
   ffixavg,fsacavg     :SINGLE;
   rfixavg,rsacavg     :SINGLE;
   ffcount,fscount     :REAL;
   rfcount,rscount     :REAL;
   sweept,sweepn       :REAL;
   sact,sacn           :REAL;
   ffixsum,fsacsum     :ARRAY [1..MAXSUB] OF ARRAY [1..MAXCOND] OF LONGINT;
   rfixsum,rsacsum     :ARRAY [1..MAXSUB] OF ARRAY [1..MAXCOND] OF LONGINT;
   ffixcount,rfixcount :ARRAY [1..MAXSUB] OF ARRAY [1..MAXCOND] OF INTEGER;
   fsaccount,rsaccount :ARRAY [1..MAXSUB] OF ARRAY [1..MAXCOND] OF INTEGER;
   timesum             :ARRAY [1..MAXSUB] OF ARRAY [1..MAXCOND] OF LONGINT;
   trialsum            :ARRAY [1..MAXSUB] OF ARRAY [1..MAXCOND] OF LONGINT;
   wordsum             :ARRAY [1..MAXSUB] OF ARRAY [1..MAXCOND] OF LONGINT;
   sweeptime,sweepnum  :ARRAY [1..MAXSUB] OF ARRAY [1..MAXCOND] OF LONGINT;
   sactime,sacnum      :ARRAY [1..MAXSUB] OF ARRAY [1..MAXCOND] OF LONGINT;
   x,y                 :ARRAY [0..MAXFIX] OF SHORTINT;
   startf,endf         :ARRAY [0..MAXFIX] of LONGINT;
   subj                :ARRAY [1..MAXSUB] OF STRING[15];

{ ************************************************** }
PROCEDURE Header;

BEGIN
  CLRSCR;
  WRITELN('Welcome to DAMPDRY. Developed by Gary Raney (version 6-21-2000).');
  WRITELN('The following data are output by trial and averaged by condition.');
  WRITELN('  . Subject.');
  WRITELN('  . Condition.');
  WRITELN('  . Item (trial data) or number of trials in average (condition data).');
  WRITELN('  . Reading time.');
  WRITELN('  . Reading rate (wpm).');
  WRITELN('  . Nnumber of words.');
  WRITELN('  . Average fixation duration (forward).');
  WRITELN('  . Number of fixations (forward).');
  WRITELN('  . Average saccade length (forward, not including return sweeps).');
  WRITELN('  . Number of saccades (forward, not including return sweeps).');
  WRITELN('  . Average fixation duration (regressive).');
  WRITELN('  . Number of fixations (regressive).');
  WRITELN('  . Average saccade length (regressive, not including returns to prior lines).');
  WRITELN('  . Number of saccades (regressive, not including returns to prior lines).');
  WRITELN('  . Average saccade duration (foward and regressive combined).');
  WRITELN('  . Average return sweep duration.');
  WRITELN('  . Number of return sweeps. For trial data only.');
  WRITELN('  . Number of short return sweeps. For trial data only.');
  WRITELN('  . Line number of last fixation. For trial data only.');
  WRITELN('Maximum number: subjects = ',maxsub,'  Items = ',maxitem,'  Conditions = ',maxcond,'.');
  WRITE('Press ENTER to begin.');
  READLN;
  CLRSCR;
END;

{ ************************************ }

PROCEDURE Getsubs;            {Get subject names for data files.}

BEGIN
  WRITELN;
  WRITE('How many SUBJECTS? -> ');
  READLN(nsubs);
  WRITE('Enter subjects from Keyboard or File (K/F)? -> ');
  READLN(input);
  input:=UPCASE(input);
  IF input='K' THEN
    BEGIN
      FOR s:= 1 TO nsubs DO
        BEGIN
          WRITE(' Enter FILE NAME STEM for subject ',s,' (omitt extension, e.g., .DA1) -> ');
          READLN(subj[s]);
        END;
      WRITE('Extension for data file names (omitt the dot ".")? -> ');
      READLN(ext);
    END
  ELSE
    BEGIN
      WRITELN('WARNING: File list should not specify path!');
      WRITE('Enter SUBJECT LIST file name (include path if needed). -> ');
      READLN(subfile);
      WRITELN('Subject file name = ',subfile);
      ASSIGN(iii,subfile);
      {$I-}
      RESET(iii);
      {$I+}
      FOR s:=1 TO nsubs DO READLN(iii,subj[s]);
    END;
  WRITELN;
  WRITE('Name of OUTPUT file (including path and extension)? -> ');
  READLN(oname);
  WRITELN('Data file name = ',oname);
  ASSIGN(ooo,oname);
  REWRITE(ooo);
  WRITELN;
  WRITE('Number of conditions? -> ');
  READLN(numcond);
  WRITELN('Default settings:');
  WRITELN('  Exclude fixations less than 100 msec.');
  WRITELN('  Exclude fixations greater than 1000 msec.');
  WRITELN('  Exclude saccades less than 10 msec.');
  WRITELN('  Exclude saccades greater than 80 msec.');
  WRITELN('  Return sweep boundary = 15.');
  WRITELN('  Maximum number of characters per line = 80.');
  WRITELN('  Minimum acceptable y-value = 0. (After EYEWASH.)');
  WRITELN('  Use only complete trials for average reading time, WPM, and number of words.');
  shortfix:=100; longfix:=1000; shortsac:=10; longsac:=1000; rsb:=15;
  charline:=80; miny:=0; complete:='Y';
  WRITE('Do you want to change any of the default settings? (Y/N) -> ');
  READLN(change);
  change:=UPCASE(change);
  IF (change = 'Y') THEN
    BEGIN
      WRITE('Exclude fixations less than X msec. (100) -> ');
      READLN(shortfix);
      WRITE('Exclude fixations greater than X msec. (1000) -> ');
      READLN(longfix);
      WRITE('Exclude saccades less than X msec. (10) -> ');
      READLN(shortsac);
      WRITE('Exclude saccades greater than X msec. (80) -> ');
      READLN(longsac);
      WRITE('Return sweep boundary (15). -> ');
      READLN(rsb);
      WRITE('Maximum number of characters per line? (e.g., 60/72/80) -> ');
      READLN(charline);
      WRITE('Minimum acceptable y-value? (Usually 0 after EYEWASH.) -> ');
      READLN(miny);
      WRITE('Use only complete trials for average reading time & WPM? (Y/N) -> ');
      READLN(complete);
      complete:=UPCASE(complete);
      WRITELN;
    END;
  WRITE('Maximum acceptable y-value? (Exclude if greater than maximum.) -> ');
  READLN(maxy);
  WRITE('Examine entire Trial or a specific Region? (T/R) -> ');
  READLN(examine);
  examine:=UPCASE(examine);
  IF examine = 'R' THEN
    BEGIN
      WRITE('Region number to examine? -> ');
      READLN(region);
      WRITE('Name of *.CNT file (include path if needed)? -> ');
      READLN(cntfile);
    END
  ELSE
    BEGIN
      FOR c:=1 TO maxcond DO
        FOR i:=1 to maxitem DO
          BEGIN
            rstart[c,i]:=0;
            rend[c,i]:=(maxy+1)*charline;
            {IF c=1 THEN writeln('rstart[',c,',',i,'] = ',rstart[c,i],'   rend[',c,',',i,'] = ',rend[c,i]);}
          END;
    END;
  {
  WRITE('Include incorrect response trials? (Y/N) -> ');
  READLN(incorrect);
  incorrect:=UPCASE(incorrect);
  }
  incorrect:='Y';
  WRITE('Do you want to print trial-by-trial data? (Y/N) -> ');
  READLN(printtrial);
  printtrial:=UPCASE(printtrial);
END;

{ ************************************************** }
PROCEDURE Makefilename;

BEGIN
  IF input='K' THEN iname:=path+subj[s]+'.'+ext ELSE iname:=path+subj[s];
  WRITELN;
  WRITELN('Input file name = ',iname);
  delay(500);
  ASSIGN(iii,iname);
  {$I-}
  RESET(iii);
  {$I+}
END;

{ ************************************************** }
PROCEDURE Zerobuff;

BEGIN
  FOR s:=1 TO maxsub DO
  FOR c:=1 TO maxcond DO
    BEGIN
      ffixsum[s,c]:=0;
      fsacsum[s,c]:=0;
      ffixcount[s,c]:=0;
      fsaccount[s,c]:=0;
      rfixsum[s,c]:=0;
      rsacsum[s,c]:=0;
      rfixcount[s,c]:=0;
      rsaccount[s,c]:=0;
      timesum[s,c]:=0;
      trialsum[s,c]:=0;
      wordsum[s,c]:=0;
      sweeptime[s,c]:=0;
      sweepnum[s,c]:=0;
      sactime[s,c]:=0;
      sacnum[s,c]:=0;
    END;
END;

{ ************************************************** }
PROCEDURE Zerobufftrial;

BEGIN
  ffixsumx:=0;
  fsacsumx:=0;
  ffixcountx:=0;
  fsaccountx:=0;
  ffixavgx:=0;
  fsacavgx:=0;
  rfixsumx:=0;
  rsacsumx:=0;
  rfixcountx:=0;
  rsaccountx:=0;
  rfixavgx:=0;
  rsacavgx:=0;
  count:=0;
  sweeptimex:=0;
  sweepnumx:=0;
  shortcount:=0;
  sactimex:=0;
  sacnumx:=0;
  temp:=0;
  omitt:=0;
  ycheck:=0;
  FOR i:=0 to MAXFIX DO
    BEGIN
      x[i]:=0;
      y[i]:=0;
      startf[i]:=0;
      endf[i]:=0;
    END;
END;

{ ************************************************** }
PROCEDURE Readsenpos;                 {Reads .POS file to determine how many }
                                      {lines of material are presented for   }
BEGIN                                 {each trial and number of words.       }
  {WRITELN;}
  WRITELN('WARNING: If you do not have a position file, then WPM, average reading time,');
  WRITELN('         average number of fixations (forward and regressive) and average');
  WRITELN('         number of saccades (forward and regressive) will not be calculated.');
  WRITELN('         Using a position file is recommended.');
  WRITE('Do you have a position file? (Y/N) -> ');
  READLN(poschoice);
  poschoice:=UPCASE(poschoice);
  IF poschoice = 'N' THEN
    BEGIN
      WRITE('How many lines in each trial? (Actual line number on screen.) -> ');
      READLN(lines);
      lines:=lines-1;
      FOR i:=1 TO MAXCOND DO
        FOR j:=1 TO maxitem DO
          BEGIN
            numlines[i,j]:=lines;
            numwords[i,j]:=0;
          END;
    END
  ELSE
    BEGIN
      WRITE('Name of POSITION file (including path and extention)? -> ');
      READLN(posfile);
      WRITELN;
      WRITELN('Reading position file: ',posfile);
      ASSIGN(spf,posfile);
      {$I-}
      RESET(spf);
      {$I+}
      REPEAT
        READ(spf,cond,item,words);       {Read first three numbers.}
        temp:=0;
        numsen:=1;
        wordcount:=1;
        {FOR i:=1 TO words-1 DO}
        FOR i:=1 TO words DO
          READ(spf,dum1,lines);          {Read positions for one trial.}
        READLN(spf);
        numlines[cond,item]:=lines;
        numwords[cond,item]:=words;
      UNTIL EOF(spf);
      CLOSE(spf);
    END;
END;

{ ************************************************** }

PROCEDURE Readcnt;           {Reads .CNT  file to determine position }
                             {of each region.}
BEGIN                        
  {WRITELN;}
  WRITELN('Reading *.CNT file: ',cntfile);
  ASSIGN(cnt,cntfile);
  {$I-}
  RESET(cnt);
  {$I+}
  REPEAT
    READ(cnt,item,cond,numreg);      {Read first three numbers.}
    FOR i:=1 TO numreg DO READ(cnt,reg[i]);
    rstart[cond,item]:=reg[region];
    IF region < numreg THEN
      rend[cond,item]:=(reg[region+1]-1)
    ELSE
      rend[cond,item]:=maxy*charline;
    READLN(cnt);
    {
    write('item ',item,'   cond ',cond,'   numreg ', numreg);
    for j:=1 to numreg DO write('  ',reg[j]);
    writeln;
    readln;
    }
    {
    writeln('r: rstart[',cond,',',item,'] = ',rstart[cond,item],'   rend[',cond,',',item,'] = ',rend[cond,item]);
    IF cond = 8 THEN readln;
    }
  UNTIL EOF(cnt);
  CLOSE(cnt);
END;

{ ************************************************** }

BEGIN                                    {MAIN BODY OF PROGRAM}
Header;                                         {Procedure}
REPEAT
 CLRSCR;
 WRITE('Enter data file path (e.g., d:\subdata\) or Q to Quit -> ');
 READLN(path);
 IF (path<>'Q') AND (path<>'q') THEN
 BEGIN
   Getsubs;                                      {Procedure}
   Readsenpos;                                   {Procedure}
   IF examine = 'R' THEN Readcnt;                {Procedure}
   Zerobuff;                                     {Procedure}
   FOR s:=1 TO nsubs DO                          {Loop for number of subjects.}
    BEGIN
      Makefilename;                              {Procedure}
      REPEAT                                     {Repeat for number of trials.}
        Zerobufftrial;
        READ(iii,trial,cond,item);               {Read first three numbers.}
        IF cond<100 THEN                         {Determine if question.}
          BEGIN
            {READ(iii,trialtime,answer,duptime,duptime,duptime,dum1,numfix);}
            READ(iii,trialtime,answer,duptime,dum1,numfix);
            FOR i:=1 TO numfix DO READ(iii,x[i],y[i],startf[i],endf[i]);
            READLN(iii);                         {Read fixations.}
            writeln('trial = ',trial,'  cond = ',cond,'  item = ',item);
            {readln;}
            {
            for i:=1 to numfix DO
              BEGIN
                writeln(i, ' x = ',x[i],' y = ',y[i], '  start = ',startf[i],' end = ',endf[i]);
              END;
            }

            {***Correct for rollover of reading times during data collection.***}
            FOR i:=1 TO numfix DO
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

            {***Set answer depending on rejection criteria.***}
            IF incorrect='Y' THEN answer:=1;

            {***Sum fixation durations, lengths, etc.***}
            c:=cond;
            FOR i:=2 to numfix DO
            IF ((endf[i]-startf[i]) >= shortfix) AND ((endf[i]-startf[i]) <= longfix)
              AND (y[i] >= miny) AND (y[i] <= maxy) AND
              ((y[i]*charline)+x[i] >= rstart[cond,item]) AND
              ((y[i]*charline)+x[i] <= rend[cond,item]) AND (answer=1) THEN
              {Include if shortfix <= fixation <= longfix AND within range AND correct.}
              BEGIN
                count:=i;
                ycheck:=1;
                fixdur:=endf[i]-startf[i];
                sacdur:=startf[i]-endf[i-1];

                {***Create across trial summary measures.***}

                {Check if complete trial.}
                {IF (y[numfix-1] = (numlines[c,item])) AND (answer=1) THEN}
                IF (y[numfix] >= miny) THEN
                  BEGIN
                    IF (y[i]=y[i-1]) AND (sacdur>shortsac) AND (sacdur<longsac) THEN
                      BEGIN                        {sum saccade duration}
                        sactime[s,c]:=sactime[s,c]+sacdur;
                        sacnum[s,c]:=sacnum[s,c]+1;
                      END;
                    IF (y[i] > y[i-1]) THEN        {return sweep}
                      BEGIN
                        IF (x[i] <= rsb) THEN      {full return sweep }
                          BEGIN
                            ffixsum[s,c]:=ffixsum[s,c]+fixdur;
                            ffixcount[s,c]:=ffixcount[s,c]+1;
                            sweeptime[s,c]:=sweeptime[s,c]+(startf[i]-endf[i-1]);
                            sweepnum[s,c]:=sweepnum[s,c]+1;
                          END
                        ELSE                       {partial return sweep}
                          BEGIN
                            IF (x[i] > x[i+1]) THEN   {next fix to left}
                              BEGIN
                              sweeptime[s,c]:=sweeptime[s,c]+(startf[i+1]-endf[i-1]);
                                {Include intermediate fix in sweeptime.}
                              {sweeptime[s,c]:=sweeptime[s,c]+(endf[i]-endf[i-1]);}
                                {Exclude intermediate fix in sweeptime.}
                              sweepnum[s,c]:=sweepnum[s,c]+1;
                              END
                            ELSE                      {next fix to right}
                              BEGIN
                                ffixsum[s,c]:=ffixsum[s,c]+fixdur;
                                ffixcount[s,c]:=ffixcount[s,c]+1;
                                sweeptime[s,c]:=sweeptime[s,c]+(startf[i]-endf[i-1]);
                                sweepnum[s,c]:=sweepnum[s,c]+1;
                              END;
                          END;
                      END

                    ELSE IF (y[i] < y[i-1]) THEN   {regression to prior line}
                      BEGIN
                        rfixsum[s,c]:=rfixsum[s,c]+fixdur;
                        rfixcount[s,c]:=rfixcount[s,c]+1;
                      END

                    ELSE IF (y[i] = y[i-1]) THEN   {saccade on same line}
                      BEGIN
                        IF (x[i] >= x[i-1]) THEN
                          BEGIN                    {forward saccade}
                            ffixsum[s,c]:=ffixsum[s,c]+fixdur;
                            fsacsum[s,c]:=fsacsum[s,c]+(x[i]-x[i-1]);
                            ffixcount[s,c]:=ffixcount[s,c]+1;
                            fsaccount[s,c]:=fsaccount[s,c]+1;
                          END
                        ELSE                       {regressive saccade}
                          BEGIN
                            rfixsum[s,c]:=rfixsum[s,c]+fixdur;
                            rsacsum[s,c]:=rsacsum[s,c]+(x[i-1]-x[i]);
                            rfixcount[s,c]:=rfixcount[s,c]+1;
                            rsaccount[s,c]:=rsaccount[s,c]+1;
                          END;
                      END;
                  END;

                {***Create trial-by-trial summary measures.***}

                IF (y[i]=y[i-1]) AND (sacdur>shortsac) AND (sacdur<longsac) THEN
                    BEGIN                        {sum saccade duration}
                      sactimex:=sactimex+sacdur;
                      sacnumx:=sacnumx+1;
                    END;
                IF (y[i] > y[i-1]) THEN          {return sweep}
                  BEGIN
                    IF (x[i] <= rsb) THEN        {full return sweep }
                      BEGIN
                        ffixsumx:=ffixsumx+fixdur;
                        ffixcountx:=ffixcountx+1;
                        sweeptimex:=sweeptimex+(startf[i]-endf[i-1]);
                        sweepnumx:=sweepnumx+1;
                      END
                    ELSE                         {partial return sweep}
                      BEGIN
                        IF (x[i] > x[i+1]) THEN  {next fix to left}
                          BEGIN
                            sweeptimex:=sweeptimex+(startf[i+1]-endf[i-1]);
                              {Include intermediate fix in sweeptime.}
                            {sweeptimex:=sweeptimex+(endf[i]-endf[i-1]);}
                              {Exclude intermediate fix in sweeptime.}
                            sweepnumx:=sweepnumx+1;
                            {y[i]:=y[i]-1;}
                            shortcount:=shortcount+1;
                          END
                        ELSE                     {next fix to right}
                          BEGIN
                            ffixsumx:=ffixsumx+fixdur;
                            ffixcountx:=ffixcountx+1;
                            sweeptimex:=sweeptimex+(startf[i]-endf[i-1]);
                            sweepnumx:=sweepnumx+1;
                          END;
                      END;
                  END

                ELSE IF (y[i] < y[i-1]) THEN     {regression to prior line}
                  BEGIN
                    rfixsumx:=rfixsumx+fixdur;
                    rfixcountx:=rfixcountx+1;
                  END

                ELSE IF (y[i] = y[i-1]) THEN     {saccade on same line}
                  BEGIN
                    IF (x[i] >= x[i-1]) THEN     {forward saccade}
                      BEGIN
                        ffixsumx:=ffixsumx+fixdur;
                        fsacsumx:=fsacsumx+(x[i]-x[i-1]);
                        ffixcountx:=ffixcountx+1;
                        fsaccountx:=fsaccountx+1;
                      END
                    ELSE
                      BEGIN                      {regressive saccade}
                        rfixsumx:=rfixsumx+fixdur;
                        rsacsumx:=rsacsumx+(x[i-1]-x[i]);
                        rfixcountx:=rfixcountx+1;
                        rsaccountx:=rsaccountx+1;
                        END;
                  END;
                  {
                  IF (item > 0) THEN BEGIN
                    WRITELN('Cond = ',cond,'  Item = ',item);
                    WRITELN('duration = ',fixdur,',  x = ',x[i],', y = ',y[i],
                      '      prior fix: x = ',x[i-1],', y = ',y[i-1]);
                    WRITELN('ffixsum[',c,'] = ',ffixsum[s,c],'  ffixcount[',c,'] = ',ffixcount[s,c]);
                    WRITELN('fsacsum[',c,'] = ',fsacsum[s,c],'  fsaccount[',c,'] = ',fsaccount[s,c]);
                    WRITELN('rfixsum[',c,'] = ',rfixsum[s,c],'  rfixcount[',c,'] = ',rfixcount[s,c]);
                    WRITELN('rsacsum[',c,'] = ',rsacsum[s,c],'  rsaccount[',c,'] = ',rsaccount[s,c]);
                    WRITELN('sactime[',c,'] = ',sactime[s,c],'  sacnum[',c,'] = ',sacnum[s,c]);
                    WRITELN('sweepnumx = ',sweepnumx:3:0,'  sweeptimex = ',sweeptimex:5:0,'  shortcount = ',shortcount:2);
                    readln;
                  END;
                  }
              END {***End of valid fixations.***}

            ELSE      {Shortfix, longfix, or out of range}
              BEGIN
                IF (y[i] >= miny) AND (y[i] <= maxy) THEN
                  BEGIN                {Within range.}
                    IF (endf[i]-startf[i] < shortfix) THEN
                      BEGIN            {Short fix}
                        x[i]:=x[i-1];    {set x and y for short fix's equal to   }
                        y[i]:=y[i-1];    {prior fix to avoid error when comparing}
                      END                {with next fix, otherwise the short fix }
                                         {will be used to determine if the next  }
                                         {fixation is a forward or backward move.}
                    ELSE {y[i]:=y[i-1];} {Long fix}
                    count:=i;
                  END
                ELSE                   {Out of range}
                  BEGIN
                    IF (y[i] < miny) AND (ycheck=0) THEN omitt:=startf[i+1];
                    y[i]:=y[i-1];
                  END;
                {WRITELN('Caution: fix ',i,' was omitted.');}
              END;

            {***Set first fix as a forward fix for individual trial data.***}

            IF examine='T' THEN
              BEGIN
              IF ((endf[1]-startf[1]) >= shortfix) AND ((endf[1]-startf[1]) <= longfix)
              AND (y[1] >= miny) AND (y[1] <= maxy) THEN
                {Include if shortfix <= fixation <= longfix AND within range.}
                BEGIN
                  ffixsumx:=ffixsumx+(endf[1]-startf[1]);
                  ffixcountx:=ffixcountx+1;
                  omitt:=0
                END;
              END;

            {***Calculate individual trial averages and write to file.***}

            {IF (examine='R') AND (numfix>0) THEN y[numfix-1]:=numlines[c,item];}
            IF (examine='R') AND (numfix>0) THEN y[numfix]:=numlines[c,item];
            {IF (maxy<numlines[c,item]) AND (ffixcountx>0) THEN y[numfix-1]:=numlines[c,item];}
            IF (maxy<numlines[c,item]) AND (ffixcountx>0) THEN y[numfix]:=numlines[c,item];
            IF (ffixcountx>0) THEN ffixavgx:=ffixsumx/ffixcountx
              ELSE ffixavgx:=0;
            IF (fsaccountx>0) THEN fsacavgx:=fsacsumx/fsaccountx
              ELSE fsacavgx:=0;
            IF (rfixcountx>0) THEN rfixavgx:=rfixsumx/rfixcountx
              ELSE rfixavgx:=0;
            IF (rsaccountx>0) THEN rsacavgx:=rsacsumx/rsaccountx
              ELSE rsacavgx:=0;
            IF (sacnumx>0) THEN sactimex:=sactimex/sacnumx
              ELSE sactimex:=0;
            IF (sweepnumx>0) THEN sweeptimex:=sweeptimex/sweepnumx
              ELSE sweeptimex:=0;
            {IF (numfix>1) AND (y[numfix] = (numlines[c,item])) THEN}
            {IF (numfix>1) AND (ffixcountx>1) AND (y[numfix] = (numlines[c,item])) THEN}
            IF (numfix>1) AND (ffixcountx>1) THEN
              BEGIN
                temp:=(endf[count]-omitt)/1000;
                {
                If (y[numfix] > 0) THEN
                  realnumwords:=(numwords[c,item] * (y[numfix] / numlines[c,item]))
                ELSE
                  realnumwords:=numwords[c,item];
                }
                realnumwords:=numwords[c,item];
                wpm:=((realnumwords/temp)*60);
                {
                writeln('cond = ',cond,' item = ',item);
                writeln('numwords = ',numwords[c,item],' wpm = ',wpm:3:2,' numlines = ',numlines[c,item],
                  ' y[numfix] =',y[numfix]);
                writeln('realnumwords = ',realnumwords:3:2,' wpm = ',wpm:3:2);
                readln;
                }
              END
            ELSE
              BEGIN
                wpm:=0;
                endf[count]:=omitt;
              END;
            {
            writeln('numwords = ',numwords[c,item],'    numfix = ',numfix);
            writeln('temp = ',temp,'    wpm = ',wpm:5:0);
            writeln('endf = ',endf[count],'    omitt = ',omitt);
            readln;
            }
            IF printtrial = 'Y' THEN
              BEGIN
                {IF (y[numfix] = (numlines[c,item])) THEN}
                IF (y[numfix] >= miny) THEN
                  BEGIN                      {Complete or valid trial}
                    {writeln('complete trial'); writeln; delay(1000);}
                    WRITELN(ooo,subj[s]:6,' ',c:2,' ',item:3,' ',
                      (endf[count]-omitt):5,' ',wpm:3:0,' ',numwords[c,item]:3,' ',
                      ffixavgx:4:0,' ',ffixcountx:3,' ',fsacavgx:4:1,' ',fsaccountx:3,' ',
                      rfixavgx:4:0,' ',rfixcountx:3,' ',rsacavgx:4:1,' ',rsaccountx:3,' ',
                      sactimex:3:0,' ',sweeptimex:3:0,' ',sweepnumx:2:0,' ',shortcount:2,' ',y[numfix]:2);
                    {Write to screen.}
                    {
                    WRITELN(subj[s]:6,' ',c:2,' ',item:3,' ',
                      (endf[count]-omitt):5,' ',wpm:3:0,' ',numwords[c,item]:3,' ',
                      ffixavgx:4:0,' ',ffixcountx:3,' ',fsacavgx:4:1,' ',fsaccountx:3,' ',
                      rfixavgx:4:0,' ',rfixcountx:3,' ',rsacavgx:4:1,' ',rsaccountx:3,' ',
                      sactimex:3:0,' ',sweeptimex:3:0,' ',sweepnumx:2:0,' ',shortcount:2,' ',y[numfix]:2);
                    readln;
                    }
                  END
                ELSE
                  BEGIN                          {Cancelled or bad trial}
                    temp:=numwords[c,item];
                    IF numfix=0 THEN
                      BEGIN
                        Zerobufftrial;           {Procedure}
                        temp:=0;
                      END;
                    WRITELN(ooo,subj[s]:6,' ',c:2,' ',item:3,' ',
                      (endf[count]-omitt):5,' ',wpm:3:0,' ',numwords[c,item]:3,' ',
                      ffixavgx:4:0,' ',ffixcountx:3,' ',fsacavgx:3:1,' ',fsaccountx:3,' ',
                      rfixavgx:4:0,' ',rfixcountx:3,' ',rsacavgx:3:1,' ',rsaccountx:3,' ',
                      sactimex:3:0,' ',sweeptimex:3:0,' ',sweepnumx:2:0,' ',shortcount:2,' ','00');
                  END;
              END;

            {***Sum and store total reading times across trials.***}
            {IF examine='R' THEN IF numfix>0 THEN y[numfix-1]:=numlines[c,item];}
            IF (complete='Y') THEN
              BEGIN                {Include only complete trials.}
                IF (y[numfix] = (numlines[c,item])) THEN
                BEGIN              
                  {Set first fixation as a forward fixation.}
                  IF examine='T' THEN
                    BEGIN
                      IF ((endf[1]-startf[1]) >= shortfix) AND ((endf[1]-startf[1]) <= longfix)
                        AND (y[1] >= miny) AND (y[1] <= maxy) AND (answer=1) THEN
                        {Include if shortfix <= fixation <= longfix AND within range.}
                        BEGIN
                          ffixsum[s,c]:=ffixsum[s,c]+(endf[1]-startf[1]);
                          ffixcount[s,c]:=ffixcount[s,c]+1;
                        END;
                    END;
                  {Sum reading times.}
                  timesum[s,c]:=timesum[s,c]+endf[count]-omitt;
                  wordsum[s,c]:=wordsum[s,c]+numwords[c,item];
                  trialsum[s,c]:=trialsum[s,c]+1;
                END;
              END
            ELSE
              BEGIN                {Include incomplete trials.}
                IF (y[numfix] >= miny) AND (numfix > 0) THEN
                BEGIN              
                  {Set first fixation as a forward fixation.}
                  IF examine='T' THEN
                    BEGIN
                      IF ((endf[1]-startf[1]) >= shortfix) AND ((endf[1]-startf[1]) <= longfix)
                        AND (y[1] >= miny) AND (y[1] <= maxy) AND (answer=1) THEN
                        {Include if shortfix <= fixation <= longfix AND within range.}
                        BEGIN
                          ffixsum[s,c]:=ffixsum[s,c]+(endf[1]-startf[1]);
                          ffixcount[s,c]:=ffixcount[s,c]+1;
                        END;
                    END;
                  {Sum reading times.}
                  timesum[s,c]:=timesum[s,c]+endf[count]-omitt;
                  wordsum[s,c]:=wordsum[s,c]+numwords[c,item];
                  trialsum[s,c]:=trialsum[s,c]+1;
                END;
              END;

          END {of non-question trial}

        ELSE                                     {Question trial}
          BEGIN
            READLN(iii,trialtime,answer,dum1,dum2);
            writeln('question trial # ',trial);
          END;

      {***End of individual trial calculations.***}

      UNTIL EOF(iii);   {keep reading file until EOF}
      CLOSE(iii);
    END;                {of loop for number of subjects}

    {***Calculate across trial averages and output to file.***}
    {Format: subject, cond, number of trials, avg reading time, }
    {        avg reading rate (wpm), avg number of words,       }
    {        ffixavg, ffixcount, fsacavg, fsaccount,            }
    {        rfixavg, rfixcount, rsacavg, rsaccount, sweept     }
    IF (complete<>'Y') THEN
      BEGIN
        WRITELN('WARNING! Including incomplete trials in summary data.');
        WRITELN('Including incomplete trials could impact the accuracy');
        WRITELN('of mean reading time, WPM, and number of words.');
        WRITELN;
        {DELAY(5000);}
        WRITELN('Press ENTER to continue');
        READLN;
      END;
    FOR s:=1 TO nsubs DO
      FOR c:=1 TO numcond DO
        BEGIN
          IF (trialsum[s,c] > 0) THEN timeavg:=timesum[s,c]/trialsum[s,c]
            ELSE timeavg:=0;
          IF (ffixcount[s,c] > 0) THEN ffixavg:=ffixsum[s,c]/ffixcount[s,c]
            ELSE ffixavg:=0;
          IF (fsaccount[s,c] > 0) THEN fsacavg:=fsacsum[s,c]/fsaccount[s,c]
            ELSE fsacavg:=0;
          IF (rfixcount[s,c] > 0) THEN rfixavg:=rfixsum[s,c]/rfixcount[s,c]
            ELSE rfixavg:=0;
          IF (rsaccount[s,c] > 0) THEN rsacavg:=rsacsum[s,c]/rsaccount[s,c]
            ELSE rsacavg:=0;
          IF (trialsum[s,c] > 0) THEN ffcount:=ffixcount[s,c]/trialsum[s,c]
            ELSE ffcount:=0;
          IF (trialsum[s,c] > 0) THEN fscount:=fsaccount[s,c]/trialsum[s,c]
            ELSE fscount:=0;
          IF (trialsum[s,c] > 0) THEN rfcount:=rfixcount[s,c]/trialsum[s,c]
            ELSE rfcount:=0;
          IF (trialsum[s,c] > 0) THEN rscount:=rsaccount[s,c]/trialsum[s,c]
            ELSE rscount:=0;
          IF (sweepnum[s,c] > 0) THEN sweept:=sweeptime[s,c]/sweepnum[s,c]
            ELSE sweept:=0;
          IF (sacnum[s,c] > 0) THEN sact:=sactime[s,c]/sacnum[s,c]
            ELSE sact:=0;
          IF (trialsum[s,c] > 0) THEN
            BEGIN
              temp:=timesum[s,c]/1000;
              wpm:=((wordsum[s,c]/temp)*60);
              avgwords:=wordsum[s,c]/trialsum[s,c];
            END
          ELSE
            BEGIN
              wpm:=0;
              avgwords:=0;
              {
              writeln('trialsum[',s,',',c,'] = ',trialsum[s,c],' set wpm = 0');
              readln;
              }
            END;
          {
          writeln('trialsum[',s,',',c,'] = ',trialsum[s,c]);
          writeln('ffixcount[',s,',',c,'] = ',ffixcount[s,c]);
          writeln('ffcount = ',ffcount);
          readln;
          }
          WRITELN(ooo,subj[s]:6,' ',c:2,' ',trialsum[s,c]:2,' ',timeavg:5:0,
            ' ',wpm:3:0,' ',avgwords:4:1,' ',
            ffixavg:4:0,' ',ffcount:4:1,' ',fsacavg:4:1,' ',fscount:4:1,' ',
            rfixavg:4:0,' ',rfcount:4:1,' ',rsacavg:4:1,' ',rscount:4:1,' ',
            sact:3:0,' ',sweept:3:0);
        END;
      WRITELN('Finished writing summary data.');
      DELAY(1500);
    CLOSE(ooo);

 END;
UNTIL (path = 'q') OR (path = 'Q');
END.