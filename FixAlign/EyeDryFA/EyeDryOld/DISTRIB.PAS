PROGRAM DISTRIB;      {Program to read .DA1 files and create a distribution }
USES CRT,DOS;         {of fixation and saccade durations. }
                      {Gary Raney, 7-15-92    }

CONST
    maxsub = 60;    {maximum number of subjects }
    maxitem = 300;  {maximum number of items }
    maxcond = 15;   {maximum number of conditions }
    maxfix = 300;
    maxgroup = 150;

TYPE
    STR25     =       STRING[25];

VAR
   oname,iname        :STR25;
   path,ext,subnum    :STR25;
   subfile,cntfile    :STR25;
   incorrect,quit     :CHAR;
   input,ptype        :CHAR;
   examine            :CHAR;
   iii,ooo,cnt        :TEXT;
   c,i,j,l,s,t        :INTEGER;
   dum1,dum2          :SHORTINT;
   item,trial         :SHORTINT;
   numcond,cond       :SHORTINT;
   answer,nsubs       :SHORTINT;
   miny,maxy          :SHORTINT;
   output,temp        :SHORTINT;
   testcount          :INTEGER;
   numgroup,dur       :INTEGER;
   group,gwidth       :INTEGER;
   numfix,tempcount   :INTEGER;
   short,long         :INTEGER;
   shortfix,longfix   :INTEGER;
   charline,region    :INTEGER;
   numreg,fixdur      :INTEGER;
   duptime            :LONGINT;
   trialtime          :LONGINT;
   totalfix           :REAL;
   num,reg            :ARRAY [1..maxcond] OF INTEGER;
   count              :ARRAY [1..maxcond] OF ARRAY [0..MAXGROUP] OF REAL;
   startf,endf        :ARRAY [1..MAXFIX] OF LONGINT;
   x,y                :ARRAY [1..MAXFIX] OF INTEGER;
   subj               :ARRAY [1..MAXSUB] OF STRING[28];
   rstart,rend        :ARRAY [1..MAXCOND] OF ARRAY [1..MAXITEM] OF INTEGER;

{ ************************************************** }
PROCEDURE Header;

BEGIN
  CLRSCR;
  WRITELN('Welcome to DISTRIB.');
  WRITELN('Developed by Gary Raney at the University of Massachusetts.');
  WRITELN;
  WRITELN('This program creates distributions of fixation durations, saccade durations,');
  WRITELN('or saccade lengths.  The distribution can be based on all fixations/saccades');
  WRITELN('in a trial or on an individual region.  The distribution data are output');
  WRITELN('as both frequency and percentage.');
  WRITELN('The user may specify the width of grouping bins, the range of valid line');
  WRITELN('numbers (y-values), and minimum/maximum durations or lengths.');
  WRITELN;
  WRITELN('Output format is:');
  WRITELN('Top line indicates condition numbers.');
  WRITELN('Column 1 = Bin labels.');
  WRITELN('Column 2 = Frequency of fixations/saccades > bin label for condition 1.');
  WRITELN('Column 3 = Percentage of fixations/saccades > bin label for condition 1.');
  WRITELN('...');
  WRITELN('Column n-1 = Frequencies for condition N.');
  WRITELN('Column n   = Percentages for condition N.');
  WRITELN('Bottom line indicates number of fixations/saccades for each condition.');
  WRITELN;
  WRITELN('Maximum number: subjects = ',maxsub,'  Items = ',maxitem,'  Conditions = ',maxcond,'.');
  WRITELN;
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
      WRITE('Enter filename for subject list (include path if needed). -> ');
      READLN(subfile);
      WRITELN('Subject file name = ',subfile);
      WRITELN;
      ASSIGN(iii,subfile);
      {$I-}
      RESET(iii);
      {$I+}
      FOR s:=1 TO nsubs DO READLN(iii,subj[s]);
    END;
  WRITE('Name of OUTPUT file (including path and extension)? -> ');
  READLN(oname);
  WRITELN('Data file name = ',oname);
  ASSIGN(ooo,oname);
  REWRITE(ooo);
  WRITELN;
  WRITELN('Create distribution based on:');
  WRITE('  Fixation durations (1), saccade durations (2), or saccade lengths (3)?  -> ');
  READLN(output);
  IF output = 1 THEN
    BEGIN
      WRITELN('NOTE: Regressions will appear as negative values in the distribution.');
      WRITELN('      If you do not allow negative values, regressions will be');
      WRITELN('      added to smallest bin.');
      WRITE('Put fixations less than X msec into one bin. -> ');
      READLN(short);
      WRITE('Put fixations greater than OR equal to X msec into one bin. -> ');
      READLN(long);
      WRITE('Desired width of grouping bins (in msec)? -> ');
      READLN(gwidth);
    END
  ELSE IF output = 2 THEN
    BEGIN
      WRITELN('NOTE: Regressions will appear as negative values in the distribution.');
      WRITELN('      If you do not allow negative values, regressions will be');
      WRITELN('      added to smallest bin.  Return sweeps and regressions to prior');
      WRITELN('      lines are not included in the distribution.');
      WRITE('Put saccades less than X msec into one bin. -> ');
      READLN(short);
      WRITE('Put saccades greater than OR equal to X msec into one bin. -> ');
      READLN(long);
      WRITE('Desired width of grouping bins (in msec)? -> ');
      READLN(gwidth);
      WRITE('Ignore saccades preceeding fixations less than X msec. -> ');
      READLN(shortfix);
      WRITE('Ignore saccades preceeding fixations greater than X msec. -> ');
      READLN(longfix);
    END
  ELSE IF output = 3 THEN
    BEGIN
      WRITELN('NOTE: Regressions will appear as negative values in the distribution.');
      WRITELN('      If you do not allow negative values, regressions will be');
      WRITELN('      added to smallest bin.  Return sweeps and regressions to prior');
      WRITELN('      lines are not included in the distribution.');
      WRITE('Put saccades less than X character spaces into one bin. -> ');
      READLN(short);
      WRITE('Put saccades greater than OR equal to X character spaces into one bin. -> ');
      READLN(long);
      WRITE('Desired width of grouping bins (in characters)? -> ');
      READLN(gwidth);
      WRITE('Ignore saccades preceeding fixations less than X msec. -> ');
      READLN(shortfix);
      WRITE('Ignore saccades preceeding fixations greater than X msec. -> ');
      READLN(longfix);
    END;
  WRITE('Number of conditions? -> ');
  READLN(numcond);
  WRITE('Minimum acceptable y-value? (Exclude if less than minimum.) -> ');
  READLN(miny);
  WRITE('Maximum acceptable y-value? (Exclude if greater than maximum.) -> ');
  READLN(maxy);
  WRITE('Base percentages on ALL fixations or only on INCLUDED fixations? (A/I) -> ');
  READLN(ptype);
  ptype:=UPCASE(ptype);
  WRITE('Number of characters per line? (60/72) -> ');
  READLN(charline);
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
  WRITE('Include incorrect response trials? -> ');
  READLN(incorrect);
  incorrect:=UPCASE(incorrect);
  }
  incorrect:='Y';
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
  FOR c:=1 TO maxcond DO
    BEGIN
      num[c]:=0;
      FOR i:=0 TO maxgroup DO count[c,i]:=0;
      FOR i:=1 to maxitem DO
        BEGIN
          rstart[c,i]:=0;
          rend[c,i]:=0;
        END;
    END;
END;

{ ************************************************** }
PROCEDURE Readcnt;           {Reads .CNT  file to determine position }
                             {of each region.}
BEGIN                        
  WRITELN;
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
   testcount:=0;
   Zerobuff;                                     {Procedure}
   Getsubs;                                      {Procedure}
   IF examine = 'R' THEN Readcnt;                {Procedure}
   FOR s:=1 TO nsubs DO                          {Loop for number of subjects.}
    BEGIN
      Makefilename;                              {Procedure}
      REPEAT                                     {Repeat for number of trials.}
        READ(iii,trial,cond,item);               {Read first three numbers.}
        IF cond<100 THEN                         {Determine if question.}
          BEGIN
            READ(iii,trialtime,answer,duptime,dum1,numfix);
            FOR i:=1 TO numfix DO READ(iii,x[i],y[i],startf[i],endf[i]);
            READLN(iii);                         {Read fixations.}
            writeln('trial = ',trial);
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

            {***Group fixations or saccades into bins.***}
            c:=cond;
            numgroup:=((long-short) DIV gwidth)+1;
            FOR i:=1 to numfix DO
            BEGIN
              {Calculate durations or length.}
              temp:=y[i];  {Temporary y-value. Set to 99 to omitt a fixation.}
              IF output = 1 THEN            {Fix dur}
                BEGIN
                  dur:=(endf[i]-startf[i]);
                  IF (i > 1) THEN IF (x[i] < x[i-1]) THEN dur:=dur*(-1);
                END;
              IF output = 2 THEN            {Sac dur}
                BEGIN
                  dur:=(startf[i]-endf[i-1]); 
                  fixdur:=(endf[i]-startf[i]);
                  IF (i > 1) THEN IF (x[i] < x[i-1]) THEN dur:=dur*(-1);
                  {Force out of range if fix is on new line or if i = 1.}
                  IF (i = 1) THEN temp:=99;
                  IF (i > 1) THEN IF (y[i] <> y[i-1]) THEN temp:=99;
                  IF (fixdur < shortfix) OR (fixdur > longfix) THEN temp:=99;
                END;
              IF output = 3 THEN            {Sac length}
                BEGIN
                  dur:=(x[i]-x[i-1]);         
                  fixdur:=(endf[i]-startf[i]);
                  {Force out of range if fix is on new line or if i = 1.}
                  IF (i = 1) THEN temp:=99;
                  IF (i > 1) THEN IF (y[i] <> y[i-1]) THEN temp:=99;
                  IF (fixdur < shortfix) OR (fixdur > longfix) THEN temp:=99;
                END;

              {***Begin sorting.***}
              IF (y[i] >= miny) AND (temp <= maxy) AND (answer = 1) AND
              ((y[i]*charline)+x[i] >= rstart[cond,item]) AND
              ((y[i]*charline)+x[i] <= rend[cond,item]) THEN
              {Include if within range.}
                BEGIN
                  num[c]:=num[c]+1;
                  IF (dur >= short) AND (dur < long) THEN
                    BEGIN
                      group:=((dur-short) DIV gwidth)+1;
                      count[c,group]:=count[c,group]+1;
                      {
                      IF (cond = 3) AND (item < 30) THEN
                        BEGIN
                          testcount:=testcount+1;
                          write('dur = ',dur,'    group = ',group,'    numfix = ',i);
                          writeln('    testcount = ',testcount);
                          readln;
                        END;
                      }
                    END
                  ELSE
                    IF dur < short THEN
                      BEGIN
                        count[c,0]:=count[c,0]+1;
                        {
                        IF (cond = 3) AND (item < 3) THEN
                          BEGIN
                            writeln('Short: dur = ',dur,'    group = ',0);
                            readln;
                          END;
                        }
                      END
                  ELSE
                    IF dur >= long THEN
                      BEGIN
                        count[c,numgroup]:=count[c,numgroup]+1;
                        {
                        IF (cond = 3) AND (item < 3) THEN
                          BEGIN
                            writeln('Long: dur = ',dur,'    group = ',numgroup);
                            readln;
                          END;
                        }
                      END;
                END
              ELSE      {Out of range.}
                {
                IF (cond = 3) AND (item < 3)  THEN
                  BEGIN
                    writeln('fix ',i,' not included.  Cond = ',cond,'   Trial = ',trial);
                    writeln('Dur = ',dur,'  x = ',x[i],'  y = ',y[i],':  x-1 = ',x[i-1],'  y-1 = ',y[i-1]);
                    readln;
                  end;
                }
            END;
          END  {do nothing if a question trial}

      {***End of individual trial calculations.***}

      UNTIL EOF(iii);   {keep reading file until EOF}
      CLOSE(iii);
    END;                {of loop for number of subjects}

    {***Write to screen.***}
    (*
    WRITELN('Summary by condition.');
    j:=0;
    FOR c:=1 TO numcond DO
      BEGIN
        writeln('num[',c,'] = ',num[c]);
        FOR i:=0 TO numgroup DO
          BEGIN
            j:=j+1;
            writeln('count[',c,',',i,'] = ',count[c,i]:6:0);
            IF j=21 then READ;
          END;
        j:=0;
        WRITE('Press ENTER to continue.');
        READLN;
      END;
    *)

    {***Write to output file.***}
    {Write condition labels.}
    WRITE(ooo,'00':5);
    For c:=1 to numcond DO WRITE(ooo,c:6,c:6);
    WRITELN(ooo);
    {Write bin labels, frequencies, and percentages for each condition.}
    FOR i:=0 TO numgroup DO
      BEGIN                                      {Bin label}
        IF i=0 THEN WRITE(ooo,'-0':5) ELSE WRITE(ooo,(short+((i-1)*gwidth)):5);
        IF ptype = 'A' THEN
          BEGIN                                  {Frequency & percent}
            FOR c:=1 to numcond DO WRITE(ooo,count[c,i]:6:0,((count[c,i]/num[c])*100):6:1);
          END
        ELSE            {ptype = 'I'}
          FOR c:=1 TO numcond DO
            BEGIN
              totalfix:=num[c]-count[c,0]-count[c,numgroup];
              WRITE(ooo,count[c,i]:6:0,((count[c,i]/totalfix)*100):6:1);
            END;
        WRITELN(ooo);
      END;
    WRITE(ooo,'00':5);
    IF ptype = 'A' THEN                          {Totals}
      FOR c:=1 TO numcond DO WRITE(ooo,num[c]:6,num[c]:6)
    ELSE
      FOR c:=1 TO numcond DO
        BEGIN
          totalfix:=num[c]-count[c,0]-count[c,numgroup];
          WRITE(ooo,num[c]:6,totalfix:6:0);
        END;
    WRITELN(ooo);

    (*
    {Format for horizontal output (each condition on a line)}
    {Write bin labels.}
    WRITE(ooo,'00':3,'0':6);
    FOR i:=1 to numgroup DO WRITE(ooo,(short+((i-1)*gwidth)):6);
    writeln(ooo,'00':7);

    FOR c:=1 TO numcond DO
      BEGIN
        WRITE(ooo,c:3);
        {Write bin frequency.}
        FOR i:=0 TO numgroup DO write(ooo,count[c,i]:6:0);
        writeln(ooo,num[c]:7);
        WRITE(ooo,c:3);
        {Write bin percent.}
        IF ptype = 'A' THEN
          BEGIN
            FOR i:=0 TO numgroup DO write(ooo,((count[c,i]/num[c])*100):6:1);
            WRITELN(ooo,num[c]:7);
          END
        ELSE
          BEGIN
            totalfix:=num[c]-count[c,0]-count[c,numgroup];
            FOR i:=0 TO numgroup DO write(ooo,((count[c,i]/totalfix)*100):6:1);
            WRITELN(ooo,totalfix:7:0);
          END;
      END;
    *)
    CLOSE(ooo);

 END;
UNTIL (path = 'q') OR (path = 'Q');
END.
