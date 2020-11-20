      PROGRAM anova
C
C  ANOVA WRITTEN MARCH, 1969 BY D. H. BUTLER, A. S. KAMLET, AND
C  R. A. MONTY, U. S. ARMY HUMAN ENGINEERING LABORATORIES, ABERDEEN
C  PROVING GROUND, MARYLAND.
C
C  MODIFIED MARCH, 1973 BY PAUL G. NEUMANN, INSTITUTE FOR THE STUDY
C  OF INTELLECTUAL BEHAVIOR, UNIVERSITY OF COLORADO, BOULDER, 80302.
C  CODE MODIFIED TO INCLUDE OPTIONAL READ AND PRINT OF LABELS FOR
C  WITHIN AND BETWEEN VARIABLES, PRINT SUMS OF SQUARES AND MEAN
C  SQUARES TO FOUR DECIMAL PLACES, SUBROUTINE PF1 ADDED TO COMPUTE
C  EXACT PROBABILITY OF F-RATIOS, SOURCE CODE MODIFIED TO CALL THIS
C  ROUTINE AND TO PRINT THE PROBABILITIES.
C
C  FURTHER MODIFICATIONS BY LEWIS O. HARVEY, JR.
C  FEBRUARY 1976.
C
C  and still more by Chuck Clifton, UMass, for Supersoft Fortran 86
C  (Fortran 66) June 1984
C
C
c  modified for microsoft fortran 77 v 4.0 8/10/87, CEC
c

      implicit integer (i-p)
      character*80 fmt
      character*16 dfile
      character*24 blab(9),wlab(9)
      integer N1(9),N2(9),N3(9),NB(9),NW(9),N(9)
      character*1 d(15),d1(24)
      character*8 b(4)
      real prob
      real A(14000)
      external output,srink,index,clear,page,pf1
      COMMON A/AA/L5,MO,MI,MK
C
C  MI=5  ANOVA DATA INPUT
C  MK=6  ANOVA CONTROL CARD INPUT
C  MO=7  ANOVA OUTPUT
C
C      MI=5
C      MK=6
C      MO=7
c      DATA D(1),D(2),D(3),D(4),D(5),D(6),D(7),D(8),D(9),D(10),D(11),D(12)
c     1,D(13),D(14),D(15) /1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,1H,1HW,1H=
c     1,1HB,1H.,1H0/
      DATA D(1),D(2),D(3),D(4),D(5),D(6) /1H1,1H2,1H3,1H4,1H5,1H6/
      DATA D(7),D(8),D(9),D(10),D(11),D(12) /1H7,1H8,1H9,1H ,1HW,1H=/
      DATA D(13),D(14),D(15) /1HB,1H.,1H0/

c      DATA D(1),D(2),D(3) /1H1,1H2,1H3 /

      DATA B(1),B(2),B(3),B(4) /4HSTOP,4HCONT,3H(7F,5H10.3) /
 1    FORMAT (10I5,2I1)
9     format(' STORAGE REQUIRED = ',i7,/i5)
22    format(' Too many within or between variables.')
25    FORMAT(' E',14A1,I6,3F15.4)
30    FORMAT(2X,13A6)
41    format(//' SOURCE',13x,'DF',12x,'SS',13x,'MS',11x,'F',13x,'P<'
     1//' SUBJ',11x,i6,f15.4)
45    format(' ',15a1,i6,3f15.4,f13.6)
54    format('1 FORMAT TOO LONG')
55    format(' ',60x,'TABLE OF MEANS')
57    format(//' TSQ/N=',f17.4,'       N=',i6,'SST= ',F15.4//)
C5000  format(i0)
C5001  format(A0)
      NSIZE=14000

C
C  *********************  BEGIN PART I  *******************************
C
C  BEGIN SECTION 10@ DETERMINE ANALYSIS PARAMETERS.
C
C  READ IN  THE NUMBER OF SUBJECTS PER LEVEL  OF  BETWEEN VARIABLE 1,

C  THE NUMBER LEVELS OF BETWEEN VARIABLE 1,THE NUMBER OF LEVELS OF
C  BETWEEN VARIABLE 2,...,
C
      write(0,7000)
7000  format(/' COLANOV micro version, August 1987 (CEC)'/,' Up to 8
     1 between and 8 within factors, equal n.'/,' Specify slowest-moving
     1 factors first; only data from one subject on a data line.'//)

43    write (0,*) ' Number of subjects per group? - '
      read (0,'(i4)') ns
      if(ns.eq.0) goto 9999
      i = 0
1006  i = i+1
      write (0,1003) i
1003  format (' number of levels of between-S factor ', I3,'? ')
      read (0,'(i4)') nb(i)
      if(nb(i).gt.1.and.i.le.9) goto 1005
      lb = i-1
      nblab = lb
      do 1004 ii=i,9
1004  nb(ii) = 1
      goto 1002
1005  write (0,*) 'factor name? '
      read (0,'(a)') blab(i)
      goto 1006
1002  continue


C
C  IF THE NUMBER OF BETWEEN VARIABLES IS GREATER THAN 8, STOP.
C
      IF (NB(9).le.1) goto 21
20    WRITE(0,22)
       goto 9999
C
C  READ IN NUMBER OF LEVELS OF WITHIN VARIABLE 1, 2 ...FORMAT OPTION
C
21    i=0
211   i = i+1
      write (0,1013) i
1013  format (' number of levels of within-S factor ', I3, '? ')
      read (0,'(i4)') nw(i)
      if(nw(i).gt.1.and.i.le.9) goto 1015
      lw = i-1
      nwlab = lw
      do 1014 ii=i,9
1014  nw(ii) = 1
      goto 1012
1015  write (0,*) 'factor name? '
      read (0,'(a)') wlab(i)
      goto 211
1012  continue



C
C  TEST TO SEE IF THE WITHIN VARIABLES ARE GREATER THAN 8,YES STOP.
C
      IF (NW(9) .GT. 1) GO TO 20
C
C  READ IN VARIABLE FORMAT
C
      write(0,*) 'data input format? '
      read(0,'(A)') fmt

      do 2 i = 1,9
2     n(i) = i
C
C  BEGIN SECTION 20@ DETERMINE STORAGE REQUIREMENTS.
C
      MSABD=NTC(N,NW,0)
      MC=NTC(N,NB,1)
      MCT=MC*MSABD
      L1=MCT
      L2=L1+MCT
      L3=L2+MCT
      MVW=2**LW
      MVB=2**LB
      MV=MVW*(MVB+1)
      MBW=(MV-MVW)/2
      L3V=L3+MBW
      L4=L3+MV
      L5=L4
      LAST=L5+MCT
C
C     debug
C
C      write(0,8000)msabd,mc,mct,last
C8000  format('msabd, mc, mct, last',4i5)
C      write(0,8001) mct,l1,l2,l3,mvw,mvbd,mv,mbw,l4,l5
C8001  format('mct,l1,l2,l3,mvw,mvbd,mv,mbw,l4,l5',10i4)
C
C  IF LB=0 MOVE W2 TO C
C
      IF (LB.NE.0) GO TO 24
      LAST=L5
      L5=L1
C
C  IF STORAGE REQUIREMENTS ARE GREATER THAN NSIZE, STOP.
C
24    IF (LAST-NSIZE)  8,8,10
10    WRITE(0,9)  LAST,NSIZE
      IF (LAST .GT. NSIZE) goto 9999
 8    CONTINUE
C
C  CLEAR ALL SUMS OF SQUARES, SUMS.
C
      DO 7 I=MCT,L4
 7    A(I)=0.0

C
C     open data file as unit 7
C

2001  write(0,*) 'data file name? '
      read(0,'(A)')dfile
      open(5,FILE=dfile,status='old')
c      if(ioread(7,2,0,dfile)) goto 2001
C
C  BEGIN SECTION 30@ FORM SUBJECT DATA ENTRIES.
C
C     write(0,*) 'output file name?'
C     read(0,'(A)') efile
      open(7,FILE='colanov.out',status='unknown')
      MP=1
      M1=NB(1)
      DO 11 JC=1,M1
      IF(LB.EQ.0.or.mp.gt.1) GOTO 56
      CALL PAGE(LB,LW,MP)
      WRITE(7,55)
56    L=MCT-MSABD

      DO 12 JC1=1,MC
      L=L+MSABD
C
C  READ IN DATA SUBJECT AT A TIME.
C
      do 12 js=1,ns
      read(5,fmt)(a(i),i=1,msabd)
C
C  FORM CONDITION MATRIX AND TOTAL MATRIXES
C
      DO 13 I=1,MSABD
      J=I+L
      A(J)=A(J)+A(I)
      J=I+L+MCT
13    A(J)=A(J)+A(I)
C
C  FORM SUMS OF SQUARES FOR ERROR TERMS
C
      CALL CLEAR(N)
      DO 14 I=1,MVW
      CALL SRINK(NW,N,1,0,0,NP,L5)
C
C     debug
C
c      write(7,8003) np
c8003  format(' back from srink after 13, np = ',i6)
      J=L4-MVW+I
      NC=MSABD/NP
      A(J)=A(J)+SS(NP,NC)
14    CALL INDEX(N,LW)
12    CONTINUE
C
C  BEGIN SECTION 40@ FORM VARIABLE 1 ENTRIES.
C  TEST FOR ZERO BETWEEN VARIABLES, YES GO TO SECTION 5.
C
      MVB1=MVB/2
      IF (MVB1)28,29,28
29    MVB1=1
      GO TO 27
28    CALL CLEAR(N1)
      DO 15 I=1,MVB1
C
C  TEST FOR PRINT OPTION
C
C
C  CONTRACT ON BETWEEN VARIABLES NOT IN N1
C
      CALL SRINK(NB,N1,MSABD,L1,1,NPB,0)
C
C     debug
C
c      write(7,8004) npb
c8004  format(' back from srink after 28, npb = ',i6)
      NP=NPB*MSABD
      CALL CLEAR(N)
C
C  FORM SUM OF SQUARES FOR VARIABLE 1 TERMS AFTER APPROPRIATE
C  WITHIN VARIABLE CONTRACTION
C
      DO 16 J=1,MVW
      DO 61 K=1,NPB
      L51=(K-1)*NPW+L5
      LI= (K-1)*MSABD
61    CALL SRINK(NW,N,1,LI,0,NPW,L51)
C
C     debug
C
c      write(7,8005) npw
c8005  format(' back from srink after 61, npw = ',i6)
      K=L3V+J+(I-1)*MVW
      NP=NPB*NPW
      NC=(MCT*NS )/NP
      A(K)=A(K)+SS(NP,NC)
      CALL OUTPUT(NW,NB,N,N1,LW,LB,D,NPW,NPB,MCT,JC,NS,D1)
16    CALL INDEX(N,LW)
      CALL INDEX(N1,LB-1)
15    CONTINUE
C
C  CLEAR VARIABLE 1 MATRIX
C
      DO 17 I=L1,L2
17    A(I)=0.0
11    CONTINUE
27    CONTINUE
C
C  BEGIN SECTION 50@ TOTAL SUM ENTRIES
C
      NCS=NS*NB(1)
C     IF(JFLAG.NE.2.and.mp.eq.1)CALL PAGE(LB,LW,MP)
C     IF(JFLAG.NE.2)WRITE(7,55)
      CALL CLEAR(N1)
      DO 18 I=1,MVB1
      CALL SRINK(NB,N1,MSABD,L2,1,NPB,0)
C
C     debug
C
c      write(7,8006) npb
c8006  format(' back from srink after 27, npb = ',i6)
      NP=NPB*MSABD
      CALL CLEAR(N)
      DO 19 J=1,MVW
      DO 60 K=1,NPB
      L51=(K-1)*NPW+L5
      LI= (K-1)*MSABD
60    CALL SRINK(NW,N,1,LI,0,NPW,L51)
C
C     debug
C
c      write(7,8007) npw
c8007  format(' back from srink at 60, npw = ',i6)
      K=L3 +J+(I-1)*MVW
      NP=NPB*NPW
      NC=(MCT*NCS)/NP
      A(K)=A(K)+SS(NP,NC)
      CALL OUTPUT(NW,NB,N,N1,LW,LB,D,NPW,NPB,MCT,0 ,NCS,D1)
19    CALL INDEX(N,LW)
      CALL INDEX(N1,LB-1)
18    CONTINUE
C
C  *******************  BEGIN PART II  **********************
C
C  BEGIN SECTION 1@ FINAL FORMATION OF SUMS OF SQUARES TOTAL COEF.
C
      LI=L3+1
      SST =A(L4)-A(LI)
      I=L4-MVW+1
      SUBJ=A(I )-A(LI)
      K=L3
      CALL CLEAR(N1)
      DO 31 I=1,MVB1
      CALL CLEAR(N)
      DO 32 J=1,MVW
      K=K+1
      M=K-L3
      K1=L3V+M
      K2=MOD(M,MVW)
      IF (K2 .EQ. 0) K2 = MVW
      K2=K2+L4-MVW
      CALL CLEAR(N3)
      DO 33 I1=1,I
      CALL TEST(N1,N3,IANS)
      CALL CLEAR(N2)
      DO 34 J1=1,J
      CALL TEST (N,N2,JANS)
      L=J1+(I1-1)*MVW+L3
      KANS=0
      IF (I1+J1.EQ.I+J) KANS=1
      IF ((IANS+JANS+KANS).EQ.0) A(K)=A(K)-A(L)
      IF (LB.NE.0.AND.(IANS+JANS).EQ.0) A(K1)=A(K1)-A(L)
      IF (IANS+JANS.EQ.0.AND.I1.EQ.I) A(K2)=A(K2)-A(L)
34    CALL INDEX(N2,LW)
33    CALL INDEX(N3,LB-1)
32    CALL INDEX(N ,LW)
31    CALL INDEX(N1,LB-1)
C
C  BETWEEN VARIABLE 1 ...
C
      IF (LB.EQ.0) GO TO 50
      K=L3V
      CALL CLEAR(N1)
      DO 35  I=1,MVB1
      CALL CLEAR(N)
      DO 36 J=1,MVW
      K=K+1
      M=K-L3V
      K1=MOD(M,MVW)
      IF (K1 .EQ. 0) K1=MVW
      K1=K1+L4-MVW
      CALL CLEAR(N3)
      DO 37 I1=1,I
      CALL TEST(N1,N3,IANS)
      CALL CLEAR(N2)
      DO 38 J1=1,J
      L=J1+(I1-1)*MVW+L3V
      KANS=0
      CALL TEST(N,N2,JANS)
      IF (I1+J1 .EQ. I+J) KANS=1
      IF (IANS+JANS+KANS .EQ.0)  A(K)=A(K)-A(L)
      IF (IANS+JANS.EQ.0.AND.I1.EQ.I) A(K1)=A(K1)-A(L)
38    CALL INDEX(N2,LW)
37    CALL INDEX(N3,LB-1)
36    CALL INDEX(N ,LW)
35    CALL INDEX(N1,LB-1)
C
C  SUBJECT COEFFICIENT.
C
50    K=L4-MVW
      CALL CLEAR(N)
      DO 39  J=1,MVW
      K=K+1
      L=L4-MVW
      CALL CLEAR(N2)
      DO 40  J1=1,J
      L=L+1
      CALL TEST(N,N2,JANS)
      KANS=0
      IF (J1 .EQ. J) KANS=1
      IF (JANS+KANS.EQ.0)  A(K)=A(K)-A(L)
40    CALL INDEX(N2,LW)
39    CALL INDEX(N,LW)
C
C  BEGIN SECTION 2@ PRINT OUT  ********************************
C
      if(mp.eq.1)CALL PAGE(LB,LW,MP)
      NDF=NB(1)*MC*NS-1
      WRITE(7,41) NDF,SUBJ
      EDF=float(NB(1)*MC*(NS-1))
      K3=L4-MVW
      CALL CLEAR(N)
      DO 51  I=1,MVW
      K=L3+I
C
C  FORM WITHIN IDENTIFICATION BLOCK
C
      IC=1
      DO 42  J=1,LW
      IJ=N(J)
      IF (IJ .EQ. 0)  GO TO 42
      IC=IC+1
      D1(IC)=D(IJ)
42    CONTINUE
      D1(1)=D(11)
      IF (IC .EQ. 1) D1(1)=D(13)
      IF (IC.NE. 1) IC=IC+1
      IDS=IC
      DFW=float(NFC(N,NW,0))
      DFE=DFW*EDF
      K3=K3+1
      SSE=A(K3)
      ER=SSE/DFE
      CALL CLEAR(N1)
      NFE=ifix(DFE)
      DO 52 J=1,MVB1
      IF (J.EQ. 1) D1(IDS)=D(10)
      S2=A(K)
      DFB=float(NFC(N1,NB,1))
      DF=DFB*DFW
      SM=S2/DF
      NDF=ifix(DF)
      F=SM/ER
C
C  CLEAR  BETWEEN IDENTIFICATION
C
      DO 46 L=IC,14
      IK=L+1
46    D1(IK)=D(10)
C
C  FORM BETWEEN  IDENTIFICATION BLOCK
C
      ICS=IC
      DO 44  L=1,LB
      IF (N1(L).EQ. 0) GO TO 44
      IJ=N1(L)+1
      ICS=ICS+1
      D1(ICS)=D(IJ)
44    CONTINUE
      IF(I+J.NE.2) CALL PF1(F,NDF,NFE,PROB)
      IF (I+J .NE. 2) WRITE(7,45)(D1(M),M=1,15),NDF,S2,SM,F,PROB
      D1(IDS)=D(13)
      IF (LB .EQ. 0 ) GO TO 52
      K1=K+MVB1*MVW
      NDF=NDF*(NB(1)-1)
      DF=NDF
      S2=A(K1)
      SM=S2/DF
      F=SM/ER
      K1=0
C
C  FORM BETWEEN IDENTIFICATION BLOCK
C
      DO 47 L=IC,ICS
      IK=ICS-K1
      IK1=IK+1
      K1=K1+1
47    D1(IK1)=D1(IK)
      ICS=IC+1
      D1(ICS)=D(1)
      CALL PF1(F,NDF,NFE,PROB)
      WRITE(7,45) (D1(M),M=1,15),NDF,S2,SM,F,PROB
      CALL INDEX(N1,LB-1)
52    K=K+MVW
      DO 49  L=1,14
      IK=15-L
      IK1=IK+1
49    D1(IK1)=D1(IK)
      IF (LB+I.GT. 1)
     1WRITE(7,25) (D1(M),M=2,15),NFE,SSE,ER
      CALL INDEX(N,LW)
51    continue
      NCS=MCT*NB(1)*NS
      DO 59 I=2,15
59    D1(I)=D(10)
      NDF=NCS-NCS/MSABD
      S2=SST-SUBJ
      D1(1)=D(11)
      WRITE(7,45) (D1(M),M=1,15),NDF,S2
      WRITE(7,57) A(LI),NCS,SST
      IF(NBLAB.GT.0) WRITE(7,101)(IQ,BLAB(IQ),IQ=1,NBLAB)
101   FORMAT (//' BETWEEN VARIABLE LABELS'//,8(5X,I1,' = ',a//))
      IF(NWLAB.GT.0) WRITE(7,102)(IQ,WLAB(IQ),IQ=1,NWLAB)
102   FORMAT (//' WITHIN VARIABLE LABELS'//,8(5X,I1,' = ',a//))
C      GO TO 10
9999  write(7,*) 'programmed termination '

      stop
      END

C
C
      SUBROUTINE OUTPUT(NW,NB,N,NI,LW,LB,D,NPW,NPB,MCT,NC1,NS,D1)
      implicit integer (i-p)
      real A(1)
      integer NW(9),NB(9),N(9),NI(9),JT(7),IT(8),MT(7),NT(8)
      character*1 D(15),D1(24)
      EQUIVALENCE (JT(1),J1),(JT(2),J2),(JT(3),J3),(JT(4),J4),(JT(5),J5
     1),(JT(6),J6),(JT(7),J7),(MT(1),M1),(MT(2),M2),(MT(3),M3),(MT(4),
     2 M4),(MT(5),M5),(MT(6),M6),(MT(7),M7),(IT(1),I1),(IT(2),I2),
     3(IT(3),I3),(IT(4),I4),(IT(5),I5),(IT(6),I6),(IT(7),I7),(IT(8),I8),
     4(NT(1),N1),(NT(2),N2),(NT(3),N3),(NT(4),N4),(NT(5),N5),(NT(6),N6),
     5(NT(7),N7),(NT(8),N8)
      COMMON A/AA/L5,MO,MI,MK

C
4     format(' '/' ',16a1,' W',8a1)
20    format(' ',16a1,1x,10f10.3/99(18x,10f10.3/))
21    FORMAT(18X,10(2X,A1,I1,A1,I2,3X))
22    FORMAT(17X,A1,I1,A1,I2)
23    FORMAT(35X,10(A1,I1,A1,I2,5X))
24    FORMAT(25X,A1,I1,A1,I2,10f10.3/99(30x,10f10.3/))
C
C     debug
C
C      write(7,8000)lw,lb,npw,npb,mct,nc1,ns
C8000  format(' in output: lw,lb,npw,npb,mct,nc1,ns ',7i5)
      NR=NPW*NPB
      R=float(MCT*NS/NR)
C
C  FORM AVG. OF CONTRACTED VARIABLES.
C
      DO 1 I=1,NR
      J=I+L5
1     A(J)=A(J)/R
C
C  FORM IDENIFICATION BLOCK - BETWEEN.
C
      DO 2 I=1,24
 2    D1(I)=D(10)
      DO 3 I=1,LB
      J1=2*I-1
      J2=J1+1
      D1(J1)=D(13)
 3    D1(J2)=D(I)
C
C  FORM IDENIFICATION BLOCK - WITHIN.
C
      K1=1
      DO 5 I=1,LW
      K=I+16
      IF (N(K1)-I) 7,6,7
 6    D1(K)=D(I)
      K1=K1+1
      GO TO 5
 7    D1(K)=D(14)
 5    CONTINUE
      WRITE(7,4) (D1(I),I=1,24)
      DO 8 I=1,8
      NT(I)=1
 8    MT(I)=1
      DO 9 I=1,7
      IF (NI(I) .EQ. 0) GO TO 10
      J=NI(I)+1
 9    MT(J-1)=NB(J)
10    IJ=LB-1
      DO 11 I=1,8
      IF (N(I) .EQ. 0) GO TO 12
      J=N(I)
11    NT(I)=NW(J)
12    MI=I-1
C
C     debug
C
C      write(7,8010)lw,lb,npw,npb,mct,nc1,ns,mi
C8010  format(' in output: lw,lb,npw,npb,mct,nc1,ns,d1,mi: '/,12x,8i6)
C      write(7,8020) n
C8020  format(' n vector: ',9i6)
      IF (MI.EQ. 0) GOTO 25
      NJ=NT(MI)
      NT(MI)=1
      NJS=1 +L5-NJ
      NJF=L5
C
C  FOR PRINTOUTS OF WITHIN VARIABLES WITH MORE THAN 10 LEVELS.
C
      II=MIN0(NJ,10)
25    IP=MI-2
      IP1=IP+1
C
C  FORM FIRST PART OF BETWEEN BLOCK
C
      K5=NC1/10
      J2=MOD(K5,10)
      D1(1)=D(10)
      IF (J2 .NE. 0) D1(1)=D(J2)
      J2=NC1-10*K5
      D1(2)=D(15)
      IF (J2 .NE. 0) D1(2)=D(J2)
      IF (NC1.EQ. 0) D1(2)=D(14)
      K=0
      IL=0
      DO 13 J1=1,M1
      DO 13 J2=1,M2
      DO 13 J3=1,M3
      DO 13 J4=1,M4
      DO 13 J5=1,M5
      DO 13 J6=1,M6
      DO 13 J7=1,M7
C
C     debug
C
C      write(7,8011) j1,j2,j3,j4,j5,j6,j7,k,il,ij
C8011  format(' in output, 13 loop: j1-7, k, il,ij ',/10i6)
      IF (IJ .LE. 0) GO TO 99
C
C  FORM REST OF BETWEEN BLOCK.
C
      DO 14 I=1,IJ
      K1=2*I+1
      K2=K1+1
      K5=JT(I )/10
      K3=MOD(K5,10)
      D1(K1)=D(10)
      IF (K3 .NE. 0) D1(K1)=D(K3)
      K4=JT(I )-10*K5
      D1(K2)=D(15)
      IF (K4 .NE. 0) D1(K2)=D(K4)
      IF (MT(I) .EQ. 1) D1(K2)=D(14)
14    CONTINUE
99    K=K+1
      IF (MI .NE. 0) GO TO 15
C
C  TYPE 1 PRINT OUT.
C
      M=L5+K
      WRITE(7,20) (D1(I),I=1,16),A(M)
      GO TO 13
15    IF (MI .NE. 1) GO TO 16
C
C  TYPE NO. 2 PRINT OUT.
C
      I2=(K-1)*NPW+L5 +1
      I3=K*NPW+L5
      IF (IL .EQ. 0)
     1WRITE(7,21) (D(11),N(1),D(12),I,I=1,II)
      IL=1
      WRITE(7,20) (D1(I),I=1,16),(a(l),l=i2,i3)
      GO TO 13
16    continue
C
C  TYPE NO. 3 PRINTOUT
C
      WRITE(7,20) (D1(I),I=1,16)
      L=1
      DO 17 I1=1,N1
      DO 17 I2=1,N2
      DO 17 I3=1,N3
      DO 17 I4=1,N4
      DO 17 I5=1,N5
      DO 17 I6=1,N6
      DO 17 I7=1,N7
      DO 17 I8=1,N8
      NJS=NJS+NJ
      NJF=NJF+NJ
C
C     debug
C
C      write(7,8012) i1,i2,i3,i4,i5,i6,i7,i8,njs,njf,ip
C8012  format(' IN OUTPUT, 17 LOOP: I1-i8,njs,jnf,ip: ',/11i5)
      IF (IP .EQ. 0) GO TO 18
      IF (IT(IP)-L) 18,19,18
19    IF (K .EQ. NT(IP)) K=0
      L=L+1
      DO 26 I=1,IP
26    WRITE(7,22) D(11),N(I),D(12),IT(I)
      IF (IT(IP) .EQ. NT(IP)) L=1
18    IF (IL .EQ. 0)
     1WRITE(7,23) (D(11),N(MI),D(12),I,I=1,II)
      IL=1
17    WRITE(7,24) D(11),N(IP1),D(12),IT(IP1),(a(i),i=njs,njf)
13    CONTINUE
      RETURN
      END
C
      SUBROUTINE SRINK(NB,N,MSABD,IC,IS,NP,L51)
      implicit integer (i-p)
      integer NB(9), N(9),NT(8) ,MT(8)
      real a(1)
      EQUIVALENCE (NT(1),N1),(NT(2),N2),(NT(3),N3),(NT(4),N4),
     1  (NT(5),N5),(NT(6),N6),(NT(7),N7),(NT(8),N8),(MT(1),M1),
     1 (MT(2),M2),(MT(3),M3),(MT(4),M4),(MT(5),M5),(MT(6),M6),
     1   (MT(7),M7),(MT(8),M8)
      COMMON A/AA/L5,MO,MI,MK

C
C     debug
C
c      write(7,8000)msabd,ic,is,np,l51
c8000  format(' in srink: msabd, ic, is, np, l51 ',5i5)
      NP=NTC(N,NB,IS)
      K=NP*MSABD
      DO 1 I=1,K
      J=I+L51
1     A(J)=0.0
      K=1
      J=0
      DO 3 I=1,8
      I1=I+IS
      MT(I)=1
      NT(I)=NB(I1)
      IF (NB(I1).EQ. 1) J=J+1
      IF (I .NE. N(K)) GO TO 3
      MT(I)=NB(I1)
      K=K+1
3     CONTINUE
      K=8-J
      IF (K.EQ. 0 .OR.J.EQ.0) GO TO 2
      DO 6 I=1,K
      I1=K-I+1
      I2=I1+J
      NT(I2)=NT(I1)
      MT(I2)=MT(I1)
      NT(I1)=1
6     MT(I1)=1
2     NX=IC
      DO 7 I1=1,N1
      J1=0
      IF(M1.NE.1)J1 =I1-1
      DO 7 I2=1,N2
      J2=J1
      IF(M2.NE.1) J2=I2-1+J2*N2
      DO 7 I3=1,N3
      J3=J2
      IF(M3.NE.1) J3=I3-1+J3*N3
      DO 7 I4=1,N4
      J4=J3
      IF (M4.NE.1) J4=I4-1+J4*N4
      DO 7 I5=1,N5
      J5=J4
      IF (M5.NE.1)J5=I5-1+J5*N5
      DO 7 I6=1,N6
      J6=J5
      IF (M6.NE.1) J6=I6-1+J6*N6
      J0=N7*J6
      DO 7 I7=1,N7
      J7=J6
      IF (M7.NE.1) J7=I7-1+J0
      J9=N8*J7
      DO 7 I8=1,N8
      J8=J7
      IF (M8.NE.1) J8=I8-1+J9
      IF (IS .EQ. 1) J8=J8*MSABD
      J8=J8+L51
      DO 7 K=1,MSABD
      NX2=J8+K
      NX=NX+1
7     A(NX2)=A(NX2)+A(NX)
      RETURN
      END
C
      SUBROUTINE INDEX(N,LW)
      implicit integer (i-p)
      integer N(9)
C
C     debug
C
c      write(7,8000) lw
c8000  format(' in index: lw ',i5)
      DO 1 L=1,LW
      K=LW-L+1
      IF (N(K)) 1,1,2
1     CONTINUE
2     IF (N(K)-LW) 3,4,4
3     N(K)=N(K)+1
      RETURN
4     DO 5 L=1,K
      J=K-L+1
      IF (N(J).NE. (LW+1-L)) GO TO 6
5     CONTINUE
      N(1)=1
      GO TO 7
6     N(J)=N(J)+1
      K=K-1
7     DO 8 I=J,K
      J1=I+1
8     N(J1)=N(I)+1
      RETURN
      END
C
      SUBROUTINE TEST(N,N3,IANS)
      implicit integer (i-p)
      integer N(9),N3(9)
      IANS=0
C
C     debug
C
c      write(7,8000) ians
c8000  format('in test: ians',i5)
      DO  1 I=1,8
      K=  N3(I)
      IF (K .EQ. 0 ) RETURN
      DO  2 J=1,K
      IF ( N(J) .EQ. K) GO TO 1
2     CONTINUE
      IANS=1
      RETURN
1     CONTINUE
      RETURN
      END
C
      real FUNCTION SS(MS,M)
      real A(1)
      COMMON A/AA/L5,MO,MI,MK

      S=0.0
      RM=float(M)
      DO 2 I=1,MS
      J=I+L5
      R=A(J)
2     S=S+R*R
      SS=S/RM
C
C     debug
C
C      write(7,8000)SS,ms,m
C8000  format(' function SS returning:',f8.2,' from ms,m ',2i6)
      RETURN
      END
C
      integer FUNCTION NTC(N1,NB,IS)
      integer N1(9),NB(9)
      N=1
      DO 1 I=1,8
      II =N1(I)+IS
      IF (N1(I).NE. 0) N=N*NB(II)
1     CONTINUE
      NTC=N
C
C     debug
C
c      write(7,8000) ntc
c8000  format(' function NTC returning:',i8)
      RETURN
      END
C
      integer FUNCTION NFC(N1,NB,IS)
      integer N1(9),NB(9)
      N=1
      DO 1 I=1,8
      K=N1(I)+IS
      IF (N1(I) .NE. 0 ) N=N*(NB(K)-1)
1     CONTINUE
      NFC=N
C
C     debug
C
c      write(7,8000)ntc
c8000  format(' function NFC returning:',i8)
      RETURN
      END
C
      SUBROUTINE CLEAR(N)
      implicit integer (i-p)
      integer N(9)
      DO 1 I=1,9
1     N(I)=0
      RETURN
      END
C
      SUBROUTINE  PAGE(LB,LW,K)
      real A(1)
      COMMON A/AA/L5,MO,MI,MK

      WRITE(7,199) LB,LW,K
      K=K+1
 199  format(' ANALYSIS OF VARIANCE',i2,' BETWEEN ',i2,' WITHIN',
     1  20x,'page',i2)
      return
      END
C
C
C  Jim's P calculation
C
C  The following code replaces the original PF1 which gives slightly
C      inaccurate F values
C
      real function S (A, B, C, D)
      integer A, B, C, A1, B1
      S0 = 1
      S1 = 1
      A1 = A - 2
      B1 = B - 2
      do 10 J=B,C,2
      A1 = A1 + 2
      B1 = B1 + 2
      S1 = S1*A1/B1*D
      S0 = S0 + S1
   10 continue
      S = S0
      RETURN
      END
C
      real function LnFactor( N )
C
C    Calculates ln(N!) = ln( Gamma(N + 1) )
C
      G = 0
      do 10 J = 1,N
   10 G = G + LOG( REAL(J) )
      LnFactor = G
      RETURN
      END
C
      real function LnGammaO( V )
C
C    Calculates ln( Gamma(V + .5) ) when degrees of freedom are odd
C
      integer V
      Pi = 3.1415927
      G = LOG( SQRT(Pi) )
      do 10 J = INT( REAL(V)/2 - 1 + 0.5 ), 1, -1
   10 G = G + LOG( REAL(J) - 0.5)
      LnGammaO = G
      RETURN
      END
C
      logical function Odd( I )
      if (MOD(I,2).EQ.0) then
         Odd = .FALSE.
      else
         Odd = .TRUE.
      end if
      RETURN
      END
C
      subroutine pf1( F, NDFN, NDFD, P)
C Subroutine to calculate probabilities of F using algorithm in
C  MICROSTAT
C
      integer V1, V2
      logical Odd
      real LnFactor, LnGammaO
      P1(X,V1,V2) = ( X**(REAL(V2)/2) )*S(V2,2,V1-2,1.0-X)
      P2(X,V1,V2) = 1 - ( (1.0-X)**(REAL(v1)/2) )* S(V1,2,V2-2,X)
      Pi = 3.1415927
      if (F.le.0) then
         P = 1.0
         return
      end if
      if (F.GE.1) then
          V1 = NDFN
          V2 = NDFD
          F0 = F
      else
          V1 = NDFD
          V2 = NDFN
          F0 = 1.0/F
      end if
      X = V2/(V2 + V1*F0)
      if ( Odd(V1).AND.Odd(V2) ) then
         AT = ATAN( SQRT( V1*F0/V2 ) )
         if (V2.GT.1) then
            Y = AT + SIN(AT) * COS(AT) * S( 2, 3, V2-2, COS(AT)**2 )
            Temp = 2/Pi*Y
         else
            Temp = 2*AT/Pi
         end if
         if (V1.GT.1) then
            Y = V2 * S( V2+1, 3, V1-2, SIN(AT)**2)
            P = 1 - Temp
     1             + EXP(
     2                  LOG( 2/SQRT(Pi) )
     3                  + LnFactor(  INT( (V2-1)/2 )  )
     4                  - LnGammaO( V2 )
     5                  + LOG(  SIN(AT)*( COS(AT)**Y )  )
     6                    )
         else
            P = 1 - Temp
         end if
      else if ( (.NOT.Odd(V1)).AND.(.NOT.Odd(V2)) )  then
              if (V1.LT.V2) then
                   P = P1( X, V1, V2)
              else
                   P = P2( X, V1, V2)
              end if
           else if ( Odd(V2) ) then
                   P = P1( X, V1, V2)
                else
                   P = P2( X, V1, V2)
                end if
      if (F.LT.1) P = 1.0 - P
      RETURN
      END











