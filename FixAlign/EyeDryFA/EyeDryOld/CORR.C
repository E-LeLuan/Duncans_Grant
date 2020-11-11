
/***  program for correlating reading times with phrase length  ***/

/* modified and made to work again with MS-DOS, Borland TC, 6-2-93 */

/*  to compile with c-86:  cc corr.c -v -i libm.olb  */

/*  or compile with c-88:                            */


#define CPMEOF 0x1f
#include "stdio.h"
#include "stdlib.h"
#include "math.h"

int n;
double sumx, sumy, sumxy, sumx2, sumy2, alpha, beta;      /* global vars */
int ntrials;
char ch;
int scrap_preview = 1;
main(argc, argv)

int argc;
char *argv[];

{
FILE  *fopen(), *fp1, *fp2, *fp3;
double correlate();
char datafile[20], phrasefile[20], adjdatafile[20];  /* input file names */
char answer[10];
int trials;
double rvalue;   /* correlation value */
char doagain;    /* flag to run program */
int fitem,litem,fcond,lcond;

printf("\nMCORR program: general version, Mark Williams C compiled");
printf("\nassumes trial #, cond, item, rt, r, rt, r, #obs, obs1, ...,obsn");

printf("\n Chatter? y or n-");
gets(answer);
if(tolower(answer[0]) == 'y')
   ch=1;
else
   ch=0;

printf("\nHow many trials? ");
ntrials = atoi(gets(answer));
printf("Lowest numbered item? ");
fitem = atoi(gets(answer));
printf("Highest numbered item? ");
litem = atoi(gets(answer));
printf("Lowest numbered condition? ");
fcond = atoi(gets(answer));
printf("Highest numbered condition? ");
lcond = atoi(gets(answer));

doagain = 'y';
while (doagain == 'y')
	{

	trials = 0;
	n = 0;
	sumx = 0.0;
	sumy = 0.0;
	sumxy = 0.0;
	sumx2 = 0.0;
	sumy2 = 0.0;

	if (argc == 1)
		{
		printf("\nData file:   ");
		argv[1] = gets(datafile);
		++argc;
		printf("\nPhrase length file:  ");	
		argv[2] = gets(phrasefile);
		++argc;
		printf("\nCorrected data file: ");
		argv[3] = gets(adjdatafile);
		++argc;
		}
	else if (argc != 4)                                       /* error */
		{
		printf("\nUsage:  corr file1 file2");
		exit(0);
		}
	fp1 = fopen(argv[1], "r");
	fp2 = fopen(argv[2], "r");
	fp3 = fopen(argv[3], "w");
	if (fp1 == NULL)                                           /* error */
		{
		printf("\ncan't open %s\n ", argv[1]);
		exit(0);
		}
	else if (fp2 == NULL)                                      /* error */
		{
		printf("\ncan't open %s\n ", argv[2]);
		exit(0);
		}
	else                                           /* do the important part  */ 
		{
		if(ch)
		printf("\nbeginning first trial");
		while (++trials <= ntrials)
			readstuff(fp1, fp2,trials,fitem,litem,fcond,lcond);
		rvalue = correlate();
		printf("\n\n**********************************");
		printf("\n\n\nthe correlation is: %f", rvalue);
		printf("\n\nthat was for subject file %s", argv[1]);
		printf("\n\nand phrase length file %s", argv[2]);
		printf("\n\n\n**********************************");
		rewind (fp1);
		rewind (fp2);
		for(trials=0;trials<ntrials;trials++)
			writestuff(fp1,fp2,fp3,trials);
		printf("\n\ndo you want to run the program again? ");
		gets(answer);
		doagain = tolower(answer[0]);
		fclose(fp1);
		fclose(fp2);
		fputc(CPMEOF,fp3);
		fclose(fp3);
		argc = 1;
		}
	}
		exit(0);
}    /* end main */



readstuff(fp1, fp2,trials,fitem,litem,fcond,lcond)  /* reads in lines of data; skips over junk before and after */

FILE *fp1, *fp2;
int trials,fitem,litem,fcond,lcond;

{
char temp[200];
int junk, i, nphrase1, nphrase2, rt, phrlen, temp1, temp2;
int condd,condc,itemd,itemc;
fpos_t *pointer;

if(ch)
	printf("\nentering readstuff procedure");
/* do
	fscanf(fp2,"%d %d %d",&temp2,&condc,&itemc);	
while(itemc < fitem || itemc > litem || condc < fcond || condc > lcond);
do
	{
	fscanf(fp1,"%d %d %d",&temp1,&condd,&itemd);	
	if(itemc != itemd)
		fgets(temp,200,fp1);
	}
while (itemc != itemd);

if(condc != condd)
	{
	printf("\nINP MISMATCH condd = %d itemd = %d, condc = %d, itemc = %d",condd,itemd,condc,itemc);
	exit(0);
	}
*/			/* OLD STUFF ABOVE */
do			/* NEW STUFF BELOW */
	{
	fscanf(fp2,"%d %d %d",&temp2,&condc,&itemc);	/* read junk,cond,item from cnt */
	if(itemc < fitem || itemc > litem || condc < fcond || condc > lcond)
		{
		fgets(temp,200,fp1);	/* throw rest of line awaY */
		fgets(temp,200,fp2);
		}
	}
while(itemc < fitem || itemc > litem || condc < fcond || condc > lcond);
fscanf(fp1,"%d %d %d",&temp1,&condd,&itemd);	/* read data " */

if(condc != condd || itemc != itemd) 
	{
	printf("\nINP MISMATCH condd = %d itemd = %d, condc = %d, itemc = %d",condd,itemd,condc,itemc);
	exit(0);
	}

if(ch)
	printf("\nskipping over data line %d",temp1);			

for(junk = 0; junk < 4; junk++)
	{
	fscanf(fp1, "%d", &temp1);		/* skip r1,rt1,r2,rt2, data file */
	fscanf(fp2, "%d", &temp2);     /* ditto, phrase-length file */
	if(ch)
		printf("temp 1 %d temp 2 %d",temp1, temp2);
	}
/*	nphrase1 = readkludge(fp1);     reads in # of data points on line */
/*		if(ch) */
/*	printf("\nnphrase1 = %d", nphrase1);*/
/*	nphrase2 = readkludge(fp2);    ditto for phrases; better be the same */
/*		if(ch)*/
/*	printf("\nnphrase2 = %d", nphrase2);*/

fgetpos(fp1,pointer);	/* get current file pointer */
if(strlen(fgets(temp,200,fp1)) > 2)	/* data to work on */
{
fsetpos(fp1,pointer);	/* reset pointer */
fscanf(fp1,"%d",&nphrase1);
fscanf(fp2,"%d",&nphrase2);
if(scrap_preview == 1)
   {
   nphrase1--;
   fscanf(fp1,"%d",&rt);   /* read in and throw away preview times */
   }


if(ch)
	printf("\nnphrase1 = %d nphrase2 = %d",nphrase1,nphrase2);
if(nphrase1 != nphrase2)
	{
	printf("\n\nMISMATCH: nphrase1 = %d nphrase2 = %d trial %d",nphrase1,nphrase2,trials);
	exit(0);
	}

i = 0;
while (++i <= nphrase1)
	{
	fscanf(fp1, "%d", &rt);        /* read in a reaction time */
	fscanf(fp2, "%d", &phrlen);    /* read in corresponding phrase length */
	addin(rt, phrlen);
	}
fgets(temp,200,fp1);	/* get to end of line */
}				/* jump to here */
fgets(temp,200,fp2);
}      /* end readstuff */


addin(rt, phrlen)     /* adds value of rt and phraselength to sum of x, etc.  */
int rt, phrlen;
{ 
double drt, dphrlen;   /* converts rt and phrlen to doubles  */
++n;
drt = (double)(rt);
dphrlen = (double)(phrlen);
if(ch)
	printf("\nn is %d", n);
sumy += drt;
if(ch)
	printf("\nsumx is %f", sumx);
sumx += dphrlen;
if(ch)
	printf("\nsumy is %f", sumy);
sumxy += (drt * dphrlen);
if(ch)
	printf("\nsumxy is %f", sumxy);
sumy2 += (pow(drt,2.));
if(ch)
	printf("\nsumx2 is %f", sumx2);
sumx2 += (pow(dphrlen,2.));
if(ch)
	printf("\nsumy2 is %f", sumy2);
}




double correlate()
{
double varx, vary;
double denom, numer, corrret;
printf("\n\nn is %d; sumx is %f, sumy is %f; sumxy is %f ", n, sumx, sumy, sumxy);
printf("\nsumx2 is %f; sumy2 is %f", sumx2, sumy2);
varx = (sumx2 - ((pow(sumx,2.))/n));	
printf("\nvarx is %f", varx);
vary = (sumy2 - ((pow(sumy,2.))/n));	
printf("\nvary is %f", vary);
numer = (sumxy - ((sumx * sumy) / n));  
printf("\nnumer is %f", numer);
denom = (sqrt(varx * vary));
printf("\ndenom is %f", denom);
corrret = numer / denom;	
printf("\ncorr is equal to %f", corrret);
beta = numer / varx;
alpha = sumy/n - beta*(sumx/n);
printf("\nbeta = %f, alpha = %f", beta, alpha);
return(corrret);
}

readkludge(fp0)     /*  kludge.  Gets around format problem in input files  */
         /* by reading in fixed number of chars and converting to integers */
FILE *fp0;
{
int temp, place, nphrase0;
char c;
if(ch)
	printf("\nentering readkludge procedure");
c = ' ';
place = 0;
while (c == ' ')
	{
	if(ch)
	printf("\nreading a char; ");
	c = getc(fp0);
	if(ch)
	printf("the char is %c", c);
	++place;
	}
if (place == 4)
	{
	nphrase0 = (c - '0');    /* a one-digit number */
	if(ch)
	printf("\nnphrase0 is %d", nphrase0);
	}
else if (place == 3)
	{
	temp = ((c - '0') * 10);   /* first digit of two-digit number*/
	if(ch)
		printf("\nreading a char; ");
	c = getc(fp0);            /* second digit */	
	if(ch)
		printf("the char is %c", c);
	nphrase0 = (temp + (c - '0'));
	if(ch)
		printf("\nnphrase0 is %d", nphrase0);
	}
	return(nphrase0);
}

writestuff(fp1,fp2,fp3,trials)
FILE *fp1,*fp2,*fp3;
int trials;
{
char temp[200];
int i, nphrase1, nphrase2, rt, phrlen, temp1, temp2 ,crt;
int condd,condc,itemd,itemc;
int rt11,rt12,rt21,rt22,r11,r12,r21,r22;
fpos_t *pointer;

if(ch)
	printf("\nentering writestuff procedure");
fscanf(fp2,"%d %d %d",&temp2,&condc,&itemc);	/* read trial,cond,item from cnt */
do
	{
	fscanf(fp1,"%d %d %d",&temp1,&condd,&itemd);	/* read data " */
	if(itemc != itemd)
		{
		fgets(temp,200,fp1);		/* get to end ofline */
		fgets(temp,200,fp2);
		}
	}
while (itemc != itemd);

if(condc != condd)
	{
	printf("\nOUT MISMATCH condd = %d itemd = %d, condc = %d, itemc = %d",condd,itemd,condc,itemc);
	exit(0);
	}

if(ch)
	printf("\nOUT skipping over data line %d",temp1);			

fscanf(fp1,"%d %d %d %d",&rt11,&r11,&rt21,&r21);
fscanf(fp2,"%d %d %d %d",&rt12,&r12,&rt22,&r22);

fgetpos(fp1,pointer);	/* get current file pointer */
if(strlen(fgets(temp,200,fp1)) > 2)	/* data to work on */
{
fsetpos(fp1,pointer);	/* reset pointer */

fscanf(fp1,"%d",&nphrase1);
fscanf(fp2,"%d",&nphrase2);

if(scrap_preview == 1)
   {
   nphrase1--;
   fscanf(fp1,"%d",&rt);   /* read in and throw away preview times */
   }

if(ch)
	printf("\nOUT nphrase1 = %d nphrase2 = %d",nphrase1,nphrase2);
if(nphrase1 != nphrase2)
	{
	printf("\n\nOUT MISMATCH: nphrase1 = %d nphrase2 = %d trial %d",nphrase1,nphrase2,trials);
	exit(0);
	}

fprintf(fp3,"%4d %4d %6d %4d %6d %4d %4d",condd,itemd,rt11,r11,rt21,r21,nphrase1);

for(i=0;i< nphrase1;i++)
	{
	fscanf(fp1, "%d", &rt);        /* read in a reaction time */
	fscanf(fp2, "%d", &phrlen);    /* read in corresponding phrase length */
	crt=rt-(int)(alpha+beta*(double)(phrlen));
	fprintf(fp3,"%5d",crt);
	}
	fprintf(fp3,"\n");
fgets(temp,200,fp1);
}
fgets(temp,200,fp2);	/* get to end of line */
}      /* end readstuff */
