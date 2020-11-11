
/***  program for computing msec per char reading times */
/* compile with c-86:  cc corr.c -v -i libm.olb  */

/*  or compile with c-88:                            */

#include "stdio.h"
#include "stdlib.h"
#include "math.h"
#include "string.h"
#define EQ ==

int n;
int ntrials;
char ch;

main(argc, argv)
int argc;
char *argv[];
{
FILE  *fopen(), *fp1, *fp2, *fp3;
FILE *control;
char controlname[20];
char datafile[20], phrasefile[20], adjdatafile[20];  /* input file names */
char answer[10];
int trials;
char doagain;    /* flag to run program */
int fitem,litem,fcond,lcond;

printf("\nMSEC PER CHARACTER program.");
printf("\nassumes trial #, cond, item, rt, r, rt, r, #obs, obs1, ...,obsn");

printf("\n Chatter? y or n-");
if(tolower(*(gets(answer)))EQ'y')
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
printf("Control file? (names of data files and segment length files): ");
scanf("%s",controlname);
control = fopen(controlname,"r");
if(control == NULL)
	{
	printf("\nCan't open %s\n",controlname);
	exit(0);
	}

while (fscanf(control,"%s%s",datafile,phrasefile) != EOF)
	{
	strcpy(adjdatafile,"c");
	strcat(adjdatafile,datafile);
	printf("\n%s %s %s",datafile,adjdatafile,phrasefile);
	trials = 0;
	n = 0;
	fp1 = fopen(datafile, "r");
	fp2 = fopen(phrasefile, "r");
	fp3 = fopen(adjdatafile, "w");
	if (fp1 == NULL)                                           /* error */
		{
		printf("\ncan't open %s\n ", datafile);
		exit(0);
		}
	else if (fp2 == NULL)                                      /* error */
		{
		printf("\ncan't open %s\n ", phrasefile);
		exit(0);
		}
	else                                           /* do the important part  */ 
		{
		if(ch)
		printf("\nbeginning first trial");
		while (++trials <= ntrials)
			readstuff(fp1,fp2,fp3,trials,fitem,litem,fcond,lcond);
		}
	}
	fclose(fp1);
	fclose(fp2);
	fclose(fp3);
	fclose(control);
	exit(0);
}    /* end main */



readstuff(fp1,fp2,fp3,trials,fitem,litem,fcond,lcond)  /* reads in lines of data; skips over junk before and after */

FILE *fp1, *fp2,*fp3;
int trials,fitem,litem,fcond,lcond;

{
char temp[200];
int junk, i, nphrase1, nphrase2, rt, phrlen, temp1, temp2;
int condd,condc,itemd,itemc;
int rt11,rt12,rt21,rt22;
int r11,r12,r21,r22;

if(ch)
	printf("\nentering readstuff procedure");
do	
	{
	fscanf(fp2,"%d %d %d",&temp2,&condc,&itemc);	/* read junk,cond,item from cnt */
	if(itemc < fitem || itemc > litem || condc < fcond || condc > lcond)
		{
		fscanf(fp1,"%d %d %d",&temp2,&condd,&itemd);	/* read junk,cond,item from data */
		if(ch)
			printf("\nSkipping cond %d item %d from control",condc,itemc);	
		fgets(temp,200,fp1);	/* throw away line of data */
		if(ch)
			printf("\necho from data line %s",temp);
		fgets(temp,200,fp2);	/* throw away control line too */
		if(ch)
			printf("\necho from control line %s",temp);
		}
	}
while(itemc < fitem || itemc > litem || condc < fcond || condc > lcond);
fscanf(fp1,"%d %d %d",&temp1,&condd,&itemd);	/* read data " */
if(ch)
	printf("\nGot one: cond %d item %d",condc,itemc);
if(condc != condd || itemc != itemd) 
	{
	printf("\nINP MISMATCH condd = %d itemd = %d, condc = %d, itemc = %d",condd,itemd,condc,itemc);
	exit(0);
	}
fscanf(fp1,"%d %d %d %d",&rt11,&r11,&rt21,&r21);
fscanf(fp2,"%d %d %d %d",&rt12,&r12,&rt22,&r22);
fscanf(fp1,"%d",&nphrase1);
fscanf(fp2,"%d",&nphrase2);
if(ch)
	printf("\nnphrase1 = %d nphrase2 = %d",nphrase1,nphrase2);
if(nphrase1 != nphrase2)
	{
	printf("\n\nMISMATCH: nphrase1 = %d nphrase2 = %d trial %d",nphrase1,nphrase2,trials);
	exit(0);
	}
fprintf(fp3,"%4d%4d%6d%4d%6d%4d%4d",condd,itemd,rt11,r11,rt21,r21,nphrase1);
i = 0;
for(i=1;i <= nphrase1;i++)
	{
	fscanf(fp1, "%d", &rt);        /* read in a reaction time */
	fscanf(fp2, "%d", &phrlen);    /* read in corresponding phrase length */
	if(rt < 4000)			/* KLUDGE FOR PAUL */
		rt /= phrlen;				/* msec per character */
	fprintf(fp3,"%6d",rt);
	}
fprintf(fp3,"\n");
fgets(temp,200,fp1);	/* get to end of line */
if(ch)
	printf("\ngone to end of data line: %s",temp);
fgets(temp,200,fp2);
if(ch)
	printf("\ngone to end of control line: %s",temp);
}      /* end readstuff */
