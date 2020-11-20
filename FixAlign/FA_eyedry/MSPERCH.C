/***  program for computing msec per char reading times */
/* modified from CP/M-86 11/89 */
/* version that assumes a file of pairs of raw data file names + their
corresponding count file names */
/* and appends a specified letter to the front of each raw data file name
to create the name of the adjusted/corrected data file */
/* TIDIED UP FOR PREVIEW TIMES */

#include "stdio.h"
#include "stdlib.h"
#include "math.h"
#include "string.h"
#define EQ ==

int n;
int ntrials;
char ch;
int scrap_preview;

main(argc, argv)
int argc;
char *argv[];
{
FILE *fpdata, *fpchars, *fpoutdat;
FILE *control;
char crap[20];
char controlname[20];
char datafile[40], phrasefile[40], adjdatafile[40];  /* input file names */
char answer[40];
int trials;
int fitem,litem,fcond,lcond;
int ques;
char prefix[20];

printf("\nMSEC PER CHARACTER program.");
printf("\nassumes trial #, cond, item, rt, r, rt, r, #obs, obs1, ...,obsn");

printf("\n Do you want a typed trace of program's execution? y or n- ");
if(tolower(*(gets(answer)))EQ'y')
   ch=1;
else
   ch=0;
printf("Do question data appear on the same line as reading data: y or n- ");
if(tolower(*(gets(answer)))EQ'y')
   ques=1;
else
   ques=0;
printf("How many trials? ");
ntrials = atoi(gets(answer));
printf("Lowest numbered item? ");
fitem = atoi(gets(answer));
printf("Highest numbered item? ");
litem = atoi(gets(answer));
printf("Lowest numbered condition? ");
fcond = atoi(gets(answer));
printf("Highest numbered condition? ");
lcond = atoi(gets(answer));
printf("Do you want to skip the first presentation region (preview): y or n? ");
gets(crap);
scrap_preview = (tolower(crap[0]) == 'y') ? 1 : 0;
printf("Control file? (with names of data and segment length files): ");
gets(controlname);
/*scanf("%s",controlname);*/
control = fopen(controlname,"r");
if(control == NULL)
	{
	printf("\nCan't open %s\n",controlname);
	exit(0);
	}
printf("What letter do you want to prefix the ms/char files with? ");
gets(prefix);
if(prefix[0] == '\0')
	{
	printf("\nNo dummy, you gotta prefix the file name with SOMETHING!");
	exit(0);
	}
prefix[1] = '\0';

while (fscanf(control,"%s%s",datafile,phrasefile) != EOF)
	{
	strcpy(adjdatafile,prefix);
	strcat(adjdatafile,datafile);
	printf("\n%s %s %s",datafile,adjdatafile,phrasefile);
	trials = 0;
	n = 0;
	fpdata = fopen(datafile, "r");
	fpchars = fopen(phrasefile, "r");
	fpoutdat = fopen(adjdatafile, "w");
	if (fpdata == NULL)                                           /* error */
		{
		printf("\ncan't open %s\n ", datafile);
		exit(0);
		}
	else if (fpchars == NULL)                                      /* error */
		{
		printf("\ncan't open %s\n ", phrasefile);
		exit(0);
		}
	else if (fpoutdat == NULL)
		{
		printf("\ncan't open %s\n",adjdatafile);
		exit(1);
		}
	else                                           /* do the important part  */ 
		{
		while (++trials <= ntrials)
			readstuff(fpdata,fpchars,fpoutdat,trials,fitem,litem,fcond,lcond,ques);
		}
	fclose(fpdata);
	fclose(fpchars);
	fclose(fpoutdat);
	}
	fclose(control);
	exit(0);
}    /* end main */



readstuff(fpdata,fpchars,fpoutdat,trials,fitem,litem,fcond,lcond,ques)  /* reads in lines of data; skips over junk before and after */
FILE *fpdata, *fpchars,*fpoutdat;
int trials,fitem,litem,fcond,lcond,ques;
{
char temp[400], string[80];
int junk, i, nphrase_data, nphrase_count, rt, phrlen, temp1, temp2;
int condd,condc,itemd,itemc;
int rt11,rt12,rt21,rt22;
int r11,r12,r21,r22;
fpos_t *pointer;

if(ch)
	printf("\n\nENTERING READSTUFF PROCEDURE, TRIAL %d",trials);
do	
	{
	fscanf(fpchars,"%d%d%d",&temp2,&condc,&itemc);	/* read junk,cond,item from cnt */
	if(itemc < fitem || itemc > litem || condc < fcond || condc > lcond)
		{
		if(ch)
			printf("\nSkipping cond %d item %d from control",condc,itemc);	
		if(condc <= lcond  || !ques)	/* not a question trial, on same line */
			{	/* assumes conditions increase through data file */
			fscanf(fpdata,"%d%d%d",&temp2,&condd,&itemd);	/* read junk,cond,item from data */
			fgets(temp,400,fpdata);	/* throw away line of data */
			if(ch)
				printf("\necho from data line t %d c %d i %d %s",temp2,condd,itemd,temp);
			}
/*		else
			printf("\n");*/
		fgets(temp,400,fpchars);	/* throw away rest of control line too */
		if(ch)
			printf("\necho from control line outside limits %s",temp);
		}
	}
while(itemc < fitem || itemc > litem || condc < fcond || condc > lcond);


do				/* get rid of leading data trials not being analyzed */
	{
	fscanf(fpdata,"%d %d %d",&temp1,&condd,&itemd);	/* read junk,cond,item from data */
	if(itemd < fitem || itemd > litem || condd < fcond || condd > lcond)
		{
		fgets(temp,400,fpdata);	/* throw away line of data */
		if(ch)
			printf("\necho from data line, data outside limits %s",temp);
		}
	}
while(itemd < fitem || itemd > litem || condd < fcond || condd > lcond);

if(ch)
	printf("\nGot one: cond %d item %d",condc,itemc);
if(condc != condd || itemc != itemd) 
	{
	printf("\nINPUT MISMATCH\ncond from data = %d, itemd = %d; cond from count = %d, itemc = %d",condd,itemd,condc,itemc);
	exit(0);
	}
fscanf(fpdata,"%d %d %d %d",&rt11,&r11,&rt21,&r21);
fscanf(fpchars,"%d %d %d %d",&rt12,&r12,&rt22,&r22);
fgetpos(fpdata,pointer);	/* get current file pointer */
if(strlen(fgets(temp,400,fpdata)) > 2)	/* data to work on */
{
/*printf("\nlength of string %d",strlen(temp));*/
fsetpos(fpdata,pointer);	/* reset pointer */
fscanf(fpdata,"%d",&nphrase_data);
fscanf(fpchars,"%d",&nphrase_count);
if(scrap_preview == 1)
   {
   nphrase_data--;
   fscanf(fpdata,"%d",&rt);   /* read in and throw away preview times */
   }
if(ch)
	printf("\nnphrase_data = %d nphrase_count = %d",nphrase_data,nphrase_count);
if(nphrase_data != nphrase_count)
	{
	printf("\n\nNUMBER OF PHRASES MISMATCH\nphrases from data = %d, phrases from count = %d, trial %d",nphrase_data,nphrase_count,trials);
	printf("\nDo you want to delete First time, delete Last time, or Quit? F L or Q: ");
	gets(string);
	if(tolower(string[0]) == 'f')
		{
		nphrase_data--;
		fscanf(fpdata,"%d",&rt);	/* read and discard */
		}
	else if(tolower(string[0]) == 'l')
		nphrase_data--;
	else
		exit(0);
	}
fprintf(fpoutdat,"%4d%4d%6d%4d%6d%4d%4d",condd,itemd,rt11,r11,rt21,r21,nphrase_data);
i = 0;
for(i=1;i <= nphrase_data;i++)
	{
	fscanf(fpdata, "%d", &rt);        /* read in a reaction time */
	fscanf(fpchars, "%d", &phrlen);    /* read in corresponding phrase length */
	if(rt > 0 && phrlen > 0)
		rt /= phrlen;
	fprintf(fpoutdat,"%6d",rt);
	}
fprintf(fpoutdat,"\n");
fgets(temp,400,fpdata);	/* get to end of line */
		/* go here to scrap trial */
}
fgets(temp,400,fpchars);
}      /* end readstuff */
