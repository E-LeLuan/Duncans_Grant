char version[] = "INCREMENTAL GRAMM JUDGE 6/13/97";
/*...................................................
MSEGMENT.C

Version for incremental grammaticality judgment or makes sense 
experiment. Analysis regions need not be identical to presentation
regions. However, normal use will have first analysis region be the
sentence up to point of interest, then word by word analysis
regions through end of region of interst. No lookahead for question
answers is allowed.
Long and short times not eliminated; long truncated, short replaced
by mean (or by 1000 if no mean is available)

Subj and item outputs now include number reject, possible reject,
and cumulative proportion reject for each region.

program for analyzing self-pace and pigtrack data
consisting of:
... cond# ... item# ... #datapoints ... dp1 dp2 dp3...dpn
optionally with question to be discarded after each trial

VERSION DESIGNED TO TAKE EXCEPTIONS FILE, IN FORMAT:
item# +-changetocond#
item# +-changetocond#
...
it does the following things:
	1. writes a file of mean values for each combination
	of subject,item, subcondition, and segment (where 
	subcondition refers to the first, second,...nth
	condition used for an item)
	2. writes a file of mean values for each combination
	of subject, condition, and segment, averaged over items
	3. writes a file of mean values for each combination
	of item, condition, and segment, averaged over subjects
	4. writes to the printer or to a file the
	means over items, and the means over subjects and
	items, of the data for each condition and segment

To run it, you need a control file with one line for each
item/condition combination used in the experiment, 
containing the following information:

item# cond# #analysis segments start of 2nd analysis segment ... start of last analysis segment 
	#presentation regions	length of first pres reg	length of second pres reg...
	
the program assumes that the first segment starts immediately after
the #datapoints entry in the data, and the last ends #datapoints
later.

When the control file gets read in, the condition number for 
a sentence gets stored as the start of the first analysis segment
(third index = 0) which is always 0 anyway.

Note, the condition numbers indicated are those in the datafile
before adjustment from the exceptions file.

2/28/88: code added to allow rejection of trials where the question
was answered incorrectly
6/88: restructured to calloc data storage area
6/88: if changetocond == 99, item is thrown out
11/89: modified to accept parameters file
6/92: modified to automatically calculate ms/char if desired
6/93, deviation from regression analysis added; regression based on
whatever item and condition limits you specify when you write the .cor
files -- something that is done automatically when the .cor files don't
exist
1/97: modified as described above to use with stops making sense expts

......................................................*/


#include "stdio.h"
#include "stdlib.h"
#include "math.h"
#include "ctype.h"
#include "alloc.h"
#include "dos.h"
#include "string.h"
#define EQ ==
#define MAXLINE 80
#define MAXLINEX 400
#define MAXREGIONS 25	/* max number of presentation regions */
#define TRUE 1
#define FALSE 0
#define ERR NULL			/* from CPM86 */

int correct(int *debug,int *nimin,int *nimax,int *nsegmax,int *ncmin,int *ncmax,int *nscmax,int *cpos,int *ipos,int *npos,int *dpos,int *longtime,int *shorttime,int *skiptime,int *mschar,int *nitot,int *nctot,int *avg,char *allname,char *subname,char *senname,char *nsubname, char *nsenname);
void get_regression(double *alpha, double *beta,int nimin,int nimax,int ncmin,int ncmax,int nscmax,int cpos,int ipos,int npos,int dpos,int longtime,int shorttime,int skiptime,int nitot);
char *tgets(char *buff,FILE *stream);
getcontrol(int nimin,int nimax,int ncmin,int ncmax,int nitot,int nscmax,int nsegmax);
sortsubject(int i,int nimin,int nimax,int ncmin,int ncmax,int nscmax,int cpos,int ipos,int npos,int dpos,int longtime,int shorttime,int skiptime,int nitot,int mschar,int nctot);
readnext(int control);
spacebuff(char *bptr,int val);
cleararrays(int nitot,int nctot,int nscmax,int nsegmax);


char buff [MAXLINEX];
FILE *dbuff,*subbuff,*nsubbuff,*nsenbuff,*senbuff,*allbuff;
FILE *printer;
FILE *control;
char file [MAXLINE];
char *tbuff;
char *tgets();

int ti1,ti2,ti3;			/* temporary indices into arrays */
int *C;	/* control info; indices are sentence#-1,
	item-cond #, and segment#-1; values are	segment-starts */
int *V;	/* individual subject values */
int *NV;	/* corresponding counts */
long int *CUMV;	/*cumulative values, cumulated over subjects */
int *CUMNV;
long int *CONDV;		/* indiv S condition values */
int *CONDNV;
int *CONDNR_TOT;	/* count of total rejection responses, over Ss */
int *CONDNP_TOT;	/* count of possible rejections */
long int *CUMCV;
int *CUMCNV;
int *ITEMCOND;	/* list of actual conditions for each item;
						changes for each subject */
int *NR_OBS;		/* number rejections for a condition, subj by subj */
int *NP_OBS;		/* number of possible rejections */
int *NR_OBI;		/* number rejections for a condition, item by item */
int *NP_OBI;
int *REGION_LENGTH,*REGION_NUMBER;

int debug;
FILE *datalst,*except;
int vexcept[200];		/* 200 items max */
int exceptflag;
int avg;					/* average or cumulate multiple phrases */


void oops(char *string);
void openfail(char *string);
void summarize(int nimin,int nimax,int nscmax,int nsegmax,int ncmin,int ncmax,int senfileq,int nitot,int nctot,int nsub);
void collapsecond(int i,int c,int n,int nm,int ns,int ne,int nt,int nctot,int mschar);

main()
{
int changes,newvals;
int ni;
int nsub,k,j,i,l,jc;
int nimax,senfileq,allfileq,subfileq;
int nitot,nctot;
int nimin;
int nsegmax,nscmax;
int ncmin,ncmax;
int cpos,ipos,npos,dpos,mschar;
int longtime, shorttime,skiptime;
int subj_count,subj_cum;
char string[100];
char allname[20],subname[20],senname[20],nsubname[20],nsenname[20];
char c;
struct date today;
printf("\nDo you want hard copy? y or n: ");
gets(string);
if(tolower(string[0])=='y')
	{
	if ((printer = fopen("prn","w")) == NULL)
		openfail("prn");
	}
else
	{
	printf("What is output trace file name? ");
	while((printer=fopen(gets(string),"w")) EQ NULL)
		printf("\nCAN'T OPEN FILE, TRY AGAIN OR QUIT AND CHECK DISK SPACE\n");
	printf("Writing output on file %s",string);
	}
fprintf(printer,"\nVersion %s\n",version);
printf("\nSEGMENT version %s\n",version);
getdate(&today);
fprintf(printer,"DATE: %d/%d/%d\n",today.da_mon,today.da_day,today.da_year);
printf("This version is for incremental grammaticality judgment data.\nAnalysis regions must be identical to presentation regions.");
printf("\nIt also permits an EXCEPTIONS FILE of item-condition adjustment pairs.");
printf("\nIt also lets you request ms/char or deviation from regression analyses;\nbut to use them, you MUST create a .cnt file using the new version of selfpace.tem\n");
printf("\n\nType an identifying string to print out, with date\n");
gets(string);
fprintf(printer,"\n%s\n",string);

printf("Type name of file with control values (the .CRN file)\n or press ENTER if there is none. ");
tgets(buff,stdin);
if(strlen(buff) != 0)
	{
	newvals = 0;
	if((control = fopen(buff,"r")) == NULL)
		openfail(buff);
	fprintf(printer,"\nOLD control file %s",buff);
	fscanf(control,"%d%d",&debug);
	fscanf(control,"%d%d%d%d%d%d%d%d%d%d%d%d%d%d",&nimin,&nimax,&nsegmax,&ncmin,&ncmax,&nscmax,&cpos,&ipos,&npos,&dpos,&longtime,&shorttime,&skiptime,&mschar);
	nitot=nimax-nimin+1;
	nctot=ncmax-ncmin+1;
	fscanf(control,"%d",&avg);
	if(mschar != 0)
		avg = 1;		/* force to average non-raw data */
	fscanf(control,"%s",allname);
	if(!strcmp("0",allname))
		strcpy(allname,"");
	fscanf(control,"%s",subname);
	if(!strcmp("0",subname))
		strcpy(subname,"");
	fscanf(control,"%s",senname);
	if(!strcmp("0",senname))
		strcpy(senname,"");
	fscanf(control,"%s",nsubname);
	if(!strcmp("0",nsubname))
		strcpy(nsubname,"");
	fscanf(control,"%s",nsenname);
	if(!strcmp("0",nsenname))
		strcpy(nsenname,"");
	}
else
	{
	newvals = 1;
	printf("Debug level (0 = no, 1 = getcontrol, 2 = sortsubj, 3 = elsewhere - ");
	debug = atoi(gets(buff));
	printf("What is smallest numbered experimental item? - ");
	nimin = atoi(gets(buff));
	printf("What is largest numbered experimental item? - ");
	nimax = atoi(gets(buff));
	nitot = nimax-nimin+1;
	printf("How many analysis segments maximum? - ");
	nsegmax = atoi(gets(buff));
	printf("What is the smallest condition number (after any exception adjustment)? - ");
	ncmin = atoi(gets(buff));
	printf("What is the largest condition number (after exception adjustment)? -" );
	ncmax = atoi(gets(buff));
	nctot = ncmax-ncmin+1;
	printf("How many subconditions maximum for any one item? - ");
	nscmax = atoi(gets(buff));
	printf("What field is the condition number in? ");
	cpos = atoi(gets(buff));
	printf("What field is the item number in? ");
	ipos = atoi(gets(buff));
	printf("What field is the number-of-presentation-segments number in? ");
	npos = atoi(gets(buff));
	printf("What field do the data start in? ");
	dpos = atoi(gets(buff));
	printf("What field (e.g. 10) do you want to start eliminating times in?\nNote: this is so you can skip the READY time in the longtime filter): ");
	skiptime = atoi(gets(buff));
	printf("Do you want RAW times (r), MS/CHAR (m) or REGRESSION (g)? ");
	if((tolower(*gets(buff))) == 'g')
		mschar = 2;
	else if ((tolower(buff[0])) == 'm')
		mschar = 1;
	else
		mschar = 0;
/*	if(mschar == 1)
		printf("Shortest reading time, ms/char (discard if below): ");
	else */
		printf("Shortest reading time, raw ms (discard if below): ");
	shorttime = atoi(gets(buff));
/*	if(mschar == 1)
		printf("Longest reading time, ms/char (TRUNCATE if above): ");
	else */
		printf("Longest reading time, msec (TRUNCATE if above): ");
	longtime = atoi(gets(buff));
	if(mschar == 0)
		{
		printf("Do you want to CUMULATE or AVERAGE multiple phrases in a segment? c or a- ");
		avg = (tolower(*gets(buff))) EQ 'a'? 1 : 0;	/* 1 if average */
		}
	else
		{
		printf("The program will average reading times across regions.\n");
		avg = 1;
		}
	printf("Item X subject file name (press ENTER if no IXS output): ");
	gets(allname);
	printf("Subject file name (press ENTER if no Sub output): ");
	gets(subname);
	printf("Item file name (press ENTER if no Item output): ");
	gets(senname);
	printf("Subject acceptance count file (press ENTER if no Sub output): ");
	gets(nsubname);
	printf("Item acceptance count file (press ENTER if no Item output): ");
	gets(nsenname);
	}
while((changes = correct(&debug,&nimin,&nimax,&nsegmax,&ncmin,&ncmax,&nscmax,&cpos,&ipos,&npos,&dpos,&longtime,&shorttime,&skiptime,&mschar,&nitot,&nctot,&avg,allname,subname,senname,nsubname,nsenname))> 0)
	;

if((changes != 0) || newvals)
{
printf("If you want to save these new values, type a file name;\notherwise press ENTER): ");
tgets(file,stdin);
if(strlen(file) != 0)
	{
	fprintf(printer,"\nsaved as %s",file);	
	if ((control = fopen(file,"w")) == NULL)
		openfail(file);
	fprintf(control,"%d %d \n",debug);
	fprintf(control,"%d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",nimin,nimax,nsegmax,ncmin,ncmax,nscmax,cpos,ipos,npos,dpos,longtime,shorttime,skiptime,mschar);
	fprintf(control,"%d\n",avg);
	if(strlen(allname) == 0)
		fprintf(control,"0\n");
	else
		fprintf(control,"%s\n",allname);
	if(strlen(subname) == 0)
		fprintf(control,"0\n");
	else
		fprintf(control,"%s\n",subname);
	if(strlen(senname) == 0)
		fprintf(control,"0\n");
	else
		fprintf(control,"%s\n",senname);
	if(strlen(nsubname) == 0)
		fprintf(control,"0\n");
	else
		fprintf(control,"%s\n",nsubname);
	if(strlen(nsenname) == 0)
		fprintf(control,"0\n");
	else
		fprintf(control,"%s\n",nsenname);
	fclose(control);
	}
}
if(strlen(allname))
	{
	allfileq = 1;
	if ((allbuff = fopen(allname,"w")) == NULL)
		openfail(allname);
	}
else
	allfileq = 0;
if(strlen(subname))
	{
	subfileq = 1;
	if ((subbuff = fopen(subname,"w")) == NULL)
		openfail(subname);
	if ((nsubbuff = fopen(nsubname,"w")) == NULL)
		openfail(nsubname);
	}
else
	subfileq = 0;
if(strlen(senname))
	{
	senfileq = 1;
	if ((senbuff = fopen(senname,"w")) == NULL)
		openfail(senname);
	if ((nsenbuff = fopen(nsenname,"w")) == NULL)
		openfail(nsenname);
	}
else
	senfileq = 0;
nsegmax+=2;		/* add 2 to it */


printf("\nAllocating storage regions\n");
if((C = (int *)(calloc(nitot*nscmax*nsegmax,sizeof(int))))==NULL)
	oops("C");
if((V = (int *)(calloc(nitot*nscmax*nsegmax,sizeof(int))))==NULL)
	oops("V");
if((REGION_LENGTH = (int *)(calloc(nitot*nscmax*MAXREGIONS,sizeof(int))))==NULL)
	oops("REGION_LENGTH");
if((REGION_NUMBER = (int *)(calloc(nitot*nscmax,sizeof(int))))==NULL)
	oops("REGION_NUMBER");
if((NV =  (int *)(calloc(nitot*nctot*nsegmax,sizeof(int))))==NULL)
	oops("NV");
if((CUMV =  (long int *)(calloc(nitot*nscmax*nsegmax,sizeof(long int))))==NULL)
	oops("CUMV");
if((CUMNV =  (int *)(calloc(nitot*nscmax*nsegmax,sizeof(int))))==NULL)
	oops("CUMNV");
if((CONDV =  (long int *)(calloc(nctot*nsegmax,sizeof(long int))))==NULL)
	oops("CONDV");
if((CONDNV =  (int *)(calloc(nctot*nsegmax,sizeof(int))))==NULL)
	oops("CONDNV");
if((CONDNR_TOT =  (int *)(calloc(nctot*nsegmax,sizeof(int))))==NULL)
	oops("CONDNR_TOT");
if((CONDNP_TOT =  (int *)(calloc(nctot*nsegmax,sizeof(int))))==NULL)
	oops("CONDNP_TOT");
if((CUMCV =  (long int *)(calloc(nctot*nsegmax,sizeof(long int))))==NULL)
	oops("CUMCV");
if((CUMCNV =  (int *)(calloc(nctot*nsegmax,sizeof(int))))==NULL)
	oops("CUMCNV");
if((ITEMCOND =  (int *)(calloc(nitot*nctot,sizeof(int))))==NULL)
	oops("ITEMCOND");
if((NR_OBS =  (int *)(calloc(nctot*nsegmax,sizeof(int))))==NULL)
	oops("NR_OBS");
if((NP_OBS =  (int *)(calloc(nctot*nsegmax,sizeof(int))))==NULL)
	oops("NP_OBS");
if((NR_OBI =  (int *)(calloc(nitot*nscmax*nsegmax,sizeof(int))))==NULL)
	oops("NR_OBI");
if((NP_OBI =  (int *)(calloc(nitot*nscmax*nsegmax,sizeof(int))))==NULL)
	oops("NP_OBI");
printf("What is the name of the file that lists data files? ");
while((datalst=fopen(gets(file),"r")) EQ NULL)
	printf("\nBad file name, try again.\n");
fprintf(printer,"\ndatalst = %s\n",file);
nsub = 0;
while((c = fgetc(datalst)) != EOF)	/* count the subjects */
	{
/*	printf("%c",c);*/
	if(c == '\n')
		nsub++;
	}
rewind(datalst);
printf("%d subjects\n",nsub);
printf("Any exceptions file? y or n: ");
exceptflag = (tolower(*gets(buff)) EQ 'y') ? TRUE : FALSE;
if(exceptflag)
	{
	printf("What is the exceptions file name? ");
	while((except=fopen(gets(file),"r")) EQ NULL)
		printf("\nBAD FILE NAME, TRY AGAIN\n");
	fprintf(printer,"\nExceptions file %s\n",file);
	while(fscanf(except,"%d %d",&i,&j) != EOF)
		{
		fprintf(printer,"\nitem %d condition adjustment %d",i,j);
		vexcept[i-nimin] = j;	/* vector of condition adjustment values */
		}
	}
ni = getcontrol(nimin,nimax,ncmin,ncmax,nitot,nscmax,nsegmax);	/* get the control list into C */
if(debug != 0)
	printf("\n NI = %d nimax = %d nscmax = %d",ni,nimax,nscmax);
for(i=0;i<nsub;i++)
	{
	cleararrays(nitot,nctot,nscmax,nsegmax);
	sortsubject(i,nimin,nimax,ncmin,ncmax,nscmax,cpos,ipos,npos,dpos,longtime,shorttime,skiptime,nitot,mschar,nctot);
	if(allfileq)
		for(j = 0; j < nitot; j++)
			for(k = 0; k < nscmax; k++)
			{
			ti1 = k*nitot + j;
			if (*(NV+ti1) != 0)	/* write only when data */
				{
				fprintf(allbuff,"%5d%5d%5d",i+1,j+1,*(ITEMCOND + ti1));
				for(l = 0; l < nsegmax-2; l++)
					{
					ti2 = l*nitot*nscmax + k*nitot + j;
					if(avg)
						fprintf(allbuff,"%5d",(*(V+ti2))/(*(NV+ti2)));
					else
						fprintf(allbuff,"%5d",*(V+ti2));
					}
				fprintf(allbuff,"\n");
				}
			else
				fprintf(allbuff,"XX\n");
			}
	for(j=0;j<nitot;j++)
		for(k=0;k<nscmax;k++)
			{
			for(l=0;l<nsegmax-2;l++)
				{
				ti1 = l*nscmax*nitot + k*nitot + j;
				if(*(NV + ti1) != 0)			/* cumulate when have data */
					{
					if(avg)
						*(CUMV+ti1) += (*(V+ti1))/(*(NV+ti1));
					else
						*(CUMV+ti1) += *(V+ti1);
						(*(CUMNV+ti1))++;
					}
				}
			}
	for(j=0,jc=ncmin-1;jc<ncmax;j++,jc++)
		collapsecond(j,jc,nimin,nimax,nscmax,nsegmax,nitot,nctot,mschar);
	if(subfileq)
		for(j=0,jc=ncmin-1;jc<ncmax;j++,jc++)
			{
			fprintf(subbuff,"%5d%5d",i+1,jc+1);
			/* print out item, cond */
			for(k = 0; k < nsegmax-2; k++)
				{
				ti1 = k*nctot + j;
				if(*(CONDNV+ti1))
					fprintf(subbuff,"%5ld",*(CONDV+ti1)/(long)(*(CONDNV+ti1)));
				else
					fprintf(subbuff,"%5d",0);
				}
			fprintf(subbuff,"\n");
			
			/* print out item, cond, and number of possible obs for this S */
			fprintf(nsubbuff,"%5d%5d",i+1,jc+1);
			for(k=0,subj_count=0,subj_cum=0; k < nsegmax-2; k++)
				{
				ti1 = k*nctot + j;
				if(subj_count == 0)
					subj_count = *(NP_OBS+ti1);
				subj_cum += *(NR_OBS+ti1);
				fprintf(nsubbuff,"%5d%5d%5d",*(NR_OBS+ti1),*(NP_OBS+ti1),(subj_cum*100)/subj_count);
				/* subject output file, number of acceptance Rs in each segment */
				}
			fprintf(nsubbuff,"\n");
			}
	for(j=0,jc=ncmin-1;jc<ncmax;j++,jc++)
		for(k=0;k<nsegmax-2;k++)
			{
			ti1 = k*nctot + j;
			if(*(CONDV + ti1) != 0l)
				{
				*(CUMCV + ti1)+=(*(CONDV+ti1)/(long)(*(CONDNV+ti1)));
				(*(CUMCNV+ti1))++;
				}
			*(CONDNR_TOT+ti1) += *(NR_OBS+ti1);		/* sum rejection responses */
			*(CONDNP_TOT+ti1) += *(NP_OBS+ti1);		/* sum possibilities */
			}
	if(debug != 0)
		printf("\nEnd of subject %d",i+1);
	}
summarize(nimin,nimax,nscmax,nsegmax,ncmin,ncmax,senfileq,nitot,nctot,nsub);
fprintf(printer,"\f\n");
if(allfileq)
	fclose(allbuff);
if(senfileq)
	{
	fclose(senbuff);
	fclose(nsenbuff);
	}
if(subfileq)
	{
	fclose(subbuff);
	fclose(nsubbuff);
	}
fclose(datalst);
fclose(printer);
if(exceptflag)
	fclose(except);
free(ITEMCOND);
free(CUMCNV);
free(CUMCV);
free(CONDNV);
free(CONDNR_TOT);
free(CONDNP_TOT);
free(NR_OBS);
free(NP_OBS);
free(NR_OBI);
free(NP_OBI);
free(CONDV);
free(CUMNV);
free(CUMV);
free(NV);
free(V);
free(C);
return(1);
}  

/* FUNCTION to get control (count) list into C[item][subcond][segment]   */
/* item is real item number */
/* real condition number is stored at segment = 0 */
/* Also fills up REGION_LENGTH */

getcontrol(int nimin,int nimax,int ncmin,int ncmax,int nitot,int nscmax,int nsegmax)
{
int ccond, i;
int segment,index,icond;
int ni;
int item,tempitem;
int number_analysis_regions, number_presentation_regions;
FILE *conbuff;
char c;
item = 0;
icond = -1;	/* will go to 0 first time around */
printf(" Name of file containing segment counts (the .cnt file)? - ");
while((conbuff=fopen(gets(file),"r")) EQ 0)
	printf("\nCan't find control file; try again.\n");
fprintf(printer,"COUNT file %s\n",file);
ni = 0;
while((c = fgetc(conbuff)) != EOF)	/* count the lines */
	{
/*	printf("%c",c);*/
	if(c == '\n')
		ni++;
	}
fprintf(printer,"count file length %d\n",ni);
rewind(conbuff);

for(index=0;index<ni;index++)
	{
	if(fgets(buff,MAXLINEX,conbuff) EQ NULL)
		{
		printf("\nError reading control file, item %d ",index+1);
		exit(1);
		}
	if((tempitem = readnext(0)) EQ ERR)	/* get item number */
		{
		printf("\nError reading control file, item %d ",index+1);
		exit(1);
		}
	if(tempitem-nimin EQ item)	/* this item same as last one */
		icond++;
	else
		{
		icond = 0;
		item = tempitem -nimin;
		}
	ccond = readnext(1);
	if(debug EQ 1)
		printf("\n\nGETCONTROL: ITEM %d CONDITION %d",tempitem,ccond);
	if(tempitem >= nimin && tempitem <= nimax && ccond >= ncmin && ccond <= ncmax)
		{
		*(C+icond*nitot + item) = ccond;
		number_analysis_regions = readnext(1);
		if(debug)
			printf("\n%d = number_analysis_regions",number_analysis_regions);
		for(segment = 1; segment < number_analysis_regions;segment++)
			{
			*(C+(segment)*nitot*nscmax + icond*nitot + item) = readnext(1);
			if(debug EQ 1)	/* read in segment starts */
				printf("\n  C[%d][%d][%d] = %d",item,icond,segment,*(C + segment*nitot*nscmax + icond*nitot + item));
			if(segment+1 > nsegmax)
				{
				printf("\nPROBLEMS: %d is more segments that you calloc'd space for",segment);
				exit(1);
				}
			}
			*(C+(++segment)*nitot*nscmax + icond*nitot + item) = 0;	/* end it */
	
		number_presentation_regions = readnext(1);
		*(REGION_NUMBER+ icond*nitot + item) = number_presentation_regions;
		if(debug)
			printf("\n%d = number_presentation_regions",number_presentation_regions);
		for(segment=0;segment<number_presentation_regions;segment++)
			{
			*(REGION_LENGTH+(segment*nitot*nscmax + icond*nitot + item)) = readnext(1);
				{
				if(debug EQ 1)
					printf("\n  REGION_LENGTH[%d][%d][%d] = %d",item,icond,segment,*(REGION_LENGTH+segment*nitot*nscmax + icond*nitot+item));
				if(segment+1 > MAXREGIONS)
					{
					printf("\nPROBLEMS:\n  %d is more than the max number of presentation regions, MAXREGIONS;\nchange value of MAXREGIONS and recompile.\n",segment);
					exit(1);
					}
				}
			}
		}
	}
fclose(conbuff);
return(ni);
}


/* reads next ASCII-coded number from buff[] */
/* starts at beginning of buff[] if control = 0 */


readnext(int control)
{
static int bptr;
int value;
if(control EQ 0)
	bptr = 0;
while (!isdigit(*(buff+bptr)))
	bptr++;
value = atoi(buff+bptr);
while(isdigit(*(buff+bptr++)))
	;
if(debug EQ 1)
	printf("\nIn readnext: value = %d, bptr = %d, control = %d",value,bptr,control);
return(value);
}


/* FUNCTION FOR READING INDIVIDU2AL SUBJECT DATA, SORTING INTO
V[item][subcondition][segment] */


sortsubject(int i,int nimin,int nimax,int ncmin,int ncmax,int nscmax,int cpos,int ipos,int npos,int dpos,int longtime,int shorttime,int skiptime,int nitot,int mschar,int nctot)
{
FILE *gdbuff;
int rpos = 5;			/* position of which response made -- for grammaticality decision */
int okresp = 1;		/* response indicating OK */
int resp_made;
int fpt,lpt,tix,ticont;
int endsen, cnt,sptrptr, cptrptr, icond, ns;
int temp;
double ftemp;
int qcond,qresp;
char qbuff[MAXLINEX];
int skippos;
int nchar;
long grandtot,grandn;
double alpha,beta;
int relcond;			/* condition number relative to minimum */


fscanf(datalst,"%s",file);
fprintf(printer,"\nS:*** %s ***",file);
if((gdbuff=(fopen(file,"r"))) EQ NULL)
	openfail(file);
if(debug EQ 2)
	printf("\nIn sortsubject, i = %d nimax = %d nscmax = %d gdbuff = %d",i,nimax,nscmax,gdbuff);
if(mschar == 2)
	get_regression(&alpha,&beta,nimin,nimax,ncmin,ncmax,nscmax,cpos,ipos,npos,dpos,longtime,shorttime,skiptime,nitot);
grandtot=grandn=0l;
while(fgets(buff,MAXLINEX,gdbuff) != NULL && strlen(buff) > 1)	/* keep going until EOF */
	{
	cptrptr = atoi(buff+spacebuff(buff,cpos));
	sptrptr = atoi(buff+spacebuff(buff,ipos));
	ns = sptrptr-nimin;
	if(exceptflag)
		{
		if(vexcept[ns] == 99)
			cptrptr = ncmax+1;		/* discard item */
		else
			cptrptr += vexcept[ns];	/* adjust condition */
		}
	if(debug EQ 2)
		printf("\n sentence %d, cond %d ",sptrptr,cptrptr);
				/* skip junk sentence plus question if needed */
	if(cptrptr < ncmin || cptrptr > ncmax || sptrptr > nimax || sptrptr<nimin)
		{
		if(debug EQ 2)
			printf("\nskipping junk sen # %d, cond # %d",sptrptr,cptrptr);
		}
	else		/* OK condition number, get data position and data */
		{
		if(strlen(buff+spacebuff(buff,npos)) > 2)	/* data to work on */
			{
			resp_made = atoi(buff+spacebuff(buff,rpos));	/* get response for gramm judg */
			endsen = atoi(buff+spacebuff(buff,npos));
			tbuff = buff+spacebuff(buff,dpos);
			skippos = skiptime-dpos;	/* how many times to skip in longtime filter? */
			if(debug EQ 2)
				{
				printf("\nSPTRPTR = %d",sptrptr);
				printf("\nCPTRPTR = %d",cptrptr);
				printf("\nENDSEN  = %d",endsen);
				}
			for(icond=0;icond<nscmax;icond++)	/* find match to cond # */
				{
				ti1 = icond * nitot + ns;
				if(*(C+ti1) EQ cptrptr)
					break; 
				}
			if(icond EQ nscmax)
				{
				printf("\nSCREWUP! sen %d cond %d",sptrptr,cptrptr);
				exit(1);
				}
			relcond = cptrptr - ncmin;		/* relative cond #: 0,1,2,...,nctot-1 */
			if(debug EQ 2)
				printf("\n    NS = %d, adjusted cond = %d",ns,cptrptr);
			ti1 = icond*nitot + ns;
			*(ITEMCOND+ti1) = cptrptr; /* cond no for this subj,this sent */
			for(cnt=1,fpt=1,lpt=0;lpt < endsen+1;cnt++)
				{
				if(debug == 5)
					printf("\nin REJECT loop, cnt %d fpt %d lpt %d endsen %d",cnt,fpt,lpt,cnt);
				ticont = cnt*nitot*nscmax + icond*nitot + ns;
				ti1 = (cnt-1)*nctot + relcond;
				ti2 = (cnt-1)*nitot*nscmax + icond*nitot + ns;
				if((lpt = *(C+ticont)) == 0)
					lpt = endsen+1;		/* set trap for end of item */
				for(;fpt < lpt;fpt++)
					{
					if(debug == 5)
							printf("\nREJECTIONS,  region %d, lpt %d, REGION_LENGTH = %d",fpt,lpt,*(REGION_LENGTH+ti3));
					if(fpt == lpt-1)			/* count only once for analysis region */
						{
						(*(NP_OBS + ti1))++;
						(*(NP_OBI + ti2))++;
						if(debug == 5)
							printf("\n        ....increased NP_OBS for ti1 = %d",ti1);
						}
					}
				}			/* end of loop through presentation segments, counting possible Rs */
				if(resp_made != okresp)		/* response 1 is REJECT */
					{
					(*(NR_OBS + ti1))++;
					(*(NR_OBI + ti2))++;		/* count rejects for this sub and for items */
					endsen--;						/* last region was judged bad; ignore it in RTs */
					}
				for(cnt=1,fpt=1,lpt=0;lpt != endsen+1 ;cnt++)  /* changed for incr gramm */
				{
				if(debug == 2)
						printf("\nin loop, cnt %d fpt %d lpt %d endsen %d",cnt,fpt,lpt,cnt);
				ticont = cnt*nitot*nscmax + icond*nitot + ns;
				ti2 = (cnt-1)*nitot*nscmax + icond*nitot + ns;
				if((lpt = *(C+ticont)) == 0)
					lpt = endsen+1;		/* set trap for end of item */
				for(;fpt < lpt;skippos--,fpt++)
					{
					ti3 = (fpt-1)*nitot*nscmax + icond*nitot + ns;
					if(debug == 2)
						printf("\n  region %d, REGION_LENGTH = %d",fpt,*(REGION_LENGTH+ti3));
					temp = atoi(tbuff);
					grandtot += (long)temp;		/* keeping track of grand mean */
					grandn++;
/* msegment, do time filter before conversion to ms/char or regr time */
					if(temp > longtime && skippos <= 0)	/* time longer than set for */
						{
						fprintf(printer,"\n  LONG TIME, RT %8d cond %3d item %4d region %3d",temp,cptrptr,sptrptr,fpt);
						temp = longtime;			/* truncate */
						}
					if(temp < shorttime)
						{
						fprintf(printer,"\n  short time, RT %8d cond %3d item %4d region %3d",temp,cptrptr,sptrptr,fpt);
						temp = 0;
						if(*(NV+ti2) == 0)
							{
							if(grandn <= 0)
								{
								temp = 500;										/* bad fudge */
								fprintf(printer,"\n BAD FUDGE, substituted 500 ms\n");
								}
							else
								{	
								temp = (int)(grandtot/grandn);
								fprintf(printer,"\n PRETTY BAD FUDGE, substituted grand mean %d ms\n",temp);
								}		
							}															
						}
					if (temp == 0)
							temp = (*(V+ti2))/(*(NV+ti2));		/* replace short with average */
					if(mschar == 1)	/* ms per char	*/
						{
						if(*(REGION_LENGTH + ti3) > 0)		
							temp /= *(REGION_LENGTH + ti3);
						}
					else if(mschar == 2)
						{
						ftemp = ((double)(temp)) - (alpha + beta*(double)(*(REGION_LENGTH + ti3)));
						}							
					while(isspace(*tbuff))	/* move on to next number */
						tbuff++;
					while(!isspace(*tbuff))
						tbuff++;
					while(isspace(*tbuff))
						tbuff++;
					if(mschar == 2)
						temp = (int)(ftemp);
					*(V+ti2)+=temp;
					(*(NV+ti2))++;
					if(mschar != 2)
						if(*(V+ti2) < 0)
							fprintf(printer,"\nNEGATIVE CUMULATED READING TIME: item %d cond %d cnt %d",sptrptr,cptrptr,cnt);
					if(debug EQ 2)
						{
						tix = (cnt-1)*nitot*nscmax + icond*nitot*ns;
						printf("\nFPT = %d LPT = %d CNT = %d NS = %d D = %d N= %d",fpt,lpt,cnt,ns,*(V+ti3),*(NV+tix));
						}
					}
				}
			}
		}
	}
fclose(gdbuff);
return(1);
}


/* SET INDIVIDUAL SUBJECT ARRAYS TO ZERO */

cleararrays(int nitot,int nctot,int nscmax,int nsegmax)
{
int i,j,k;

for (i=0;i<nitot;i++)
	for (j=0;j<nscmax;j++)
		for (k=0;k<nsegmax;k++)
			{
			ti1 = k*nitot*nscmax + j*nitot + i;
			*(V+ti1)=*(NV+ti1)=0;
			}
for (i=0;i<nctot;i++)
	for(j=0;j<nsegmax;j++)
		{
		ti1 = j*nctot + i;
		*(CONDV+ti1)=0l;
		*(CONDNV+ti1)=0;
		*(NR_OBS+ti1)=0;
		*(NP_OBS+ti1)=0;
		}
for (i=0;i<nitot;i++)
	for(j=0;j<nscmax;j++)
		{
		ti1 = j*nitot + i;
		*(ITEMCOND+ti1) = 0;
		}
return;
}


/* COLLAPSE VALUES ACROSS ALL ITEMS IN A CONDITION */

void collapsecond(ic,cond,nimin,nimax,nscmax,nsegmax,nitot,nctot,mschar)
int ic,cond,nimin,nimax,nscmax,nsegmax,nitot,nctot;
{
int cnt,icond,ns;

for(ns=0;ns<nimax-nimin+1;ns++)	/* check all item/subcondition combos */
	for(icond=0;icond<nscmax;icond++)
		{
		ti1 = icond * nitot + ns;
		if(*(ITEMCOND+ti1) EQ cond+1)	/* data for this subj,sen,&cond */
			{
			for(cnt=0;cnt<nsegmax-2;cnt++)
				{
				ti2 = cnt*nitot*nscmax + icond*nitot + ns;
				ti3 = cnt*nctot + ic;
				if(*(NV+ti2))
					{
					if(avg)
						*(CONDV+ti3)+=(long)(*(V+ti2)/(*(NV+ti2)));
					else
						*(CONDV+ti3)+=(long)(*(V+ti2));
					(*(CONDNV+ti3))++;
					if(*(CONDV+ti3)<0l)
					if(mschar != 2)
						fprintf(printer,"\nNEGATIVE CONDV: item %d cond %d seg %d CONDV %ld CONDNV %d",ns+1,icond+1,cnt+1,*(CONDV+ti3),*(CONDNV+ti3));
					}
				if(debug EQ 3)
					printf("\nrel cond %d cond %d icond %d ns %d cnt %d CONDV %ld CONDNV %d",ic,cond+1,icond,ns+1,cnt,*(CONDV+ti3),*(CONDNV+ti3));
				}
			}
		}
} 


/* PRINT OUT SUMMARY VALUES */
/* and write item by item file averaged over subjects */

void summarize(nimin,nimax,nscmax,nsegmax,ncmin,ncmax,senfileq,nitot,nctot,nsub)
int nimin,nimax,nscmax,nsegmax,ncmin,ncmax,senfileq,nitot,nctot,nsub;
{
char temp[20];
int ns,icond,cond,cnt;
int cump,maxn,cumc;
int item_count,item_cum;
for(ns=0;ns<nimax-nimin+1;ns++)
	for(icond=0;icond <nscmax;icond++)
		{
		if(senfileq)
			{
			ti1 = icond*nitot + ns;
			fprintf(senbuff,"%5d%5d",ns+nimin,*(C+ti1));
			for(cnt=0;cnt<nsegmax-2;cnt++)
				{
				ti2 = cnt*nitot*nscmax + icond*nitot + ns;
				if(*(CUMNV+ti2))
					fprintf(senbuff,"%5d",*(CUMV+ti2)/(*(CUMNV+ti2)));
				else
					fprintf(senbuff,"%5d",0);
				}
			fprintf(senbuff,"\n");

			fprintf(nsenbuff,"%5d%5d",ns+nimin,*(C+ti1));
			for(cnt=0,item_cum=0,item_count=0;cnt<nsegmax-2;cnt++)
				{
				ti2 = cnt*nitot*nscmax + icond*nitot + ns;
				if(item_count == 0)
					item_count = *(NP_OBI+ti2);
				item_cum += *(NR_OBI+ti2);
				if(item_count)
					fprintf(nsenbuff,"%5d%5d%5d",*(NR_OBI+ti2),*(NP_OBI+ti2),(item_cum*100/item_count));
				else
					fprintf(nsenbuff,"%5d%5d00000",*(NR_OBI+ti2),*(NP_OBI+ti2));
				}
			fprintf(nsenbuff,"\n");
			}
		}
fprintf(printer,"\n\nAVERAGED OVER ITEMS AND SUBJECTS, BY CONDITIONS\n");
fprintf(printer,"NOTE: RTs ARE FOR OK RESPONSES ONLY.\n");
for(icond=0,cond=ncmin-1;cond<ncmax;icond++,cond++)
	{
	fprintf(printer,"\nCONDITION %4d",cond+1);
	for(cnt=0;cnt<nsegmax-2;cnt++)
		{
		if(cnt != 0 && cnt%6 == 0)
			fprintf(printer,"\n              ");
		ti1 = cnt*nctot + icond;
		if(*(CUMCNV+ti1))
			fprintf(printer,"%5d",*(CUMCV+ti1)/(*(CUMCNV+ti1)));
		else
			fprintf(printer,"%5d",0);
		fprintf(printer,"(%3d)",*(CUMCNV+ti1));
		}
	}
for(icond=0,cond=ncmin-1;cond<ncmax;icond++,cond++)
	{
	cumc=0;
	maxn = (*(CONDNP_TOT+icond));
	fprintf(printer,"\nCONDITION %4d",cond+1);
	for(cnt=0;cnt<nsegmax-2;cnt++)
		{
		if(cnt != 0 && cnt%6 == 0)
			fprintf(printer,"\n              ");
		ti1 = cnt*nctot + icond;
		cumc += (*(CONDNR_TOT+ti1));
		if(maxn > 0)
			cump = (cumc*100)/maxn;
		else
			cump = 0;
		if(*(CONDNP_TOT+ti1) > 0)
			fprintf(printer,"%5d(%3d)",((*(CONDNR_TOT+ti1))*100)/(*(CONDNP_TOT+ti1)),cump);
/*			fprintf(printer,"%5d%5d",*(CONDNR_TOT+ti1),*(CONDNP_TOT+ti1));*/
		else
			fprintf(printer,"   0");
		}
	}
printf("\nDo you want a typeout of the item-by-item data? y or n: ");
gets(temp);
if(tolower(temp[0])EQ 'y')
{
fprintf(printer,"\n\nSUMMARY BY ITEMS, AVERAGED OVER ALL SUBJECTS\n");
fprintf(printer,"\nINDIVIDUAL ITEM-SUBCONDITION COMBINATIONS\n");
for(ns=0;ns<nimax-nimin+1;ns++)
	for(icond=0;icond <nscmax;icond++)
		{
		ti1 = icond*nitot + ns;
		fprintf(senbuff,"%5d%5d",ns+nimin,*(C+ti1));
		fprintf(printer,"\nITEM %d SUBCOND %d",ns+1,*(C+ti1));
		for(cnt=0;cnt<nsegmax-2;cnt++)
			{
			ti2 = cnt*nitot*nscmax + icond*nitot + ns;
			if(*(CUMNV+ti2))
				fprintf(printer,"%5d",*(CUMV+ti2)/(*(CUMNV+ti2)));
			else
				fprintf(printer,"%5d",0);
			fprintf(printer,"(%3d)",*(CUMNV+ti2));
			}
		}
		fprintf(printer,"\n");
printf("\nDone with item by item.");
}
}


/***************************************************************************/
/* get specified datum from buff */
spacebuff(char *bptr,int val)
{
int i,j;
val--;
j=0;

if(val EQ 0)
	{
	while(isspace(*(bptr+j)))
		j++;
	return(j);
	}

for(i=0;i<val;i++)
	{
	while(isspace(*(bptr+j)))
		j++;	/* move to first/next nonspace */
	while(!isspace(*(bptr+j)))
		j++;	/* then move to next space */
	while(isspace(*(bptr+j)))
		j++;	/* and finally on to next nonspace */
	}
	j--;
return(j);
}

void oops(string)
char string[];
{
printf("\nOut of allocation space at %s.",string);
exit(1);
}



correct(int *debug,int *nimin,int *nimax,int *nsegmax,int *ncmin,int *ncmax,int *nscmax,int *cpos,int *ipos,int *npos,int *dpos,int *longtime,int *shorttime,int *skiptime,int *mschar,int *nitot,int *nctot,int *avg,char *allname,char *subname,char *senname, char *nsubname, char *nsenname)
{
int i,j,k;
char temp[20];
static int changes = 0;
printf("\n Type a number to change a control parameter, or press ENTER to quit.");
printf("\n0: debug level        =       %d",*debug);
printf("\n1, 2, 3, 4: no lookahead...");
printf("\n5: smallest item      =       %d",*nimin);
printf("\n6: largest item       =       %d",*nimax);
printf("\n7: maximum regions    =       %d",*nsegmax);
printf("\n8: smallest condition =       %d",*ncmin);
printf("\n9: largest condition  =       %d",*ncmax);
printf("\n10: number of subconds=       %d",*nscmax);
printf("\n11:position of cond # =       %d",*cpos);
printf("\n12:position of item # =       %d",*ipos);
printf("\n13:position of #seg   =       %d",*npos);
printf("\n14:data start position=       %d",*dpos);
printf("\n15:cutoff start posit =       %d",*skiptime);
printf("\n16:raw=0 ms/ch=1 reg=2:		%d",*mschar);
printf("\n17:longcutoff         =       %d",*longtime);
printf("\n18:shorttime          =       %d",*shorttime);
printf("\n19, 20:  pos. of ques");
printf("\n21: cum = 0, avg = 1  :       %d",*avg);
printf("\n221: S X I file name   =       %s",allname);
printf("\n23: SUBS file name    =       %s",subname);
printf("\n24: ITEMS file name   =       %s",senname);
printf("\n25: SUBS count file   =       %s",nsubname);
printf("\n26: ITEMS countfile   =       %s   ? ",nsenname);


gets(buff);
if(strlen(buff) == 0)
	{
	fprintf(printer,"\n0: debug level        =       %d",*debug);
	fprintf(printer,"\n5: smallest item      =       %d",*nimin);
	fprintf(printer,"\n6: largest item       =       %d",*nimax);
	fprintf(printer,"\n7: maximum regions    =       %d",*nsegmax);
	fprintf(printer,"\n8: smallest condition =       %d",*ncmin);
	fprintf(printer,"\n9: largest condition  =       %d",*ncmax);
	fprintf(printer,"\n10: number of subconds=       %d",*nscmax);
	fprintf(printer,"\n11:position of cond # =       %d",*cpos);
	fprintf(printer,"\n12:position of item # =       %d",*ipos);
	fprintf(printer,"\n13:position of #seg   =       %d",*npos);
	fprintf(printer,"\n14:data start position=       %d",*dpos);
	fprintf(printer,"\n15:cutoff start posit =       %d",*skiptime);
	fprintf(printer,"\n16:r=0 ms/ch=1 reg=2  :       %d",*mschar);
	fprintf(printer,"\n17:longcutoff         =       %d",*longtime);
	fprintf(printer,"\n18:shorttime          =       %d",*shorttime);
	fprintf(printer,"\n  (next 2 zero unless rejecting on basis of questions)");
	fprintf(printer,"\n21: cumulate = 0, average = 1:  %d",*avg);
	fprintf(printer,"\n22: S X I file name   =    %s",allname);
	fprintf(printer,"\n23: SUBS file name    =    %s",subname);
	fprintf(printer,"\n24: ITEMS file name   =    %s",senname);
	fprintf(printer,"\n23: SUBS count file   =    %s",nsubname);
	fprintf(printer,"\n24: ITEMS count file  =    %s",nsenname);
	if(changes)
		return(-1);
	else
		return(0);
	}

changes = 1;
i = atoi(buff);
if(i < 22)
	{
	printf("\nNew value? ");
	j = atoi(gets(buff));
	}
else
	printf("\nNew name? ");
switch(i)
	{
	case 0:
		*debug = j;
		break;
	case 1:
	case 2:
	case 3:
	case 4:
		break;
	case 5:
		*nimin = j;
		*nitot = *nimax-*nimin+1;
		break;
	case 6:
		*nimax = j;
		*nitot = *nimax-*nimin+1;
		break;
	case 7:
		*nsegmax = j;
		break;
	case 8:
		*ncmin = j;
		*nctot = *ncmax-*ncmin+1;
		break;
	case 9:
		*ncmax = j;
		*nctot = *ncmax-*ncmin+1;
		break;
	case 10:
		*nscmax = j;
		break;
	case 11:
		*cpos = j;
		break;
	case 12:
		*ipos = j;
		break;
	case 13:
		*npos = j;
		break;
	case 14:
		*dpos = j;
		break;
	case 15:
		*skiptime = j;
		break;
	case 16:
		*mschar = j;
		break;
	case 17:
		*longtime = j;
		break;
	case 18:
		*shorttime = j;
		break;
	case 19:
	case 20:
		break;
	case 21:
		*avg = j;
		break;
	case 22:
		gets(allname);
		break;
	case 23:
		gets(subname);
		break;
	case 24:
		gets(senname);
		break;
	case 25:
		gets(nsubname);
		break;
	case 26:
		gets(nsenname);
		break;
	default:
		break;
	}

return(changes);	/* ok, normal return */
}

/************************************************************************/

void openfail(filestr)
char *filestr;
{
printf("\nCan't open file %s. ",filestr);
exit(1);
}


char *tgets(char *buff,FILE *stream)
	{
	int i;
	fgets(buff,MAXLINE,stream);
	for(i=0;buff[i] != '\n' && i < MAXLINE;i++)
		;
	buff[i] = '\0';
	return(buff);
	}

void get_regression(double *alpha, double *beta,int nimin,int nimax,int ncmin,int ncmax,int nscmax,int cpos,int ipos,int npos,int dpos,int longtime,int shorttime,int skiptime,int nitot)
{
FILE *corrbuff;
FILE *gdbuff;
int fpt,lpt,tix;
int endsen, cnt,sptrptr, cptrptr, icond, ns;
int phrlen,rt;
double dphrlen,drt,sumx,sumy,sumxy,sumx2,sumy2;
double varx, vary, numer,denom, corret;
int ci;
int temp;
double ftemp;
int qcond,qresp;
char qbuff[MAXLINEX];
char tempname[80];
int skippos;
int nchar,n;

strcpy(tempname,file);
for(ci=0;tempname[ci] != '.';ci++)
	;
tempname[++ci]='c';
tempname[++ci]='o';
tempname[++ci]='r';
if((corrbuff = fopen(tempname,"r")) != NULL)
	{
	fscanf(corrbuff,"%lf%lf%lf",alpha,beta,&corret,&ci);	/* .cor file has alpha, beta, correlation */
//printf("\nalpha %f beta %f corret %f nimin %d",*alpha,*beta,corret,nimin);
	fclose(corrbuff);					/* then min and max items and conds */
	return;
	}
n = 0;
sumx=sumy=sumxy=sumx2=sumy2=0.0;
if((corrbuff = fopen(tempname,"w")) == NULL)
	openfail(tempname);
if((gdbuff=fopen(file,"r")) EQ NULL)
	openfail(file);
while(fgets(buff,MAXLINEX,gdbuff) != NULL)	/* keep going until EOF */
	{
	cptrptr = atoi(buff+spacebuff(buff,cpos));
	sptrptr = atoi(buff+spacebuff(buff,ipos));
	ns = sptrptr-nimin;
	if(exceptflag)
		{
		if(vexcept[ns] == 99)
			cptrptr = ncmax+1;		/* discard item */
		else
			cptrptr += vexcept[ns];	/* adjust condition */
		}
	if(cptrptr < ncmin || cptrptr > ncmax || sptrptr > nimax || sptrptr<nimin)
		{
		;
		}
	else		/* OK condition number, get data position and data */
		{
		if(strlen(buff+spacebuff(buff,npos)) > 2)	/* data to work on */
			{
			endsen = atoi(buff+spacebuff(buff,npos));
			tbuff = buff+spacebuff(buff,dpos);
			skippos = skiptime-dpos;	/* how many times to skip in longtime filter? */
			for(icond=0;icond<nscmax;icond++)	/* find match to cond # */
				{
				ti1 = icond * nitot + ns;
				if(*(C+ti1) EQ cptrptr)
					break; 
				}
			if(icond EQ nscmax)
				{
				printf("\nSCREWUP! sen %d cond %d",sptrptr,cptrptr);
				exit(1);
				}
			if(debug EQ 2)
				printf("\n    NS = %d, adjusted cond = %d",ns,cptrptr);
			if(endsen != *(REGION_NUMBER+icond*nitot + ns))
				{
				fprintf(printer,"\nDiscarding Item %d Cond %d; had %d presentation regions, should be %d",sptrptr,cptrptr,endsen,*(REGION_NUMBER+icond*nitot + ns));
				}
			else
				{
				ti1 = icond*nitot + ns;
				*(ITEMCOND+ti1) = cptrptr; /* cond no for this subj,this sent */
				for(cnt=1,fpt=1,lpt=0;lpt != endsen+1 ;cnt++)  /* Kludge #1 */
					{
					ti2 = cnt*nitot*nscmax + icond*nitot + ns;
					if((lpt = *(C+ti2)) EQ 0)
						lpt = endsen+1;	/* the kludge is baited */
					for(;fpt < lpt;skippos--,fpt++)
						{
						ti3 = (fpt-1)*nitot*nscmax + icond*nitot + ns;
						if(debug == 2)
							printf("\n  region %d, REGION_LENGTH = %d",fpt,*(REGION_LENGTH+ti3));
						temp = atoi(tbuff);
						while(isspace(*tbuff))	/* move on to next number */
							tbuff++;
						while(!isspace(*tbuff))
							tbuff++;
						while(isspace(*tbuff))
							tbuff++;
						if((temp > shorttime) && (temp < longtime && skippos <= 0))
							{
							drt = (double)(temp);
							dphrlen = (double)(*(REGION_LENGTH + ti3));
							sumy += drt;
							sumx += dphrlen;
							sumxy += (drt * dphrlen);
							sumy2 += (pow(drt,2.));
							sumx2 += (pow(dphrlen,2.));
							n++;
							}
						}
					}
				}
			}
		}
	}
fclose(gdbuff);
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
corret = numer / denom;	
printf("\ncorr is equal to %f", corret);
*beta = numer / varx;
*alpha = sumy/n - *beta*(sumx/n);
printf("\nbeta = %f, alpha = %f", *beta, *alpha);
fprintf(corrbuff,"%f %f %f %d %d %d %d\n",*alpha,*beta,corret,nimin,nimax,ncmin,ncmax);
fclose(corrbuff);
}

