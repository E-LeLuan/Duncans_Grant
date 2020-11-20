/* kludged for rating data. Call response RT and multiply by 1000
to put on 1000-5000 scale. 
And force "response" (from RT column) to be 1 */

char version[] = "7/27/2008 for IXS file; old .crn won't work right";
/* to compile, using Mark Williams CC86
modified for Lattice c
cc a:mrt.c libm.olb -v -xtm */

/* modified to allow min and max conds */
/* allows exceptions file of form  item#  +/-condition#adjustment */
/* exceptions file applies to last condition when data are scored
by the previous condition number e.g. for questions */
/*  modified for PC using Turbo-C September 17, 1987 */
/* modified to permit pulling cond #s for questions from previous line */
/* using ncmin and ncmax for the limits on the current trial's condition number */
/* and ncminl and ncmaxl, the presumed limits for the previous trial, */
/* as the limits for actually storing and reporting data*/
/* bug in medians corrected 4/15/04 */
/* version to write ixs file in comma-delimited format
subject,item,condition,correct/error (1/0),RT (blank if missing), actual response index
(number in list of subcond/responses, 0 = first in list).
Works only if "means" not "medians" selected; gives RT only when response
is in range selected for mean RT */
/* July 2008, bug fix to allow exception file when condition scored as previous trial */


void main(void);
void sort(int nf,int median,int ccpl,int i);
void oops(char *string);
void writemeans(int ns,int truncelim);
void openfail(char *filestr);
int correct(int *cutoff,float *sigma,int *truncelim,char *sfname,char *msfname,
char *ifname,char *mifname,char *spname,char *mspname,char *ipname,
char *mipname,int *ccpl,int *icpl,int *median,char *ixsfname);
int filtsub(int cutoff,float sigma,int truncelim,char *filename,int spcon,int exceptflag);

#include "stdio.h"
#include "io.h"
#include "stdlib.h"
#include "math.h"
#include "ctype.h"
#include "alloc.h"
#include "dos.h"
#define ERR -1
#define MAXLINE 512
#define MAXLINEX 4000
#define	EQ	== 

/* externals */
FILE *printer,*subfile,*msubfile,*psubfile,*pmsubfile,*ixsfile;
char *tgets();
int order();
struct trial
	{
	int cond;
	int lcond;
	int item;
	int rt;
	int subclass;
	};

struct trial trials[200];
char file[MAXLINE];
char buff[MAXLINEX];
int *nmat,*nrejtot,*nsum,*nrejsum,*inmat,*insum,*cntsum,*corrsum,*icntsum,*icorrsum;
long int *totmat,*totsum,*itotmat,*itotsum;
int *items_array;	/* array of items x conds x trials */
int *items_count_array;	/* array of counts of items x conds */
int *mdnrts;
int ncmax,ncmin;
int nimax,nimin;
int ncmaxl,ncminl;
int last_cond;
int nc,ni,nsc,nsca,corrsc,cval,ival,rtval,scval;
int sc[20];
char cstring[80];
int DEBUG;
int vexcept[400];
int lowcutoff;

/* main program */

void main()
{
char sfname[20],ifname[20],msfname[20],mifname[20],ixsfname[20]; /* names for RT files */
char spname[20],ipname[20],mspname[20],mipname[20]; /* names for prop files */
char sfile[MAXLINE];
FILE *control;
FILE *efp;
char temp[80];
struct date today;
int i,ns,cutoff,median,n,truncelim,ccpl,icpl;
float sigma;
int spcon;
int exceptflag,ei,ej;
int c;
int printer_control;
char tch[20];

printf("\n\nMRT VERSION %s",version);
printf("\nType debug level: 0, 1, 2, or 3: ");
DEBUG = atoi(fgets(buff,MAXLINE,stdin));
printf("\nDo you want hard copy? y or n: ");
gets(tch);
if(tolower(tch[0])=='y')
	{    
	if ((printer = fopen("prn","w")) == NULL)
		openfail("prn");
	}
else
	{
	printf("What is output trace file name? ");
	while((printer=fopen(gets(cstring),"w")) EQ NULL)
		printf("\nCAN'T OPEN FILE, TRY AGAIN OR QUIT AND CHECK DISK SPACE\n");
	printf("Writing output on file %s",cstring);
	}
fprintf(printer,"\nMRT Version %s\n",version);
printf("\nType identifying string.");
fprintf(printer,"%s",gets(temp));
getdate(&today);
fprintf(printer,"\nDATE: %d/%d/%d\n",today.da_mon,today.da_day,today.da_year);
printf("Type name of file containing control info (CR if none). ");
tgets(buff,stdin);
if(strlen(buff) != 0)
	{
	if((control = fopen(buff,"r")) == NULL)
		openfail(buff);
	fprintf(printer,"Old CRN file %s",buff);
	fscanf(control,"%d%d%d%d%d%d",&ncmin,&ncmax,&ni,&nsc,&nsca,&corrsc);
	for(i=0;i<nsc;i++)
		fscanf(control,"%d",sc+i);	/* getting subclass vals for cols */
	fscanf(control,"%d%d%d",&nimin,&nimax,&last_cond);
	fscanf(control,"%d%d",&ncminl,&ncmaxl);
	fscanf(control,"%d%d%d%d",&cval,&ival,&rtval,&scval);
	fscanf(control,"%d%f%d%d",&cutoff,&sigma,&truncelim,&lowcutoff);
	fscanf(control,"%s%s%s%s",sfname,ifname,msfname,mifname);
	fscanf(control,"%s%s%s%s",spname,ipname,mspname,mipname);
	fscanf(control,"%d%d%d%s%s",&ccpl,&icpl,&median,ixsfname);
	fclose(control);
	nc = ncmaxl-ncminl+1;			/* addition from CPM */
	}else
	{
	printf("Smallest condition? ");
	ncmin = atoi(fgets(buff,MAXLINE,stdin));
	printf("Largest condition? ");
	ncmax = atoi(fgets(buff,MAXLINE,stdin));
	printf("Smallest item? ");
	nimin = atoi(fgets(buff,MAXLINE,stdin));
	printf("Largest item? ");
	nimax = atoi(fgets(buff,MAXLINE,stdin));
	printf("How many items (per condition block, if desired)? ");
	ni = atoi(fgets(buff,MAXLINE,stdin));
	printf("Do you want to classify data under the condition number of the previous trial\n(i.e., classify question answers under the cond. of their sentence? y or n: ");
	gets(buff);
	last_cond = (tolower(buff[0]) == 'y') ? 1 : 0;
	if(last_cond)
		{
		printf("\nSmallest condition number for previous trial? ");
		ncminl = atoi(fgets(buff,MAXLINE,stdin));
		printf("\nLargest condition number for previous trial? ");
		ncmaxl = atoi(fgets(buff,MAXLINE,stdin));
		}
	else
		{
		ncminl = ncmin;
		ncmaxl = ncmax;
		}
	nc = ncmaxl-ncminl+1;			/* addition from CPM */
	printf("Which position in data indicates condition #? ");
	cval = atoi(fgets(buff,MAXLINE,stdin));
	printf("Which position in data indicates item #? ");
	ival = atoi(fgets(buff,MAXLINE,stdin));
	printf("Which position in data indicates reaction time? ");
	rtval = atoi(fgets(buff,MAXLINE,stdin));
	printf("Which position in data indicates response value? ");
	scval = atoi(fgets(buff,MAXLINE,stdin));
	printf("How many different response values were recorded in the experiment? ");
	nsc = atoi(fgets(buff,MAXLINE,stdin));
	printf("Enter the values that were recorded. Begin with the value or values\nthat you want to consider as 'correct' and average RT over. (e.g. if you\nrecorded the value 1 for correct responses and 2 for errors enter 1 then 2.)\n");
	for(i=0;i<nsc;i++)
		{
		printf("   What is the value of response number %d? ",i+1);
		sc[i] = atoi(fgets(buff,MAXLINE,stdin));
		}
	printf("MRT will average RTs over the first one or more responses.\n   How many response RTs do you want to average over? ");
	nsca = atoi(fgets(buff,MAXLINE,stdin));
	printf("MRT will treat the first one or more responses as 'correct' responses.\n   How many do you want to treat as correct? ");
	corrsc = atoi(fgets(buff,MAXLINE,stdin));
	printf("What is absolute cutoff in msec? ");
	cutoff = atoi(fgets(buff,MAXLINE,stdin));
	printf("What is relative cutoff in S.D. units? 0 if not used");
	sigma = atof(fgets(buff,MAXLINE,stdin));
	printf("Replace or eliminate long RTs? 0 or 1, respectively: ");
	truncelim = atoi(fgets(buff,MAXLINE,stdin));
	printf("What is lower cutoff in msec? ");
	lowcutoff = atoi(fgets(buff,MAXLINE,stdin));
	printf("Do you want means (= 0) or medians (= 1)? ");
	median = atoi(fgets(buff,MAXLINE,stdin));
	printf("Type in the common portion of the data files' name, up to 5 characters.");
	printf("\nUse this to indicate the experiment you are analyzing.");
	printf("\nThe computer will add information about RT vs PR and SUB vs ITM.");
	do
		{
		printf("\n    Five characters maximum: ");
		tgets(buff,stdin);
		}
	while(strlen(buff) > 5);
	
	strcpy(sfname,buff);
	strcat(sfname,"RT.SUB");
	strcpy(msfname,buff);
	strcat(msfname,"RTX.SUB");
	strcpy(ifname,buff);
	strcat(ifname,"RT.ITM");
	strcpy(mifname,buff);
	strcat(mifname,"RTX.ITM");
	strcpy(spname,buff);
	strcat(spname,"PR.SUB");
	strcpy(mspname,buff);
	strcat(mspname,"PRX.SUB");
	strcpy(ipname,buff);
	strcat(ipname,"PR.ITM");
	strcpy(mipname,buff);
	strcat(mipname,"PRX.ITM");
	strcpy(ixsfname,buff);
	strcat(ixsfname,"IXS.TXT");


	printf("How many conditions go on one subject-by-subject output file line? ");
	ccpl = atoi(fgets(buff,MAXLINE,stdin));
	printf("How many conditions go on one item-by-item output file line? ");
	icpl = atoi(fgets(buff,MAXLINE,stdin));
	}

while(correct(&cutoff,&sigma,&truncelim,sfname,msfname,ifname,mifname,spname,mspname,ipname,mipname,&ccpl,&icpl,&median,ixsfname))	/* allow changes */
	;

printf("\nWhat file name do you want to save these values as (CR if none)? ");
tgets(file,stdin);
if(strlen(file) != 0)
	{
	if ((control = fopen(file,"w")) == NULL)
		openfail(file);
	fprintf(printer,"CRN file name %s",file);
	fprintf(control,"%d %d %d %d %d %d\n",ncmin,ncmax,ni,nsc,nsca,corrsc);
	for(i=0;i<nsc;i++)
		fprintf(control," %d",sc[i]);	/* getting subclass vals for cols */
	fprintf(control,"\n%d %d %d",nimin,nimax,last_cond);
	fprintf(control,"\n%d %d",ncminl,ncmaxl);
	fprintf(control,"\n%d %d %d %d",cval,ival,rtval,scval);
	fprintf(control,"\n%d %f %d %d",cutoff,sigma,truncelim,lowcutoff);
	fprintf(control,"\n%s %s %s %s",sfname,ifname,msfname,mifname);
	fprintf(control,"\n%s %s %s %s",spname,ipname,mspname,mipname);
	fprintf(control,"\n%d %d %d %s",ccpl,icpl,median,ixsfname);
	fclose(control);
	}
nc = ncmaxl-ncminl+1;
printf("\n%d conditions: from %d to %d (labeled as %d to %d)",nc,ncmin,ncmax,ncminl,ncmaxl);
if ((subfile = fopen(sfname,"w")) == NULL)
	openfail(sfname);
if ((msubfile = fopen(msfname,"w")) == NULL)
	openfail(sfname);
if ((psubfile = fopen(spname,"w")) == NULL)
	openfail(spname);
if ((pmsubfile = fopen(mspname,"w")) == NULL)
	openfail(mspname);
if((ixsfile = fopen(ixsfname,"w")) == NULL)
	openfail(ixsfname);


printf("\nFiles opened, now allocating space for arrays.");

if((totmat =(long int *)calloc(nc*nsc,4)) == NULL)
	oops("totmat");
else
	printf("\ntotmat = %p",totmat);
if((nmat = (int *)calloc(nc*nsc,2)) == NULL)
	oops("nmat");
else
	printf("\nnmat = %p",nmat);
if((nrejtot = calloc(nc,2)) == NULL)
	oops("nrejtot");
else
	printf("\nnrejtot = %x",nrejtot);
if((totsum = (long int *)calloc(nc,4)) == NULL)
	oops("totsum");
else
	printf("\ntotsum = %p",totsum);
if((nsum = (int *)calloc(nc,2)) == NULL)
	oops("nsum");
else
	printf("\nnsum = %p",nsum);
if((nrejsum = (int *)calloc(nc*ni,2)) == NULL)
	oops("nrejsum");
else
	printf("\nnrejsum = %p",nrejsum);
if((itotmat = (long int *)calloc(ni*nc*nsc,4)) == NULL)
	oops("itotmat");
else
	printf("\nitotmat = %p",itotmat);
if((inmat = (int *)calloc(ni*nc*nsc,2)) == NULL)
	oops("inmat");
else
	printf("\ninmat = %p",inmat);
if((itotsum = (long int *)calloc(ni*nc,4)) == NULL)
	oops("itotsum");
else
	printf("\nitotsum = %p",itotsum);
if((insum = (int *)calloc(ni*nc,2)) == NULL)
	oops("insum");
else
	printf("\ninsum = %p",insum);
if((cntsum = (int *)calloc(nc,2)) == NULL)
	oops("cntsum");
else
	printf("\ncntsum = %p",cntsum);
if((corrsum = (int *)calloc(nc,2)) == NULL)
	oops("corrsum");
else
	printf("\ncorrsum = %p",corrsum);
if((icntsum = (int *)calloc(ni*nc,2)) == NULL)
	oops("icntsum");
else
	printf("\nicntsum = %p",icntsum);
if((icorrsum = (int *)calloc(ni*nc,2)) == NULL)
	oops("icorrsum");
else
	printf("\nicorrsum = %p",icorrsum);
if(median)
	{
	if((mdnrts = (int *)calloc(600,2)) == NULL)
		oops("mdnrts");
	if((items_array = (int *)calloc(ni*nc*200,2)) == NULL)
		oops("items_array");
	if((items_count_array = (int *)calloc(ni*nc,2)) == NULL)
		oops("items_array");
	}


printf("\nName of file containing data file names? ");
while((control = fopen(gets(file),"r")) == NULL)
	printf("\nBad file name, try again ");
ns = 0;
while((c = fgetc(control)) != EOF)	/* count the subjects */
	{
	printf("%c",c);
	if(c == '\n')
		ns++;
	}

printf("\n\n%d subjects",ns);
printf("\nDo you want printer output of subject means and bad RTs? y or n ");
fgets(buff,MAXLINE,stdin);
spcon = (tolower(buff[0])) == 'y' ? 1 : 0;

printf("\nIs there an exceptions file (format = item# +-condition#) - y or n?");
gets(file);
if(tolower(file[0]) == 'y')
	{
	exceptflag = 1;
	printf("Exceptions file name: ");
	while((efp = fopen(gets(file),"r")) == NULL)
		printf("\nBAD FILE NAME, try again ");
	fprintf(printer,"\nEXCEPTIONS FILE: %s",file);
	while(fscanf(efp,"%d %d",&ei,&ej) != EOF)
		{
		fprintf(printer,"\nItem %d condition adjustment %d",ei,ej);
		vexcept[ei-1] = ej;	/* vector of condition adjustment values */
		}
	}
else
	exceptflag = 0;

rewind(control);
fprintf(ixsfile,"Subj,Item,Cond,Corr,RT,Resp\n");
for(i=0;i<ns;i++)
	{
	bdos(11,0,0);		/* check for operator interrupt */
	fscanf(control,"%s",sfile);
	n = filtsub(cutoff,sigma,truncelim,sfile,spcon,exceptflag);
	if(n != ERR)
		sort(n,median,ccpl,i);
	else
		printf("\nERROR on subject %d, %s",ns+1,sfile);
	}
end:
if (fclose(subfile) == EOF)
	closefail(subfile);
if (fclose(msubfile) == EOF)
	closefail(msubfile);
if (fclose(psubfile) == EOF)
	closefail(psubfile);
if (fclose(pmsubfile) == EOF)
	closefail(pmsubfile);
if(fclose(ixsfile) == EOF)
	closefail(ixsfile);
fclose(control);

if ((subfile = fopen(sfname,"r")) == NULL)
	openfail(sfname);
if ((msubfile = fopen(msfname,"r")) == NULL)
	openfail(sfname);
if ((psubfile = fopen(spname,"r")) == NULL)
	openfail(spname);
if ((pmsubfile = fopen(mspname,"r")) == NULL)
	openfail(mspname);

printf("\nGOING TO WRITEMEANS");
writemeans(ns,truncelim);

writeitem(ifname,mifname,ipname,mipname,icpl,median);

fclose(printer);
}

/* routine to allow correction of parameter values */
/* revised to include lowcutoff */

int correct(int *cutoff,float *sigma,int *truncelim,char *sfname,char *msfname,
char *ifname,char *mifname,char *spname,char *mspname,char *ipname,
char *mipname,int *ccpl,int *icpl,int *median, char *ixsfname)
{
int i,j,k;
float fj;
printf("\n Type a number to change a control parameter, or CR to quit.");
printf("\n0: min condition =        %4d",ncmin);
printf("              1: max condition =       %4d",ncmax);
printf("\n2: min item =             %4d",nimin);
printf("              3: max item =            %4d",nimax);
printf("\n4: last_cond =            %4d",last_cond);
printf("\n5: items(/cond block)=    %4d",ni);
printf("\n6: # responses	 =   %4d",nsc);
printf("\n7: responses to sum =      %4d",nsca);
printf("              8: 'correct' responses =  %4d",corrsc);
printf("\n9: value of responses = ");
for(i=0;i<nsc;i++)
	printf(" %d",sc[i]);
printf("\n10: position of condition %4d",cval);
printf("             11: position of item      %4d",ival);
printf("\n12: position of RT        %4d",rtval);
printf("             13: position of response  %4d",scval);
printf("\n14: cutoff               %5d",*cutoff);
printf("             15: sigma                 %4f",*sigma);
printf("\n16: trunc = 0, elim = 1   %4d",*truncelim);
printf("\n17: lowcutoff             %4d",lowcutoff);
printf("\n18: File: RT sum,subj    %12s",sfname);
printf("     19: Entries per line = %4d",*ccpl);
printf("\n20: File: RT matrix,subj %12s",msfname);
printf("\n21: File: RT sum,item    %12s",ifname);
printf("     22: Entries per line = %4d",*icpl);
printf("\n23: File: RT matrix,item %12s",mifname);
printf("\n24: File: pr sum,subj    %12s",spname);
printf("\n25: File: pr matrix,subj %12s",mspname);
printf("\n26: File: pr sum,item    %12s",ipname);
printf("\n27: File: pr matrix,item %12s",mipname);
if(last_cond)
	{
	printf("\n28: Min cond of previous item: %d",ncminl);
	printf("  29: Max cond of previous item: %d",ncmaxl);
	}
printf("\n30: Mean (0) or Median (1): %d",*median); 
printf("\n31: File: IXS            %12s  TYPE NUMBER TO CHANGE (ENTER if none): ",ixsfname);

gets(buff);
if(strlen(buff) == 0)
	{
	if(!last_cond)
		{
		ncminl = ncmin;
		ncmaxl = ncmax;
		}
	fprintf(printer,"\nmin condition =       %4d",ncmin);
	fprintf(printer,"\nmax condition =       %4d",ncmax);
	fprintf(printer,"\nmin item =            %4d",nimin);
	fprintf(printer,"\nmax item =            %4d",nimax);
	fprintf(printer,"\nlast_cond =           %4d",last_cond);
	if(last_cond)
		{
		fprintf(printer,"\nmin cond last item=%4d",ncminl);
		fprintf(printer,"\nmax cond last item=%4d",ncmaxl);
		}
	fprintf(printer,"\nitems(/cond block) =  %4d",ni);
	fprintf(printer,"\nresponses =  %4d",nsc);
	fprintf(printer,"\nresponses to sum =     %4d",nsca);
	fprintf(printer,"\n'correct' responses =  %4d",corrsc);
	fprintf(printer,"\nvalue of responses = ");
	for(i=0;i<nsc;i++)
		fprintf(printer," %d",sc[i]);
	fprintf(printer,"\nposition of condition %4d",cval);
	fprintf(printer,"\nposition of item      %4d",ival);
	fprintf(printer,"\nposition of RT        %4d",rtval);
	fprintf(printer,"\nposition of response  %4d",scval);
	fprintf(printer,"\ncutoff               %5d",*cutoff);
	fprintf(printer,"\nsigma                 %4f",*sigma);
	fprintf(printer,"\ntrunc = 0, elim = 1   %4d",*truncelim);
	fprintf(printer,"\nlowcutoff =           %4d",lowcutoff);
	fprintf(printer,"\nmean = 0,median = 1   %4d",*median);
	fprintf(printer,"\nFile: RT sum,subj     %s",sfname);
	fprintf(printer,"\nEntries per line      %4d",*ccpl);
	fprintf(printer,"\nFile: RT matrix,subj  %s",msfname);
	fprintf(printer,"\nFile: RT sum,item     %s",ifname);
	fprintf(printer,"\nEntries per line      %4d",*icpl);
	fprintf(printer,"\nFile: RT matrix,item  %s",mifname);
	fprintf(printer,"\nFile: pr sum,subj     %s",spname);
	fprintf(printer,"\nFile: pr matrix,subj  %s",mspname);
	fprintf(printer,"\nFile: pr sum,item     %s",ipname);
	fprintf(printer,"\nFile: pr matrix,item  %s",mipname);
	fprintf(printer,"\nFile: ixs             %s",ixsfname);
	fprintf(printer,"\n\n\n\n");
	return(NULL);
	}
i = atoi(buff);
if(i == 15)	/* sigma, float */
	{
	printf("\nNew value? ");
	fj = atof(gets(buff));
	*sigma = fj;
	}
else if((i >= 0 && i < 9) || (i > 9&&i < 18) || i==19 || i==22 || i==28 || i==29  || i==30)
	{
	printf("\nNew value? ");
	j = atoi(gets(buff));
	switch(i)
		{
		case 0:
			ncmin = j;
			break;
		case 1:
			ncmax = j;
			nc = ncmax - ncmin + 1;
			break;
		case 2:
			nimin = j;
			break;
		case 3:
			nimax = j;
			break;
		case 4:
			last_cond = j;
		case 5:
			ni = j;
			break;
		case 6:
			nsc = j;
			break;
		case 7:
			nsca = j;
			break;
		case 8:
			corrsc = j;
			break;
		case 10:
			cval = j;
			break;
		case 11:
			ival = j;
			break;
		case 12:
			rtval = j;
			break;
		case 13:
			scval = j;
			break;
		case 14:
			*cutoff = j;
			break;
		case 16:
			*truncelim = j;
			break;
		case 17:
			lowcutoff = j;
			break;
		case 19:
			*ccpl = j;
			break;
		case 22:
			*icpl = j;
			break;
		case 28:
			ncminl = j;
			break;
		case 29:
			ncmaxl = j;
			break;
		case 30:
			*median = j;
			break;
		}
	}
else if (i == 9)
	{
	for (k = 0; k < nsc; k++)
		{
		printf("value of subclassification column %d? ",k+1);
		sc[k] = atoi((gets(buff)));
		}
	}
else if ((i > 17 && i < 28) || i == 31)
	{
	printf("File name? ");
	switch(i)
		{
		case 18:
			gets(sfname);
			break;
		case 20:
			gets(msfname);
			break;
		case 21:
			gets(ifname);
			break;
		case 23:
			gets(mifname);
			break;
		case 24:
			gets(spname);
			break;
		case 25:
			gets(mspname);
			break;
		case 26:
			gets(ipname);
			break;
		case 27:
			gets(mipname);
			break;
		case 31:
			gets(ixsfname);
			break;
		}
	}
return(1);	/* ok, normal return */
}

/************************************************************************/
void oops(char *string)
{
printf("\nOut of allocation space at %s.",string);
exit(1);
}



/*************************************************************************/
/* read in subject's data, throw away outliers,
leave in trial structs */


int filtsub(int cutoff,float sigma,int truncelim,char *filename,int spcon,int exceptflag)
{
FILE *subbuff;
int i,j,tsc,nf;
double rts,rtsq,nrt;
extern double sqrt();
int last_cond_value = ncminl;

/*printf("\XXX!!! filename %s.",filename);*/

if((subbuff=(fopen(filename,"r"))) == NULL)
	{
	fprintf(printer,"\n!!!!!!!  Can't open subject file %s.!!!!!!!",filename);
	return(ERR);
	}
fprintf(printer,"\nFile %s.",filename);
/* printf("\nXXX File %s.",filename);*/

/* get data into trials struct */

nf = 0;
while(fgets(buff,MAXLINEX,subbuff) != NULL && strlen(buff) > 1)
	{
	trials[nf].cond = atoi(buff+spacebuff(cval));
	trials[nf].item = atoi(buff+spacebuff(ival));
	if(exceptflag && (last_cond == 0))	/* exceptions flag set */
		{
		if(vexcept[(trials[nf].item)-1] == 99)
			trials[nf].cond = 999;			/* eliminate item */
		else
			trials[nf].cond += vexcept[(trials[nf].item) -1];	/* correction */
		}

/* kludge for accuracy */
	tsc = 1;
	trials[nf].subclass = 0;


/*	tsc = atoi(buff+spacebuff(scval));
	trials[nf].subclass = nsc;

	if(tsc < 0)
		trials[nf].subclass = nsc;
	else
		{
		for(j=0;j<nsc;j++)
			if(sc[j]==tsc)
				{
				trials[nf].subclass = j;
				break;
				}
		} */

	if(trials[nf].cond >= ncmin && trials[nf].cond <= ncmax && trials[nf].subclass < nsc && trials[nf].item >= nimin && trials[nf].item <= nimax)
		{
		trials[nf].lcond = last_cond_value;	/* copy from previous trial */
		trials[nf].rt = atoi(buff+spacebuff(rtval));
/* kludge for accuracy - treat response as "RT" and get means * 1000 */
		trials[nf].rt *= 1000;
		if (DEBUG)
		printf("\nGot legal R: nc %d nlc %d ni %d sc %d rt %d",trials[nf].cond,trials[nf].lcond,trials[nf].item,trials[nf].subclass,trials[nf].rt);
		nf++;
		}
	else if(last_cond && trials[nf].cond >= ncminl && trials[nf].cond <= ncmaxl)
		{
		if(exceptflag)
			{
			if(vexcept[(trials[nf].item)-1] == 99)
				last_cond_value = 999;			/* eliminate item */
			else
				last_cond_value = trials[nf].cond + vexcept[(trials[nf].item) -1];	/* correction */
			}
		else
			last_cond_value = trials[nf].cond;		/* save it for the next trial */
		}
	else
		last_cond_value=999;
	}

if(DEBUG)
{
	printf("\nDone reading in data file %s; press key to continue",filename);
getch();
}

/* filter out data */
rts = (rtsq = (nrt = 0));	/* initialize */

for (i=0;i<nf;i++)
	{
	if(trials[i].rt < lowcutoff)
		{
		if(spcon)
			fprintf(printer,"\n lowcutoff %d %d %d %d %d",trials[i].cond,trials[i].lcond,trials[i].subclass,trials[i].item,trials[i].rt);
		}
	if(trials[i].rt >= cutoff)
		{
		if(spcon)
			fprintf(printer,"\n cutoff %d %d %d %d %d",trials[i].cond,trials[i].lcond,trials[i].subclass,trials[i].item,trials[i].rt);
		if(truncelim)
			trials[i].rt=0;	/* truncelim = 1, eliminate */
		else
			trials[i].rt=cutoff;	/* truncelim = 0, truncate */
		(*(nrejsum + trials[i].cond - ncmin + (trials[i].item-nimin)*nc))++;
		}
	 if(trials[i].subclass < nsca && trials[i].rt != 0 && trials[i].item >= nimin && trials[i].item <= nimax)
						/* counting from zero */
						/* only paying attention to items being summated */
		{
		rts = rts + (double)(trials[i].rt);
		rtsq = rtsq + (double)(trials[i].rt) * (double)(trials[i].rt);
		nrt += 1;
		if(DEBUG)
			printf("\nSummating for SD: trial %d, total %d",i+1,(int)nrt);
		}
	else
		{
		if(DEBUG)
			{
			printf("\nDidn't summate trial %d item %d subclass %d RT %d",i+1,trials[i].item,trials[i].subclass,trials[i].rt);
			printf("\n nimin %d nimax %d nsca %d",nimin,nimax,nsca);
			}
		}
	}
/* calculate variance and CI times variance, as rts */
if(nrt != 0.0)
	{
	rts /= nrt;
	rtsq = (rtsq/nrt - rts*rts);
	rtsq = sqrt(rtsq);
	if(spcon)
		fprintf(printer,"\n   Mean %g S.D. %g",rts,rtsq);
	rts = rts + (double)(sigma)*rtsq;
	}
else
	{
	rts = 0;
	fprintf(printer,"\n!!! NO RTs SUBJECT %s",filename);
	}
	
for(i=0;i<nf;i++)
	if(sigma != 0 && trials[i].rt > (int)(rts) && trials[i].subclass <= nsc && trials[i].rt != cutoff)
		{
		(*(nrejsum + trials[i].cond - ncmin + (trials[i].item-nimin)*nc))++;
		if(spcon)
			fprintf(printer,"\n sigmacutoff %d %d %d %d",trials[i].cond,trials[i].subclass,trials[i].item,trials[i].rt);
		if(truncelim)
			trials[i].rt = 0;
		else
			trials[i].rt = (int)(rts);
		}
fclose(subbuff);
if(DEBUG)
	{
	printf("\nDone with data file %s, press key to continue",filename);
	getch();
	}
return(nf);
}


/***************************************************************************/
/* get specified datum from buff */
spacebuff(val)
int val;
{
int i,j;
val--;
j=0;

if(val EQ 0)
	{
	while(isspace(buff[j]))
		j++;
	return(j);
	}

for(i=0;i<val;i++)
	{
	while(isspace(buff[j]))
		j++;	/* move to first/next nonspace */
	while(!isspace(buff[j]))
		j++;	/* then move to next space */
	while(isspace(buff[j]))
		j++;	/* and finally on to next nonspace */
	}
	j--;
return(j);
}



/*********************************************************************/
/* sort the filtered data into the allocated regions */


void sort(int nf,int median,int ccpl,int subnum)
{
int i,j,k,mptr,cptr,ip;
int tnc,tmax,tmin,tcond;
int imptr,icptr,jccpl;
int adj_item;
int total_corrsum,total_count;
k = nc*nsc;
total_corrsum = total_count = 0;
for(i=0;i<k;i++)
	{
	*(totmat+i) = 0l;
	*(nmat+i) = 0;
	}
for(i=0;i<nc;i++)
	{
	*(totsum+i) = 0l;
	*(nsum+i) = 0;
	*(cntsum+i) = 0;
	*(corrsum+i) = 0;
	}
if(!median)
{
for(i=0;i<nf;i++)
	{
	if((last_cond && trials[i].lcond >= 99) || !last_cond && trials[i].cond >= 99)
		;
	else
	{
	if(last_cond)
		{
	 	mptr = (trials[i].lcond - ncminl) * nsc + trials[i].subclass;
		cptr = trials[i].lcond - ncminl;
		if(cptr < 0 | cptr >= nc)
			fprintf(printer,"\nCond (%d) outside range of ncminl (%d) - ncmaxl (%d), trial %d",trials[i].lcond,ncminl,ncmaxl,i+1);
		}
	else
		{
		mptr = (trials[i].cond - ncmin) * nsc + trials[i].subclass;
		cptr = trials[i].cond - ncmin;
		}
	adj_item = ((trials[i].item - nimin)%ni);
	imptr = (adj_item) * nc * nsc + mptr;
	icptr = (adj_item) * nc + cptr;
	*(cntsum+cptr) += 1;
	*(icntsum+icptr) += 1;
	total_count += 1;
	if(last_cond)
		fprintf(ixsfile,"%d,%d,%d,",subnum+1,trials[i].item,trials[i].lcond);
	else
		fprintf(ixsfile,"%d,%d,%d,",subnum+1,trials[i].item,trials[i].cond);
	if(trials[i].subclass < corrsc)
		{
		*(corrsum+cptr) += 1;
		*(icorrsum+icptr) += 1;
		total_corrsum += 1;
		fprintf(ixsfile,"1,");
		}
	else
		fprintf(ixsfile,"0,");	
	if(trials[i].rt > lowcutoff)			/* long excluded trials set to 0 */
		{
		*(totmat+mptr) += (long)(trials[i].rt);
		*(nmat+mptr) += 1;
		*(itotmat + imptr) += (long)(trials[i].rt);
		*(inmat + imptr) += 1;
		if(trials[i].subclass < nsca)
			{
			*(totsum + cptr) += (long)(trials[i].rt);
			*(nsum + cptr) += 1;
			*(itotsum + icptr) += (long)(trials[i].rt);
			*(insum + icptr) += 1;
			fprintf(ixsfile,"%d,",trials[i].rt);
			}
		if (DEBUG == 2)
		{
		printf("\nXXXSort, sn %d in %d totmat %ld itotmat %ld",*(nmat+mptr),*(inmat+imptr),*(totmat+mptr),*(itotmat+imptr));
		printf("\nXXX   snsum %d insum %d totsum %ld itotsum %ld ",*(nsum+cptr),*(insum+icptr),*(totsum+cptr),*(itotsum+icptr));
		printf("\nXXX  addresses: totsum+cptr, %x, nsum+cptr, %x",&(*(totsum+cptr)),&(*(nsum+cptr)));
		printf("\nXXX  imptr %d icptr %d mptr %d cptr %d",imptr,icptr,mptr,cptr);
		}
		}
	else
		{
		fprintf(ixsfile,",");
		if(DEBUG == 2)
			printf("\nXXXSort fastresp, imptr %d icptr %d mptr %d cptr %d",imptr,icptr,mptr,cptr);
		}
	fprintf(ixsfile,"%d\n",trials[i].subclass);
	}
	}
	for(i=0,jccpl=1;i<nc;i++,jccpl++)
		{
		cptr = i;
		if(*(nsum+cptr) != 0)
			fprintf(subfile,"%6ld",*(totsum+cptr)/(long)(*(nsum+cptr)));
		else
			fprintf(subfile,"%6d",0);
		if(*(cntsum+cptr) != 0)
			{
			fprintf(psubfile,"%6.3f",(double)(*(corrsum+cptr))/(double)(*(cntsum+cptr)));
			}
		else
			{
			fprintf(psubfile,"%6.3f",0.0);
			}
		for(j=0;j<nsc;j++)
			{
			mptr = (i * nsc) + j;
			if(*(nmat+mptr) != 0)
				fprintf(msubfile,"%6ld",*(totmat+mptr)/(long)(*(nmat+mptr)));
			else
				fprintf(msubfile,"%6d",0);
			fprintf(pmsubfile,"%6d",*(nmat+mptr));
			}
		fprintf(msubfile,"\n");
		fprintf(pmsubfile,"\n");
		if(jccpl == ccpl || i == nc-1)
			{
			jccpl = 0;
			fprintf(subfile,"\n");
			fprintf(psubfile,"\n");
			}
		}
		
	}
else	/* medians */
	{
	if(last_cond)
		{
		tmin = ncminl;
		tmax = ncmaxl;
		}
	else
		{
		tmin = ncmin;
		tmax = ncmax;
		}
	for(tnc=tmin;tnc<=tmax;tnc++)
		{
		for(i=0,ip=0;i<nf;i++)		
			{
			if(last_cond)
				tcond = trials[i].lcond;
			else
				tcond = trials[i].cond;
			if(tcond == tnc)
				{
if(DEBUG)
	printf("\nMDN match cond = %d trial = %d observation %d",tnc,i,ip);
				if(last_cond)
					{
				 	mptr = (trials[i].lcond - ncminl) * nsc + trials[i].subclass;
					cptr = trials[i].lcond - ncminl;
					if(cptr < 0 | cptr >= nc)
						{
						printf("\nOOPS: Cond (%d) outside range of ncminl (%d) - ncmaxl (%d), trial %d",trials[i].lcond,ncminl,ncmaxl,i+1);
						exit(1);
						}
					}
				else
					{
					mptr = (trials[i].cond - ncmin) * nsc + trials[i].subclass;
					cptr = trials[i].cond - ncmin;
					}
				adj_item = ((trials[i].item - nimin)%ni);
				imptr = (adj_item) * nc * nsc + mptr;
				icptr = (adj_item) * nc + cptr;
				*(cntsum+cptr) += 1;
				*(icntsum+icptr) += 1;
				total_count += 1;
				if(trials[i].subclass < corrsc)
					{
					*(corrsum+cptr) += 1;
					*(icorrsum+icptr) += 1;
					total_corrsum += 1;
					}
				if(trials[i].rt > lowcutoff)
					{
					*(nmat+mptr) += 1;
					*(inmat + imptr) += 1;
					if(trials[i].subclass < nsca)
						{
						mdnrts[ip++] = trials[i].rt;
						*(nsum + cptr) += 1;
						*(insum + icptr) += 1;
						/* store data for item */
						*(items_array+(icptr*200 + *(items_count_array+icptr))) = trials[i].rt;
						(*(items_count_array + icptr))++;
						}
					if (DEBUG == 2)
						{
						printf("\nXXXSort, sn %d in %d totmat %ld itotmat %ld",*(nmat+mptr),*(inmat+imptr),*(totmat+mptr),*(itotmat+imptr));
						printf("\nXXX   snsum %d insum %d totsum %ld itotsum %ld ",*(nsum+cptr),*(insum+icptr),*(totsum+cptr),*(itotsum+icptr));
						printf("\nXXX  addresses: totsum+cptr, %x, nsum+cptr, %x",&(*(totsum+cptr)),&(*(nsum+cptr)));
						printf("\nXXX  imptr %d icptr %d mptr %d cptr %d",imptr,icptr,mptr,cptr);
						}
					}
				else
					{
					if(DEBUG == 2)
						printf("\nXXXSort fastresp, imptr %d icptr %d mptr %d cptr %d",imptr,icptr,mptr,cptr);
					}
				}		
			}			/* end of this condition */
			if(ip != 0)	/* get the median, add it in */
			{
			if(DEBUG)
				{
				printf("\nMDN CALCULATION, %d scores, cptr = %d",ip,cptr);
				printf("\n  initial mdnrts[ip/2] = %d",mdnrts[ip/2]);
				}
			qsort(mdnrts,ip,sizeof(mdnrts[0]),order);	/* perform sort */
			*(totsum + cptr) = (long)((mdnrts[(ip-1)/2] + mdnrts[(ip)/2])/2);
			if(DEBUG)
				{
				printf("\n  final mdnrts[ip/2] = %d",mdnrts[(ip)/2]);
				}
			*(itotsum + icptr) += *(totsum + cptr);
			}		
		}
	for(i=0,jccpl=1;i<nc;i++,jccpl++)
		{
		cptr = i;
		if(*(nsum+cptr) != 0)
			fprintf(subfile,"%6ld",*(totsum+cptr));
		else
			fprintf(subfile,"%6d",0);
		if(*(cntsum+cptr) != 0)
			fprintf(psubfile,"%6.3f",(double)(*(corrsum+cptr))/(double)(*(cntsum+cptr)));
		else
			fprintf(psubfile,"%6.3f",0);
		for(j=0;j<nsc;j++)
			{
			mptr = (i * nsc) + j;
			if(*(nmat+mptr) != 0)
				fprintf(msubfile,"%6ld",*(totmat+mptr));
			else
				fprintf(msubfile,"%6d",0);
			fprintf(pmsubfile,"%6d",*(nmat+mptr));
			}
		fprintf(msubfile,"\n");
		fprintf(pmsubfile,"\n");
		if(jccpl == ccpl || i == nc-1)
			{
			jccpl = 0;
			fprintf(subfile,"\n");
			fprintf(psubfile,"\n");
			}
		}
	}
if(total_count > 0)
	fprintf(printer,"\npercentage correct subject %f",(float)total_corrsum/(float)total_count);
}



		

/************************************************************************/

writeitem(ifname,mifname,ipname,mipname,icpl,median)
char *ifname,*mifname,*ipname,*mipname,icpl,median;
{
int i,j,k,jicpl,icount;
FILE *itemfile,*mitemfile;
FILE *pitemfile,*pmitemfile;
long int temp;
int cptr,iptr;
printf("\nDo you want item by item data files written? y or n: ");
fgets(buff,MAXLINE,stdin);
if(tolower(buff[0]) == 'n')
	return;

if ((itemfile = fopen(ifname,"w")) == NULL)
	openfail(ifname);
if ((mitemfile = fopen(mifname,"w")) == NULL)
	openfail(mifname);
if ((pitemfile = fopen(ipname,"w")) == NULL)
	openfail(ipname);
if ((pmitemfile = fopen(mipname,"w")) == NULL)
	openfail(mipname);

printf("\nDo you want item by item printer output? y or n: ");
fgets(buff,MAXLINE,stdin);			/* leave the answer in buff[0] */

for (i=0; i < ni; i++)
	{
	bdos(11,0,0);
	if(buff[0] == 'y')
		fprintf(printer,"\nItem %4d ",i+1);
	for(j=0,jicpl=1;j < nc;j++,jicpl++)
		{
		if(!median)
			{
			cptr = i*nc + j;
			if(*(insum+cptr) != 0)
				fprintf(itemfile,"%6ld",(temp = ((*(itotsum + cptr))/(long)(*(insum + cptr)))));
			else
				fprintf(itemfile,"%6ld",(temp = 0));
			if(*(icntsum+cptr) != 0)
				fprintf(pitemfile,"%6.3f",(double)(*(icorrsum+cptr))/(double)(*(icntsum+cptr)));
			else
				fprintf(pitemfile,"000000");
			if(buff[0] == 'y')
				fprintf(printer,"\n   Cond %4d TOT %6ld SC",j+1,temp);
			for(k=0;k<nsc;k++)
				{
				iptr = i*nc*nsc + j*nsc + k;
				if(*(inmat + iptr) != 0)
					fprintf(mitemfile,"%6ld",(temp = (*(itotmat + iptr)/(long)(*(inmat + iptr)))));
				else
					fprintf(mitemfile,"%6ld",(temp = 0));
				fprintf(pmitemfile,"%6d",*(inmat+iptr));
				if(buff[0] == 'y')
					fprintf(printer,"%6ld",temp);
				}
			fprintf(mitemfile,"\n");
			fprintf(pmitemfile,"\n");
			if(jicpl == icpl || j == nc-1)
				{
				jicpl = 0;
				fprintf(itemfile,"\n");
				fprintf(pitemfile,"\n");
				}
			}
		else			/* median */
			{
			cptr = i*nc + j;
			icount = *(items_count_array + cptr);
			qsort(items_array+(cptr*200),icount,sizeof(items_array[0]),order);	/* perform sort */
			temp = (*(items_array+(cptr*200+(icount-1)/2)) + *(items_array+(cptr*200+icount/2)))/2;
			fprintf(itemfile,"%6d",temp);
			if(DEBUG)
				{
				printf("\n items median %d, cond %d, item %d mdnrts[ip/2] = %d",temp,j,i);
				}
			if(*(icntsum+cptr) != 0)
				fprintf(pitemfile,"%6.3f",(double)(*(icorrsum+cptr))/(double)(*(icntsum+cptr)));
			else
				fprintf(pitemfile,"000000");
			if(buff[0] == 'y')
				fprintf(printer,"\n   Cond %4d TOT %6ld SC",j+1,temp);
			if(jicpl == icpl || j == nc-1)
				{
				jicpl = 0;
				fprintf(itemfile,"\n");
				fprintf(pitemfile,"\n");
				}
			}
		}
	}
fprintf(printer,"\f");
if (fclose(itemfile) == EOF)
	closefail(itemfile);
if (fclose(mitemfile) == EOF)
	closefail(mitemfile);
if (fclose(pitemfile) == EOF)
	closefail(pitemfile);
if (fclose(pmitemfile) == EOF)
	closefail(pmitemfile);
return;
}



int order(first,second)
int *first;
int *second;
{
if(*first < *second)
	return(-1);
else if(*first == *second)
	return(0);
else
	return(1);
}





void writemeans(int ns,int truncelim)
{
int i,j,k,mptr;
long int tempr;
float tempp;
int tempi;
rewind(subfile);
rewind(msubfile);
rewind(psubfile);
rewind(pmsubfile);

/* do it all first for RT means */

if(truncelim == 1)
	fprintf(printer,"\n\nMean RTs, eliminating trials over long cutoff\n");
else
	fprintf(printer,"\n\nMean RTs, replacing long trials with cutoff\n");


for(i=0;i<nc;i++)
	{
	for(j=0;j<nsc;j++)
		{
		mptr = i*nsc + j;
		*(totmat+mptr) = 0l;
		*(nmat+mptr) = 0;
		}
	*(nsum+i) = 0;
	*(totsum+i) = 0l;
	}

for(i=0;i<ns;i++)
	{
	for(j=0;j<nc;j++)
		{
		for(k=0;k<nsc;k++)
			{
			mptr = j*nsc + k;
			fscanf(msubfile,"%ld",&tempr);
			if (DEBUG == 3)
				printf("\nsub %d cond %d scond %d --matrix RT %ld ",i,j,k,tempr);
			if(tempr != 0 || !truncelim)		/* add zeros too if truncelim = 0 */
				{
				*(nmat+mptr) += 1;
				*(totmat+mptr) += tempr;
				}
			}
		fscanf(subfile,"%ld",&tempr);
		if (DEBUG == 3)
			printf("\nSUMMARY -- sub %d cond %d  --sum RT %ld ",i,j,tempr);
		if(tempr != 0 || !truncelim)		/* add zeros too if truncelim = 0 */
			{
			*(nsum + j) += 1;
			*(totsum + j) += tempr;
			if (DEBUG == 3)
				printf("        cum %ld n %d",*(totsum+j),*(nsum + j));
			}
		}
	}

for(i=0;i<nc;i++)
	{
	if(*(nsum + i) != 0)
		fprintf(printer,"\nCond %3d RT Total %6ld     SC",i+ncminl,*(totsum+i)/(long)(*(nsum+i)));
	else
		fprintf(printer,"\nCond %3d RT Total  00000     SC",i+ncminl);
	for(j=0;j<nsc;j++)
		{
		mptr = i*nsc + j;
		if(*(nmat+mptr) != 0)
			fprintf(printer,"%6ld",*(totmat+mptr)/(long)*(nmat+mptr));
		else
			fprintf(printer," 00000");
		}
	}

/* and again for mean proportions */

fprintf(printer,"\n\n    proportions are        raw counts exclude");
fprintf(printer,"\n    based on means         all trials below lower");
fprintf(printer,"\n    of individual S        cutoff; also above upper");
fprintf(printer,"\n    proportions,           cutoff if 'eliminate'");
fprintf(printer,"\n    including all Rs       is selected");
fprintf(printer,"\n    regardless of RT\n");

for(i=0;i<nc;i++)
	{
	for(j=0;j<nsc;j++)
		{
		mptr = i*nsc + j;
		*(totmat+mptr) = 0;
		*(nmat+mptr) = 0;
		}
	*(nsum+i) = 0;
	*(totsum+i) = 0l;
	}

for(i=0;i<ns;i++)
	{
	for(j=0;j<nc;j++)
		{
		for(k=0;k<nsc;k++)
			{
			mptr = j*nsc + k;
			fscanf(pmsubfile,"%d",&tempi);
			*(totmat+mptr) += (long)(tempi);
			}
		fscanf(psubfile,"%f",&tempp);
		*(totsum + j) += (long)(tempp*1000);
		*(nsum + j) += 1;
		if(DEBUG == 3)
			printf("\nsubj %d      cond j %d *(totsum+j) %ld *(nsum+j) %d",i+1,j+1,*(totsum+j),*(nsum+j));
		}
	}

for(i=0;i<nc;i++)
	{
	if(*(nsum+i) != 0)
		fprintf(printer,"\nCond %3d PC Total %6.3f     SC",i+ncminl,((double)(*(totsum+i)/(long)(*(nsum+i))))/1000);
	else
		fprintf(printer,"\nCond %3d PC Total 000000     SC");
	for(j=0;j<nsc;j++)
		{
		mptr = i*nsc + j;
		fprintf(printer,"%6ld",*(totmat+mptr));
		}
	}

/* print matrix of number rejected */


fprintf(printer,"\n\n\nREJECTS: Item x cond");
fprintf(printer,"\nITEM                  COND\n");  
fprintf(printer,"\n   ");
for(j=0;j<nc;j++)
	{
	fprintf(printer,"%3d",j+ncmin);
	nrejtot[j] = 0;
	}
fprintf(printer,"\n\n");
for(i=0;i<ni;i++)
	{
	fprintf(printer,"\n%3d",i+1);
	for(j=0;j<nc;j++)
		{
		nrejtot[j] += (*(nrejsum + i*nc + j));
		fprintf(printer,"%3d",(*(nrejsum + i*nc +j)));
		}
	}
fprintf(printer,"\n\nTOT");
for(j=0;j<nc;j++)
	fprintf(printer,"%3d",nrejtot[j]);

fprintf(printer,"\f");
}

void openfail(char *filestr)
{
printf("\nCan't open file %s. ",filestr);
exit(1);
}

closefail(filestr)
FILE *filestr;
{
printf("\nCan't close file %d; check disk space! ",filestr);
return;
}


/*rewind(stream)
FILE *stream;
{
fseek(stream,0L,0);
}*/

char *tgets(buff,stream)
char *buff;
FILE *stream;
	{
	int i;
	fgets(buff,MAXLINE,stream);
	for(i=0;buff[i] != '\n' && i < MAXLINE;i++)
		;
	buff[i] = '\0';
	return(buff);
	}
