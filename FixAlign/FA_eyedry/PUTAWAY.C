/* PUTAWAY.C */
/* JULY 7 1996 */
/* DESIGNED TO TAKE .IXS FILES FROM EYEDRY AND TRANSFORM THEM TO BE SUITABLE
AS INPUT TO STANDARD STATS PACKAGES.
ITS OUTPUT IS ONE LINE PER SUBJECT (IN THE SUBJECT OUTPUT FILE) AND ONE LINE
PER ITEM (IN THE ITEM OUTPUT FILE), WITH REGIONS OF SENTENCE THE SLOWER MOVING
VARIABLE AND CONDITIONS THE FASTER MOVING VARIABLE. E.G.:

(SUB 1)   reg 1/cond1 reg 1/cond 2 reg1/cond 3 reg2/cond1 reg2/cond2 reg2/cond3


NOTE, IT CAN BE USED NICELY AS AN INPUT TO FOO TO REPLACING MISSING SCORES.

*/




#include <stdio.h>
#include <stdlib.h>
#include <io.h>
#include <process.h>
#include <fcntl.h>
#include <sys\stat.h>
#include <ctype.h>
#include <alloc.h>
void main(void);
void oops(char *string);
int readnext(int control);
float readnext_float(int control);
void readerr(char *string);
int bptr;


int IV[200];		/* room for 200 items */
int IC[40];

#define MAXLINEX 2000
int cond[50];
char buff[MAXLINEX];
void main()
{
float *ICSum,*SCSum;
int *ICCnt,*SCCnt;
FILE *infile, *subfile, *itmfile;
int ncon,nsub,nreg,nitm,ncond,allitm;
int i,j,ns,nc,ni,nr,isen;
int xi,tii,tis;
int thissub,thisitem,thiscond;
int nsx,ncx;
int another;
int discard_zeros,trial_out,trash_trial;
int mincond,maxcond,minitem,maxitem;
float data_vector[10];
float datum;
char filename[80];

for(i=0;i<100;i++)
	IV[i] = 0;
mincond = maxcond = minitem = maxitem = 0;	
printf("\nType name of input file: ");
gets(filename);
if ((infile = fopen(filename,"r")) == NULL)
	{
	printf("\nCan't open file %s",filename);
	exit(1);
	}
printf("How many items are there in total? ");
scanf("%d",&allitm);
printf("How many subjects are there? ");
scanf("%d",&nsub);
printf("How many conditions total? ");
scanf("%d",&ncon);
nitm = 0;
ncond = 0;
printf("\n\nSCANNING FILE %s TO FIND ITEM AND CONDITION NUMBERS. \n",filename);
for(ns=0;ns<nsub;ns++)
	for(ni=0;ni<allitm;ni++)
		for(nc=0;nc<ncon;nc++)
			{
			if(fgets(buff,MAXLINEX,infile) == NULL)
				readerr("buff with fgets in initial counting");
			if((thissub = readnext(0)) == NULL)
				readerr("subject in initial counting");
			if((thisitem = readnext(1)) == NULL)
				readerr("item in initial counting");
			thiscond = readnext(1);
			if(thiscond != 0)
				{
				if(thiscond < mincond || mincond == 0)
					mincond = thiscond;
				if(thiscond > maxcond)
					{
					maxcond = thiscond;						
					}
				if(thisitem < minitem || minitem == 0)
					minitem = thisitem;
				if(thisitem > maxitem)
					{
					maxitem = thisitem;
					IV[nitm++] = thisitem;
					}
				else
					{
					for(i=0;i<nitm;i++)
						{
						if (thisitem == IV[i])		/* already logged the item */
							break;
						else if(thisitem < IV[i])	/* new item, lower than current one in list */
							{
							for(j=++nitm;j>i;j--)
								IV[j] = IV[j-1];
							IV[j] = thisitem;	
							break;
							}
						}
					}
				}
			}
for(i=0;i<nitm;i++)
	printf(" %d",IV[i]);
printf("\nFound %d items from %d to %d",nitm,minitem,maxitem);
printf(" and %d conditions from %d to %d.",ncon,mincond,maxcond);
printf("\nIs this correct? y or n. ");
scanf("%s",buff);
if(maxcond-mincond+1 != ncon)
	{
	printf("\nPROBLEMS: you say you have %d conditions but the numbers range from\n%d to %d, which is not right. The condition numbers must be contiguous.",ncon,mincond,maxcond);
	exit(1);
	}
if(tolower(buff[0] == 'n'))
	exit(1);
	
gets(buff);			/* clear its throat...*/
rewind(infile);
printf("\nType name of subject-by-subject output file: ");
gets(filename);
if((subfile = fopen(filename,"w")) == NULL)
	{
	printf("\nCan't open file %s",filename);
	exit(1);
	}
printf("Type name of item-by-item output file: ");
gets(filename);
if ((itmfile = fopen(filename,"w")) == NULL)
	{
	printf("\nCan't open file %s",filename);
	exit(1);
	}
printf("How many regions are there for each item? ");
scanf("%d",&nreg);
printf("Do you want to Discard or Keep zero values? Type d or k: ");
scanf("%s",buff);
if(tolower(buff[0] == 'k'))
	{
	discard_zeros = 0;
	printf("Keeping zeros.");
	}
else
	{
	discard_zeros = 1;
	printf("Discarding zeros.");
	}
printf("\nHow many successive 0's are required to discard the rest of a trial.\nType 0 if you don't want trials discarded. ");
scanf("%d",&trial_out);
	

/*printf("\nncon %d nitm %d nreg %d nsub %d pro1 %d pro2 %d",ncon,nitm,nreg,nsub,ncon*nitm*nreg,ncon*nsub*nreg);*/
if((ICSum = (float *)(calloc(ncon*nitm*nreg,sizeof(float)))) == NULL)
	oops("ICSum");
if((SCSum = (float *)(calloc(ncon*nsub*nreg,sizeof(float)))) == NULL)
	oops("SCSum");
if((ICCnt = (int *)(calloc(ncon*nitm*nreg,sizeof(int)))) == NULL)
	oops("ICCnt");
if((SCCnt = (int *)(calloc(ncon*nsub*nreg,sizeof(int)))) == NULL)
	oops("SCCnt");

for(i=0;i<ncon*nsub*nreg;i++)
	*(SCCnt+i)=*(SCSum+i)=0;
for(i=0;i<ncon*nitm*nreg;i++)
	*(ICCnt+i) = *(ICSum+i) = 0;
	
for(ns=0;ns<nsub;ns++)
	{
	for(ni=0;ni<allitm;ni++)
		{
		for(nc=0;nc<ncon;nc++)
			{
			if(fgets(buff,MAXLINEX,infile) == NULL)
				{
				readerr("buff with fgets");
				}
			if((thissub = readnext(0)) == NULL)
				readerr("subject");
			if((thisitem = readnext(1)) == NULL)
				readerr("item");
			thiscond = readnext(1);
			if(thiscond != 0)
				{
				ncond = thiscond-mincond;
				for(xi=0;xi<nitm;xi++)
					if(IV[xi] == thisitem)
						break;
				if(xi != nitm)
					{
					thisitem = xi;
					for(nr = 0;nr < nreg;nr++)
						{
						datum = readnext_float(1);
						for(i=0;i<9;i++)
							data_vector[i+1] = data_vector[i];
						data_vector[0] = datum;
						trash_trial = 0;
						if(trial_out != 0 && datum == 0 && nr >= trial_out-1)
							{
							/* eliminate rest of trial here*/
							for(i=1,trash_trial = 1;i < trial_out && trash_trial;i++)
								if(data_vector[i] != 0)
									trash_trial = 0; 
							}
						if(trash_trial)
							nr = nreg;
						else
							{
							tis = ns*ncon*nreg + ncond*nreg + nr;
							tii = (thisitem)*ncon*nreg + ncond*nreg + nr;
							if(!discard_zeros || datum != 0)		/* ELIMINATE ZERO EYETRACK MEASURES */
								{
								*(SCSum+tis) += datum;
								(*(SCCnt+tis))++;
								*(ICSum+tii) += datum;
								(*(ICCnt+tii))++;
								}
							}
						}
					}
				}
			}
		}
	}	
for(ns=0;ns<nsub;ns++)
	{
	for(nr=0;nr<nreg;nr++)
		{
		for(nc=0;nc<ncon;nc++)
			{
/*printf("\nReady to print for subj %d reg %d cond %d",ns,nr,nc);*/
			tis = ns*ncon*nreg + nc*nreg + nr;
			if(*(SCCnt+tis) == 0)
				fprintf(subfile,"       0");
			else
				fprintf(subfile,"%8.2f",(*(SCSum+tis))/(float)(*(SCCnt+tis)));
			}
		}
	fprintf(subfile,"\n");
	}

for(ni=0;ni<nitm;ni++)
	{
	for(nr=0;nr<nreg;nr++)
		{
		for(nc=0;nc<ncon;nc++)
			{
/*printf("\nReady to print for item %d reg %d cond %d",ni,nr,nc);*/
			tii = ni*ncon*nreg + nc*nreg + nr;
			if(*(ICCnt+tii) == 0)
				fprintf(itmfile,"       0");
			else
				fprintf(itmfile,"%8.2f",(*(ICSum+tii))/(float)(*(ICCnt+tii)));
			}
		}
	fprintf(itmfile,"\n");
	}
fclose(itmfile);
fclose(subfile);
fclose(infile);
free(SCCnt);
free(SCSum);
free(ICCnt);
free(ICSum);
}


void oops(char *string)
{
printf("\nOut of allocation space at %s.",string);
exit(1);
}

void readerr(char *string)
{
printf("\nError while reading %s",string);
exit(1);
}

int readnext(int control)
{
int value;
if(control == 0)
	bptr = 0;
while (!isdigit(*(buff+bptr)) && *(buff+bptr) != '-')
	bptr++;
value = atoi(buff+bptr);
while(isdigit(*(buff+bptr++)) || *(buff+bptr) == '-')
	;
/*printf("\nreadnext %d",value);*/
return(value);
}

float readnext_float(int control)
{
float value;
if(control == 0)
	bptr = 0;
while (!isdigit(*(buff+bptr)) && *(buff+bptr) != '.' && *(buff+bptr) != '-')
	bptr++;
value = atof(buff+bptr);
while(isdigit(*(buff+bptr)) || *(buff+bptr) == '.'|| *(buff+bptr) == '-')
	bptr++;
/*printf("\nreadnext_float %f",value);	*/
return(value);
}


