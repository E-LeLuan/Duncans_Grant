/* program based on foo.c to get t of contrast between 2 contitions
in cumulative position at each time slice */

void oops(char *x);

#include <stdio.h>
#include <stdlib.h>
#include <io.h>
#include <process.h>
#include <fcntl.h>
#include <sys\stat.h>
#include <ctype.h>
#include <math.h>

main()
{
char ichar;
FILE *infile, *outfile1, *outfile2, *outfile3, *sigfile;
int nslice,ncon,count;
int nsub,n_line;
int isub,icon;
int i,j,ns,nc,nc2,nsl;
int nsx,ncx;
int another;
double ct, diff;
double *tempdata = NULL;
double *data = NULL;
double *square_data = NULL;
double *sd = NULL;
int *subcount = NULL;

char filename[80],tf[80];
printf("\nType name of input file: ");
gets(filename);
if ((infile = fopen(filename,"r")) == NULL)
	{
	printf("\nCan't open file %s",filename);
	exit(1);
	}
printf("\nType name of body of output file (mean, t, smd, and sig-diff files\nwill open with extensions .cmn,.ctt,.cse,.csg): ");
gets(filename);
strcpy(tf,filename);
strcat(tf,".cmn");
if ((outfile1 = fopen(tf,"w")) == NULL)
	{
	printf("\nCan't open file %s",tf);
	exit(1);
	}
strcpy(tf,filename);
strcat(tf,".ctt");
if ((outfile2 = fopen(tf,"w")) == NULL)
	{
	printf("\nCan't open file %s",tf);
	exit(1);
	}
strcpy(tf,filename);
strcat(tf,".cse");
if ((outfile3 = fopen(tf,"w")) == NULL)
	{
	printf("\nCan't open file %s",tf);
	exit(1);
	}
strcpy(tf,filename);
strcat(tf,".csg");
if ((sigfile = fopen(tf,"w")) == NULL)
	{
	printf("\nCan't open file %s",tf);
	exit(1);
	}
printf("\nHow many time slices total? ");
scanf("%d",&nslice);
printf("How many subjects? ");
scanf("%d",&nsub);
printf("How many conditions per subject? ");
scanf("%d",&ncon);
printf("What is critical value of t? ");
scanf("%lf",&ct);
if((tempdata = (double *)calloc(nslice * ncon,sizeof(double))) == NULL)
	oops("tempdata");
if((subcount = (int *)calloc(nslice * (ncon*ncon-ncon)/2,sizeof(int))) == NULL)
	oops("subcount");
if((data = (double *)calloc(nslice * (ncon*ncon-ncon)/2,sizeof(double))) == NULL)
	oops("data");
if((square_data = (double *)calloc(nslice * (ncon*ncon-ncon)/2,sizeof(double))) == NULL)
	oops("square_data");
if((sd = (double *)calloc(nslice *  (ncon*ncon-ncon)/2,sizeof(double))) == NULL)
	oops("sd");
for(ns=0;ns<nsub;ns++)
	{
	for(nc=0;nc<ncon;nc++)		/* read data for one subj in */
		{
		fscanf(infile,"%d%d",&isub,&icon);
		printf("Subj %d cond %d\n",isub,icon);
		for(nsl=0;nsl<nslice;nsl++)
			{
			fscanf(infile,"%lf",(tempdata + nc*nslice + nsl));
			}
		}
	for(nsl = 0;nsl < nslice;nsl++)
		{
		count = 0;					/* get sums and sums of squares */
		for(nc=0;nc < ncon-1;nc++)
			{
			for(nc2 = nc+1;nc2 < ncon;nc2++)
				{
				if((*(tempdata + nc*nslice + nsl)) > - 99.9 &&(*(tempdata + nc2*nslice + nsl)) > - 99.9)
					{
					diff = *(tempdata + nc*nslice + nsl) - *(tempdata + nc2*nslice + nsl);
					(*(data + count*nslice + nsl)) += diff;
					(*(square_data + count*nslice + nsl)) += (diff*diff);
					(*(subcount + count*nslice + nsl))++;
					}
				count++;
				}
			}
		}
	}
for(nsl = 0;nsl < nslice;nsl++)
	{
	count = 0;					/* get sums and sums of squares */
	for(nc=0;nc < ncon-1;nc++)
		{
		for(nc2 = nc+1;nc2 < ncon;nc2++)
			{
			(*(data+count*nslice + nsl)) /= (double)(*(subcount+count*nslice+nsl));		/*convert to mean */
			*(sd + count*nslice + nsl) = (sqrt(((*(square_data + count*nslice + nsl))/(double) (double)(*(subcount+count*nslice+nsl))) - (*(data+count*nslice+nsl))*(*(data+count*nslice+nsl))));
			*(sd + count*nslice + nsl) = (*(sd + count*nslice+nsl))/sqrt((double)( (double)(*(subcount+count*nslice+nsl))-1));	/* smd */
			count++;
			}
		}
	}
count = 0;
for(nc = 0; nc < ncon-1;nc++)
	for(nc2 = nc+1;nc2 < ncon; nc2++)
		{
		fprintf(sigfile,"%d %d",nc+1,nc2+1);		/* file of sig diffs */
		fprintf(outfile1,"%d %d",nc+1,nc2+1);		/* file of means */
		for(nsl = 0;nsl < nslice;nsl++)
			{
			fprintf(outfile1," %.3lf",*(data+count*nslice + nsl));	
			}
		fprintf(outfile1,"\n");
		fprintf(outfile2,"%d %d",nc+1,nc2+1);		/* file of t's */
		for(nsl = 0;nsl < nslice;nsl++)
			{
			*(data + count*nslice + nsl) = (*(data + count*nslice + nsl))/(*(sd + count * nslice + nsl));		/* value of t */
			fprintf(outfile2," %.3lf",*(data+count*nslice + nsl));	
			if((fabs(*(data+count*nslice+nsl))) > ct)
				fprintf(sigfile," slice %d",nsl+1);
			}
		fprintf(outfile2,"\n");
		fprintf(sigfile,"\n");
		fprintf(outfile3,"%d %d",nc+1,nc2+1);			/* file of std errors */
		for(nsl = 0;nsl < nslice;nsl++)
			{
			fprintf(outfile3," %.3lf",*(sd+count*nslice + nsl));	
			}
		fprintf(outfile3,"\n");
	count++;
	}
fclose(infile);
fclose(outfile1);
fclose(outfile2);
fclose(outfile3);
free(subcount);
free(data);
free(square_data);
free(tempdata);
free(sd);
}

void oops(char *x)
{
printf("\nCalloc error %s",x);
exit(1);
}
