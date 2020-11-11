/* a program based on Myers 1972 page 171 iterative technique for replacing
missing data */

#include <stdio.h>
#include <stdlib.h>
#include <io.h>
#include <process.h>
#include <fcntl.h>
#include <sys\stat.h>
#include <ctype.h>

main()
{
FILE *infile, *outfile;
int ncon,nsub,n_line;
int i,j,ns,nc,isen;
int nsx,ncx;
int another;
float ts[200];
float tc[200];
float t;
float *data = NULL;
float *prov_data = NULL;

char filename[80];
printf("\nAssumes Subjects (rows) x Conditions (columns) matrix, with nothing\nexcept data -- no S#, C#, etc.\n");
printf("\nType name of input file: ");
gets(filename);
if ((infile = fopen(filename,"r")) == NULL)
	{
	printf("\nCan't open file %s",filename);
	exit(1);
	}
printf("\nType name of output file: ");
gets(filename);
if ((outfile = fopen(filename,"w")) == NULL)
	{
	printf("\nCan't open file %s",filename);
	exit(1);
	}
printf("\nHow many conditions total? ");
scanf("%d",&ncon);
printf("How many subjects? ");
scanf("%d",&nsub);
another = 1;
data = (float *)calloc(ncon * nsub,sizeof(float));
prov_data = (float *)calloc(ncon * nsub,sizeof(float));
for(ns=0;ns<nsub;ns++)
	for(nc=0;nc<ncon;nc++)
		{
		fscanf(infile,"%f",data+(ns*ncon)+nc);
		}
while(another)
	{
	if(another == 1)
		{
		for(ns=0;ns<nsub;ns++)
			for(nc=0;nc<ncon;nc++)
				{		
				*(prov_data+(ns*ncon)+nc) = *(data+(ns*ncon)+nc);
				if(*(prov_data+(ns*ncon)+nc) <= 0.0)
					*(prov_data+(ns*ncon)+nc) = 500.0;
				}
		}
	for(ns = 0; ns < nsub;ns++)
		for(nc=0;nc<ncon;nc++)
			{
			if(*(data+(ns*ncon)+nc) == 0)
				{
				for(j=0;j<nsub;j++)	
					ts[j] = 0.0;
				for(j=0;j<ncon;j++)
					tc[j] = 0.0;
				t = 0.0;
				for(nsx=0;nsx < nsub;nsx++)
					for(ncx=0;ncx < ncon;ncx++)
						{
						if(ncx != nc || nsx != ns)
							{
							ts[nsx] += *(prov_data + (nsx*ncon) + ncx);
							tc[ncx] += *(prov_data + (nsx*ncon) + ncx);
							t+= *(prov_data + (nsx*ncon) + ncx);
							}
						}
				*(prov_data+(ns*ncon)+nc) = (ts[ns]*(float)(nsub) + tc[nc]*(float)(ncon) - t)/((float)(ncon-1)*(float)(nsub-1));
				printf("\nREPLACED sub %d cond %d value %f",ns+1,nc+1,*(prov_data+(ns*ncon)+nc));
				}
			}
	printf("\nWANT ANOTHER ITERATION? y or n: ");
	scanf("%s",filename);
	if(tolower(filename[0]) == 'y')
		another=2;
	else
		another=0;
	}
for(ns=0;ns<nsub;ns++)
	{
	for(nc=0;nc<ncon;nc++)
		{
		fprintf(outfile,"%8.0f",*(prov_data+(ns*ncon)+nc));
		}
	fprintf(outfile,"\n");
	}
free(data);
free(prov_data);
}
