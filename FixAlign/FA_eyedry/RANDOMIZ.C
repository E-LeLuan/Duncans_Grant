#include "pcexpt.h"
void main()
{
int k,kk,n,kn,b,bb;
unsigned order[500];
char filename[MAXLINE];
FILE *file = 0;

raninit();
n = input("\nHow many numbers do you want to randomize? ");
k = input("\nHow many times do you want each number to appear per block? ");
printf("\nHow many blocks of %d occurrences of each number? ",k);
scanf("%d",&b);
gets(filename);		/* dummy */
/*b = input("\nHow many blocks of %d occurrences of each number? ",k);*/
printf("\nOutput file name (CR if listing on terminal): ");
if(gets(filename) != 0)
	file = fopen(filename,"w");
else
	file = 0;
for(bb = 0; bb < b; bb++)	
		{
		if(file == 0)
			printf("\n");
		permute((unsigned)(n*k),1,order,1);
		for(kn = 0; kn < n*k; kn++)
			if(file == 0)
				{
				if(kn % 10 EQ 0)
					printf("\n");
				if((order[kn])%n < 99)
					printf(" ");
				if((order[kn])%n <9)
					printf(" ");
				printf(" %d",((order[kn])%n)+1);
				}
			else
				fprintf(file,"%d\n",((order[kn])%n)+1);		
		}
if(file != 0)
	fclose(file);
}
	
