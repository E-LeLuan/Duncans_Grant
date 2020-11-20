# include <stdlib.h>
# include <stdio.h>
# define half_target 18
# define number_fillers 108
# define scale 2

int main(int argc,char *argv[])
{
	FILE *fin;
	int i;
	char header_data[80];
  /*Shawn*/
  float fsps;
  float fdatalength;
  float div1, div2;

	struct wav_filespace{
		char riff[4];
		long int total_bytes;
		char wave [4];
		char fmt [4];
		long int length;
		int format; /*pcm*/
		int channels; /*1=mono  2=stereo*/
		long sps;
		long avg_sps;
		int block_align;
		int bits_per_sample;
		char data[4];
		long int data_length;}
		wav_cf;
	
	char *wav_cfptr= (char*)(&wav_cf);


		if((fin= fopen(argv[1], "r"))==NULL){
		printf("Cannot open file %s.\n",argv[1]);
		exit(1);
		}

		if (fread(header_data, sizeof(char), 44, fin) !=44){
		printf("Error occured at read");
		exit(1);
		}

  /*printf("%c", fin);*/

	for(i=0;i<44; i++)
		*(wav_cfptr + i)= *(header_data +i);

	fdatalength = (float)wav_cf.data_length;
  fsps = (float)wav_cf.sps;
  div1 = fdatalength/2.0;
  div2 = div1/fsps; 

  printf("FILE %s  %8.0f ms \n",argv[1],div2*1000);

	fclose(fin);

	return 1;

}
