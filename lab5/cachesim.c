/* 15-213 Fall 2010
 * A Cache simulator that can replay traces from Valgrind and output
 * statistics such as number of hits, misses, and evictions.
 * The replacement policy is LRU.
 *
 * Implementation and assumptions:
 *  1. Each load/store can cause at most one cache miss. (I examined the trace,
 *  the largest request I saw was for 8 bytes).
 *  2. Instruction loads (I) are ignored, since we are interested in evaluating
 *  trans.c in terms of its data cache performance.
 *  3. data modify (M) is treated as a load followed by a store to the same
 *  address. Hence, an M operation can result in two cache hits, or a miss and a
 *  hit plus an possible eviction.
 *
 * The function printCachesimResults() is given to print output.
 * Please use this function to print the number of hits, misses and evictions.
 * This is crucial for the driver to evaluate your work.
 *
 *
 * Your Name: Brian Flores
 * Your Andrew ID: bflores
 *
 */


#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <limits.h>
#include <unistd.h>

// S,s, E, B,b are defined as in CSAPP page 597
unsigned int s;
unsigned int S;
unsigned int b;
unsigned int B;
unsigned int E;
char* trace_file;

unsigned int miss_count = 0;
unsigned int hit_count = 0;
unsigned int eviction_count = 0;

//Strut of contents of a cache block
typedef struct
{
	long int tag;
	int lul;     //last used line
}cache_t;

/* Method used when evicting a word from cache 
 * uses LRU policy
 */
void evict(cache_t*** c,int newTag,int newLul)
{
	int line,block;
	int lowestLine = 9999999;   //generally large number size 
	int evictMe_block, evictMe_line;

	//loops through all lines and blocks searching for lowest line
	for(line =0; line<S*E; line++)
	{
		for(block=0;block<B;block++)
		{
			if(c[line][block] != NULL && c[line][block]->lul < lowestLine)
			{
				//when found keep track of location with variables
				lowestLine = c[line][block]->lul;
				evictMe_line = line;
				evictMe_block = block;
			}
		}
	}

	//relace with new cached data;
	c[evictMe_line][evictMe_block]->tag = newTag;
	c[evictMe_line][evictMe_block]->lul = newLul;
}

//method used to simulate the cache
//takes in the address being sent into cache and line it was found on
//as well as a pointer to the cache
void runInCache(long int addr, cache_t*** c, int line)
{
	//general varibled throughout method
	long int i,j,k;

	//masks for parcing through addr
	//creates masks for: tag, line indx, block indx
	int b_copy =b;
	i = (~0x0)<<(b+s);
	j = ~0x0;
	while (b_copy>0)
	{
		j = j<<1;
		b_copy--;
	}
	j=~j;
	k = ~(i|j);
	
	//assign values using masks
	long int tag;
	int ln,blck;
	tag = addr&i;
	ln = (addr&k)>>b;
	blck = addr&j;
	
	//cases for cache actions:

	//if a certain block is NULL it resembles an empty cache block 
	if(c[ln][blck] == NULL)
	{	
		miss_count++;
		
		//make entire line full of blocks with tag 'tag'
		int p;
		for(p=0; p<B;p++)
		{
			c[ln][p]=(cache_t*)calloc(1,sizeof(cache_t));
			c[ln][p]->tag = tag;
			//create genertic last line used for not cached block
			c[ln][p]->lul = 99999999;
		}

		//assign correct last line used for cached block
		c[ln][blck]->lul = line;
	}

	//if tag mactches its a hit
	else if(c[ln][blck]->tag == tag)
	{
		hit_count++;

		//update last line used for cache block
		c[ln][blck]->lul=line;
	}

	//if tag does not match its an eviction
	else if(c[ln][blck]->tag != tag)
	{
		miss_count++;
		eviction_count++;

		//iterate through line to change all blocks tags
		int p;
		for(p=0;p<B;p++)
		{
			c[ln][p]->tag =tag;
			c[ln][p]->lul = 99999999;
		}

		//assign correct last line used 
		c[ln][blck]->lul = line;
	}

	//if not any of the above cases its not in an already full cache
	//so evidit based on LRU policy
	else 
	{
		evict(c,tag,line);

		miss_count++;
		eviction_count++;
	}
}

void printUsage(char* argv[]){
	printf("\nUsage example: %s -s 4 -E 1 -b 4 -t simple_test.trace\n",
			argv[0]);
	printf("Explanation of options:\n");
	printf("-s: number of set index bits.\n");
	printf("-E: number of lines per set.\n");
	printf("-b: number of block bits.\n");
	printf("-t: the file containing the input trace.\n\n");
	printf("Please see text book CASPP page 597 for a more\n") ;
	printf("detailed explanation of s, S, b, B and E.\n");
	exit(1);
}

void printCachesimResults(unsigned int num_hit,
		unsigned int num_miss, unsigned int num_eviction){
	printf("Cachesim Results:   Hits: %d  Misses: %d  Evictions: %d\n",
			num_hit, num_miss, num_eviction);
}

int main(int argc, char* argv[]){
	// Parse commandline options
	char c;

	while( (c=getopt(argc,argv,"s:E:b:t:")) != -1){
		switch(c){
			case 's':
				s = atoi(optarg);
				break;
			case 'E':
				E = atoi(optarg);
				break;
			case 'b':
				b = atoi(optarg);
				break;
			case 't':
				trace_file = optarg;
				break;
			case '?':
				printUsage(argv);
				exit(1);
			default:
				printUsage(argv);
				exit(1);
		}
	}

	S = (unsigned int) pow(2, s);
	B = (unsigned int) pow(2, b);

	////////////////////////////////////
	/* create cache in this format:
	 * cache_t*** = entire cache
	 * cache_t**  = lines in cache (S*E)
	 * cache_t*   = blocks in a line
	 * cache_t    = block 
	 */
	cache_t *** C;
	C = (cache_t***)calloc(S*E,sizeof(cache_t**));//all sets
	
	//callocing for lines but not for individual blocks
	int i,j;
	for(i=0;i<(S*E);i++)
	{
		C[i] = (cache_t**)calloc(B,sizeof(cache_t*));   //a line
	}

	//read from file
	
	FILE * input = fopen(argv[argc-1], "rt");
	int currline=0;
	long int addr;
	char * line = (char*)malloc(20);

	while(fgets(line, 20, input) != NULL)
	{
		currline++;
		
		//parse through read line for address
		sscanf(line," %*c %lx, %*i", &addr);

		//look for letters M,S,L
		switch(line[1])
		{
			case 'M':
				runInCache(addr, C, currline);
				hit_count++;
				break;

			case 'S':
				runInCache(addr, C, currline);
				break;

			case 'L':
				runInCache(addr, C, currline);
				break;

		}
	}

	//free cache and close file
	for(i = 0; i < S*E; i++)
		for(j = 0; j<B;j++)
			free(C[i][j]);
	for(i = 0; i < S*E; i++)
		free(C[i]);
	free(C);
	
	free(line);

	fclose(input);

	///////////////////////////////////


	// output results so that the driver can grade my work
	printCachesimResults(hit_count, miss_count, eviction_count);
	return 0;
}
