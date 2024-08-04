
#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>

struct node {
  struct node *next;
  struct node *prev;
  int val;
};
typedef struct node Node;

Node *mknode (int i);
Node *init(int n);
Node *removeNode(Node *p);
Node *forwardNode(Node *p);
void puzzle(int n);

int verbose;

Node *mknode (int i){
 Node *p = (Node *)malloc(sizeof (Node));
 if (NULL == p){
   fprintf(stderr,"failed allocated node %d\n" ,i);
   exit(2);
 }
// printf("size of node = %d \n" , sizeof(Node));
 p->next = 0;
 p->prev = 0;
 p->val = i;
 return p;
}

Node *init(int n){
  int size = n;
  int half = size / 2 ; 
  //List *ls = mkList(size);
  int i = 1;
  Node *hd = 0;
  Node *tl = 0;
  Node *ptr = 0;
  while(i <= size){
    Node *tmp = mknode(i);    
    if( i == half) {
      printf("init has half %d \n" , i);
      ptr = tmp;
    }

    if(!hd){
      hd = tmp;
      tl = tmp;      
    }
    else if (hd == tl) {
      tl->next = tmp;
      tl = tmp;
      tl->prev = hd;
        }
    else {
      tl->next = tmp;
      tmp->prev = tl;
      tl = tmp;
    }
    i ++;
  }
  tl->next = hd;
  hd->prev = tl;
 /* ptr */
   return ptr;
}

Node *removeNode(Node *p){
  if(verbose) {printf("removing node %d \n" , p->val);}
  Node *prev = p->prev;
  Node *next = p->next;
  prev->next = next;
  next->prev = prev;
  return next;
}

Node *forwardNode(Node *p){
  return p->next;
}

void puzzleOdd(int n){
  printf("init started ...\n");
  Node *ptr = init(n);
  printf("init completed\n");
  int size = n;  
  ptr = forwardNode(ptr);
  ptr = removeNode(ptr); size --;  
      
  while(size > 1){
    ptr = forwardNode(ptr);
    /* ptr = forwardNode(ptr); */
    ptr = removeNode(ptr); size --;
    /* ptr = forwardNode(ptr); */
    if (size > 1) {
      ptr = removeNode(ptr); size --;
    }
  }
  printf("surviving node is %d\n" , ptr->val);
  
}


void puzzleEven(int n){
  printf("init started ...\n");
  Node *ptr = init(n);
  printf("init completed\n");
  int size = n;  
  ptr = forwardNode(ptr);
  ptr = removeNode(ptr); size --;
  /* ptr = removeNode(ptr); size --; */
  /* ptr = forwardNode(ptr); */
  ptr = removeNode(ptr); size --;  
      
  while(size > 1){
    ptr = forwardNode(ptr);
    /* ptr = forwardNode(ptr); */
    ptr = removeNode(ptr); size --;
    /* ptr = forwardNode(ptr); */
    if (size > 1) {
      ptr = removeNode(ptr); size --;
    }
  }
  printf("surviving node is %d\n" , ptr->val);
  
}

void puzzle(int n){
  if (n % 2 == 0){
    puzzleEven(n);
  }
  else {
    puzzleOdd(n);
  }
}

int main(int argc, char **argv){
 verbose = 1;
  if (argc > 1) {
    int n = atoi(argv[1]);
    printf("requested solution to size %d \n" , n);
    puzzle(n);
  }
  else {
    //printf("please provide integer after command \n");
    verbose = 0;
    puzzle(3005290);
}
  return 0;
}

/*

init started ...
init has half 1502645 
init completed
surviving node is 1410967

real	0m0.072s
user	0m0.044s
sys	0m0.028s

doubly linked list means do not need to traverse list too much , only first long 
traversal which can be captured when make cyclic list so that is negated too.

then its just a case of getting removeNode forward node in right sequence 
depending on whether its an odd or even number
if we move forward when we remove a node , we avoid extra forward calls 

some small hand worked examples helped check output of program matched expectations 
we found initially we got halfway mark , we needed to move forward again 
then we could start removing and forward then enter repeated loop

successs
this completes all the advent of code puzzles



*/

