#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct bilet_avion{
    char destinatie[20];
    float pret;
    int reducere;
}bilet_avion;

bilet_avion a[100];
bilet_avion b[100];

void shellsort(bilet_avion *bilet, int n)
{
 int i,j,step;
 unsigned char m;
 bilet_avion temp;
 int h[4];
 h[0]= 9;
 h[1]= 5;
 h[2]= 3;
 h[3]= 1;
 for(m=0; m<4; m++)
 { 
 step= h[m];
 for(i=step+1; i< n; i++)
 {
 temp=bilet[i]; 
 
 for(j=i;j>=step && bilet[j-step].reducere<temp.reducere;j=j-step)
 {
     bilet[j]=bilet[j-step];
 }
 bilet[j]=temp;
} 
}
}


void quicksort(bilet_avion *bilet,int left,int right) 
{ 
int i=left,j=right,x=bilet[(left+right)/2].pret;
bilet_avion temp;
do {
 while(bilet[i].pret>x)
 i++;
 while(bilet[j].pret<x)
 j--;
 
 if(i<=j)
 {
 temp=bilet[i]; 
 bilet[i]=bilet[j];
 bilet[j]=temp;
 i++;
 j--;
 }
}while(i<=j);

if(left<j)
quicksort(bilet,left,j);
if(right>i)
quicksort(bilet,i,right);
}


int main()
{
    
    int indexfarareducere;
    int n;
    scanf("%d",&n);
    
    for(int i=0;i<n;i++)
    {
        scanf("%s",&a[i].destinatie);
        scanf("%f",&a[i].pret);
        scanf("%d",&a[i].reducere);
        b[i]=a[i];
    }
    
    printf("Datele inainte de sortare:\n");
    for(int i=0;i<n;i++)
    {
        printf("%s ",a[i].destinatie);
        printf("%f ",a[i].pret);
        printf("%d ",a[i].reducere);
        printf("\n");
    }
    
    shellsort(a,n);
    
    printf("Datele dupa shellsort:\n");
      for(int i=0;i<n;i++)
    {
        printf("%s ",a[i].destinatie);
        printf("%f ",a[i].pret);
        printf("%d ",a[i].reducere);
        printf("\n");
    }
    
    for(int i=0;i<n;i++)
    {
        if(a[i].reducere==0)
        {
            indexfarareducere=i; //am gasit primul bilet fara reducere
            break;
        }
    }
    
    quicksort(a,indexfarareducere,n);
    

    
    printf("Datele dupa quicksort:\n");
      for(int i=0;i<n;i++)
    {
        printf("%s ",a[i].destinatie);
        printf("%f ",a[i].pret);
        printf("%d ",a[i].reducere);
        printf("\n");
    }
    
    
 
    
    return 0;
}