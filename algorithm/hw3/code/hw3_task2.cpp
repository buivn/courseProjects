// Optimized implementation of Bubble sort 
#include <stdio.h> 
void swap(char *xp, char *yp) 
{ 
    char temp = *xp; 
    *xp = *yp; 
    *yp = temp; 
} 
// An optimized version of Bubble Sort 
void bubbleSort_variant(char arr[], int n) 
{ 
   int i, j, m, l, start, end; 
   // bool swapped;
   start = 0;
   end = 0;
   bool still_loop = true;
   while (still_loop)
   {
      // loop run  from left to right
     for (i = start; i < n-1-end; i++) 
     {  
       for (j = i; j < n-1-end; j++) 
       { 
          if (arr[j] > arr[j+1]) 
          { swap(&arr[j], &arr[j+1]); } 
       }
     }
     printf("Loop softing from left to right\n");
     for (k=0; k < n; k++)
        printf("%c ", arr[k]);
     printf("\n");
     end++;
     // for loop from right to left
     for (m = n-1-end; m > start; m--) 
     { 
        for (l = m; l > start; l--)
          if (arr[l] < arr[l-1]) 
          {  swap(&arr[l], &arr[l-1]); } 
     }
     start++;
     if ((end+start) > n-4)
     {
        still_loop = false;
        // break;
     }
     printf("Loop sorting from right to left\n");
     for (k=0; k < n; k++)
        printf("%c ", arr[k]);
     printf("\n");
    } 
}  
/* Function to print an array */
void printArray(char arr[], int size) 
{ 
    int i; 
    for (i=0; i < size; i++) 
        printf("%c ", arr[i]); 
    printf("\n"); 
} 
  
// Main program to test above functions 
int main() 
{ 
    char arr[] = {'E', 'A', 'S', 'Y', 'Q', 'U', 'E', 'S', 'T', 'I', 'O', 'N'}; 
    int n = sizeof(arr)/sizeof(arr[0]);
    printf("The input array: \n");
    printArray(arr, n);  
    bubbleSort_variant(arr, n); 
    printf("\n");
    printf("Sorted array: \n"); 
    printArray(arr, n); 
    return 0; 
} 
