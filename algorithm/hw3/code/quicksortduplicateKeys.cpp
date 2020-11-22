// C++ program for 3-way quick sort 
#include <stdio.h> 
using namespace std; 

void swap(int *xp, int *yp) 
{ 
    int temp = *xp; 
    *xp = *yp; 
    *yp = temp; 
} 
// This function partitions an array into 3 parts: smaller, equal to, & greater than pivot
void partition(int a[], int l, int r, int &i, int &j) 
{ 
    i = l-1, j = r; 
    int p = l-1, q = r; 
    int v = a[r]; 
  
    while (true) 
    { 
        // From left, find the first element greater than or equal to v. 
        // This loop will definitely terminate as v is last element 
        while (a[++i] < v); 
  
        // From right, find the first element smaller than or equal to v 
        while (v < a[--j]) 
            if (j == l) 
                break; 
  
        // If i and j cross, then we are done 
        if (i >= j) break; 
  
        // Swap, so that smaller goes on left greater goes on right 
        swap(&a[i], &a[j]); 
  
        // Move all same left occurrence of pivot to beginning of array & keep count using p 
        if (a[i] == v) 
        { 
            p++; 
            swap(&a[p], &a[i]); 
        } 
  
        // Move all same right occurrence of pivot to end of array & keep count using q 
        if (a[j] == v) 
        { 
            q--; 
            swap(&a[j], &a[q]); 
        } 
    } 
  
    // Move pivot element to its correct index 
    swap(&a[i], &a[r]); 
  
    // Move all left same occurrences from beginning to adjacent to arr[i] 
    j = i-1; 
    for (int k = l; k < p; k++, j--) 
        swap(&a[k], &a[j]); 
  
    // Move all right same occurrences from end to adjacent to arr[i] 
    i = i+1; 
    for (int k = r-1; k > q; k--, i++) 
        swap(&a[i], &a[k]);

    // count++;
    int m; 
    printf("Sorting step ... :\n");
    for (m=0; m < r; m++) 
        printf("%d ", a[m]); 
    printf("\n");
} 
  
// 3-way partition based quick sort 
void quicksort(int a[], int l, int r) 
{ 
    if (r <= l) return; 
    int i, j; 
    // Note that i and j are passed as reference 
    partition(a, l, r, i, j); 
    // Recur 
    quicksort(a, l, j); 
    quicksort(a, i, r); 
} 

// A utility function to print an array 
void printArr(int a[], int n) 
{ 
    for (int i = 0; i < n; ++i) 
        printf("%d  ", a[i]); 
    printf("\n"); 
} 
  
// Main program 
int main() 
{ 
    int a[] = {4, 9, 4, 4, 1, 9, 4, 9, 4, 4, 1, 4, 15, 21, 16, 15, 21, 21, 15, 16, 16}; 
    int size = sizeof(a) / sizeof(int); 
    printf("The input array: \n");
    printArr(a, size); 
    quicksort(a, 0, size - 1); 
    printf("The sorted array: \n");
    printArr(a, size); 
    return 0; 
} 