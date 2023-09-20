// This function check whether an array is a maxheap or not 
#include <iostream>

bool checkMaxHeap(float arr[], int n) 
{ 
   int i, max;
   max = 0;   
   // running to the middle of the array
   for (i = 0; i < n/2+1; i++)
   {
      if (i == 0)
      {
        if (arr[1] > arr[2])
        {
            max = arr[1];
        }
        else
        {
            max = arr[2];
        }
      }
      else
      {
        if (arr[2*i+1] > arr[2*i+2])
            max = arr[2*i+1];
        else
            max = arr[2*i+2];
      }
      if(arr[i] < max)
      {
        std::cout << "This is NOT a max heap" << std::endl;
        return false;
      }
   }
   std::cout << "This is a max heap" << std::endl;
   return true;
}

/* Function to print an array */
void printArray(float arr[], int size) 
{ 
    int i; 
    for (i=0; i < size; i++) 
        printf("%5.2f ", arr[i]); 
    printf("\n"); 
} 
  
// Main program to test above functions 
int main() 
{ 
    // input part
    float arrA[] = {16, 14, 10, 8, 7, 9, 3, 2, 4, 1};
    float arrB[] = {10, 3, 9, 7, 2, 11, 5, 1, 6};
    int n = sizeof(arrA)/sizeof(arrB[0]);
    //print out the input data
    printf("The input array: \n");
    // printArray(arrA, n);
    printArray(arrB, n);
    printf("\n");
    
    // call the check MaxHeap function
    checkMaxHeap(arrB, n);
    return 0; 
} 
