// This function check whether an array is a maxheap or not 
#include <iostream>
#include <stdio.h>
#include <cstdlib>

bool maxS(int S[], int L[], int C[], int n) 
{ 
    int i, j,k, max, pre_event=0;
    max = 0; 
    int m;
    for (i = 0; i<n; i++) 
    {
      if (i == 0)
      {
        S[i] = 1;
        pre_event = -1;
      }
      else
      {
        int O[i] = { };
        for (j = 0; j < i; j++)
        {
            O[j] = -1;
            if ((i-j) >= abs(C[i]-C[j]))
                O[j] = j;
        }
        for (j = i; j >0; j--)
        {
            if (not(O[j-1] == -1))
            {
                if (max < S[j-1])
                {
                    max = S[j-1];
                    pre_event = j-1;
                }
            }
        }
        if ((pre_event == 0) and (not(i == 1)))
            pre_event = -1;
        S[i] = max+1;
        max = 0;
      }
      L[i] = pre_event;
      pre_event = 0;
    }

    // If must seen an event K at last
    int get_preEvent = 0;
    
    int g = 0;
    for (g = n; g>0;g--)
    {
        for (k = n; k > 0; k--)
        {
            if (k == g)
            {
                printf("If event %2i must be seen\n", k-1);
                printf("There are %2i events in total can be seen \n", S[k-1]);
                get_preEvent = L[k-1];
                if (not(get_preEvent == -1))
                    printf("go to Event %2i \n", get_preEvent);
            }
            if (k == (get_preEvent+1))
            {
                get_preEvent = L[k-1];
                if (not(get_preEvent == -1))
                    printf("go to Event %2i \n", get_preEvent);
            }
        }
    }
    return true;
}

/* Function to print an array */
void printArray(int arr[], int size) 
{ 
    int i; 
    for (i=0; i < size; i++) 
        printf("%4i ", arr[i]); 
    printf("\n"); 
} 
  
// Main program to test above functions 
int main() 
{ 
    // input part
    int Event[] = {0, 1, 2,3,4,5,6,7,8,9};
    int Coor[] = {0, 1, -4, -1, 4, 5, -4, 6, 7,-2};
    int n = sizeof(Event)/sizeof(Event[0]);
    int S[n] ={}, i, L[n] ={};
 
    //print out the input data
    printf("The input array (event and coordinate ist): \n");
    printArray(Event, n);
    printArray(Coor, n);
    printf("\n");
    
    // call the maxS function
    maxS(S, L, Coor, n);
    printf("\n");

    std::cout << "Maximum number of event can be seen if event (0 to 9) MUST be seen: \n";
    for (i = 0; i<n; i++) 
       printf("%2i ", i);
    printf("\n");
    for (i = 0; i<n; i++) 
       printf("%2i ", S[i]);
    printf("\n");
    printf("\n");
    
    std::cout << "Previous event List to reach maximum number of event can be seen for event (0 to 9):\n";
    std::cout << "(-1 value means that event is the starting event)\n";
    for (i = 0; i<n; i++) 
       printf("%2i ", i);
    printf("\n");
    for (i = 0; i<n; i++) 
       printf("%2i ", L[i]);
    printf("\n");
    return 0; 
} 
