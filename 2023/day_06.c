#include <stdio.h>
#include <stdlib.h>


void incr(int* digits, int n){

    int i = 0;
    while (digits[i] == 9 && i < n){
        digits[i] = 0;
        i ++;
    }
    if (i < n){
        digits[i] += 1;
    }

}


void waysToWin(double time, double distance){


    int *digits = (int*)malloc(sizeof(int) * 10);
    for (int i = 0; i < 10; i++)
    {
        digits[i] = 0;
    }
    

    for (double i = 1; i < time; i++){
        if ((time - i) * i > distance)
        {
            incr(digits, 10);
        }
    }

    for (int i = 9; i >= 0; i--)
    {
        printf("%d", digits[i]);
    }
    printf("\n");
}



int main(void){

    /*
    int times[4] = {58, 99, 64, 69};
    int distances[4] = {478, 2232, 1019, 1071};

    int total = 1;

    for (int i = 0; i < 4; i++)
    {
        total *= waysToWin(times[i], distances[i]);
    }

    printf("total01 = %d\n", total);*/
    
    waysToWin(58996469, 478223210191071);


    return 0;
}