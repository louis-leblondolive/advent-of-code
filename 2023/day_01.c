#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>


int nbLignes(void){
    FILE* text = fopen("input_day_01.txt", "r");
    int cpt = 0;
    char temp;
    fseek(text, 0, SEEK_SET);
    while (fscanf(text, "%c", &temp) != EOF){
        if (temp == '\n'){
            cpt ++;
        }
    }
    fclose(text);
    return cpt;
}

int nbChar(void){
    FILE* text = fopen("input_day_01.txt", "r");
    int cpt = 0;
    char temp;
    fseek(text, 0, SEEK_SET);
    while (fscanf(text, "%c", &temp) != EOF){
        cpt ++;
    }
    fclose(text);
    return cpt;
}



int main(void){

    int nb_lignes = nbLignes();
    int nb_char = nbChar();

    FILE* file = fopen("jour01.txt", "r");
    fseek(file, 0, SEEK_SET);
    
    char *text = (char*)malloc(sizeof(char) * nb_char);
    int *rgs_fin_lignes = (int*)malloc(sizeof(int) * (nb_lignes + 1));
    rgs_fin_lignes[0] = -1;


    char temp;
    int k = 0;
    while (fscanf(file, "%c", &temp) != EOF){
        text[k] = temp;
        k ++;
    }
    
    int cpt = 1;
    for (int i = 0; i < nb_char; i++)
    {
        if (text[i] == '\n'){
            rgs_fin_lignes[cpt] = i;
            cpt ++;
        }
    }

    int total = 0;
    
    for (int i = 0; i < nb_lignes + 1; i++)
    {
        int min = 0;
        int max = 0;
        int j = rgs_fin_lignes[i] + 1;
        printf("first char %c\n", text[j]);
        

        while(!isdigit(text[j])){
            j++;
            
        }
        printf("min %c\n", text[j]);
        min = text[j] - '0';

        max = min;
        while (j < rgs_fin_lignes[i+1]){
            if (isdigit(text[j])){
                max = text[j] - '0';
            }
            j ++;
        }

        total += 10 * min + max;
        printf("sous total : %d\n", 10*min+max);
    }
    


    printf("%d\n", total);

    fclose(file);
    return 0;
}