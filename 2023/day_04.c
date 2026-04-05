#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>


bool in_list(int *tab, int n, int x){

    for (int i = 0; i < n; i++){
        if (tab[i] == x){
            tab[i] = -1;
            return true;
        }
    }
    return false;
}

int points(char *line){

    int *winning_nbs = (int*)malloc(sizeof(int) * 10);
    int *card_nbs = (int*)malloc(sizeof(int) * 25);

    for (int i = 0; i < 10; i++){
        
        winning_nbs[i] = 0;
        if (isdigit(line[10 + i*3])){
            winning_nbs[i] += 10 * (line[10 + i*3] - '0');
        }
        if (isdigit(line[10 + i*3 + 1])){
            winning_nbs[i] += (line[10 + i*3 + 1] - '0');
        }
    }
    
    for (int i = 0; i < 25; i++)
    {
        card_nbs[i] = 0;
        if (isdigit(line[42 + i*3])){
            card_nbs[i] += 10 * (line[42 + i*3] - '0');
        }
        if (isdigit(line[42 + i*3 + 1])){
            card_nbs[i] += (line[42 + i*3 + 1] - '0');
        }
    }

    int subtotal = 0;

    for (int i = 0; i < 25; i++)
    {   
        if (in_list(winning_nbs, 10, card_nbs[i])){
            printf("found winning %d in numbers \n", card_nbs[i]);
            if (subtotal == 0){
                subtotal = 1;
            }
            else{
                subtotal *= 2;
            }
        }
    }
    printf("end card, worth %d points\n\n", subtotal);
    return subtotal;
}

int nbLignes(void){
    FILE* text = fopen("input_day_04.txt", "r");
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
    FILE* text = fopen("input_day_04.txt", "r");
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

    FILE* file = fopen("input_day_04.txt", "r");
    fseek(file, 0, SEEK_SET);


    // load text
    char *text = (char*)malloc(sizeof(char) * nb_char);
    char temp;
    int k = 0;
    while (fscanf(file, "%c", &temp) != EOF){
        text[k] = temp;
        k ++;
    }

    // load lines
    char **lignes = (char**)malloc(sizeof(char*) * nb_lignes);
    
    for (int i = 0; i < nb_char / 116; i++)
    {
        lignes[i] = (char*)malloc(sizeof(char) * 117);
        
        for (int j = 0; j < 117; j++)
        {
            lignes[i][j] = text[117*i + j];
        }
        
        printf("ligne %s\n", lignes[i]);
    }
    
    // calcul
    int total = 0;
    for (int i = 0; i < nb_char / 116 - 1; i++)
    {
        total += points(lignes[i]);
    }
    
    printf("total = %d\n", total);

    return 0;
}