#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


typedef struct Segment{
    double key_start;
    double key_end;
    double value_start;
    double value_end;
} segment;

typedef struct Table{
    segment *segments;
    int nb_segments;
} table;



int nbLignes(char* path){
    FILE* text = fopen(path, "r");
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

int nbChar(char* path){
    FILE* text = fopen(path, "r");
    int cpt = 0;
    char temp;
    fseek(text, 0, SEEK_SET);
    while (fscanf(text, "%c", &temp) != EOF){
        cpt ++;
    }
    fclose(text);
    return cpt;
}

segment creerSegment(char* ligne){

    double tab[3] = {0,0,0}; 

    int temp_rg = 0;
    for (int k = 0; k < 3; k++)
    {   
        int i = 0;
        while (isdigit(ligne[i + temp_rg])){
            i ++;
        }
        temp_rg = i + 1;

        double temp = 0;
        int exp = 1;
        for ( ;i >= 0; i--){
            temp += ((ligne[i + temp_rg] - '0') * exp);
            exp *= 10;
        }
        tab[k] = temp;
    }
    
    segment seg = {.key_start = tab[1], .key_end = tab[1] + tab[2] - 1, 
                   .value_start = tab[0], .value_end = tab[0] + tab[2] - 1};
    
    return seg; 
}

table* inputTable(char* path){

    int nb_lignes = nbLignes(path) + 1;
    int nb_char = nbChar(path);

    FILE* file = fopen(path, "r");
    fseek(file, 0, SEEK_SET);


    // load text
    char *text = (char*)malloc(sizeof(char) * nb_char);
    char temp;
    int k = 0;
    while (fscanf(file, "%c", &temp) != EOF){
        text[k] = temp;
        k ++;
    }

    printf("%s", text);

    // load end of lines
    int *rgs_fin_lignes = (int*)malloc(sizeof(int) * (nb_lignes + 1));
    rgs_fin_lignes[0] = 0;
    int cpt = 1;
    for (int i = 0; i < nb_char; i++)
    {
        if (text[i] == '\n'){
            rgs_fin_lignes[cpt] = i;
            cpt ++;
        }
    } 
    
   
    char **lignes = (char**)malloc(sizeof(char*) * nb_lignes);
    

    for (int i = 0; i < nb_lignes; i++)
    {   
        for (int j = rgs_fin_lignes[i]; j < rgs_fin_lignes[i+1]; j++)
        {
           lignes[i][j] = text[j];
        }
        
        printf("ligne %s\n", lignes[i]);
    }

    table *tab = (table*)malloc(sizeof(table));
    tab->nb_segments = nb_lignes;
    tab->segments = (segment*)malloc(sizeof(segment) * nb_lignes);

    printf("hello world\n");

    for (int i = 0; i < nb_lignes; i++)
    {
        tab->segments[i] = creerSegment(lignes[i]);
    }
    
    printf("table created successfully\n");
    return tab;
}

double valFromKey(table *tab, double key){

    int i = 0;
    int len = tab->nb_segments;
    while (i < len && (tab->segments[i].key_start > key || tab->segments[i].key_end < key)){
        i ++;
    }

    segment seg = tab->segments[i];

    if (i == len - 1 && (seg.key_start > key || seg.key_end < key)){
        return key;
    }

    else{
        double res = key - (seg.key_start - seg.value_start);
        return res;
    }
}



int main(void){

    char *tables_paths[7] = {
        "table01.txt", "table02.txt", "table03.txt", "table04.txt",
        "table05.txt", "table06.txt", "table07.txt"};

    table **tables = (table**)malloc(sizeof(table*) * 7);
    for (int i = 0; i < 7; i++){
        tables[i] = inputTable(tables_paths[i]);
    }
    
    double seeds[20] = {2041142901, 113138307, 302673608, 467797997, 1787644422,
    208119536, 143576771, 99841043, 4088720102, 111819874, 946418697, 13450451,
    3459931852, 262303791, 2913410855, 533641609, 2178733435, 26814354, 1058342395, 175406592};

    int *targets = (int*)malloc(sizeof(int)*20);

    for (int i = 0; i < 20; i++)
    {
        double temp = seeds[i];
        for (int j = 0; j < 7; j++){
            temp = valFromKey(tables[j], temp);
        }
        targets[i] = temp;
    }
    
    for (int i = 0; i < 20; i++)
    {
        printf("%d  ", targets[i]);
    }
    
    return 0;
}