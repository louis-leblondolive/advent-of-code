#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct Galaxy{
    int id;
    long long x;
    long long y;
    long long xf;
    long long yf;
} galaxy;

int nbLignes(void){
    FILE* text = fopen("input_day_11.txt", "r");
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
    FILE* text = fopen("input_day_11.txt", "r");
    int cpt = 0;
    char temp;
    fseek(text, 0, SEEK_SET);
    while (fscanf(text, "%c", &temp) != EOF){
        cpt ++;
    }
    fclose(text);
    return cpt;
}


galaxy* initGalaxies(char** map, int nb_lines, int nb_galaxies){

    galaxy *galaxies = malloc(sizeof(galaxy) * nb_galaxies);

    long long cpt = 0;
    for (long long i = 0; i < nb_lines; i++){
        for (long long j = 0; j < strlen(map[i]); j++){
            if (map[i][j] == '#'){
                galaxy g = {.x = i, .y = j, .id = cpt + 1, .xf = i, .yf = j};
                galaxies[cpt] = g;
                cpt ++;
            }
        }
    }
    
    return galaxies;
}

void offsetGalaxiesRow(int rowIndex, galaxy *galaxies, int nb_galaxies){
    long long offset = 999999;
   // printf("expansion row %d\n", rowIndex);
    for (int i = 0; i < nb_galaxies; i++){
        if (galaxies[i].x > rowIndex){
            galaxies[i].xf += offset;
            //printf("offset id %d\n", galaxies[i].id);
        }
    }
}
        
void offsetGalaxiesColumn(int columnIndex, galaxy *galaxies, int nb_galaxies){
    long long offset = 999999;
    //printf("expansion column %d\n", columnIndex);
    for (int i = 0; i < nb_galaxies; i++){
        if (galaxies[i].y > columnIndex){
            galaxies[i].yf += offset;
            //printf("offset id %d\n", galaxies[i].id);
        }
    }
}

void rowExpansion(char **map, int nb_lines, galaxy *galaxies, int nb_galaxies){

    for (int i = 0; i < nb_lines; i++){
        bool expand = true;
        for (int j = 0; j < strlen(map[i]); j++){
            if (map[i][j] == '#') expand = false;
        }
        if (expand) offsetGalaxiesRow(i, galaxies, nb_galaxies);
    }

}

void columnExpansion(char **map, int nb_lines, galaxy *galaxies, int nb_galaxies){
    for (int i = 0; i < nb_lines; i++){
        bool expand = true;
        for (int j = 0; j < nb_lines; j++){
            if (map[j][i] == '#') expand = false;
        }
        if (expand) offsetGalaxiesColumn(i, galaxies, nb_galaxies);
    }
}


int main(void){

    int size = 141;

    FILE* file = fopen("input_day_11.txt", "r");
    fseek(file, 0, SEEK_SET);
    
    char *text = (char*)malloc(sizeof(char) * size * size);

    char temp;
    int k = 0;
    while (fscanf(file, "%c", &temp) != EOF){
        text[k] = temp;
        k ++;
    }
    
    char **map = malloc(sizeof(char*) * size);
    for (int i = 0; i < size; i++){
        map[i] = (char*)malloc(sizeof(char) * size);
        for (int j = 0; j < size; j++)
        {
            map[i][j] = text[size * i + j];
        }
        //printf("%s", map[i]);
    }
    //printf("\n");

    int nb_galaxies = 0;
    for (int i = 0; i < size; i++){
        for (int j = 0; j < strlen(map[i]); j++){
            if (map[i][j] == '#') nb_galaxies++;
        }
    }

    galaxy *galaxies = initGalaxies(map, size,nb_galaxies);

    rowExpansion(map, size - 1, galaxies, nb_galaxies);
    columnExpansion(map, size - 1, galaxies, nb_galaxies);

    /*
    for (int i = 0; i < nb_galaxies; i++)
    {
        printf("%d, %d\n", galaxies[i].x, galaxies[i].y);
    } */

    long long total = 0;
    for (long long i = 0; i < nb_galaxies; i++){

        galaxy g = galaxies[i];

        for (long long j = i + 1; j < nb_galaxies; j++){
            total += ( llabs(g.xf - galaxies[j].xf) + llabs(g.yf - galaxies[j].yf) );
        }
    }
    
    
    printf("total = %lld\n", total);

    return 0;
}