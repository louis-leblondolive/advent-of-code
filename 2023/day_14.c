#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>


typedef struct Rock {
    int x;
    int y;
} rock;


void slideBouldersNorth(char **map, rock **rocks, int n, int rows){
        for (int i = 0; i < n; i++){
        rock *boulder = rocks[i];
            while (boulder->x > 0) {
                if (map[boulder->x - 1][boulder->y] != '#'){
                    char temp = map[boulder->x - 1][boulder->y];
                    map[boulder->x - 1][boulder->y] = map[boulder->x][boulder->y];
                    map[boulder->x][boulder->y] = temp;
                    boulder->x --;
                }
                else{
                    break;
                }
            }
        }
}

void slideBouldersSouth(char **map, rock **rocks, int n, int rows){
    
    for (int i = n - 2; i >=  0; i--){
       rock *boulder = rocks[i];
       while (boulder->x < rows){
            if (map[boulder->x + 1][boulder->y] != '#'){
                char temp = map[boulder->x + 1][boulder->y];
                map[boulder->x + 1][boulder->y] = map[boulder->x][boulder->y];
                map[boulder->x][boulder->y] = temp;
                boulder->x ++;
            }
            else{
                break;
            }
       }
    }
}

void slideBouldersEast(char **map, rock **rocks, int n, int columns){
    for (int i = 0; i < n; i++){
       rock *boulder = rocks[i];
       while (boulder->y < columns){
            if (map[boulder->x][boulder->y + 1] != '#'){
                char temp = map[boulder->x][boulder->y + 1];
                map[boulder->x][boulder->y + 1] = map[boulder->x][boulder->y];
                map[boulder->x][boulder->y] = temp;
                boulder->y ++;
            }
            else{
                break;
            }
       }
    }
}

void slideBouldersWest(char **map, rock **rocks, int n, int columns){
       
        for (int i = n - 2; i >= 0; i--){
        rock *boulder = rocks[i];
        while (boulder->y > 0){
                if (map[boulder->x][boulder->y - 1] != '#'){
                    char temp = map[boulder->x][boulder->y - 1];
                    map[boulder->x][boulder->y - 1] = map[boulder->x][boulder->y];
                    map[boulder->x][boulder->y] = temp;
                    boulder->y --;
                }
                else{
                    break;
                }
        }
    }
}

/*
void slideBouldersNorth(char **map, int rows, int column){
    for (int i = 1; i < rows; i++){
        for (int j = 0; j < column; j++){
           
           int k = i;
           while(k > 0){
                if (map[k-1][j] == '.' && map[k][j] == 'O'){
                   map[k-1][j] = 'O';
                   map[k][j] = '.';
                   k --;
                }
                else{
                    k = 0;
                }
           }
        }
    }
}

void slideBouldersSouth(char **map, int rows, int column){
    for (int i = rows - 2; i >= 0; i--){
        for (int j = 0; j < column; j++){
           int k = i;
           while(k < rows){
                if (map[k+1][j] == '.' && map[k][j] == 'O'){
                   map[k+1][j] = 'O';
                   map[k][j] = '.';
                   k ++;
                }
                else{
                    k = rows;
                }
           }
        }
    }
}

void slideBouldersEast(char **map, int rows, int column){
    for (int j =  column - 2; j >= 0; j--){
        for (int i = 0; i < rows; i++){
           int k = j;
           while(k < column){
                if (map[i][k+1] == '.' && map[i][k] == 'O'){
                   map[i][k+1] = 'O';
                   map[i][k] = '.';
                   k ++;
                }
                else{
                    k = column;
                }
           }
        }
    }
}

void slideBouldersWest(char **map, int rows, int column){
    for (int j = 1; j < column; j++){
        for (int i = 0; i < rows; i++){
           int k = j;
           while(k > 0){
                if (map[i][k-1] == '.' && map[i][k] == 'O'){
                   map[i][k-1] = 'O';
                   map[i][k] = '.';
                   k --;
                }
                else{
                    k = 0;
                }
           }
        }
    }
}
*/
void cycle(char **map, rock **rocks, int n, int rows, int column){
    slideBouldersNorth(map, rocks, n, rows);
    slideBouldersWest(map, rocks, n, column);
    slideBouldersSouth(map, rocks, n, rows);
    slideBouldersEast(map, rocks, n, column);
}

int nbOccur(char *L, int n, char c){
    int cpt = 0;
    for (int i = 0; i < n; i++){
        if (L[i] == c) cpt ++;
    }
    return cpt;
}

int totalBouldersLoad(char **map, int rows, int column){

    int total = 0;
    for (int i = 0; i < rows; i++){
        total += (rows - i) * nbOccur(map[i], column, 'O');
    }
    return total;
}

void printMap(char **map, int n){
    for (int i = 0; i < n; i++)
    {
        printf("%s", map[i]);
    }
    printf("\n");
    printf("\n");
}


int main(void){

    int rows = 101;
    int column = 101;

    FILE* file = fopen("input_day_14.txt", "r");
    fseek(file, 0, SEEK_SET);
    
    char *text = (char*)malloc(sizeof(char) * rows * column);

    char temp;
    int k = 0;
    while (fscanf(file, "%c", &temp) != EOF){
        text[k] = temp;
        k ++;
    }
    
    char **map = malloc(sizeof(char*) * rows);
    for (int i = 0; i < rows; i++){
        map[i] = (char*)malloc(sizeof(char) * column);
        for (int j = 0; j < column; j++)
        {
            map[i][j] = text[rows * i + j];
        }
        //printf("%s", map[i]);
    }
    //printf("\n");
    int n = 0;
    for (int i = 0; i < rows; i++){
        for (int j = 0; j < column; j++){
            if (map[i][j] == 'O'){
                n ++;
            }
        }
    }
    rock **rocks = (rock**)malloc(sizeof(rock*) * n);
    int cpt = 0;
    for (int i = 0; i < rows; i++){
        for (int j = 0; j < column; j++){
            if (map[i][j] == 'O'){
                rock *r = malloc(sizeof(rock));
                r->x = i;
                r->y = j;
                rocks[cpt] = r;
                cpt ++;
                //printf("rock created at %d;%d\n", i, j);
            }
        }
    }
    
    
    for (int i = 0; i < 1000000000; i++)
    {
        cycle(map, rocks, n, rows, column);
        if (i % 100000 == 0) printf("done %d\n", i);
    }
    

    int total = totalBouldersLoad(map, rows - 1, column - 1);
    
    printf("total = %d\n", total);

    return 0;
}