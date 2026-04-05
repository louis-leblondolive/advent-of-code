#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>


typedef struct Node{
    char* name;
    
    char* left_name;
    struct Node* left;

    char* right_name;
    struct Node* right;
} node;

typedef struct Camel{
    node* position;
    int zFirstOccur;
    long long cycleLen;
} camel;


int rankInList(node* list, int n, char* x_name){
    for (int i = 0; i < n; i++){
        if (list[i].name[0] == x_name[0] && list[i].name[1] == x_name[1] && list[i].name[2] == x_name[2]){
            return i;
        }
    }
    //printf("not in list name %s\n", x_name);
    return -1;
}

camel* startCamels(node* list, int n){

    int cpt = 0;
    for (int i = 0; i < n; i++){
        if (list[i].name[2] == 'A') cpt ++;
    }
    
    camel *camels = (camel*)malloc(sizeof(camel) * cpt);

    int j = 0;
    for (int i = 0; i < n; i++){
        if (list[i].name[2] == 'A'){
             camel cam = { .position = &list[i], .cycleLen = 0, .zFirstOccur = 0};
             camels[j] = cam;
             j++;
        }
    }  
    return camels;
}

node inputNode(char* name, char* left_name, char* right_name){
    node nd = {.name = name, .left_name = left_name, .right_name = right_name};
    return nd;
}

bool haveAllArrived(node* camels, int n){
    for (int i = 0; i < n; i++){
        if (camels[i].name[2] != 'Z') return false;
    }
    return true;
}

void setCamelCycle(camel* camel, char* inst){
    node *startPosition = camel->position;
    long long cpt = 0;
    int i = 0;

    printf("setting camel \n");
    bool foundOccur = false;

    while (true){
        if (inst[i] == 'R'){
            camel->position = camel->position->right;
        }
        if (inst[i] == 'L'){
            camel->position = camel->position->left;
        }
        i ++;
        cpt ++;
        if (i >= strlen(inst)) i = 0;
        if (camel->position->name[2] == 'Z' && !foundOccur) {
            camel->zFirstOccur = cpt;
            foundOccur = true;
            printf("found z first occur at %lld\n", cpt);
            return;
        }
    }
}

int nbLignes(void){
    FILE* text = fopen("input_day_08.txt", "r");
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
    FILE* text = fopen("input_day_08.txt", "r");
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

    int nb_lignes = nbLignes() + 1;
    int nb_char = nbChar();

    FILE* file = fopen("input_day_08.txt", "r");
    fseek(file, 0, SEEK_SET);
    
    // load text
    char *text = (char*)malloc(sizeof(char) * nb_char);
    char temp;
    int k = 0;
    while (fscanf(file, "%c", &temp) != EOF){
        text[k] = temp;
        k ++;
    }

    node* node_list = (node*)malloc(sizeof(node) * nb_lignes);

    for (int i = 0; i < nb_lignes; i++)
    {
        char* name = (char*)malloc(sizeof(char) * 3);
        name[0] = text[17*i];
        name[1] = text[17*i + 1];
        name[2] = text[17*i + 2];

        char* name_L = (char*)malloc(sizeof(char) * 3);
        name_L[0] = text[17*i + 7];
        name_L[1] = text[17*i + 8];
        name_L[2] = text[17*i + 9];

        char* name_R = (char*)malloc(sizeof(char) * 3);
        name_R[0] = text[17*i + 12];
        name_R[1] = text[17*i + 13];
        name_R[2] = text[17*i + 14];

        node_list[i] = inputNode(name, name_L, name_R);
    }
    
    for (int i = 0; i < nb_lignes; i++)
    {
        node_list[i].left = &node_list[rankInList(node_list, nb_lignes, node_list[i].left_name)];
        node_list[i].right = &node_list[rankInList(node_list, nb_lignes, node_list[i].right_name)];
    }
    
    char* inst = "LRRLRRLLRRRLRRLRLRRRLRRLRRRLRLLRRRLRRRLRLRRRLRRLRRLRLRLLLRRRLRRRLRRLRRLRLRRRLRRLLRRLRRLRLLRLRLRRLRLLRLRLRRRLRRLRLLRLRLLRRLRLRRLLLRLRRLRRRLLLRRLRLRRRLLRRLLLRRRLRRRLLLRRLLRLRRLRLRRLLLRLRRLLLLRRLLRRRLRRLRRLRLRLLRLRRRLLRRLLRRLRRLRRLRRLRLLRRLRRRLRLRLLLRRRLLRRRLRRLRRLLLLRRRR";

    camel *camels = startCamels(node_list, nb_lignes);
    int camels_len = 0;
    for (int i = 0; i < nb_lignes; i++){
        if (node_list[i].name[2] == 'A') camels_len ++;
    }
    
    for (int i = 0; i < camels_len; i++)
    {
        setCamelCycle(&camels[i], inst);
    }
    
    for (int i = 0; i < camels_len; i++)
    {
        printf("camel %d has cycleLen of %d and z first occur of %d\n", i, camels[i].cycleLen, camels[i].zFirstOccur);
    }
    

    /* PROBLEME 01
    node current_node = node_list[rankInList(node_list, nb_lignes, "AAA")];

    while (current_node.name[0] != 'Z'
        && current_node.name[1] != 'Z'
        && current_node.name[2] != 'Z' ) {
            
            printf("at node %s\n", current_node.name);
            if (inst[i] == 'R'){
                current_node = *current_node.right;
            }
            if (inst[i] == 'L'){
                current_node = *current_node.left;
            }
            i += 1;
            total += 1;
            if (i >= strlen(inst)){
                i = 0;
            }
    } */

    return 0;
}