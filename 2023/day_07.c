#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>


typedef struct Hand{
    char* physicalCards;
    int* cards;             // represented as 4 , 0, 1, ...
                            //            for A, T, Q, ...
    int bid;
} hand;

int nbLignes(void){
    FILE* text = fopen("input_day_07.txt", "r");
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
    FILE* text = fopen("input_day_07.txt", "r");
    int cpt = 0;
    char temp;
    fseek(text, 0, SEEK_SET);
    while (fscanf(text, "%c", &temp) != EOF){
        cpt ++;
    }
    fclose(text);
    return cpt;
}

bool inList(int* list, int n, int x){
    printf("start in\n"); 
    printf("%d\n", list[0]);
    for (int i = 0; i < n; i++){
        if (list[i] == x) return true;
    }
    printf("end in\n");
    return false;
}

int occur(int* list, int n, int x){
    int cpt = 0;
    for (int i = 0; i < n; i++)
    {
        if (list[i] == x) cpt ++;
    }
    printf("end occur\n");
    return cpt;
}

int handStrenght(hand h){
    
    printf("strengt %s\n", h.physicalCards);
    if (inList(h.cards, 13, 5)) return 7;
    if (inList(h.cards, 13, 4)) return 6;
    if (inList(h.cards, 13, 3)){
        if (inList(h.cards, 13, 2))  return 5;
        else return 4;
    }
    if (occur(h.cards, 13, 2) >= 2) return 3;
    if (occur(h.cards, 13, 2) == 1 && occur(h.cards, 13, 1) == 3) return 2;
    if (occur(h.cards, 13, 1) == 5) return 1;

    return 0;
}   

bool compareFigures(char fig01, char fig02){
    // returns true if fig 01 is stronger, else false
    // FIGURES HAVE TO BE DIFFERENT

    char figures[13] = {'A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2'};
    for (int i = 0; i < 13; i++){
        if (figures[i] == fig01) return true;
        if (figures[i] == fig02) return false;
    }
    return false;
}

bool compareHands(hand hand01, hand hand02){
    // returns true if hand 01 is stronger, else false
    printf("start comaprison %s  %s\n", hand01.physicalCards, hand02.physicalCards); 

    int s1 = handStrenght(hand01);
    int s2 = handStrenght(hand02);
    printf("end s\n");

    if (s1 == s2){
        //printf("equality between %s and %s\n", hand01.physicalCards, hand02.physicalCards);

        for (int i = 0; i < 5; i++){
            if (hand01.physicalCards[i] != hand02.physicalCards[i]){
                bool res = compareFigures(hand01.physicalCards[i], hand02.physicalCards[i]);
                return res;
            }
        }
    }

    if (s1 > s2) return true;
    if (s1 < s2) return false;

    
    return true;
}

hand makeHand(char* cards, int bid){

    hand* h = (hand*)malloc(sizeof(hand));
    h->bid = bid;
    h->physicalCards = cards;
    char figures[13] = {'A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2'};
    
    h->cards = (int*)malloc(sizeof(int) * 13);

    for (int i = 0; i < 13; i++)
    {
        h->cards[i] = 0;
    }
    

    for (int i = 0; i < 5; i++)
    {
        for (int j = 0; j < 13; j++){
            if(figures[j] == cards[i]){
                h->cards[j] += 1;
            }
        } 
    }

    printf("card %s created with bid %d\n", cards, bid);
    for (int i = 0; i < 13; i++)
    {
        printf("%d ", h->cards[i]);
    }
    printf("\n");

    return *h;
}


int main(void){

    int nb_lignes = nbLignes() + 1;
    int nb_char = nbChar();

    FILE* file = fopen("input_day_07.txt", "r");
    fseek(file, 0, SEEK_SET);
    
    // load text
    char *text = (char*)malloc(sizeof(char) * nb_char);
    char temp;
    int k = 0;
    while (fscanf(file, "%c", &temp) != EOF){
        text[k] = temp;
        k ++;
    }

    hand* hands = (hand*)malloc(sizeof(hand));
    for (int i = 0; i < nb_lignes; i++)
    {   
        
        char* cards = (char*)malloc(sizeof(char) * 5);
        for (int j = 0; j < 5; j++)
        {
            cards[j] = text[10*i + j];
        }
        int bid = 0;
        if (isdigit(text[6 + 10*i])) bid += 100 * (text[6 + 10*i] - '0');
        if (isdigit(text[7 + 10*i])) bid += 10 * (text[7 + 10*i] - '0');
        if (isdigit(text[8 + 10*i])) bid += (text[8 + 10*i] - '0');

        hands[i] = makeHand(cards, bid);
        
    }
    // ok

    printf("card0 %s", hands[0].physicalCards);

    for (int i = 1; i < nb_lignes; i++)
    {
        int j = i;
        while (compareHands(hands[j-1], hands[j]) && j > 1){
            printf("end comparison\n");
            hand temp = hands[j-1];
            hands[j-1] = hands[j];
            hands[j] = temp;
            j --;
        }
    }

    int total = 0;
    for (int i = 0; i < nb_lignes; i++)
    { 
        total += hands[i].bid * (i+1);
        if (hands[i].bid == 999){
            printf("found card %s at rank %d", hands[i].physicalCards, i+1);
        }
    }
    
    printf("total = %d\n", total);
    return 0;
}