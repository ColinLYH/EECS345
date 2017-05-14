/* Aditya 
 * PLC HW3
 */
 
#include <stdio.h>
#include <stdlib.h>

//Struct holding the data plus pointers to prev and next nodes.
//I changed data type to int so it would work correctly 
typedef struct Node {
    int data;
    struct Node *next;
    struct Node *prev;
}Node;

//pointers in the list struct to first and last nodes
typedef struct List {
    struct Node *head;
    struct Node *tail;
}List;

///////////////////////Functions/////////////////////////////
void print_array(int array[], int length);
void compare_array(int array1[], int array2[], int length);
void add_to_front(int elem, List *ls);
void add_to_back(int elem, List *ls);
int remove_from_front(List *ls);
int remove_from_back(List *ls);
void transfer(int array1[], int array2[], int length, void (*insert)(int elem, List *ls), int (*remove)(List * l));
/////////////////////////////////////////////////////////////

void add_to_front(int elem, List * ls) {
    //Allocate memory for new node 
    Node *newNode = (Node *)malloc(sizeof(Node));
    // after making the newNode, we will set its data to the incoming data arg. Then we set the back pointer of the new node to NULL
    newNode->data = elem;
    newNode->prev = NULL;
   //Check to see if there is no tail in ls, aka empty list
    if(ls->tail == NULL) {
        //newNode->data = elem;
        newNode->next = NULL;
        //newNode->prev = NULL;
        ls->head = newNode;
        ls->tail = newNode;
    }
    else {
        //newNode->data = elem;
        //newNode->prev = NULL;
        ls->head->prev = newNode;
        newNode->next = ls->head;
        ls->head = newNode;
    }
}

void add_to_back(int elem, List *ls) {
    //Allocate memory for new node
    Node *newNode = (Node*)malloc(sizeof(Node));
    //Make a new node using the element data, giving the next pointer null 
    newNode->data = elem;
    newNode->next = NULL;
  //set the lists head pointer to the newly created node if !ls->head, else point the prev tail to the new node,
  //and lastly set the ls tail pointer to the new node    
    if (ls->head == NULL) {
        //newNode->data = elem;
        //newNode->next = NULL;
        newNode->prev = NULL;
        ls->tail = newNode;
        ls->head = newNode;
    } else {
        //newNode->data = elem;
        //newNode->next = NULL;
        ls->tail->next = newNode;
        newNode->prev = ls->tail;
        ls->tail = newNode;
    }
}

int remove_from_front(List *ls) {
  //So first it will store the removedData from the head, the free the memory and lastly set the list's header 
  //pointer to the next node and return the removed data
    int removedData = ls->head->data;
    //now we "free" the node
    ls->head = ls->head->next;
  //and if the node is empty we set pointer of tail to NULL
    if(ls->head == NULL) {
        ls->tail = NULL;
    }
    return removedData;
}

int remove_from_back(List *ls) {
    //works the same way as above but uses pointers to the tail and prev data instead 
    int removedData = ls -> tail -> data;
    //now we "free" the node 
    ls -> tail = ls -> tail -> prev;
  //and if the node is empty we set pointer of head to NULL
    if (ls->tail == NULL) {
        ls->head = NULL;
    }
    return removedData;
}

void transfer(int array1[], int array2[], int length, void (*insert)(int elem, List *ls), int (*remove)(List *ls)) {
    //create the new empty linked list 
    List * ls = (List *) malloc(sizeof(List));
    //now insert all the elements in order, first adding to front
    for (int i = 0; i < length; ++i) {
        insert(array1[i],ls); 
    }
    //and now removing from the back 
    int j = 0;
    while(ls->tail != NULL) {
        array2[j] = remove(ls);
        j++;
    }
}

int main() {
    int array1[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int array2[10] = {};
    transfer(array1, array2, 10, &add_to_front, &remove_from_front);
    printf("Array 1: {");
    for(int i = 0; i < 10; ++i) { 
      printf("%d ",array1[i]);
    }
    printf("} Array 2: {");
    for(int j = 0; j < 10; ++j) {
      printf("%d ",array2[j]);
    }
    printf("}\n");

    int array3[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int array4[10] = {};
    transfer(array3, array4, 10, &add_to_front, &remove_from_back);
    printf("Array 1: {");
    for(int i = 0; i < 10; ++i) { 
      printf("%d ",array3[i]);
    }
    printf("} Array 2: {");
    for(int j = 0; j < 10; ++j) {
      printf("%d ",array4[j]);
    }
    printf("}\n");

    int array5[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int array6[10] = {};
    transfer(array5, array6, 10, &add_to_back, &remove_from_front);
    printf("Array 1: {");
    for(int i = 0; i < 10; ++i) { 
      printf("%d ",array5[i]);
    }
    printf("} Array 2: {");
    for(int j = 0; j < 10; ++j) {
      printf("%d ",array6[j]);
    }
    printf("}\n");
    int array7[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int array8[10] = {};
    transfer(array7, array8, 10, &add_to_back, &remove_from_back);
    printf("Array 1: {");
    for(int i = 0; i < 10; ++i) { 
      printf("%d ",array7[i]);
    }
    printf("} Array 2: {");
    for(int j = 0; j < 10; ++j) {
      printf("%d ",array8[j]);
    }
    printf("}\n");
    return 0;
}

