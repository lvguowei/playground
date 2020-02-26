#include <iostream>

using namespace std;

#define MAX_SIZE 100

typedef struct {
  int *elems;
  int len;
  int size;
} SqList;

bool initList(SqList &list) {
  list.elems = new int[MAX_SIZE];
  if (!list.elems)
    return false;
  list.len = 0;
  list.size = MAX_SIZE;
  return true;
}

bool listAppend(SqList &list, int e) {
  if (list.len == list.size)
    return false;
  list.elems[list.len] = e;
  list.len++;
  return true;
}

bool listInsert(SqList &list, int i, int e) {
  if (i < 0 || i >= list.len)
    return false;
  if (list.len == list.size)
    return false;
  for (int j = list.len - 1; j >= i; j--) {
    list.elems[j + 1] = list.elems[j];
  }
  list.elems[i] = e;
  list.len++;
  return true;
}

bool listDelete(SqList &list, int i) {
  if (i < 0 || i > list.len - 1)
    return false;

  if (i == list.len - 1) {
    list.len--;
  } else {
    for (int j = i; j < list.len - 1; j++) {
      list.elems[j] = list.elems[j + 1];
    }
    list.len--;
  }
  return true;
}

void listDestroy(SqList &list) {
  if (list.elems) delete []list.elems;
  list.len = 0;
  list.size = 0;
}

void listPrint(SqList &list) {
  cout << "size: " << list.size << " len: " << list.len << endl;
  for (int i = 0; i < list.len; i++) {
    cout << list.elems[i] << " ";
  }
}

int main() {
  SqList list;
  if (initList(list)) {
    cout << "Init list success!" << endl;

    int count = 0;
    int i;
    int e;
    cout << "Input number of numbers to append: ";
    cin >> count;
    for (i = 0; i < count; i++) {
      cout << "\nElement to append: ";
      cin >> e;
      if (listAppend(list, e)) {
        cout << "Succeeded adding " << e << endl;
      } else {
        cout << "Failed adding " << e << endl;
      }
    }
    listPrint(list);

    // Insert
    cout << "Please insert position and element: ";
    cin >> i >> e;
    if (listInsert(list, i, e)) {
      cout << "Inserted successfully" << endl;
    } else {
      cout << "Insert failed" << endl;
    }

    listPrint(list);

    // Delete
    cout << "index to delete: ";
    cin >> i;
    if (listDelete(list, i)) {
      cout << "Deleted successfully" << endl;
    } else {
      cout << "Delete failed" << endl;
    }
    listPrint(list);
  }
  return 0;
}
