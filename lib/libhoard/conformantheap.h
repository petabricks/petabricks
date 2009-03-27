#ifndef _CONFORMANTHEAP_H_
#define _CONFORMANTHEAP_H_

// more concise than double declarations when the parent is complex
// and we need the SuperHeap declaration.
template <class Parent>
class ConformantHeap : public Parent {
public:
  typedef Parent SuperHeap;
};


#endif
