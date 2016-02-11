#ifndef _SIMULA_MOLECULE_TYPE_
#define _SIMULA_MOLECULE_TYPE_

#include "global.h"

using namespace global;

class Molecule_Type {
private:

  simString __name__; //< type name in string
  simI1 __amount__;   //< total number of molecule will be created 
  simI1 __id__;       //< type index

  simI2* __rpos__;    //< relative position of sub dots
  simI1  __size__;    //< number of sub dots

public:

  /** @brief Constructor **/
  Molecule_Type() {}

  /** @brief Destructor **/
  ~Molecule_Type()
  {
    if (__rpos__) { delete[] __rpos__; }
  }

  /** Getter **/
  const simString name() { return __name__; }
  const simI1  id() const { return __id__; }
  const simI1  size() const { return __size__; }
  const simI2* rpos() const { return __rpos__; }
  const simI1  amount() const { return __amount__; }

  /** Setter **/
  /** @brief set type name **/
  void set_name (const simString& name) 
  {
    __name__ = name; 
  }

  /** @brief set type index **/
  void set_id (const simI1 id)
  {
    __id__ = id;
  }

  /** @brief set type amount **/
  void set_amount (const simI1 a)
  {
    __amount__ = a;
  }

  /** @brief set relative component position with a list of integer pairs **/
  void set_rpos (const simI1 s, const simI1* list)
  {
    //--- set size first
    __size__ = s; 
    __rpos__ = new simI2[__size__];
    //--- set relative positions
    for (simI1 i = 0; i < __size__; ++i) {
      __rpos__[i].x = list[2 * i];
      __rpos__[i].y = list[2 * i + 1];
    }
  }

};

#endif // _SIMULA_MOLECULE_TYPE_
