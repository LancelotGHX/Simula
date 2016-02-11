#ifndef _SIMULA_MOLECULE_DEFINE_
#define _SIMULA_MOLECULE_DEFINE_

#include "global.h"

using namespace global;

/**
 * Molecule Type class
 **/
class Molecule_Type {
private:
  simString __name__; //< type name in string
  simI1  __am__; //< total amount of molecule will be created 
  simI1  __id__; //< type index
  simVI2 __rp__; //< relative position of sub dots
public:

  /** @brief Constructor **/
  Molecule_Type  () {}

  /** @brief Destructor **/
  ~Molecule_Type () {}

  /** Getter **/
  const simString name () const { return __name__; }
  const simI1   amount () const { return __am__; }
  const simI1   id     () const { return __id__; }
  const simI1   size   () const { return __rp__.size(); }
  const simVI2& rpos   () const { return __rp__; }

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
  void set_amount (const simI1 am)
  {
    __am__ = am;
  }

  /** @brief set relative component position with a list of integer pairs **/
  void set_rpos (const simI1 sz, const simI1* ls)
  {
    //--- set relative positions
    for (simI1 i = 0; i < 2 * sz; ++i) {
      simI1 xid = 2 * i;
      simI1 yid = 2 * i + 1;
      __rp__.push_back(simI2(ls[xid], ls[yid]));
    }
  }

};

/**
 * Molecule class
 **/
class Molecule {
private:
  //--- unchangable values
  const Molecule_Type* __tp__;
  const simI1          __id__; // molecular index
  //--- changable values
  simI1 __d__; // molecular direction
  simI1 __x__; // molecular x-position
  simI1 __y__; // molecular y-position
public:

  /** @brief Constructor **/
  Molecule (const Molecule_Type* tp, const simI1 id) 
    : __tp__(tp), __id__(id) {}
  
  /** @brief Destructor **/
  ~Molecule () {}

  /** @brief properties accessor **/
  const Molecule_Type* type () const { return __tp__; }
  const simI1 type_id () const { return __tp__->id(); }
  const simI1 self_id () const { return __id__;       }
  const simI1 x () const { return __x__; }
  const simI1 y () const { return __y__; }
  const simI1 d () const { return __d__; }
  const simI1   size () const { return __tp__->size(); }
  const simVI2& rpos () const { return __tp__->rpos(); }

  /** @brief properties setters **/
  void set_x (const simI1 xpos) { __x__ = xpos; }
  void set_y (const simI1 ypos) { __y__ = ypos; }
  void set_d (const simI1 direction) { __d__ = direction; }

};

#endif // _SIMULA_MOLECULE_DEFINE_
