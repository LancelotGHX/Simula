#ifndef _SIMULA_MOLECULE_DEFINE_
#define _SIMULA_MOLECULE_DEFINE_

#include "global.h"
#include "molecule_type_define.h"

namespace simula
{
  ////////////////////////////////////////////////////////////////////////////
  // Molecule class
  //
  class Molecule {
  private:
    //--- unchangable values
    const Molecule_Type * __tp__; // pointer to constant type instance
    simI1                 __id__; // molecular index
    //--- changable values
    simI1 __d__; // molecular direction
    simI1 __x__; // molecular x-position
    simI1 __y__; // molecular y-position
  public:

    /** @brief Constructor **/
    Molecule (const Molecule_Type * const tp, const simI1 id) 
      : __tp__(tp), __id__(id) {}

    /** @brief properties accessor **/
    const Molecule_Type* type () const { return __tp__; }
    const simI1 type_id () const { return __tp__->data_id(); }
    const simI1 self_id () const { return __id__;            }
    const simI1 x () const { return __x__; }
    const simI1 y () const { return __y__; }
    const simI1 d () const { return __d__; }
    const simI1   size () const { return __tp__->size(); }
    const simVI2& rpos () const { return __tp__->rpos(); }

    /** @brief properties setters **/
    void set_x (const simI1 xpos) { __x__ = xpos; }
    void set_y (const simI1 ypos) { __y__ = ypos; }
    void set_d (const simI1 direction) { __d__ = direction; }

#ifndef NDEBUG
    void debug()
    {
      cout << "index: " << __id__;
      cout << " type: " << type_id();
      cout << " (" << __x__ << "," << __y__ <<")"<< endl;

    }
#endif
  };
};

#endif // _SIMULA_MOLECULE_DEFINE_
