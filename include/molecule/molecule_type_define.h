#ifndef _SIMULA_MOLECULE_TYPE_DEFINE
#define _SIMULA_MOLECULE_TYPE_DEFINE

#include "global.h"

namespace simula
{
  ////////////////////////////////////////////////////////////////////////////
  // Molecule Type class
  //
  class Molecule_Type {
  private:
    simString __name__; //< type name in string
    simI1  __am__; //< total amount of molecule will be created 
    simI1  __id__; //< type index
    simVI2 __rp__; //< relative position of sub dots
  public:

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
      for (simI1 i = 0; i < sz; ++i) {
	simI1 xid = 2 * i;
	simI1 yid = 2 * i + 1;
	__rp__.push_back(simI2(ls[xid], ls[yid]));
      }
    }

  };
};

#endif // _SIMULA_MOLECULE_TYPE_DEFINE
