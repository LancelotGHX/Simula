#include "molecule/molecule_type_define.h"

using namespace simula;

/** @brief set type name **/
void Molecule_Type::set_name (const simString& name) 
{ _name_ = name; }

/** @brief set type index **/
void Molecule_Type::set_data_id (const simI1 id)
{ _dd_idx_ = id; }

/** @brief set type index **/
void Molecule_Type::set_type_id (const simI1 id)
{ _tp_idx_ = id; }

/** @brief set type amount **/
void Molecule_Type::set_amount (const simI1 am)
{ _amount_ = am; }

/** @brief set relative component position with a list of integer pairs **/
void Molecule_Type::set_rpos (const simVI2& rpos, const simVI1& ridx)
{ _rpos_ = rpos; _ridx_ = ridx; }
void Molecule_Type::set_rpos (const simI1 sz, const simI1* ls)
{
  for (simI1 i = 0; i < sz; ++i) {
    simI1 xid = 2 * i;
    simI1 yid = 2 * i + 1;
    _rpos_.push_back(simI2(ls[xid], ls[yid]));
  }
}

#ifndef NDEBUG
/** @brief  properties **/
void Molecule_Type::debug()
{
  cout << "==> Molecule" << endl;
  cout << " ** name: "   << _name_   << endl;
  cout << " ** amount: " << _amount_ << endl;
  cout << " ** data index: " << _dd_idx_ << endl;
  cout << " ** type index: " << _tp_idx_ << endl;
  for (simI1 i = 0; i < this->size(); ++i) {
    cout << " ** rpos id = " << _ridx_[i] << " (";
    cout << _rpos_[i].x << ","  ;
    cout << _rpos_[i].y << ")\n";
  }
  cout << endl;
}
#endif
