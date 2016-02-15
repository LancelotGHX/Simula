#ifndef _SIMULA_SUBSTRATE_
#define _SIMULA_SUBSTRATE_

#include "molecule.h"

///////////////////////////////////////////////////////////////////////////////
// local variable namespace
namespace _substrate_ {
	extern const simula::simI1 _bg_; //< background value
};

///////////////////////////////////////////////////////////////////////////////
// project namespace
namespace simula {

	/////////////////////////////////////////////////////////////////////////////
	// Substrate class defining substrate behaviors
	class Substrate {
	private:
		const simI1 _xlen_; //< X dimension size
		const simI1 _ylen_; //< Y dimemsion size
		simVI1 _data_;      //< array of pointers to Molecule
	private:
		///////////////////////////////////////////////////////////////////////////
		// Initialize substrate with value background value
		void _init_();
	public:
		///////////////////////////////////////////////////////////////////////////
		// Constructor
		Substrate(const simI1 Xsize, const simI1 Ysize)
			: _xlen_(Xsize), _ylen_(Ysize) 
		{
			_init_();
		}
		
		///////////////////////////////////////////////////////////////////////////
		// Destructor
		~Substrate() {}

		///////////////////////////////////////////////////////////////////////////
		// Read data
		inline simI1* data()
		{
			return _data_.data();
		}

		///////////////////////////////////////////////////////////////////////////
		// Point value getter
		const simI1 get_sub(const simI1 x, const simI1 y) const;

		///////////////////////////////////////////////////////////////////////////
		// Point value setter
		void set_sub(const simI1 x, const simI1 y, const simI1 value);

		///////////////////////////////////////////////////////////////////////////
		// Check if the point is empty
		inline simBool is_empty(const simI1 x, const simI1 y) const
		{
			return (get_sub(x, y) == _substrate_::_bg_);
		}

		///////////////////////////////////////////////////////////////////////////
		// Check if the relative positions are all empty
		simBool is_empty(const simVI2& rp, const simI1 xc, const simI1 yc) const;

		///////////////////////////////////////////////////////////////////////////
		// Land molecule on the position
		simBool land(Molecule& m, const simI1 xc, const simI1 yc, const simI1 dc);

		///////////////////////////////////////////////////////////////////////////
		// print substrate into file
		void print(const simChar* name);

		///////////////////////////////////////////////////////////////////////////
		// overload output function
		friend std::ostream& operator<<(std::ostream& os, const Substrate& sub);

	};

	///////////////////////////////////////////////////////////////////////////
	// overload output function
	std::ostream& operator<<(std::ostream& os, const Substrate& sub);

};

///////////////////////////////////////////////////////////////////////////
// local variable namespace
namespace _substrate_ {
	extern simula::Substrate * _sub_;
};

///////////////////////////////////////////////////////////////////////////
// project namespace
namespace simula {

	///////////////////////////////////////////////////////////////////////////
	// generate the substrate
	void gen_sub(simI1 xlen, simI1 ylen);

	///////////////////////////////////////////////////////////////////////////
	// clean the substrate
	inline void clean_sub()
	{
		delete _substrate_::_sub_;
	}

	///////////////////////////////////////////////////////////////////////////
	// accessor to the substrate
	inline Substrate& get_sub()
	{
		return *_substrate_::_sub_;
	}

};


#endif // _SIMULA_SUBSTRATE_
