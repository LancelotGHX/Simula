#ifndef _SIMULA_SUBSTRATE_
#define _SIMULA_SUBSTRATE_

#include "molecule.h"

/** @brief local variable namespace */
namespace _substrate_ {
	extern const simula::simI1 _bg_; //< background value
};

/** @brief project namespace */
namespace simula {

	/**
	 * @brief Substrate class defining substrate behaviors
	 **/
	class Substrate {
	private:
		const simI1 _xlen_; //< X dimension size
		const simI1 _ylen_; //< Y dimemsion size
		simI1* _data_ = NULL;      //< array of pointers to Molecule
	private:

		/** @brief Initialize substrate with value background value **/
		void _init_();

	public:

		/** @brief Constructor **/
		Substrate(const simI1 Xsize, const simI1 Ysize)
			: _xlen_(Xsize), _ylen_(Ysize) 
		{
			_init_();
		}

		/** @brief Destructor **/
		~Substrate()
		{
			if (_data_) { delete[] _data_; }
		}

		/** @brief Read data **/
		inline simI1* data()
		{
			return _data_;
		}

		/** @brief Point value getter **/
		const simI1 get_sub(const simI1 x, const simI1 y) const;

		/** @brief Point value setter **/
		void set_sub(const simI1 x, const simI1 y, const simI1 value);

		/** @brief Check if the point is empty **/
		inline simBool is_empty(const simI1 x, const simI1 y) const
		{
			return (get_sub(x, y) == _substrate_::_bg_);
		}

		/** @brief Check if the relative positions are all empty **/
		simBool is_empty(const simVI2& rp, const simI1 xc, const simI1 yc) const;

		/** @brief Land molecule on the position **/
		simBool land(Molecule& m, const simI1 xc, const simI1 yc, const simI1 dc);

		/** @brief print substrate into file **/
		void print(const simChar* name);

		/** @brief overload output function **/
		friend std::ostream& operator<<(std::ostream& os, const Substrate& sub);

	};

	/** @brief overload output function **/
	std::ostream& operator<<(std::ostream& os, const Substrate& sub);

};

/** @brief local variable namespace */
namespace _substrate_ {
	extern simula::Substrate * _sub_;
};

/** @brief project namespace */
namespace simula {

	/** @brief generate the substrate */
	void gen_sub(simI1 xlen, simI1 ylen);

	/** @brief clean the substrate */
	inline void clean_sub()
	{
		delete _substrate_::_sub_;
	}

	/** @brief accessor to the substrate */
	inline Substrate& get_sub()
	{
		return *_substrate_::_sub_;
	}

};


#endif // _SIMULA_SUBSTRATE_
