//--------------------------------------------------------------------------------------------------
//
//	Embedded: A generic interface for accessing hardware registers
//
//--------------------------------------------------------------------------------------------------
//
// The MIT License (MIT)
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software
// and associated documentation files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
// BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NON INFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
//--------------------------------------------------------------------------------------------------
//
// Copyright (c) 2016 Nic Holthaus
//
//--------------------------------------------------------------------------------------------------
//
///	@file			embedded.h
///	@brief			Single header library for communicating with embedded devices.
/// @details		Parts of the design for the embedded library are influenced by the paper "C++
///					Hardware Register Access Redux" by Ken Smith, which is available on the web
///                 at https://yogiken.files.wordpress.com/2010/02/c-register-access.pdf
///					Reading that first will be a good primer to the operation of these classes.
//
//--------------------------------------------------------------------------------------------------
//
///	@author			Nic Holthaus
/// @date			August 4, 2016
//
//--------------------------------------------------------------------------------------------------

#ifndef embedded_h__
#define embedded_h__

//------------------------
//	INCLUDES
//------------------------

// std
#include <chrono>
#include <cstdint>
#include <functional>
#include <iostream>
#include <ratio>
#include <type_traits>
#include <units.h>

/**
 * @brief		Namespace for the embedded library templates.
 * @details
 */
namespace embedded
{
	//////////////////////////////////////////////////////////////////////////
	//		FORWARD DECLARATIONS
	//////////////////////////////////////////////////////////////////////////

	template<typename>
	class Device;

	using Device8  = Device<uint8_t>;
	using Device16 = Device<uint16_t>;
	using Device32 = Device<uint32_t>;
	using Device64 = Device<uint64_t>;

	//////////////////////////////////////////////////////////////////////////
	//		TRAITS & CONCEPTS
	//////////////////////////////////////////////////////////////////////////
	//
	//	This section can probably be safely ignored by users of the embedded
	//	library.
	//
	//////////////////////////////////////////////////////////////////////////

	/**
	 * @brief		Namespace which contains the concepts and traits of the embedded library.
	 * @details
	 */
	namespace traits
	{
		/**
		 * @brief		void type.
		 * @details		Helper class for creating type traits.
		 */
		template<class...>
		struct void_t
		{
			typedef void type;
		};

		/** @cond */    // DOXYGEN IGNORE
		namespace detail
		{
			/// has_num implementation.
			template<class T>
			struct has_num_impl
			{
				template<class U>
				static auto test(U*) -> std::is_integral<decltype(U::num)>;
				template<typename>
				static std::false_type test(...);

				using type = decltype(test<T>(0));
			};

			/**
			 * @brief		Trait which checks for the existence of a static numerator.
			 * @details		Inherits from `std::true_type` or `std::false_type`. Use `has_num<T>::value` to test
			 *				whether `class T` has a numerator static member.
			 */
			template<class T>
			struct has_num : has_num_impl<T>::type
			{
			};

			/// has_den implementation.
			template<class T>
			struct has_den_impl
			{
				template<class U>
				static auto test(U*) -> std::is_integral<decltype(U::den)>;
				template<typename>
				static std::false_type test(...);

				using type = decltype(test<T>(0));
			};

			/**
			 * @brief		Trait which checks for the existence of a static denominator.
			 * @details		Inherits from `std::true_type` or `std::false_type`. Use `has_den<T>::value` to test
			 *				whether `class T` has a denominator static member.
			 */
			template<class T>
			struct has_den : has_den_impl<T>::type
			{
			};
		}    // namespace detail

		/**
		 * @brief		Trait that tests whether a type represents a std::ratio.
		 * @details		Inherits from `std::true_type` or `std::false_type`. Use `is_ratio<T>::value` to test
		 *				whether `class T` implements a std::ratio.
		 */
		template<class T>
		struct is_ratio : std::integral_constant<bool, detail::has_num<T>::value && detail::has_den<T>::value>
		{
		};

		namespace detail
		{
			// dummy base-class tags to make the trait checkers easier to write.
			struct register_base
			{
			};
			struct device_base
			{
			};
		}    // namespace detail

		/**
		 * @brief		Trait that tests whether a type represents an embedded::Device.
		 * @details		Inherits from `std::true_type` or `std::false_type`. Use `is_device<T>::value` to test
		 *				whether `class T` implements a Device.
		 */
		template<class T>
		struct is_device : std::is_base_of<detail::device_base, T>
		{
		};

		/**
		 * @brief		Trait that tests whether a type represents an embedded::Register.
		 * @details		Inherits from `std::true_type` or `std::false_type`. Use `is_register<T>::value` to test
		 *				whether `class T` implements a Register.
		 */
		template<class T>
		struct is_register : std::is_base_of<detail::register_base, T>
		{
		};

		template<class T, typename = void>
		struct register_traits
		{
			typedef void access_policy;
			typedef void value_type;
		};

		template<class T>
		struct register_traits<T, typename void_t<typename T::access_policy, typename T::value_type>::type>
		{
			typedef typename T::access_policy access_policy;    ///< Policy class used to access (read/write/etc) the register.
			typedef typename T::value_type    value_type;       ///< Value type stored by the register.
		};

	}    // namespace traits

	//////////////////////////////////////////////////////////////////////////
	//		CLASSES
	//////////////////////////////////////////////////////////////////////////

	//	----------------------------------------------------------------------------
	//	CLASS		LSB
	//  ----------------------------------------------------------------------------
	///	@brief		Class used to represent register values whose LSB is not equal to `1`.
	///	@details	Often when working with registers, the logical least-significant bit
	///				value is mapped to a number other than one. Sometimes this is a way
	///				to represent decimals in an integer system, other times to represent
	///				very large or small numbers using a subset of integers.\n\n
	///
	///				The LSB class is designed to abstract the implementation of the registers
	///				bits away from logical value that it stores. For example, the LSB class
	///				could be used to store a value in a register whose LSB has a value of `16`.
	///
	///				`using lsb_16 = LSB<std::ratio<16>, int32_t>;`
	///				`lsb_16 myReg = 32;`
	///
	///				In this case, the value of 32 represents the logical value of 32, not
	///				the actual value which needs to be stored in the register (which is 2).
	///				This makes the program much easier to read, and free of magic numbers and
	///				conversions.
	///
	///				Any class can be compatible with LSB if suitable operators are defined,
	///				and a `ValueAdapter` specialization is written.
	/// @tparam		lsb_ratio	A `std::ratio` type representing the value of a single LSB.
	/// @tparam		lsb_type	The type used to store the LSB value. For example, a integer
	///							or `unit` type.
	/// @note		doesn't inherit lsb_type via CRTP since it could be (and probably
	///				is) primitive
	//  ----------------------------------------------------------------------------
	template<typename lsb_ratio,
	         typename lsb_type = uint32_t,
	         typename enable   = typename std::conditional<!units::traits::is_unit_t<lsb_type>::value, std::true_type, std::false_type>::type>
	class LSB;

	template<typename lsb_ratio, typename lsb_type>
	class LSB<lsb_ratio, lsb_type, std::true_type>
	{
	public:
		// assert that `lsb_ratio` is a `std::ratio`.
		static_assert(traits::is_ratio<lsb_ratio>::value, "Template parameter `lsb_ratio` must be a std::ratio type representing the value of an lsb.");

		using ratio = lsb_ratio;

		// LSB is just kind of a convenience metaclass, so allow willy-nilly implicit conversions
		// between `LSB` and the `lsb_type`
		LSB(){};
		LSB(lsb_type value)
		    : m_value(value){};

		template<typename T>
		operator T() const
		{
			return static_cast<T>(m_value);
		}
		// *** NOTE ***: if you get an error: cannot convert from '[some type]' to 'device_size', and
		// you are using `unit` types, you probably need to add `` to your CMake files.

		// Operators to allow LSB types to be acted on like integers
		bool operator==(const lsb_type& rhs) const { return m_value == rhs; }
		bool operator==(const LSB& rhs) const { return m_value == rhs.m_value; }

		bool operator!=(const lsb_type& rhs) const { return m_value != rhs; }
		bool operator!=(const LSB& rhs) const { return m_value != rhs.m_value; }

		template<typename lsbRatio, typename lsbType>
		friend std::ostream& operator<<(std::ostream& os, const LSB<lsbRatio, lsbType>& val);

	private:
		lsb_type m_value;
	};

	// Output stream operator definition for LSB types. Mostly for debugging.
	template<typename lsb_ratio, typename lsb_type>
	std::ostream& operator<<(std::ostream& os, const LSB<lsb_ratio, lsb_type>& val)
	{
		return os << val.m_value;
	}

	//  ----------------------------------------------------------------------------
	///	Partial specialization for use with `units::unit_t` types.
	//  ----------------------------------------------------------------------------
	template<typename lsb_ratio,    ///< LSB Ratio. Represents the value of a single LSB. Must be a `unit_value_t` (i.e. strongly typed, dimensioned) ratio.
	         typename U,            ///< This parameter is inferred from the `unit_t` type.
	         typename T,            ///< This parameter is inferred from the `unit_t` type.
	         template<typename>
	         class S                ///< This parameter is inferred from the `unit_t` type.
	         >
	class LSB<lsb_ratio, units::unit_t<U, T, S>, std::false_type>
	    : public units::unit_t<U, T, S>    // CRTP. This way we don't need to define all the operators.
	{
	public:
		// assert that `lsb_ratio` is a `units::unit_value_t` (which is basically just a std::ratio
		// for units).
		static_assert(units::traits::is_unit_value_t<lsb_ratio>::value, "Template parameter `lsb_ratio` must be a `units::unit_value_t` \
			type representing the value of an lsb.");
		static_assert(units::traits::is_convertible_unit<typename units::traits::unit_value_t_traits<lsb_ratio>::unit_type, U>::value,
		              "Template parameters `lsb_ratio` and `lsb_value` do not represent compatible units.");

		using ratio = typename units::traits::unit_value_t_traits<lsb_ratio>::ratio;
		using unit_container =
		        typename units::unit_t<U, T, S>;    // DON'T call this `unit_type`. It will hide the `unit_t::unit_type` and cause a lot of operators to fail.

		// LSB is just kind of a convenience metaclass, so allow willy-nilly implicit conversions
		// between `LSB` and the `lsb_type`
		LSB(){};
		LSB(T value)
		    : unit_container(value){};

		// forward construction to the unit_t constructor. This will allow for unit conversions.
		// This is a two-step process:
		// 1. Create a new unit type where we only convert the underlying type (e.g. double -> int)
		// 2. Create a unit_container type which takes care of any required unit conversions via
		//	its copy constructor.
		template<typename Units, typename Ty, template<typename> class Scale>
		LSB(units::unit_t<Units, Ty, Scale> value)
		    : unit_container(units::unit_t<Units, T, Scale>(static_cast<T>(value()))){};

		template<typename lsbRatio, typename lsbType>
		friend std::ostream& operator<<(std::ostream& os, const LSB<lsbRatio, lsbType>& val);
	};

	//	----------------------------------------------------------------------------
	//	CLASS		ValueAdapter
	//  ----------------------------------------------------------------------------
	///	@brief		class which handles conversions from a registers `value_type` to its underlying `register_type`.
	///	@details	The `value_type` of a register can be a lot of different things: integral types,
	///				floating point types, LSB types, units, etc. The `register_type` is always an
	///				unsigned integral type, but may be of different widths (8, 16, 32, 64 bit). The
	///				purpose of the `ValueAdapter` class is to provide static functions which perform
	///				the conversions between those two types. The default implementation merely static-casts
	///				them, which is probably sufficient for built in types. For more complex conversions,
	///				the user should create a partial specialization of the `ValueAdapter` for the given
	///				`value_type`. For example, a specialization could be created for the LSB class which
	///				normalizes the given value by the LSB as part of the conversion.\n\n
	///
	///				This classes static members are used by the `RegisterXX_t` classes, and both
	///				should be overloaded in any user-defined partial specialization.
	///
	/// @tparam		value_type		Higher-level type used by the registers access policy. This may
	///								be an integral or floating point type, a strongly-typed enum class,
	///								an LSB representation, a unit, or whatever is else is desired.
	/// @tparam		register_type	The underlying width of the register. One of: `int8_t`, `int_16_t`,
	///								`int32_t`, or `int64_t`.
	//  ----------------------------------------------------------------------------
	template<typename value_type, typename register_type>
	struct ValueAdapter
	{
		/// Convert `value` to its underlying integral representation, as it should appear in the
		/// device hardware.
		static inline register_type toRegisterType(value_type value) { return static_cast<register_type>(value); }

		/// Convert `value` from the underlying hardware representation to the higher-level `value_type`.
		static inline value_type fromRegisterType(register_type value) { return static_cast<value_type>(value); }
	};

	//  ----------------------------------------------------------------------------
	///	Partial specialization for boolean values
	//  ----------------------------------------------------------------------------
	template<typename register_type>
	struct ValueAdapter<bool, register_type>
	{
		/// Convert `value` to its underlying integral representation, as it should appear in the
		/// device hardware.
		static inline register_type toRegisterType(bool value) { return static_cast<register_type>(value ? 1 : 0); }

		/// Convert `value` from the underlying hardware representation to the higher-level `value_type`.
		static inline bool fromRegisterType(register_type value) { return value != 0; }
	};

	//  ----------------------------------------------------------------------------
	///	Partial specialization for performing LSB conversions.
	//  ----------------------------------------------------------------------------
	template<typename register_type, typename lsb_ratio, typename lsb_type>
	struct ValueAdapter<LSB<lsb_ratio, lsb_type, std::true_type>, register_type>
	{
		using value_type = LSB<lsb_ratio, lsb_type>;

		/// Convert `value` to its underlying integral representation, as it should appear in the
		/// device hardware.
		static inline register_type toRegisterType(value_type value)
		{
			return static_cast<register_type>(value) * static_cast<register_type>(value_type::ratio::den) / static_cast<register_type>(value_type::ratio::num);
		}

		/// Convert `value` from the underlying hardware representation to the higher-level `value_type`.
		static inline value_type fromRegisterType(register_type value) { return value_type(value * value_type::ratio::num / value_type::ratio::den); }
	};

	//  ----------------------------------------------------------------------------
	///	Partial specialization for unit values.
	//  ----------------------------------------------------------------------------
	template<typename register_type, typename U, typename T, template<typename> class S>
	struct ValueAdapter<units::unit_t<U, T, S>, register_type>
	{
		using value_type = units::unit_t<U, T, S>;

		/// Convert `value` to its underlying integral representation, as it should appear in the
		/// device hardware.
		static inline register_type toRegisterType(value_type value) { return value.template to<register_type>(); }

		/// Convert `value` from the underlying hardware representation to the higher-level `value_type`.
		static inline value_type fromRegisterType(register_type value) { return value_type(value); }
	};

	//  ----------------------------------------------------------------------------
	///	Partial specialization for performing LSB conversions with units.
	//  ----------------------------------------------------------------------------
	template<typename register_type, typename lsb_ratio, typename U, typename T, template<typename> class S>
	struct ValueAdapter<LSB<lsb_ratio, units::unit_t<U, T, S>, std::false_type>, register_type>
	{
		using unit_container       = units::unit_t<U, T, S>;
		using ratio_unit_container = typename units::unit_t<typename units::traits::unit_value_t_traits<lsb_ratio>::unit_type, T, S>;
		using value_type           = LSB<lsb_ratio, units::unit_t<U, T, S>>;

		/// Convert `value` to its underlying integral representation, as it should appear in the
		/// device hardware.
		static inline register_type toRegisterType(value_type value)
		{
			return (unit_container(value) * ratio_unit_container(value_type::ratio::den) / ratio_unit_container(value_type::ratio::num))
			        .template to<register_type>();
		}

		/// Convert `value` from the underlying hardware representation to the higher-level `value_type`.
		static inline value_type fromRegisterType(register_type value)
		{
			return value_type(unit_container(value) * ratio_unit_container(value_type::ratio::num) / ratio_unit_container(value_type::ratio::den));
		}
	};

	//	----------------------------------------------------------------------------
	//	CLASS		Device
	//  ----------------------------------------------------------------------------
	///	@brief		Class used to implement and abstract device-specific functionality.
	///	@details	All invocations of vendor provided APIs or device drivers should
	///				be done through the device class. `Device` is an abstract class and
	///				intended as a base for device-specific derived classes.
	///
	///				Derived classes must at minimum
	///				provide implementations for reading and writing registers, but
	///				it's fine to add any other necessary device-specific functionality
	///				to the derived class as well. For example, a board specific DMA
	///				function.\n\n
	///
	///				'Access Policy' classes are expected to use the device static
	///				functions for all direct hardware access.\n\n
	///
	///				Device classes should be written to throw exceptions for bad device
	///				return types or for unexpected errors. Use `std::runtime_error` (or
	///				a derivation thereof).
	/// @tparam		DeviceSize	an integral type representing the size of the devices
	///							registers. Use uint32_t for 32-bit registers, uint64_t
	///							for 64-bit, etc.
	//  ----------------------------------------------------------------------------
	template<typename DeviceSize>
	class Device : public traits::detail::device_base
	{
	public:
		/// An integer type representing the bit-width of the devices registers.
		typedef DeviceSize device_size;

		// check that the device size is an integral type. This is basically to figure out if the
		// memory is 8, 16, 32, or 64 bit.
		static_assert(std::is_integral<device_size>::value, "Template parameter `device_size` must be an integral type.");

		/**
		 * @brief		Read register.
		 * @details		Reads the register named by the `Register` template parameter. The return value
		 *				type will be whatever the registers `value_type` is. It's probably easiest to
		 *				use `auto` when dealing with register read variables.
		 * @tparam		Register Register to read.
		 * @param[in]	sleep optional number of milliseconds to sleep for after a read.
		 * @returns		value stored in `Register`.
		 */
		template<class Register>
		inline typename traits::register_traits<Register>::value_type read(std::chrono::milliseconds sleep = std::chrono::milliseconds(0)) const
		{
			static_assert(traits::is_register<Register>::value, "Template parameter `Register` is not a register type.");
			return Register::read([this, sleep](device_size address) { return this->readDeviceRegister(address, sleep); });
		}

		/**
		 * @brief		Write register.
		 * @details		Writes `value` to `Register`, named by the template parameter.
		 * @param[in]	value Value to write to the register. The value type is determined by the
		 *				register definition.
		 * @param[in]	sleep optional number of milliseconds to sleep for after a read/write.
		 * @tparam		Register Register to write.
		 */
		template<class Register>
		inline void write(typename traits::register_traits<Register>::value_type value, std::chrono::milliseconds sleep = std::chrono::milliseconds(0))
		{
			static_assert(traits::is_register<Register>::value, "Template parameter `Register` is not a register type.");
			Register::write(
			        value,
			        [this, sleep](device_size address, device_size value) { return this->writeDeviceRegister(address, value, sleep); },
			        [this, sleep](device_size address) { return this->readDeviceRegister(address, sleep); });
		}

		/**
		 * @brief		Perform a device reset.
		 * @details		The derived class must implement this function with the appropriate device-specific
		 *				APIs to perform a device reset. If the device does not support software reset,
		 *				a 'blank' reset function can be implemented.
		 */
		virtual void reset() = 0;

	protected:
		/**
		 * @brief		Read device register at <i>address</i>.
		 * @details		The derived class must implement this function with the appropriate device-specific
		 *				APIs to perform a register read. The function should return the value in the
		 *				register as an integral type.
		 * @param[in]	address	Register address as an integral value.
		 * @returns		Value stored in the register at the given address as an integral value.
		 */
		virtual device_size readDeviceRegister(device_size address, std::chrono::milliseconds sleep) const = 0;

		/**
		 * @brief		Write device register <i>value</i> to <i>address</i>.
		 * @details		The derived class must implement this function with the appropriate device-specific
		 *				APIs to perform a register write. The function should return the value written to the
		 *				register as an integral type.
		 * @param[in]	address	Address of the register to value.
		 * @param[in]	value	Value to write to the register.
		 */
		virtual void writeDeviceRegister(device_size address, device_size value, std::chrono::milliseconds sleep) = 0;
	};

	//	----------------------------------------------------------------------------
	//	CLASS		BITWIDTH
	//  ----------------------------------------------------------------------------
	///	@brief		Enumeration helper for defining bit-mask values
	///	@details
	//  ----------------------------------------------------------------------------
	enum class BITWIDTH : uint64_t
	{
		ONE         = 0x1,
		TWO         = 0x3,
		THREE       = 0x7,
		FOUR        = 0xF,
		FIVE        = 0x1F,
		SIX         = 0x3F,
		SEVEN       = 0x7F,
		EIGHT       = 0xFF,
		NINE        = 0x1FF,
		TEN         = 0x3FF,
		ELEVEN      = 0x7FF,
		TWELVE      = 0xFFF,
		THIRTEEN    = 0x1FFF,
		FOURTEEN    = 0x3FFF,
		FIFTEEN     = 0x7FFF,
		SIXTEEN     = 0xFFFF,
		SEVENTEEN   = 0x1FFFF,
		EIGHTEEN    = 0x3FFFF,
		NINETEEN    = 0x7FFFF,
		TWENTY      = 0xFFFFF,
		TWENTYONE   = 0x1FFFFF,
		TWENTYTWO   = 0x3FFFFF,
		TWENTYTHREE = 0x7FFFFF,
		TWENTYFOUR  = 0xFFFFFF,
		TWENTYFIVE  = 0x1FFFFFF,
		TWENTYSIX   = 0x3FFFFFF,
		TWENTYSEVEN = 0x7FFFFFF,
		TWENTYEIGHT = 0xFFFFFFF,
		TWENTYNINE  = 0x1FFFFFFF,
		THIRTY      = 0x3FFFFFFF,
		THIRTYONE   = 0x7FFFFFFF,
		THIRTYTWO   = 0xFFFFFFFF,
		THIRTYTHREE = 0x1FFFFFFFF,
		THIRTYFOUR  = 0x3FFFFFFFF,
		THIRTYFIVE  = 0x7FFFFFFFF,
		THIRTYSIX   = 0xFFFFFFFFF,
		THIRTYSEVEN = 0x1FFFFFFFFF,
		THIRTYEIGHT = 0x3FFFFFFFFF,
		THIRTYNINE  = 0x7FFFFFFFFF,
		FORTY       = 0xFFFFFFFFFF,
		FORTYONE    = 0x1FFFFFFFFFF,
		FORTYTWO    = 0x3FFFFFFFFFF,
		FORTYTHREE  = 0x7FFFFFFFFFF,
		FORTYFOUR   = 0xFFFFFFFFFFF,
		FORTYFIVE   = 0x1FFFFFFFFFFF,
		FORTYSIX    = 0x3FFFFFFFFFFF,
		FORTYSEVEN  = 0x7FFFFFFFFFFF,
		FORTYEIGHT  = 0xFFFFFFFFFFFF,
		FORTYNINE   = 0x1FFFFFFFFFFFF,
		FIFTY       = 0x3FFFFFFFFFFFF,
		FIFTYONE    = 0x7FFFFFFFFFFFF,
		FIFTYTWO    = 0xFFFFFFFFFFFFF,
		FIFTYTHREE  = 0x1FFFFFFFFFFFFF,
		FIFTYFOUR   = 0x3FFFFFFFFFFFFF,
		FIFTYFIVE   = 0x7FFFFFFFFFFFFF,
		FIFTYSIX    = 0xFFFFFFFFFFFFFF,
		FIFTYSEVEN  = 0x1FFFFFFFFFFFFFF,
		FIFTYEIGHT  = 0x3FFFFFFFFFFFFFF,
		FIFTYNINE   = 0x7FFFFFFFFFFFFFF,
		SIXTY       = 0xFFFFFFFFFFFFFFF,
		SIXTYONE    = 0x1FFFFFFFFFFFFFFF,
		SIXTYTWO    = 0x3FFFFFFFFFFFFFFF,
		SIXTYTHREE  = 0x7FFFFFFFFFFFFFFF,
		SIXTYFOUR   = 0xFFFFFFFFFFFFFFFF,
	};

	//	----------------------------------------------------------------------------
	//	CLASS		Register
	//  ----------------------------------------------------------------------------
	///	@brief		Register definition class.
	///	@details	The register class is a meta-type and is not meant to be instantiated.
	///				Registers are defined by creating typedefs of specializations of the
	///				Register class with the appropriate template arguments.\n\n
	///
	///				A register represents a single logical element of a register, which be
	///				a single bit, or a subset of bits of an actual 32-bit device register.
	///				A separate typedef should be made for each field within a device register.\n\n
	///
	///				A registers value type should correspond to its logical value, not its
	///				bitwise definition. Enum classes, integers, units, and LSBs are supported
	///				out-of-the-box by this embedded library. Other types can be used as long
	///				as a suitable specialization of the `ValueAdapter` class is created.\n\n
	///
	///				Registers inherit from their access policies, so if any extension of
	///				functionality is necessary, the best place to do it is in a policy
	///				implementation. The register class itself should never need to be
	///				inherited or modified.\n\n
	///
	///				It's probably better to define registers in a namespace rather than as
	///				class members. The fully-resolved names can get pretty long and redundant,
	///				so putting them in a namespace will allow you to use a `using namespace`
	///				directive to shorten it.\n\n
	///
	/// @tparam		ADDRESS			Register address
	/// @tparam		MASK			Mask for the number of bits used. This should always
	///								be the number of bits, regardless of their position
	///								in the register (as position is handled by the OFFSET
	///								parameter).
	/// @tparam		OFFSET			Number of bits by which the masked value should be
	///								shifted in the register.
	/// @tparam		ValueType		Logical type stored by the register. Most likely and
	///								integral or floating point type, unit type, or LSB
	///								type. There must be a `ValueAdapter` specialization
	///								for each value type used.
	/// @tparam		AccessPolicy	Policy class used to access the register. Examples
	///								include `ReadOnlyPolicy`, `WriteOnlyPolicy`, and
	///								`ReadWritePolicy`. Register inherits from AccessPolicy,
	///								so public members of the policy can be used to extend
	///								the Register features.
	//  ----------------------------------------------------------------------------
	template<uint64_t ADDRESS, BITWIDTH MASK, uint64_t OFFSET, typename ValueType, template<class> class AccessPolicy>
	struct Register : public AccessPolicy<ValueType>, traits::detail::register_base
	{
		typedef ValueType               value_type;
		typedef AccessPolicy<ValueType> access_policy;

		/**
		 * @brief		Write wrapper function.
		 * @details		Only meant to be called via Device::write. Acts as a wrapper to the policy
		 *				write function, giving it the static address information for the register.
		 * @param[in]	value	Value to write.
		 * @param[in]	writeFunc	closure for a device write function that communicates with the
		 *							underlying API.
		 * @param[in]	readFunc	closure for a device read function that communicates with the
		 *							underlying API.
		 */
		template<typename DeviceWriteFunction, typename DeviceReadFunction>
		static void write(value_type value, DeviceWriteFunction writeFunc, DeviceReadFunction readFunc)
		{
			static_assert(!std::is_function<DeviceWriteFunction>::value, "Template parameter `DeviceWriteFunction` is not a function closure.");
			static_assert(!std::is_function<DeviceReadFunction>::value, "Template parameter `DeviceReadFunction` is not a function closure.");
			access_policy::write(ADDRESS, static_cast<uint64_t>(MASK), OFFSET, value, writeFunc, readFunc);
		}

		/**
		 * @brief		Read wrapper function.
		 * @details		Only meant to be called via Device::read. Acts as a wrapper to the policy
		 *				read function, giving it the static address information for the register.
		 * @param[in]	readFunc	closure for a device write function that communicates with the
		 *							underlying API.
		 * @returns		Value stored in the register.
		 */
		template<typename DeviceReadFunction>
		static value_type read(DeviceReadFunction readFunc)
		{
			static_assert(!std::is_function<DeviceReadFunction>::value, "Template parameter `DeviceReadFunction` is not a function closure.");
			return access_policy::read(ADDRESS, static_cast<uint64_t>(MASK), OFFSET, readFunc);
		}

		/**
		 * @returns		Register address
		 */
		static constexpr uint64_t address() { return ADDRESS; }
	};

	//////////////////////////////////////////////////////////////////////////
	//		ACCESS POLICIES
	//////////////////////////////////////////////////////////////////////////
	//
	//	Access policies are the classes that registers use to access device
	//	APIs (via the `DeviceFunction`). They have two main purposes: to expose,
	//	or not expose, required functionality. For example, a write-only register
	//	will expose a `write` function, but not a `read` function.
	//
	//	The second purpose is to adapt the Register `value_type` to an integral type
	//	suitable for direct storage in a register. The `ValueAdapter` class is
	//	used to appropriately perform these conversions.
	//
	//	Registers inherit from their access policies, so if you want to extend
	//	the functionality of a register, you can create a new access policy with
	//	additional public member functions.
	//
	//	The access policies are never actually instantiated, so be sure all
	//	member functions are static.
	//
	//////////////////////////////////////////////////////////////////////////

	//	----------------------------------------------------------------------------
	//	CLASS		ReadOnlyPolicy
	//  ----------------------------------------------------------------------------
	///	@brief		Access policy for read-only registers.
	///	@details	Attempting to write to a read-only register results in a compile
	///				error.
	/// @tparam		value_type Register value storage type.
	//  ----------------------------------------------------------------------------
	template<typename value_type>
	class ReadOnlyPolicy
	{
	protected:
		/**
		 * @brief		Read function wrapper.
		 * @details		Calculates the position of the register using ADDRESS, MASK, and OFFSET, then
		 *				reads the value_type from that address using `func`, then converts the
		 *				integral type to a `value_type` type using a `ValueAdapter` specialization.
		 * @param[in]	ADDRESS		Register address
		 * @param[in]	MASK		Number of bits used by the register field.
		 * @param[in]	OFFSET		Offset of the register field within a larger register.
		 * @param[in]	readFunc	Function closure which reads a value from a device address using vendor
		 *							provided APIs.
		 * @returns		value stored in the register as a logical `Register::value_type`.
		 */
		template<typename DeviceReadFunction>
		static value_type read(uint64_t ADDRESS, uint64_t MASK, uint64_t OFFSET, DeviceReadFunction readFunc)
		{
			// deduce the device register size
			using device_size = decltype(readFunc(0));

			// convert the value to the register type, and then `(value & mask) << offset`.
			// it's a write-only register so don't try any reading.
			device_size registerValue = (readFunc(static_cast<device_size>(ADDRESS)) >> OFFSET) & MASK;
			return ValueAdapter<value_type, device_size>::fromRegisterType(registerValue);
		}

		/**
		 * @brief		Not implemented by design. Results in a compile error.
		 */
		template<typename DeviceWriteFunction, typename DeviceReadFunction>
		static void write(uint64_t, uint64_t, uint64_t, value_type, DeviceWriteFunction, DeviceReadFunction)
		{
			// this assertion will always be true, but you can't use `true` in a static_assert or your
			// code will never compile
			static_assert(std::is_function<DeviceWriteFunction>::value, "Cannot write to a read-only Register.");
		}
	};

	//	----------------------------------------------------------------------------
	//	CLASS		WriteOnlyPolicy
	//  ----------------------------------------------------------------------------
	///	@brief		Access Policy for write-only registers.
	///	@details	A write overwrites any previous value to the register. Attempting
	///				a read operation will result in a compile error.
	/// @tparam		value_type Register value storage type.
	//  ----------------------------------------------------------------------------
	template<typename value_type>
	class WriteOnlyPolicy
	{
	protected:
		/**
		 * @brief		Not implemented by design. Results in a compile error.
		 */
		template<typename DeviceReadFunction>
		static value_type read(uint64_t, uint64_t, uint64_t, DeviceReadFunction)
		{
			// this assertion will always be true, but you can't use `true` in a static_assert or your
			// code will never compile
			static_assert(std::is_function<DeviceReadFunction>::value, "Cannot read from a write-only Register.");
		}

		/**
		 * @brief		Device write wrapper function.
		 * @details		Calculates the position of the register using ADDRESS, MASK, and OFFSET, then
		 *				writes the value_type to that address using `func`, after having converted the
		 *				`value_type` to and integral type using a `ValueAdapter` specialization.
		 * @param[in]	ADDRESS		Register address
		 * @param[in]	MASK		Number of bits used by the register field.
		 * @param[in]	OFFSET		Offset of the register field within a larger register.
		 * @param[in]	value		Value to write to the register. This will be converted to an integral
		 *							type using `ValueAdapter::toRegisterType` before being written.
		 * @param[in]	writeFunc	Function closure which writes a value to a device address using vendor
		 *							provided APIs.
		 * @param[in]	readFunc	Function closure which reads a value from a device address using vendor
		 *							provided APIs.
		 */
		template<typename DeviceWriteFunction, typename DeviceReadFunction>
		static void write(uint64_t ADDRESS, uint64_t MASK, uint64_t OFFSET, value_type value, DeviceWriteFunction writeFunc, DeviceReadFunction)
		{
			// deduce the device register size
			using device_size = decltype(readFunc(0));

			// convert the value to the register type, and then `(value & mask) << offset`.
			// it's a write-only register so don't try any reading.
			device_size registerValue = (ValueAdapter<value_type, device_size>::toRegisterType(value) & MASK) << OFFSET;
			writeFunc(static_cast<device_size>(ADDRESS), registerValue);
		}
	};

	//	----------------------------------------------------------------------------
	//	CLASS		ReadWritePolicy
	//  ----------------------------------------------------------------------------
	///	@brief		Access policy for registers with read/write access.
	///	@details
	/// @tparam		value_type Register value storage type.
	//  ----------------------------------------------------------------------------
	template<typename value_type>
	class ReadWritePolicy
	{
	protected:
		/**
		 * @brief		Read function wrapper.
		 * @details		Calculates the position of the register using ADDRESS, MASK, and OFFSET, then
		 *				reads the value_type from that address using `func`, then converts the
		 *				integral type to a `value_type` type using a `ValueAdapter` specialization.
		 * @param[in]	ADDRESS	Register address
		 * @param[in]	MASK	Number of bits used by the register field.
		 * @param[in]	OFFSET	Offset of the register field within a larger register.
		 * @param[in]	func	Function closure which reads a value from a device address using vendor
		 *						provided APIs.
		 * @returns		value stored in the register as a logical `Register::value_type`.
		 */
		template<typename DeviceReadFunction>
		static value_type read(uint64_t ADDRESS, uint64_t MASK, uint64_t OFFSET, DeviceReadFunction readFunc)
		{
			// deduce the device register size
			using device_size = decltype(readFunc(0));

			// convert the value to the register type, and then `(value & mask) << offset`.
			device_size registerValue = (readFunc(static_cast<device_size>(ADDRESS)) >> OFFSET) & MASK;
			return ValueAdapter<value_type, device_size>::fromRegisterType(registerValue);
		}

		/**
		 * @brief		Device write wrapper function.
		 * @details		Calculates the position of the register using ADDRESS, MASK, and OFFSET, then
		 *				writes the value_type to that address using `func`, after having converted the
		 *				`value_type` to and integral type using a `ValueAdapter` specialization.
		 * @param[in]	ADDRESS	Register address
		 * @param[in]	MASK		Number of bits used by the register field.
		 * @param[in]	OFFSET		Offset of the register field within a larger register.
		 * @param[in]	writeFunc	Function closure which writes a value to a device address using vendor
		 *							provided APIs.
		 * @param[in]	readFunc	Function closure which reads a value from a device address using vendor
		 *							provided APIs.
		 * @param[in]	value		Value to write to the register. This will be converted to an integral
		 *							type using `ValueAdapter::toRegisterType` before being written.
		 */
		template<typename DeviceWriteFunction, typename DeviceReadFunction>
		static void write(uint64_t ADDRESS, uint64_t MASK, uint64_t OFFSET, value_type value, DeviceWriteFunction writeFunc, DeviceReadFunction readFunc)
		{
			// deduce the device register size
			using device_size = decltype(readFunc(0));

			// read the register
			device_size currentRegisterValue = readFunc(static_cast<device_size>(ADDRESS));

			// convert the value to the register type, and then `(value & mask) << offset`.
			// it's a write-only register so don't try any reading.
			device_size registerFieldValue = (ValueAdapter<value_type, device_size>::toRegisterType(value) & MASK) << OFFSET;

			device_size registerValue = (currentRegisterValue & ~(MASK << OFFSET)) | registerFieldValue;
			writeFunc(static_cast<device_size>(ADDRESS), registerValue);
		}
	};
}    // namespace embedded

#endif    // embedded_h__