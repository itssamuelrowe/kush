#ifndef KUSH_ABSTRACT_MODULE_H
#define KUSH_ABSTRACT_MODULE_H

/* The Abstract Module binary format describes the layout in which function and structure
 * declarations are stored in ".am" files.
 */

/*******************************************************************************
 * Flags                                                                       *
 *******************************************************************************/

#define hasSynthetic(modifiers) (modifiers & KUSH_MODIFIER_SYNTHETIC) != 0
#define hasNative(modifiers) (modifiers & KUSH_MODIFIER_NATIVE) != 0
#define hasVariableParameter(modifiers) (modifiers & KUSH_MODIFIER_VARIABLE_PARAMETER)

/**
 * @author Samuel Rowe
 * @since Kush 0.1
 */
struct Module {
	/**
	 * This attribute should always be equal to 0x3AE031999.
	 */
	uint32_t magicNumber;
	uint16_t majorVersion;
	uint16_t minorVersion;
	uint16_t structureCount;
	k_Structure_t** structures;
	uint16_t functionCount;
	k_Function_t** functions;

};

typedef struct Module Module;

/**
 * @author Samuel Rowe
 * @since Kush 0.1
 */
struct k_Structure_t {
	uint16_t flags;

	/**
	 * The size of the structure name.
	 */
	uint16_t nameSize;

	/**
	 * The name of the structure.
	 */
	uint8_t* name;

	/**
	 * The number of attributes that are members of the structure.
	 */
	uint16_t attributeCount;

	/**
	 * An array of integers that represent the length of each type name.
	 */
	uint16_t* attributeNameSizes;

	/**
	 * An array of strings that represent the type names of each attribute.
	 */
	uint8_t** attributeNames;
};

typedef struct k_Structure_t k_Structure_t;

/**
 * @author Samuel Rowe
 * @since Kush 0.1
 */
struct k_Function_t {
	uint16_t flags;
	uint16_t nameSize;
	uint8_t* name;
	uint16_t signatureSize;
	uint8_t* signature;
	/**
	 * The captures are used to evaluate the life expectancy of an object
	 * that is passed an argument to a function. This allows us to allocate
	 * object on stack when the die within the stack frame where they were
	 * declared.
	 *
	 * Since, only 8 bits are allocated to this attribute, the maxium
	 * number of parameters a function can receive is 255.
	 */
	uint8_t captureCount;
	uint8_t* captures;
};

typedef struct k_Function_t k_Function_t;

#endif /* KUSH_ABSTRACT_MODULE_H */
