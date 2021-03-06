'' Title: ext/json.bi
''
'' About: License
''  Copyright (c) 2007-2013, FreeBASIC Extended Library Development Group
''
''  Distributed under the FreeBASIC Extended Library Group license. See
''  accompanying file LICENSE.txt or copy at
''  http://code.google.com/p/fb-extended-lib/wiki/License

#ifndef __FB_EXT_JSON__
#define __FB_EXT_JSON__ -1

#include once "ext/detail/common.bi"

#if not __FB_MT__
    #inclib "ext-json"
    #ifdef FBEXT_MULTITHREADED
        #error "The multithreaded version of the library must be built using the -mt compiler option."
    #endif
#else
    #inclib "ext-json.mt"
    #ifndef FBEXT_MULTITHREADED
        #define FBEXT_MULTITHREADED 1
    #endif
#endif

''Namespace: ext.json
namespace ext.json

    ''Enum: jvalue_type
    ''Possible types present in a JSON document.
    enum jvalue_type
        jnull = 0
        jobject
        jstring
        array
        number
        boolean
    end enum

    type jarray_f as JSONarray
    type jobject_f as JSONobject

    ''Class: JSONvalue
    ''Represents a possible JSON value.
    type JSONvalue
        public:
        ''Sub: Number Constructor
        ''Assign this JSONvalue a double value.
        declare constructor( byval n as double )

        ''Sub: String Constructor
        ''Assign this JSONvalue a string value.
        declare constructor( byref x as const string )

        ''Sub: Bool Constructor
        ''Assign this JSONvalue a boolean value.
        declare constructor( byval b as bool )

        ''Sub: <JSONobject> Constructor
        ''Assign this JSONvalue a <JSONobject> value.
        declare constructor( byval o as jobject_f ptr )

        ''Sub: <JSONarray> Constructor
        ''Assign this JSONvalue a <JSONarray> value.
        declare constructor( byval a as jarray_f ptr )

        ''Sub: Default Constructor
        ''Assign this JSONvalue a null value.
        declare constructor()

        ''Function: valueType
        ''Determine what type of content this type contains.
        ''
        ''Returns:
        ''<jvalue_type>
        declare function valueType() as jvalue_type

        ''Function: getString
        ''Get the string value, or "" if this object is not a string.
        declare function getString() as string

        ''Function: getNumber
        ''Get the number value, or 0 if this object is not a number.
        declare function getNumber() as double

        ''Function: getBool
        ''Get the boolean value, or invalid if this object is not a boolean.
        declare function getBool() as bool

        ''Function: getObject
        ''Get the <JSONobject> ptr value, or null if this object is not an <JSONobject>.
        declare function getObject() as jobject_f ptr

        ''Function: getArray
        ''Get the <JSONarray> ptr value, or null if this object is not an <JSONarray>.
        declare function getArray() as jarray_f ptr

        ''Function: cast
        ''Returns the JSON string format of this object.
        declare operator cast () as string

        declare destructor
        private:
        as double m_number
        as string m_string
        as bool m_bool
        m_child as any ptr
        m_type as jvalue_type
    end type

    'JSONpair is only used internally and will not be exposed.
    type JSONpair
        declare constructor( byref k as const string, byval v as JSONvalue ptr )
        as string key
        as JSONvalue ptr value
        declare destructor
        declare operator cast () as string
    end type

    ''Class: JSONobject
    ''In JSON an object is similiar to a hashtable.
    type JSONobject
        public:
        ''Function: loadString
        ''Loads the JSON document passed as a string.
        ''
        ''Returns:
        ''Pointer to this object for chaining.
        ''Null may be returned on error.
        declare function loadString( byref jstr as const string ) as JSONobject ptr

        ''Function: addChild
        ''Add the specified value to this object.
        ''
        ''Parameters:
        ''k - the key to give the value in the object.
        ''v - the <JSONvalue> to assign to k
        ''
        ''Returns:
        ''v, for chaining
        declare function addChild( byref k as const string, byval v as JSONvalue ptr ) as JSONvalue ptr

        ''Function: child
        ''Looks up the specified child.
        ''
        ''Parameters:
        ''c - The key of the value to retrieve.
        ''
        ''Returns:
        ''The <JSONvalue> requested or null on failure.
        declare function child( byref c as const string ) as JSONvalue ptr

        ''Function: children
        ''Returns the 1-based number of children to this object.
        declare function children() as uinteger
        declare destructor

        ''Operator: cast
        ''Convert this object and all children to a JSON string.
        declare operator cast () as string
        private:
        m_child as JSONpair ptr ptr
        m_children as uinteger

    end type

    ''Class: JSONarray
    ''Represents an array of <JSONvalue> objects.
    type JSONarray
        public:
        ''Sub: Constructor
        ''Used when you need to construct your own array.
        ''
        ''Parameters:
        ''i - pointer array of <JSONvalue> pointers
        ''i_len - the 1-based length of i
        declare constructor( byval i as JSONvalue ptr ptr, byval i_len as uinteger )

        ''Function: at
        ''Retrieve a value from the array.
        ''
        ''Parameters:
        ''index - the 0 based index to retrieve
        ''
        ''Returns:
        ''<JSONvalue> ptr of the index or null on index error
        declare function at( byval index as uinteger ) as JSONvalue ptr

        ''Function: length
        ''Retrieve the number of items in the array.
        declare function length() as uinteger

        ''Operator: cast
        ''Returns the JSON string value of the array and it's contents.
        declare operator cast () as string
        declare destructor
        private:
        m_child as JSONvalue ptr ptr
        m_children as uinteger
    end type

end namespace

#endif
