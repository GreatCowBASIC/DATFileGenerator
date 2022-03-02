'' Title: ext/preprocessor/tuple/remparens.bi
''  This file is part of the <ext/Preprocessor> library API, and can be
''  directly included by user programs.
''
'' About: License
''  Copyright (c) 2007-2012, FreeBASIC Extended Library Development Group
''  Copyright (c) 2002, Paul Mensonides
''
''  Distributed under the Boost Software License, Version 1.0. See
''  accompanying file LICENSE_1_0.txt or copy at
''  http://www.boost.org/LICENSE_1_0.txt)
''
''  Distributed under the FreeBASIC Extended Library Group license. See
''  accompanying file LICENSE.txt or copy at
''  http://code.google.com/p/fb-extended-lib/wiki/License

# pragma once
# ifndef FBEXT_INCLUDED_PP_TUPLE_ENUM_BI__
# define FBEXT_INCLUDED_PP_TUPLE_ENUM_BI__ -1

# define FBEXT_PP_TUPLE_ENUM(size, tuple) fbextPP_TupleEnum(size, tuple)

'' Macro: fbextPP_TupleEnum
''  enumerates (lists) all of the elements, in order, of the tuple *tuple* of
''  size *size*.
''
''  For example, if size *size* is `3` and *tuple* is `(a,b,c)`, then this
''  macro expands to `a,b,c`.
''
'' Parameters:
''  size - is the count of elements in the tuple.
''  tuple - is the tuple containing the elements to enumerate.
# define fbextPP_TupleEnum(size, tuple) _
         _fbextPP_TupleEnum__##size tuple

# define _fbextPP_TupleEnum__0()
# define _fbextPP_TupleEnum__1(__0) __0
# define _fbextPP_TupleEnum__2(__0,__1) __0, __1
# define _fbextPP_TupleEnum__3(__0,__1,__2) __0, __1, __2
# define _fbextPP_TupleEnum__4(__0,__1,__2,__3) __0, __1, __2, __3
# define _fbextPP_TupleEnum__5(__0,__1,__2,__3,__4) __0, __1, __2, __3, __4
# define _fbextPP_TupleEnum__6(__0,__1,__2,__3,__4,__5) __0, __1, __2, __3, __4, __5
# define _fbextPP_TupleEnum__7(__0,__1,__2,__3,__4,__5,__6) __0, __1, __2, __3, __4, __5, __6
# define _fbextPP_TupleEnum__8(__0,__1,__2,__3,__4,__5,__6,__7) __0, __1, __2, __3, __4, __5, __6, __7
# define _fbextPP_TupleEnum__9(__0,__1,__2,__3,__4,__5,__6,__7,__8) __0, __1, __2, __3, __4, __5, __6, __7, __8
# define _fbextPP_TupleEnum__10(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9
# define _fbextPP_TupleEnum__11(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10
# define _fbextPP_TupleEnum__12(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11
# define _fbextPP_TupleEnum__13(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12
# define _fbextPP_TupleEnum__14(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13
# define _fbextPP_TupleEnum__15(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13,__14) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13, __14
# define _fbextPP_TupleEnum__16(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13,__14,__15) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13, __14, __15
# define _fbextPP_TupleEnum__17(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13,__14,__15,__16) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13, __14, __15, __16
# define _fbextPP_TupleEnum__18(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13,__14,__15,__16,__17) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13, __14, __15, __16, __17
# define _fbextPP_TupleEnum__19(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13,__14,__15,__16,__17,__18) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13, __14, __15, __16, __17, __18
# define _fbextPP_TupleEnum__20(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13,__14,__15,__16,__17,__18,__19) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13, __14, __15, __16, __17, __18, __19
# define _fbextPP_TupleEnum__21(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13,__14,__15,__16,__17,__18,__19,__20) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13, __14, __15, __16, __17, __18, __19, __20
# define _fbextPP_TupleEnum__22(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13,__14,__15,__16,__17,__18,__19,__20,__21) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13, __14, __15, __16, __17, __18, __19, __20, __21
# define _fbextPP_TupleEnum__23(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13,__14,__15,__16,__17,__18,__19,__20,__21,__22) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13, __14, __15, __16, __17, __18, __19, __20, __21, __22
# define _fbextPP_TupleEnum__24(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13,__14,__15,__16,__17,__18,__19,__20,__21,__22,__23) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13, __14, __15, __16, __17, __18, __19, __20, __21, __22, __23
# define _fbextPP_TupleEnum__25(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13,__14,__15,__16,__17,__18,__19,__20,__21,__22,__23,__24) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13, __14, __15, __16, __17, __18, __19, __20, __21, __22, __23, __24
# define _fbextPP_TupleEnum__26(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13,__14,__15,__16,__17,__18,__19,__20,__21,__22,__23,__24,__25) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13, __14, __15, __16, __17, __18, __19, __20, __21, __22, __23, __24, __25
# define _fbextPP_TupleEnum__27(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13,__14,__15,__16,__17,__18,__19,__20,__21,__22,__23,__24,__25,__26) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13, __14, __15, __16, __17, __18, __19, __20, __21, __22, __23, __24, __25, __26
# define _fbextPP_TupleEnum__28(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13,__14,__15,__16,__17,__18,__19,__20,__21,__22,__23,__24,__25,__26,__27) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13, __14, __15, __16, __17, __18, __19, __20, __21, __22, __23, __24, __25, __26, __27
# define _fbextPP_TupleEnum__29(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13,__14,__15,__16,__17,__18,__19,__20,__21,__22,__23,__24,__25,__26,__27,__28) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13, __14, __15, __16, __17, __18, __19, __20, __21, __22, __23, __24, __25, __26, __27, __28
# define _fbextPP_TupleEnum__30(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13,__14,__15,__16,__17,__18,__19,__20,__21,__22,__23,__24,__25,__26,__27,__28,__29) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13, __14, __15, __16, __17, __18, __19, __20, __21, __22, __23, __24, __25, __26, __27, __28, __29
# define _fbextPP_TupleEnum__31(__0,__1,__2,__3,__4,__5,__6,__7,__8,__9,__10,__11,__12,__13,__14,__15,__16,__17,__18,__19,__20,__21,__22,__23,__24,__25,__26,__27,__28,__29,__30) __0, __1, __2, __3, __4, __5, __6, __7, __8, __9, __10, __11, __12, __13, __14, __15, __16, __17, __18, __19, __20, __21, __22, __23, __24, __25, __26, __27, __28, __29, __30

# endif ' include guard
