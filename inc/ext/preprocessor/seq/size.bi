'' Title: ext/preprocessor/seq/size.bi
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
# ifndef FBEXT_INCLUDED_PP_SEQ_SIZE_BI__
# define FBEXT_INCLUDED_PP_SEQ_SIZE_BI__ -1

# include once "ext/preprocessor/cat.bi"

# define FBEXT_PP_SEQ_SIZE(seq) fbextPP_SeqSize(seq)

'' Macro: fbextPP_SeqSize
''  expands to an integer literal number representing the count of elements in
''  a sequence.
''
''  For example, if <seq> is `(a)(b)(c)(d)`, then this macro will expand to
''  `4`.
''
'' Parameters:
''  seq - is a sequence.
# define fbextPP_SeqSize(seq) _
         FBEXT_PP_CAT(_fbextPP_SeqSize_, _fbextPP_SeqSize_0 seq)

# define _fbextPP_SeqSize_0(__) _fbextPP_SeqSize_1
# define _fbextPP_SeqSize_1(__) _fbextPP_SeqSize_2
# define _fbextPP_SeqSize_2(__) _fbextPP_SeqSize_3
# define _fbextPP_SeqSize_3(__) _fbextPP_SeqSize_4
# define _fbextPP_SeqSize_4(__) _fbextPP_SeqSize_5
# define _fbextPP_SeqSize_5(__) _fbextPP_SeqSize_6
# define _fbextPP_SeqSize_6(__) _fbextPP_SeqSize_7
# define _fbextPP_SeqSize_7(__) _fbextPP_SeqSize_8
# define _fbextPP_SeqSize_8(__) _fbextPP_SeqSize_9
# define _fbextPP_SeqSize_9(__) _fbextPP_SeqSize_10
# define _fbextPP_SeqSize_10(__) _fbextPP_SeqSize_11
# define _fbextPP_SeqSize_11(__) _fbextPP_SeqSize_12
# define _fbextPP_SeqSize_12(__) _fbextPP_SeqSize_13
# define _fbextPP_SeqSize_13(__) _fbextPP_SeqSize_14
# define _fbextPP_SeqSize_14(__) _fbextPP_SeqSize_15
# define _fbextPP_SeqSize_15(__) _fbextPP_SeqSize_16
# define _fbextPP_SeqSize_16(__) _fbextPP_SeqSize_17
# define _fbextPP_SeqSize_17(__) _fbextPP_SeqSize_18
# define _fbextPP_SeqSize_18(__) _fbextPP_SeqSize_19
# define _fbextPP_SeqSize_19(__) _fbextPP_SeqSize_20
# define _fbextPP_SeqSize_20(__) _fbextPP_SeqSize_21
# define _fbextPP_SeqSize_21(__) _fbextPP_SeqSize_22
# define _fbextPP_SeqSize_22(__) _fbextPP_SeqSize_23
# define _fbextPP_SeqSize_23(__) _fbextPP_SeqSize_24
# define _fbextPP_SeqSize_24(__) _fbextPP_SeqSize_25
# define _fbextPP_SeqSize_25(__) _fbextPP_SeqSize_26
# define _fbextPP_SeqSize_26(__) _fbextPP_SeqSize_27
# define _fbextPP_SeqSize_27(__) _fbextPP_SeqSize_28
# define _fbextPP_SeqSize_28(__) _fbextPP_SeqSize_29
# define _fbextPP_SeqSize_29(__) _fbextPP_SeqSize_30
# define _fbextPP_SeqSize_30(__) _fbextPP_SeqSize_31
# define _fbextPP_SeqSize_31(__) _fbextPP_SeqSize_32
# define _fbextPP_SeqSize_32(__) _fbextPP_SeqSize_33
# define _fbextPP_SeqSize_33(__) _fbextPP_SeqSize_34
# define _fbextPP_SeqSize_34(__) _fbextPP_SeqSize_35
# define _fbextPP_SeqSize_35(__) _fbextPP_SeqSize_36
# define _fbextPP_SeqSize_36(__) _fbextPP_SeqSize_37
# define _fbextPP_SeqSize_37(__) _fbextPP_SeqSize_38
# define _fbextPP_SeqSize_38(__) _fbextPP_SeqSize_39
# define _fbextPP_SeqSize_39(__) _fbextPP_SeqSize_40
# define _fbextPP_SeqSize_40(__) _fbextPP_SeqSize_41
# define _fbextPP_SeqSize_41(__) _fbextPP_SeqSize_42
# define _fbextPP_SeqSize_42(__) _fbextPP_SeqSize_43
# define _fbextPP_SeqSize_43(__) _fbextPP_SeqSize_44
# define _fbextPP_SeqSize_44(__) _fbextPP_SeqSize_45
# define _fbextPP_SeqSize_45(__) _fbextPP_SeqSize_46
# define _fbextPP_SeqSize_46(__) _fbextPP_SeqSize_47
# define _fbextPP_SeqSize_47(__) _fbextPP_SeqSize_48
# define _fbextPP_SeqSize_48(__) _fbextPP_SeqSize_49
# define _fbextPP_SeqSize_49(__) _fbextPP_SeqSize_50
# define _fbextPP_SeqSize_50(__) _fbextPP_SeqSize_51
# define _fbextPP_SeqSize_51(__) _fbextPP_SeqSize_52
# define _fbextPP_SeqSize_52(__) _fbextPP_SeqSize_53
# define _fbextPP_SeqSize_53(__) _fbextPP_SeqSize_54
# define _fbextPP_SeqSize_54(__) _fbextPP_SeqSize_55
# define _fbextPP_SeqSize_55(__) _fbextPP_SeqSize_56
# define _fbextPP_SeqSize_56(__) _fbextPP_SeqSize_57
# define _fbextPP_SeqSize_57(__) _fbextPP_SeqSize_58
# define _fbextPP_SeqSize_58(__) _fbextPP_SeqSize_59
# define _fbextPP_SeqSize_59(__) _fbextPP_SeqSize_60
# define _fbextPP_SeqSize_60(__) _fbextPP_SeqSize_61
# define _fbextPP_SeqSize_61(__) _fbextPP_SeqSize_62
# define _fbextPP_SeqSize_62(__) _fbextPP_SeqSize_63
# define _fbextPP_SeqSize_63(__) _fbextPP_SeqSize_64
# define _fbextPP_SeqSize_64(__) _fbextPP_SeqSize_65
# define _fbextPP_SeqSize_65(__) _fbextPP_SeqSize_66
# define _fbextPP_SeqSize_66(__) _fbextPP_SeqSize_67
# define _fbextPP_SeqSize_67(__) _fbextPP_SeqSize_68
# define _fbextPP_SeqSize_68(__) _fbextPP_SeqSize_69
# define _fbextPP_SeqSize_69(__) _fbextPP_SeqSize_70
# define _fbextPP_SeqSize_70(__) _fbextPP_SeqSize_71
# define _fbextPP_SeqSize_71(__) _fbextPP_SeqSize_72
# define _fbextPP_SeqSize_72(__) _fbextPP_SeqSize_73
# define _fbextPP_SeqSize_73(__) _fbextPP_SeqSize_74
# define _fbextPP_SeqSize_74(__) _fbextPP_SeqSize_75
# define _fbextPP_SeqSize_75(__) _fbextPP_SeqSize_76
# define _fbextPP_SeqSize_76(__) _fbextPP_SeqSize_77
# define _fbextPP_SeqSize_77(__) _fbextPP_SeqSize_78
# define _fbextPP_SeqSize_78(__) _fbextPP_SeqSize_79
# define _fbextPP_SeqSize_79(__) _fbextPP_SeqSize_80
# define _fbextPP_SeqSize_80(__) _fbextPP_SeqSize_81
# define _fbextPP_SeqSize_81(__) _fbextPP_SeqSize_82
# define _fbextPP_SeqSize_82(__) _fbextPP_SeqSize_83
# define _fbextPP_SeqSize_83(__) _fbextPP_SeqSize_84
# define _fbextPP_SeqSize_84(__) _fbextPP_SeqSize_85
# define _fbextPP_SeqSize_85(__) _fbextPP_SeqSize_86
# define _fbextPP_SeqSize_86(__) _fbextPP_SeqSize_87
# define _fbextPP_SeqSize_87(__) _fbextPP_SeqSize_88
# define _fbextPP_SeqSize_88(__) _fbextPP_SeqSize_89
# define _fbextPP_SeqSize_89(__) _fbextPP_SeqSize_90
# define _fbextPP_SeqSize_90(__) _fbextPP_SeqSize_91
# define _fbextPP_SeqSize_91(__) _fbextPP_SeqSize_92
# define _fbextPP_SeqSize_92(__) _fbextPP_SeqSize_93
# define _fbextPP_SeqSize_93(__) _fbextPP_SeqSize_94
# define _fbextPP_SeqSize_94(__) _fbextPP_SeqSize_95
# define _fbextPP_SeqSize_95(__) _fbextPP_SeqSize_96
# define _fbextPP_SeqSize_96(__) _fbextPP_SeqSize_97
# define _fbextPP_SeqSize_97(__) _fbextPP_SeqSize_98
# define _fbextPP_SeqSize_98(__) _fbextPP_SeqSize_99
# define _fbextPP_SeqSize_99(__) _fbextPP_SeqSize_100
# define _fbextPP_SeqSize_100(__) _fbextPP_SeqSize_101
# define _fbextPP_SeqSize_101(__) _fbextPP_SeqSize_102
# define _fbextPP_SeqSize_102(__) _fbextPP_SeqSize_103
# define _fbextPP_SeqSize_103(__) _fbextPP_SeqSize_104
# define _fbextPP_SeqSize_104(__) _fbextPP_SeqSize_105
# define _fbextPP_SeqSize_105(__) _fbextPP_SeqSize_106
# define _fbextPP_SeqSize_106(__) _fbextPP_SeqSize_107
# define _fbextPP_SeqSize_107(__) _fbextPP_SeqSize_108
# define _fbextPP_SeqSize_108(__) _fbextPP_SeqSize_109
# define _fbextPP_SeqSize_109(__) _fbextPP_SeqSize_110
# define _fbextPP_SeqSize_110(__) _fbextPP_SeqSize_111
# define _fbextPP_SeqSize_111(__) _fbextPP_SeqSize_112
# define _fbextPP_SeqSize_112(__) _fbextPP_SeqSize_113
# define _fbextPP_SeqSize_113(__) _fbextPP_SeqSize_114
# define _fbextPP_SeqSize_114(__) _fbextPP_SeqSize_115
# define _fbextPP_SeqSize_115(__) _fbextPP_SeqSize_116
# define _fbextPP_SeqSize_116(__) _fbextPP_SeqSize_117
# define _fbextPP_SeqSize_117(__) _fbextPP_SeqSize_118
# define _fbextPP_SeqSize_118(__) _fbextPP_SeqSize_119
# define _fbextPP_SeqSize_119(__) _fbextPP_SeqSize_120
# define _fbextPP_SeqSize_120(__) _fbextPP_SeqSize_121
# define _fbextPP_SeqSize_121(__) _fbextPP_SeqSize_122
# define _fbextPP_SeqSize_122(__) _fbextPP_SeqSize_123
# define _fbextPP_SeqSize_123(__) _fbextPP_SeqSize_124
# define _fbextPP_SeqSize_124(__) _fbextPP_SeqSize_125
# define _fbextPP_SeqSize_125(__) _fbextPP_SeqSize_126
# define _fbextPP_SeqSize_126(__) _fbextPP_SeqSize_127
# define _fbextPP_SeqSize_127(__) _fbextPP_SeqSize_128
# define _fbextPP_SeqSize_128(__) _fbextPP_SeqSize_129
# define _fbextPP_SeqSize_129(__) _fbextPP_SeqSize_130
# define _fbextPP_SeqSize_130(__) _fbextPP_SeqSize_131
# define _fbextPP_SeqSize_131(__) _fbextPP_SeqSize_132
# define _fbextPP_SeqSize_132(__) _fbextPP_SeqSize_133
# define _fbextPP_SeqSize_133(__) _fbextPP_SeqSize_134
# define _fbextPP_SeqSize_134(__) _fbextPP_SeqSize_135
# define _fbextPP_SeqSize_135(__) _fbextPP_SeqSize_136
# define _fbextPP_SeqSize_136(__) _fbextPP_SeqSize_137
# define _fbextPP_SeqSize_137(__) _fbextPP_SeqSize_138
# define _fbextPP_SeqSize_138(__) _fbextPP_SeqSize_139
# define _fbextPP_SeqSize_139(__) _fbextPP_SeqSize_140
# define _fbextPP_SeqSize_140(__) _fbextPP_SeqSize_141
# define _fbextPP_SeqSize_141(__) _fbextPP_SeqSize_142
# define _fbextPP_SeqSize_142(__) _fbextPP_SeqSize_143
# define _fbextPP_SeqSize_143(__) _fbextPP_SeqSize_144
# define _fbextPP_SeqSize_144(__) _fbextPP_SeqSize_145
# define _fbextPP_SeqSize_145(__) _fbextPP_SeqSize_146
# define _fbextPP_SeqSize_146(__) _fbextPP_SeqSize_147
# define _fbextPP_SeqSize_147(__) _fbextPP_SeqSize_148
# define _fbextPP_SeqSize_148(__) _fbextPP_SeqSize_149
# define _fbextPP_SeqSize_149(__) _fbextPP_SeqSize_150
# define _fbextPP_SeqSize_150(__) _fbextPP_SeqSize_151
# define _fbextPP_SeqSize_151(__) _fbextPP_SeqSize_152
# define _fbextPP_SeqSize_152(__) _fbextPP_SeqSize_153
# define _fbextPP_SeqSize_153(__) _fbextPP_SeqSize_154
# define _fbextPP_SeqSize_154(__) _fbextPP_SeqSize_155
# define _fbextPP_SeqSize_155(__) _fbextPP_SeqSize_156
# define _fbextPP_SeqSize_156(__) _fbextPP_SeqSize_157
# define _fbextPP_SeqSize_157(__) _fbextPP_SeqSize_158
# define _fbextPP_SeqSize_158(__) _fbextPP_SeqSize_159
# define _fbextPP_SeqSize_159(__) _fbextPP_SeqSize_160
# define _fbextPP_SeqSize_160(__) _fbextPP_SeqSize_161
# define _fbextPP_SeqSize_161(__) _fbextPP_SeqSize_162
# define _fbextPP_SeqSize_162(__) _fbextPP_SeqSize_163
# define _fbextPP_SeqSize_163(__) _fbextPP_SeqSize_164
# define _fbextPP_SeqSize_164(__) _fbextPP_SeqSize_165
# define _fbextPP_SeqSize_165(__) _fbextPP_SeqSize_166
# define _fbextPP_SeqSize_166(__) _fbextPP_SeqSize_167
# define _fbextPP_SeqSize_167(__) _fbextPP_SeqSize_168
# define _fbextPP_SeqSize_168(__) _fbextPP_SeqSize_169
# define _fbextPP_SeqSize_169(__) _fbextPP_SeqSize_170
# define _fbextPP_SeqSize_170(__) _fbextPP_SeqSize_171
# define _fbextPP_SeqSize_171(__) _fbextPP_SeqSize_172
# define _fbextPP_SeqSize_172(__) _fbextPP_SeqSize_173
# define _fbextPP_SeqSize_173(__) _fbextPP_SeqSize_174
# define _fbextPP_SeqSize_174(__) _fbextPP_SeqSize_175
# define _fbextPP_SeqSize_175(__) _fbextPP_SeqSize_176
# define _fbextPP_SeqSize_176(__) _fbextPP_SeqSize_177
# define _fbextPP_SeqSize_177(__) _fbextPP_SeqSize_178
# define _fbextPP_SeqSize_178(__) _fbextPP_SeqSize_179
# define _fbextPP_SeqSize_179(__) _fbextPP_SeqSize_180
# define _fbextPP_SeqSize_180(__) _fbextPP_SeqSize_181
# define _fbextPP_SeqSize_181(__) _fbextPP_SeqSize_182
# define _fbextPP_SeqSize_182(__) _fbextPP_SeqSize_183
# define _fbextPP_SeqSize_183(__) _fbextPP_SeqSize_184
# define _fbextPP_SeqSize_184(__) _fbextPP_SeqSize_185
# define _fbextPP_SeqSize_185(__) _fbextPP_SeqSize_186
# define _fbextPP_SeqSize_186(__) _fbextPP_SeqSize_187
# define _fbextPP_SeqSize_187(__) _fbextPP_SeqSize_188
# define _fbextPP_SeqSize_188(__) _fbextPP_SeqSize_189
# define _fbextPP_SeqSize_189(__) _fbextPP_SeqSize_190
# define _fbextPP_SeqSize_190(__) _fbextPP_SeqSize_191
# define _fbextPP_SeqSize_191(__) _fbextPP_SeqSize_192
# define _fbextPP_SeqSize_192(__) _fbextPP_SeqSize_193
# define _fbextPP_SeqSize_193(__) _fbextPP_SeqSize_194
# define _fbextPP_SeqSize_194(__) _fbextPP_SeqSize_195
# define _fbextPP_SeqSize_195(__) _fbextPP_SeqSize_196
# define _fbextPP_SeqSize_196(__) _fbextPP_SeqSize_197
# define _fbextPP_SeqSize_197(__) _fbextPP_SeqSize_198
# define _fbextPP_SeqSize_198(__) _fbextPP_SeqSize_199
# define _fbextPP_SeqSize_199(__) _fbextPP_SeqSize_200
# define _fbextPP_SeqSize_200(__) _fbextPP_SeqSize_201
# define _fbextPP_SeqSize_201(__) _fbextPP_SeqSize_202
# define _fbextPP_SeqSize_202(__) _fbextPP_SeqSize_203
# define _fbextPP_SeqSize_203(__) _fbextPP_SeqSize_204
# define _fbextPP_SeqSize_204(__) _fbextPP_SeqSize_205
# define _fbextPP_SeqSize_205(__) _fbextPP_SeqSize_206
# define _fbextPP_SeqSize_206(__) _fbextPP_SeqSize_207
# define _fbextPP_SeqSize_207(__) _fbextPP_SeqSize_208
# define _fbextPP_SeqSize_208(__) _fbextPP_SeqSize_209
# define _fbextPP_SeqSize_209(__) _fbextPP_SeqSize_210
# define _fbextPP_SeqSize_210(__) _fbextPP_SeqSize_211
# define _fbextPP_SeqSize_211(__) _fbextPP_SeqSize_212
# define _fbextPP_SeqSize_212(__) _fbextPP_SeqSize_213
# define _fbextPP_SeqSize_213(__) _fbextPP_SeqSize_214
# define _fbextPP_SeqSize_214(__) _fbextPP_SeqSize_215
# define _fbextPP_SeqSize_215(__) _fbextPP_SeqSize_216
# define _fbextPP_SeqSize_216(__) _fbextPP_SeqSize_217
# define _fbextPP_SeqSize_217(__) _fbextPP_SeqSize_218
# define _fbextPP_SeqSize_218(__) _fbextPP_SeqSize_219
# define _fbextPP_SeqSize_219(__) _fbextPP_SeqSize_220
# define _fbextPP_SeqSize_220(__) _fbextPP_SeqSize_221
# define _fbextPP_SeqSize_221(__) _fbextPP_SeqSize_222
# define _fbextPP_SeqSize_222(__) _fbextPP_SeqSize_223
# define _fbextPP_SeqSize_223(__) _fbextPP_SeqSize_224
# define _fbextPP_SeqSize_224(__) _fbextPP_SeqSize_225
# define _fbextPP_SeqSize_225(__) _fbextPP_SeqSize_226
# define _fbextPP_SeqSize_226(__) _fbextPP_SeqSize_227
# define _fbextPP_SeqSize_227(__) _fbextPP_SeqSize_228
# define _fbextPP_SeqSize_228(__) _fbextPP_SeqSize_229
# define _fbextPP_SeqSize_229(__) _fbextPP_SeqSize_230
# define _fbextPP_SeqSize_230(__) _fbextPP_SeqSize_231
# define _fbextPP_SeqSize_231(__) _fbextPP_SeqSize_232
# define _fbextPP_SeqSize_232(__) _fbextPP_SeqSize_233
# define _fbextPP_SeqSize_233(__) _fbextPP_SeqSize_234
# define _fbextPP_SeqSize_234(__) _fbextPP_SeqSize_235
# define _fbextPP_SeqSize_235(__) _fbextPP_SeqSize_236
# define _fbextPP_SeqSize_236(__) _fbextPP_SeqSize_237
# define _fbextPP_SeqSize_237(__) _fbextPP_SeqSize_238
# define _fbextPP_SeqSize_238(__) _fbextPP_SeqSize_239
# define _fbextPP_SeqSize_239(__) _fbextPP_SeqSize_240
# define _fbextPP_SeqSize_240(__) _fbextPP_SeqSize_241
# define _fbextPP_SeqSize_241(__) _fbextPP_SeqSize_242
# define _fbextPP_SeqSize_242(__) _fbextPP_SeqSize_243
# define _fbextPP_SeqSize_243(__) _fbextPP_SeqSize_244
# define _fbextPP_SeqSize_244(__) _fbextPP_SeqSize_245
# define _fbextPP_SeqSize_245(__) _fbextPP_SeqSize_246
# define _fbextPP_SeqSize_246(__) _fbextPP_SeqSize_247
# define _fbextPP_SeqSize_247(__) _fbextPP_SeqSize_248
# define _fbextPP_SeqSize_248(__) _fbextPP_SeqSize_249
# define _fbextPP_SeqSize_249(__) _fbextPP_SeqSize_250
# define _fbextPP_SeqSize_250(__) _fbextPP_SeqSize_251
# define _fbextPP_SeqSize_251(__) _fbextPP_SeqSize_252
# define _fbextPP_SeqSize_252(__) _fbextPP_SeqSize_253
# define _fbextPP_SeqSize_253(__) _fbextPP_SeqSize_254
# define _fbextPP_SeqSize_254(__) _fbextPP_SeqSize_255
# define _fbextPP_SeqSize_255(__) _fbextPP_SeqSize_256

# define _fbextPP_SeqSize__fbextPP_SeqSize_0 0
# define _fbextPP_SeqSize__fbextPP_SeqSize_1 1
# define _fbextPP_SeqSize__fbextPP_SeqSize_2 2
# define _fbextPP_SeqSize__fbextPP_SeqSize_3 3
# define _fbextPP_SeqSize__fbextPP_SeqSize_4 4
# define _fbextPP_SeqSize__fbextPP_SeqSize_5 5
# define _fbextPP_SeqSize__fbextPP_SeqSize_6 6
# define _fbextPP_SeqSize__fbextPP_SeqSize_7 7
# define _fbextPP_SeqSize__fbextPP_SeqSize_8 8
# define _fbextPP_SeqSize__fbextPP_SeqSize_9 9
# define _fbextPP_SeqSize__fbextPP_SeqSize_10 10
# define _fbextPP_SeqSize__fbextPP_SeqSize_11 11
# define _fbextPP_SeqSize__fbextPP_SeqSize_12 12
# define _fbextPP_SeqSize__fbextPP_SeqSize_13 13
# define _fbextPP_SeqSize__fbextPP_SeqSize_14 14
# define _fbextPP_SeqSize__fbextPP_SeqSize_15 15
# define _fbextPP_SeqSize__fbextPP_SeqSize_16 16
# define _fbextPP_SeqSize__fbextPP_SeqSize_17 17
# define _fbextPP_SeqSize__fbextPP_SeqSize_18 18
# define _fbextPP_SeqSize__fbextPP_SeqSize_19 19
# define _fbextPP_SeqSize__fbextPP_SeqSize_20 20
# define _fbextPP_SeqSize__fbextPP_SeqSize_21 21
# define _fbextPP_SeqSize__fbextPP_SeqSize_22 22
# define _fbextPP_SeqSize__fbextPP_SeqSize_23 23
# define _fbextPP_SeqSize__fbextPP_SeqSize_24 24
# define _fbextPP_SeqSize__fbextPP_SeqSize_25 25
# define _fbextPP_SeqSize__fbextPP_SeqSize_26 26
# define _fbextPP_SeqSize__fbextPP_SeqSize_27 27
# define _fbextPP_SeqSize__fbextPP_SeqSize_28 28
# define _fbextPP_SeqSize__fbextPP_SeqSize_29 29
# define _fbextPP_SeqSize__fbextPP_SeqSize_30 30
# define _fbextPP_SeqSize__fbextPP_SeqSize_31 31
# define _fbextPP_SeqSize__fbextPP_SeqSize_32 32
# define _fbextPP_SeqSize__fbextPP_SeqSize_33 33
# define _fbextPP_SeqSize__fbextPP_SeqSize_34 34
# define _fbextPP_SeqSize__fbextPP_SeqSize_35 35
# define _fbextPP_SeqSize__fbextPP_SeqSize_36 36
# define _fbextPP_SeqSize__fbextPP_SeqSize_37 37
# define _fbextPP_SeqSize__fbextPP_SeqSize_38 38
# define _fbextPP_SeqSize__fbextPP_SeqSize_39 39
# define _fbextPP_SeqSize__fbextPP_SeqSize_40 40
# define _fbextPP_SeqSize__fbextPP_SeqSize_41 41
# define _fbextPP_SeqSize__fbextPP_SeqSize_42 42
# define _fbextPP_SeqSize__fbextPP_SeqSize_43 43
# define _fbextPP_SeqSize__fbextPP_SeqSize_44 44
# define _fbextPP_SeqSize__fbextPP_SeqSize_45 45
# define _fbextPP_SeqSize__fbextPP_SeqSize_46 46
# define _fbextPP_SeqSize__fbextPP_SeqSize_47 47
# define _fbextPP_SeqSize__fbextPP_SeqSize_48 48
# define _fbextPP_SeqSize__fbextPP_SeqSize_49 49
# define _fbextPP_SeqSize__fbextPP_SeqSize_50 50
# define _fbextPP_SeqSize__fbextPP_SeqSize_51 51
# define _fbextPP_SeqSize__fbextPP_SeqSize_52 52
# define _fbextPP_SeqSize__fbextPP_SeqSize_53 53
# define _fbextPP_SeqSize__fbextPP_SeqSize_54 54
# define _fbextPP_SeqSize__fbextPP_SeqSize_55 55
# define _fbextPP_SeqSize__fbextPP_SeqSize_56 56
# define _fbextPP_SeqSize__fbextPP_SeqSize_57 57
# define _fbextPP_SeqSize__fbextPP_SeqSize_58 58
# define _fbextPP_SeqSize__fbextPP_SeqSize_59 59
# define _fbextPP_SeqSize__fbextPP_SeqSize_60 60
# define _fbextPP_SeqSize__fbextPP_SeqSize_61 61
# define _fbextPP_SeqSize__fbextPP_SeqSize_62 62
# define _fbextPP_SeqSize__fbextPP_SeqSize_63 63
# define _fbextPP_SeqSize__fbextPP_SeqSize_64 64
# define _fbextPP_SeqSize__fbextPP_SeqSize_65 65
# define _fbextPP_SeqSize__fbextPP_SeqSize_66 66
# define _fbextPP_SeqSize__fbextPP_SeqSize_67 67
# define _fbextPP_SeqSize__fbextPP_SeqSize_68 68
# define _fbextPP_SeqSize__fbextPP_SeqSize_69 69
# define _fbextPP_SeqSize__fbextPP_SeqSize_70 70
# define _fbextPP_SeqSize__fbextPP_SeqSize_71 71
# define _fbextPP_SeqSize__fbextPP_SeqSize_72 72
# define _fbextPP_SeqSize__fbextPP_SeqSize_73 73
# define _fbextPP_SeqSize__fbextPP_SeqSize_74 74
# define _fbextPP_SeqSize__fbextPP_SeqSize_75 75
# define _fbextPP_SeqSize__fbextPP_SeqSize_76 76
# define _fbextPP_SeqSize__fbextPP_SeqSize_77 77
# define _fbextPP_SeqSize__fbextPP_SeqSize_78 78
# define _fbextPP_SeqSize__fbextPP_SeqSize_79 79
# define _fbextPP_SeqSize__fbextPP_SeqSize_80 80
# define _fbextPP_SeqSize__fbextPP_SeqSize_81 81
# define _fbextPP_SeqSize__fbextPP_SeqSize_82 82
# define _fbextPP_SeqSize__fbextPP_SeqSize_83 83
# define _fbextPP_SeqSize__fbextPP_SeqSize_84 84
# define _fbextPP_SeqSize__fbextPP_SeqSize_85 85
# define _fbextPP_SeqSize__fbextPP_SeqSize_86 86
# define _fbextPP_SeqSize__fbextPP_SeqSize_87 87
# define _fbextPP_SeqSize__fbextPP_SeqSize_88 88
# define _fbextPP_SeqSize__fbextPP_SeqSize_89 89
# define _fbextPP_SeqSize__fbextPP_SeqSize_90 90
# define _fbextPP_SeqSize__fbextPP_SeqSize_91 91
# define _fbextPP_SeqSize__fbextPP_SeqSize_92 92
# define _fbextPP_SeqSize__fbextPP_SeqSize_93 93
# define _fbextPP_SeqSize__fbextPP_SeqSize_94 94
# define _fbextPP_SeqSize__fbextPP_SeqSize_95 95
# define _fbextPP_SeqSize__fbextPP_SeqSize_96 96
# define _fbextPP_SeqSize__fbextPP_SeqSize_97 97
# define _fbextPP_SeqSize__fbextPP_SeqSize_98 98
# define _fbextPP_SeqSize__fbextPP_SeqSize_99 99
# define _fbextPP_SeqSize__fbextPP_SeqSize_100 100
# define _fbextPP_SeqSize__fbextPP_SeqSize_101 101
# define _fbextPP_SeqSize__fbextPP_SeqSize_102 102
# define _fbextPP_SeqSize__fbextPP_SeqSize_103 103
# define _fbextPP_SeqSize__fbextPP_SeqSize_104 104
# define _fbextPP_SeqSize__fbextPP_SeqSize_105 105
# define _fbextPP_SeqSize__fbextPP_SeqSize_106 106
# define _fbextPP_SeqSize__fbextPP_SeqSize_107 107
# define _fbextPP_SeqSize__fbextPP_SeqSize_108 108
# define _fbextPP_SeqSize__fbextPP_SeqSize_109 109
# define _fbextPP_SeqSize__fbextPP_SeqSize_110 110
# define _fbextPP_SeqSize__fbextPP_SeqSize_111 111
# define _fbextPP_SeqSize__fbextPP_SeqSize_112 112
# define _fbextPP_SeqSize__fbextPP_SeqSize_113 113
# define _fbextPP_SeqSize__fbextPP_SeqSize_114 114
# define _fbextPP_SeqSize__fbextPP_SeqSize_115 115
# define _fbextPP_SeqSize__fbextPP_SeqSize_116 116
# define _fbextPP_SeqSize__fbextPP_SeqSize_117 117
# define _fbextPP_SeqSize__fbextPP_SeqSize_118 118
# define _fbextPP_SeqSize__fbextPP_SeqSize_119 119
# define _fbextPP_SeqSize__fbextPP_SeqSize_120 120
# define _fbextPP_SeqSize__fbextPP_SeqSize_121 121
# define _fbextPP_SeqSize__fbextPP_SeqSize_122 122
# define _fbextPP_SeqSize__fbextPP_SeqSize_123 123
# define _fbextPP_SeqSize__fbextPP_SeqSize_124 124
# define _fbextPP_SeqSize__fbextPP_SeqSize_125 125
# define _fbextPP_SeqSize__fbextPP_SeqSize_126 126
# define _fbextPP_SeqSize__fbextPP_SeqSize_127 127
# define _fbextPP_SeqSize__fbextPP_SeqSize_128 128
# define _fbextPP_SeqSize__fbextPP_SeqSize_129 129
# define _fbextPP_SeqSize__fbextPP_SeqSize_130 130
# define _fbextPP_SeqSize__fbextPP_SeqSize_131 131
# define _fbextPP_SeqSize__fbextPP_SeqSize_132 132
# define _fbextPP_SeqSize__fbextPP_SeqSize_133 133
# define _fbextPP_SeqSize__fbextPP_SeqSize_134 134
# define _fbextPP_SeqSize__fbextPP_SeqSize_135 135
# define _fbextPP_SeqSize__fbextPP_SeqSize_136 136
# define _fbextPP_SeqSize__fbextPP_SeqSize_137 137
# define _fbextPP_SeqSize__fbextPP_SeqSize_138 138
# define _fbextPP_SeqSize__fbextPP_SeqSize_139 139
# define _fbextPP_SeqSize__fbextPP_SeqSize_140 140
# define _fbextPP_SeqSize__fbextPP_SeqSize_141 141
# define _fbextPP_SeqSize__fbextPP_SeqSize_142 142
# define _fbextPP_SeqSize__fbextPP_SeqSize_143 143
# define _fbextPP_SeqSize__fbextPP_SeqSize_144 144
# define _fbextPP_SeqSize__fbextPP_SeqSize_145 145
# define _fbextPP_SeqSize__fbextPP_SeqSize_146 146
# define _fbextPP_SeqSize__fbextPP_SeqSize_147 147
# define _fbextPP_SeqSize__fbextPP_SeqSize_148 148
# define _fbextPP_SeqSize__fbextPP_SeqSize_149 149
# define _fbextPP_SeqSize__fbextPP_SeqSize_150 150
# define _fbextPP_SeqSize__fbextPP_SeqSize_151 151
# define _fbextPP_SeqSize__fbextPP_SeqSize_152 152
# define _fbextPP_SeqSize__fbextPP_SeqSize_153 153
# define _fbextPP_SeqSize__fbextPP_SeqSize_154 154
# define _fbextPP_SeqSize__fbextPP_SeqSize_155 155
# define _fbextPP_SeqSize__fbextPP_SeqSize_156 156
# define _fbextPP_SeqSize__fbextPP_SeqSize_157 157
# define _fbextPP_SeqSize__fbextPP_SeqSize_158 158
# define _fbextPP_SeqSize__fbextPP_SeqSize_159 159
# define _fbextPP_SeqSize__fbextPP_SeqSize_160 160
# define _fbextPP_SeqSize__fbextPP_SeqSize_161 161
# define _fbextPP_SeqSize__fbextPP_SeqSize_162 162
# define _fbextPP_SeqSize__fbextPP_SeqSize_163 163
# define _fbextPP_SeqSize__fbextPP_SeqSize_164 164
# define _fbextPP_SeqSize__fbextPP_SeqSize_165 165
# define _fbextPP_SeqSize__fbextPP_SeqSize_166 166
# define _fbextPP_SeqSize__fbextPP_SeqSize_167 167
# define _fbextPP_SeqSize__fbextPP_SeqSize_168 168
# define _fbextPP_SeqSize__fbextPP_SeqSize_169 169
# define _fbextPP_SeqSize__fbextPP_SeqSize_170 170
# define _fbextPP_SeqSize__fbextPP_SeqSize_171 171
# define _fbextPP_SeqSize__fbextPP_SeqSize_172 172
# define _fbextPP_SeqSize__fbextPP_SeqSize_173 173
# define _fbextPP_SeqSize__fbextPP_SeqSize_174 174
# define _fbextPP_SeqSize__fbextPP_SeqSize_175 175
# define _fbextPP_SeqSize__fbextPP_SeqSize_176 176
# define _fbextPP_SeqSize__fbextPP_SeqSize_177 177
# define _fbextPP_SeqSize__fbextPP_SeqSize_178 178
# define _fbextPP_SeqSize__fbextPP_SeqSize_179 179
# define _fbextPP_SeqSize__fbextPP_SeqSize_180 180
# define _fbextPP_SeqSize__fbextPP_SeqSize_181 181
# define _fbextPP_SeqSize__fbextPP_SeqSize_182 182
# define _fbextPP_SeqSize__fbextPP_SeqSize_183 183
# define _fbextPP_SeqSize__fbextPP_SeqSize_184 184
# define _fbextPP_SeqSize__fbextPP_SeqSize_185 185
# define _fbextPP_SeqSize__fbextPP_SeqSize_186 186
# define _fbextPP_SeqSize__fbextPP_SeqSize_187 187
# define _fbextPP_SeqSize__fbextPP_SeqSize_188 188
# define _fbextPP_SeqSize__fbextPP_SeqSize_189 189
# define _fbextPP_SeqSize__fbextPP_SeqSize_190 190
# define _fbextPP_SeqSize__fbextPP_SeqSize_191 191
# define _fbextPP_SeqSize__fbextPP_SeqSize_192 192
# define _fbextPP_SeqSize__fbextPP_SeqSize_193 193
# define _fbextPP_SeqSize__fbextPP_SeqSize_194 194
# define _fbextPP_SeqSize__fbextPP_SeqSize_195 195
# define _fbextPP_SeqSize__fbextPP_SeqSize_196 196
# define _fbextPP_SeqSize__fbextPP_SeqSize_197 197
# define _fbextPP_SeqSize__fbextPP_SeqSize_198 198
# define _fbextPP_SeqSize__fbextPP_SeqSize_199 199
# define _fbextPP_SeqSize__fbextPP_SeqSize_200 200
# define _fbextPP_SeqSize__fbextPP_SeqSize_201 201
# define _fbextPP_SeqSize__fbextPP_SeqSize_202 202
# define _fbextPP_SeqSize__fbextPP_SeqSize_203 203
# define _fbextPP_SeqSize__fbextPP_SeqSize_204 204
# define _fbextPP_SeqSize__fbextPP_SeqSize_205 205
# define _fbextPP_SeqSize__fbextPP_SeqSize_206 206
# define _fbextPP_SeqSize__fbextPP_SeqSize_207 207
# define _fbextPP_SeqSize__fbextPP_SeqSize_208 208
# define _fbextPP_SeqSize__fbextPP_SeqSize_209 209
# define _fbextPP_SeqSize__fbextPP_SeqSize_210 210
# define _fbextPP_SeqSize__fbextPP_SeqSize_211 211
# define _fbextPP_SeqSize__fbextPP_SeqSize_212 212
# define _fbextPP_SeqSize__fbextPP_SeqSize_213 213
# define _fbextPP_SeqSize__fbextPP_SeqSize_214 214
# define _fbextPP_SeqSize__fbextPP_SeqSize_215 215
# define _fbextPP_SeqSize__fbextPP_SeqSize_216 216
# define _fbextPP_SeqSize__fbextPP_SeqSize_217 217
# define _fbextPP_SeqSize__fbextPP_SeqSize_218 218
# define _fbextPP_SeqSize__fbextPP_SeqSize_219 219
# define _fbextPP_SeqSize__fbextPP_SeqSize_220 220
# define _fbextPP_SeqSize__fbextPP_SeqSize_221 221
# define _fbextPP_SeqSize__fbextPP_SeqSize_222 222
# define _fbextPP_SeqSize__fbextPP_SeqSize_223 223
# define _fbextPP_SeqSize__fbextPP_SeqSize_224 224
# define _fbextPP_SeqSize__fbextPP_SeqSize_225 225
# define _fbextPP_SeqSize__fbextPP_SeqSize_226 226
# define _fbextPP_SeqSize__fbextPP_SeqSize_227 227
# define _fbextPP_SeqSize__fbextPP_SeqSize_228 228
# define _fbextPP_SeqSize__fbextPP_SeqSize_229 229
# define _fbextPP_SeqSize__fbextPP_SeqSize_230 230
# define _fbextPP_SeqSize__fbextPP_SeqSize_231 231
# define _fbextPP_SeqSize__fbextPP_SeqSize_232 232
# define _fbextPP_SeqSize__fbextPP_SeqSize_233 233
# define _fbextPP_SeqSize__fbextPP_SeqSize_234 234
# define _fbextPP_SeqSize__fbextPP_SeqSize_235 235
# define _fbextPP_SeqSize__fbextPP_SeqSize_236 236
# define _fbextPP_SeqSize__fbextPP_SeqSize_237 237
# define _fbextPP_SeqSize__fbextPP_SeqSize_238 238
# define _fbextPP_SeqSize__fbextPP_SeqSize_239 239
# define _fbextPP_SeqSize__fbextPP_SeqSize_240 240
# define _fbextPP_SeqSize__fbextPP_SeqSize_241 241
# define _fbextPP_SeqSize__fbextPP_SeqSize_242 242
# define _fbextPP_SeqSize__fbextPP_SeqSize_243 243
# define _fbextPP_SeqSize__fbextPP_SeqSize_244 244
# define _fbextPP_SeqSize__fbextPP_SeqSize_245 245
# define _fbextPP_SeqSize__fbextPP_SeqSize_246 246
# define _fbextPP_SeqSize__fbextPP_SeqSize_247 247
# define _fbextPP_SeqSize__fbextPP_SeqSize_248 248
# define _fbextPP_SeqSize__fbextPP_SeqSize_249 249
# define _fbextPP_SeqSize__fbextPP_SeqSize_250 250
# define _fbextPP_SeqSize__fbextPP_SeqSize_251 251
# define _fbextPP_SeqSize__fbextPP_SeqSize_252 252
# define _fbextPP_SeqSize__fbextPP_SeqSize_253 253
# define _fbextPP_SeqSize__fbextPP_SeqSize_254 254
# define _fbextPP_SeqSize__fbextPP_SeqSize_255 255
# define _fbextPP_SeqSize__fbextPP_SeqSize_256 256

# endif ' include guard
