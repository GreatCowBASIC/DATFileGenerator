''
''
'' ntsecpkg -- header translated with help of SWIG FB wrapper
''
'' NOTICE: This file is part of the FreeBASIC Compiler package and can't
''         be included in other distributions without authorization.
''
''
#ifndef __win_ntsecpkg_bi__
#define __win_ntsecpkg_bi__

#define ISC_REQ_DELEGATE 1
#define ISC_REQ_MUTUAL_AUTH 2
#define ISC_REQ_REPLAY_DETECT 4
#define ISC_REQ_SEQUENCE_DETECT 8
#define ISC_REQ_CONFIDENTIALITY 16
#define ISC_REQ_USE_SESSION_KEY 32
#define ISC_REQ_PROMPT_FOR_CREDS 64
#define ISC_REQ_USE_SUPPLIED_CREDS 128
#define ISC_REQ_ALLOCATE_MEMORY 256
#define ISC_REQ_USE_DCE_STYLE 512
#define ISC_REQ_DATAGRAM 1024
#define ISC_REQ_CONNECTION 2048
#define ISC_REQ_EXTENDED_ERROR 16384
#define ISC_REQ_STREAM 32768
#define ISC_REQ_INTEGRITY 65536
#define ISC_REQ_MANUAL_CRED_VALIDATION 524288
#define ISC_REQ_HTTP 268435456
#define ISC_RET_EXTENDED_ERROR 16384
#define ASC_REQ_DELEGATE 1
#define ASC_REQ_MUTUAL_AUTH 2
#define ASC_REQ_REPLAY_DETECT 4
#define ASC_REQ_SEQUENCE_DETECT 8
#define ASC_REQ_CONFIDENTIALITY 16
#define ASC_REQ_USE_SESSION_KEY 32
#define ASC_REQ_ALLOCATE_MEMORY 256
#define ASC_REQ_USE_DCE_STYLE 512
#define ASC_REQ_DATAGRAM 1024
#define ASC_REQ_CONNECTION 2048
#define ASC_REQ_EXTENDED_ERROR 32768
#define ASC_REQ_STREAM 65536
#define ASC_REQ_INTEGRITY 131072
#define SECURITY_NATIVE_DREP 16
#define SECURITY_NETWORK_DREP 0

#endif
