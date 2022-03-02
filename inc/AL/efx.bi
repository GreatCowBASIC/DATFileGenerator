#ifndef __efx_bi__
#define __efx_bi__

#define ALC_EXT_EFX_NAME "ALC_EXT_EFX"

/'*
 * Context definitions to be used with alcCreateContext.
 * These values must be unique and not conflict with other 
 * al context values.
 '/
 
#define ALC_EFX_MAJOR_VERSION &h20001
#define ALC_EFX_MINOR_VERSION &h20002
#define ALC_MAX_AUXILIARY_SENDS &h20003

/'*
 * Listener definitions to be used with alListener functions.
 * These values must be unique and not conflict with other 
 * al listener values.
 '/
 
#define AL_METERS_PER_UNIT &h20004

/'*
 * Source definitions to be used with alSource functions.
 * These values must be unique and not conflict with other 
 * al source values.
 '/
 
#define AL_DIRECT_FILTER &h20005
#define AL_AUXILIARY_SEND_FILTER &h20006
#define AL_AIR_ABSORPTION_FACTOR &h20007
#define AL_ROOM_ROLLOFF_FACTOR &h20008
#define AL_CONE_OUTER_GAINHF &h20009
#define AL_DIRECT_FILTER_GAINHF_AUTO &h2000A
#define AL_AUXILIARY_SEND_FILTER_GAIN_AUTO &h2000B
#define AL_AUXILIARY_SEND_FILTER_GAINHF_AUTO &h2000C


#define AL_REVERB_DENSITY &h0001
#define AL_REVERB_DIFFUSION &h0002
#define AL_REVERB_GAIN &h0003
#define AL_REVERB_GAINHF &h0004
#define AL_REVERB_DECAY_TIME &h0005
#define AL_REVERB_DECAY_HFRATIO &h0006
#define AL_REVERB_REFLECTIONS_GAIN &h0007
#define AL_REVERB_REFLECTIONS_DELAY &h0008
#define AL_REVERB_LATE_REVERB_GAIN &h0009
#define AL_REVERB_LATE_REVERB_DELAY &h000A
#define AL_REVERB_AIR_ABSORPTION_GAINHF &h000B
#define AL_REVERB_ROOM_ROLLOFF_FACTOR &h000C
#define AL_REVERB_DECAY_HFLIMIT &h000D
#define AL_CHORUS_WAVEFORM &h0001
#define AL_CHORUS_PHASE &h0002
#define AL_CHORUS_RATE &h0003
#define AL_CHORUS_DEPTH &h0004
#define AL_CHORUS_FEEDBACK &h0005
#define AL_CHORUS_DELAY &h0006
#define AL_DISTORTION_EDGE &h0001
#define AL_DISTORTION_GAIN &h0002
#define AL_DISTORTION_LOWPASS_CUTOFF &h0003
#define AL_DISTORTION_EQCENTER &h0004
#define AL_DISTORTION_EQBANDWIDTH &h0005
#define AL_ECHO_DELAY &h0001
#define AL_ECHO_LRDELAY &h0002
#define AL_ECHO_DAMPING &h0003
#define AL_ECHO_FEEDBACK &h0004
#define AL_ECHO_SPREAD &h0005
#define AL_FLANGER_WAVEFORM &h0001
#define AL_FLANGER_PHASE &h0002
#define AL_FLANGER_RATE &h0003
#define AL_FLANGER_DEPTH &h0004
#define AL_FLANGER_FEEDBACK &h0005
#define AL_FLANGER_DELAY &h0006
#define AL_FREQUENCY_SHIFTER_FREQUENCY &h0001
#define AL_FREQUENCY_SHIFTER_LEFT_DIRECTION &h0002
#define AL_FREQUENCY_SHIFTER_RIGHT_DIRECTION &h0003
#define AL_VOCAL_MORPHER_PHONEMEA &h0001
#define AL_VOCAL_MORPHER_PHONEMEA_COARSE_TUNING &h0002
#define AL_VOCAL_MORPHER_PHONEMEB &h0003
#define AL_VOCAL_MORPHER_PHONEMEB_COARSE_TUNING &h0004
#define AL_VOCAL_MORPHER_WAVEFORM &h0005
#define AL_VOCAL_MORPHER_RATE &h0006
#define AL_PITCH_SHIFTER_COARSE_TUNE &h0001
#define AL_PITCH_SHIFTER_FINE_TUNE &h0002
#define AL_RING_MODULATOR_FREQUENCY &h0001
#define AL_RING_MODULATOR_HIGHPASS_CUTOFF &h0002
#define AL_RING_MODULATOR_WAVEFORM &h0003
#define AL_AUTOWAH_ATTACK_TIME &h0001
#define AL_AUTOWAH_RELEASE_TIME &h0002
#define AL_AUTOWAH_RESONANCE &h0003
#define AL_AUTOWAH_PEAK_GAIN &h0004
#define AL_COMPRESSOR_ONOFF &h0001
#define AL_EQUALIZER_LOW_GAIN &h0001
#define AL_EQUALIZER_LOW_CUTOFF &h0002
#define AL_EQUALIZER_MID1_GAIN &h0003
#define AL_EQUALIZER_MID1_CENTER &h0004
#define AL_EQUALIZER_MID1_WIDTH &h0005
#define AL_EQUALIZER_MID2_GAIN &h0006
#define AL_EQUALIZER_MID2_CENTER &h0007
#define AL_EQUALIZER_MID2_WIDTH &h0008
#define AL_EQUALIZER_HIGH_GAIN &h0009
#define AL_EQUALIZER_HIGH_CUTOFF &h000A
#define AL_EFFECT_FIRST_PARAMETER &h0000
#define AL_EFFECT_LAST_PARAMETER &h8000
#define AL_EFFECT_TYPE &h8001
#define AL_EFFECT_NULL &h0000
#define AL_EFFECT_REVERB &h0001
#define AL_EFFECT_CHORUS &h0002
#define AL_EFFECT_DISTORTION &h0003
#define AL_EFFECT_ECHO &h0004
#define AL_EFFECT_FLANGER &h0005
#define AL_EFFECT_FREQUENCY_SHIFTER &h0006
#define AL_EFFECT_VOCAL_MORPHER &h0007
#define AL_EFFECT_PITCH_SHIFTER &h0008
#define AL_EFFECT_RING_MODULATOR &h0009
#define AL_EFFECT_AUTOWAH &h000A
#define AL_EFFECT_COMPRESSOR &h000B
#define AL_EFFECT_EQUALIZER &h000C
#define AL_EFFECTSLOT_EFFECT &h0001
#define AL_EFFECTSLOT_GAIN &h0002
#define AL_EFFECTSLOT_AUXILIARY_SEND_AUTO &h0003
#define AL_EFFECTSLOT_NULL &h0000
#define AL_LOWPASS_GAIN &h0001
#define AL_LOWPASS_GAINHF &h0002
#define AL_HIGHPASS_GAIN &h0001
#define AL_HIGHPASS_GAINLF &h0002
#define AL_BANDPASS_GAIN &h0001
#define AL_BANDPASS_GAINLF &h0002
#define AL_BANDPASS_GAINHF &h0003
#define AL_FILTER_FIRST_PARAMETER &h0000
#define AL_FILTER_LAST_PARAMETER &h8000
#define AL_FILTER_TYPE &h8001
#define AL_FILTER_NULL &h0000
#define AL_FILTER_LOWPASS &h0001
#define AL_FILTER_HIGHPASS &h0002
#define AL_FILTER_BANDPASS &h0003
#define LOWPASS_MIN_GAIN 0.0f
#define LOWPASS_MAX_GAIN 1.0f
#define LOWPASS_DEFAULT_GAIN 1.0f
#define LOWPASS_MIN_GAINHF 0.0f
#define LOWPASS_MAX_GAINHF 1.0f
#define LOWPASS_DEFAULT_GAINHF 1.0f
#define HIGHPASS_MIN_GAIN 0.0f
#define HIGHPASS_MAX_GAIN 1.0f
#define HIGHPASS_DEFAULT_GAIN 1.0f
#define HIGHPASS_MIN_GAINLF 0.0f
#define HIGHPASS_MAX_GAINLF 1.0f
#define HIGHPASS_DEFAULT_GAINLF 1.0f
#define BANDPASS_MIN_GAIN 0.0f
#define BANDPASS_MAX_GAIN 1.0f
#define BANDPASS_DEFAULT_GAIN 1.0f
#define BANDPASS_MIN_GAINHF 0.0f
#define BANDPASS_MAX_GAINHF 1.0f
#define BANDPASS_DEFAULT_GAINHF 1.0f
#define BANDPASS_MIN_GAINLF 0.0f
#define BANDPASS_MAX_GAINLF 1.0f
#define BANDPASS_DEFAULT_GAINLF 1.0f

/' **TODO**
/'*
 * Effect object functions.
 '/

/' Create Effect objects. '/
typedef void (__cdecl *LPALGENEFFECTS)( ALsizei n, ALuint* effects );

/' Delete Effect objects. '/
typedef void (__cdecl *LPALDELETEEFFECTS)( ALsizei n, ALuint* effects );

/' Verify a handle is a valid Effect. '/ 
typedef ALboolean (__cdecl *LPALISEFFECT)( ALuint eid );

/' Set an integer parameter for an Effect object. '/
typedef void (__cdecl *LPALEFFECTI)( ALuint eid, ALenum param, ALint value); 
typedef void (__cdecl *LPALEFFECTIV)( ALuint eid, ALenum param, ALint* values ); 

/' Set a floating point parameter for an Effect object. '/
typedef void (__cdecl *LPALEFFECTF)( ALuint eid, ALenum param, ALfloat value); 
typedef void (__cdecl *LPALEFFECTFV)( ALuint eid, ALenum param, ALfloat* values ); 

/' Get an integer parameter for an Effect object. '/
typedef void (__cdecl *LPALGETEFFECTI)( ALuint eid, ALenum pname, ALint* value );
typedef void (__cdecl *LPALGETEFFECTIV)( ALuint eid, ALenum pname, ALint* values );

/' Get a floating point parameter for an Effect object. '/
typedef void (__cdecl *LPALGETEFFECTF)( ALuint eid, ALenum pname, ALfloat* value );
typedef void (__cdecl *LPALGETEFFECTFV)( ALuint eid, ALenum pname, ALfloat* values );


/'*
 * Filter object functions
 '/

/' Create Filter objects. '/
typedef void (__cdecl *LPALGENFILTERS)( ALsizei n, ALuint* filters ); 

/' Delete Filter objects. '/
typedef void (__cdecl *LPALDELETEFILTERS)( ALsizei n, ALuint* filters );

/' Verify a handle is a valid Filter. '/ 
typedef ALboolean (__cdecl *LPALISFILTER)( ALuint fid );

/' Set an integer parameter for a Filter object. '/
typedef void (__cdecl *LPALFILTERI)( ALuint fid, ALenum param, ALint value ); 
typedef void (__cdecl *LPALFILTERIV)( ALuint fid, ALenum param, ALint* values ); 

/' Set a floating point parameter for an Filter object. '/
typedef void (__cdecl *LPALFILTERF)( ALuint fid, ALenum param, ALfloat value); 
typedef void (__cdecl *LPALFILTERFV)( ALuint fid, ALenum param, ALfloat* values ); 

/' Get an integer parameter for a Filter object. '/
typedef void (__cdecl *LPALGETFILTERI)( ALuint fid, ALenum pname, ALint* value );
typedef void (__cdecl *LPALGETFILTERIV)( ALuint fid, ALenum pname, ALint* values );

/' Get a floating point parameter for a Filter object. '/
typedef void (__cdecl *LPALGETFILTERF)( ALuint fid, ALenum pname, ALfloat* value );
typedef void (__cdecl *LPALGETFILTERFV)( ALuint fid, ALenum pname, ALfloat* values );


/'*
 * Auxiliary Slot object functions
 '/

/' Create Auxiliary Slot objects. '/
typedef void (__cdecl *LPALGENAUXILIARYEFFECTSLOTS)( ALsizei n, ALuint* slots ); 

/' Delete Auxiliary Slot objects. '/
typedef void (__cdecl *LPALDELETEAUXILIARYEFFECTSLOTS)( ALsizei n, ALuint* slots );

/' Verify a handle is a valid Auxiliary Slot. '/ 
typedef ALboolean (__cdecl *LPALISAUXILIARYEFFECTSLOT)( ALuint slot ); 

/' Set an integer parameter for a Auxiliary Slot object. '/
typedef void (__cdecl *LPALAUXILIARYEFFECTSLOTI)( ALuint asid, ALenum param, ALint value ); 
typedef void (__cdecl *LPALAUXILIARYEFFECTSLOTIV)( ALuint asid, ALenum param, ALint* values ); 

/' Set a floating point parameter for an Auxiliary Slot object. '/
typedef void (__cdecl *LPALAUXILIARYEFFECTSLOTF)( ALuint asid, ALenum param, ALfloat value ); 
typedef void (__cdecl *LPALAUXILIARYEFFECTSLOTFV)( ALuint asid, ALenum param, ALfloat* values ); 

/' Get an integer parameter for a Auxiliary Slot object. '/
typedef void (__cdecl *LPALGETAUXILIARYEFFECTSLOTI)( ALuint asid, ALenum pname, ALint* value );
typedef void (__cdecl *LPALGETAUXILIARYEFFECTSLOTIV)( ALuint asid, ALenum pname, ALint* values );

/' Get a floating point parameter for a Auxiliary Slot object. '/
typedef void (__cdecl *LPALGETAUXILIARYEFFECTSLOTF)( ALuint asid, ALenum pname, ALfloat* value );
typedef void (__cdecl *LPALGETAUXILIARYEFFECTSLOTFV)( ALuint asid, ALenum pname, ALfloat* values );
'/

#define AL_REVERB_MIN_DENSITY 0.0f
#define AL_REVERB_MAX_DENSITY 1.0f
#define AL_REVERB_DEFAULT_DENSITY 1.0f
#define AL_REVERB_MIN_DIFFUSION 0.0f
#define AL_REVERB_MAX_DIFFUSION 1.0f
#define AL_REVERB_DEFAULT_DIFFUSION 1.0f
#define AL_REVERB_MIN_GAIN 0.0f
#define AL_REVERB_MAX_GAIN 1.0f
#define AL_REVERB_DEFAULT_GAIN 0.32f
#define AL_REVERB_MIN_GAINHF 0.0f
#define AL_REVERB_MAX_GAINHF 1.0f
#define AL_REVERB_DEFAULT_GAINHF 0.89f
#define AL_REVERB_MIN_DECAY_TIME 0.1f
#define AL_REVERB_MAX_DECAY_TIME 20.0f
#define AL_REVERB_DEFAULT_DECAY_TIME 1.49f
#define AL_REVERB_MIN_DECAY_HFRATIO 0.1f
#define AL_REVERB_MAX_DECAY_HFRATIO 2.0f
#define AL_REVERB_DEFAULT_DECAY_HFRATIO 0.83f
#define AL_REVERB_MIN_REFLECTIONS_GAIN 0.0f
#define AL_REVERB_MAX_REFLECTIONS_GAIN 3.16f
#define AL_REVERB_DEFAULT_REFLECTIONS_GAIN 0.05f
#define AL_REVERB_MIN_REFLECTIONS_DELAY 0.0f
#define AL_REVERB_MAX_REFLECTIONS_DELAY 0.3f
#define AL_REVERB_DEFAULT_REFLECTIONS_DELAY 0.007f
#define AL_REVERB_MIN_LATE_REVERB_GAIN 0.0f
#define AL_REVERB_MAX_LATE_REVERB_GAIN 10.0f
#define AL_REVERB_DEFAULT_LATE_REVERB_GAIN 1.26f
#define AL_REVERB_MIN_LATE_REVERB_DELAY 0.0f
#define AL_REVERB_MAX_LATE_REVERB_DELAY 0.1f
#define AL_REVERB_DEFAULT_LATE_REVERB_DELAY 0.011f
#define AL_REVERB_MIN_AIR_ABSORPTION_GAINHF 0.892f
#define AL_REVERB_MAX_AIR_ABSORPTION_GAINHF 1.0f
#define AL_REVERB_DEFAULT_AIR_ABSORPTION_GAINHF 0.994f
#define AL_REVERB_MIN_ROOM_ROLLOFF_FACTOR 0.0f
#define AL_REVERB_MAX_ROOM_ROLLOFF_FACTOR 10.0f
#define AL_REVERB_DEFAULT_ROOM_ROLLOFF_FACTOR 0.0f
#define AL_CHORUS_MIN_WAVEFORM 0
#define AL_CHORUS_MAX_WAVEFORM 1
#define AL_CHORUS_DEFAULT_WAVEFORM 1
#define AL_CHORUS_WAVEFORM_SINUSOID 0
#define AL_CHORUS_WAVEFORM_TRIANGLE 1
#define AL_CHORUS_MIN_PHASE (-180)
#define AL_CHORUS_MAX_PHASE 180
#define AL_CHORUS_DEFAULT_PHASE 90
#define AL_CHORUS_MIN_RATE 0.0f
#define AL_CHORUS_MAX_RATE 10.0f
#define AL_CHORUS_DEFAULT_RATE 1.1f
#define AL_CHORUS_MIN_DEPTH 0.0f
#define AL_CHORUS_MAX_DEPTH 1.0f
#define AL_CHORUS_DEFAULT_DEPTH 0.1f
#define AL_CHORUS_MIN_FEEDBACK (-1.0f)
#define AL_CHORUS_MAX_FEEDBACK 1.0f
#define AL_CHORUS_DEFAULT_FEEDBACK 0.25f
#define AL_CHORUS_MIN_DELAY 0.0f
#define AL_CHORUS_MAX_DELAY 0.016f
#define AL_CHORUS_DEFAULT_DELAY 0.016f
#define AL_DISTORTION_MIN_EDGE 0.0f
#define AL_DISTORTION_MAX_EDGE 1.0f
#define AL_DISTORTION_DEFAULT_EDGE 0.2f
#define AL_DISTORTION_MIN_GAIN 0.01f
#define AL_DISTORTION_MAX_GAIN 1.0f
#define AL_DISTORTION_DEFAULT_GAIN 0.05f
#define AL_DISTORTION_MIN_LOWPASS_CUTOFF 80.0f
#define AL_DISTORTION_MAX_LOWPASS_CUTOFF 24000.0f
#define AL_DISTORTION_DEFAULT_LOWPASS_CUTOFF 8000.0f
#define AL_DISTORTION_MIN_EQCENTER 80.0f
#define AL_DISTORTION_MAX_EQCENTER 24000.0f
#define AL_DISTORTION_DEFAULT_EQCENTER 3600.0f
#define AL_DISTORTION_MIN_EQBANDWIDTH 80.0f
#define AL_DISTORTION_MAX_EQBANDWIDTH 24000.0f
#define AL_DISTORTION_DEFAULT_EQBANDWIDTH 3600.0f
#define AL_ECHO_MIN_DELAY 0.0f
#define AL_ECHO_MAX_DELAY 0.207f
#define AL_ECHO_DEFAULT_DELAY 0.1f
#define AL_ECHO_MIN_LRDELAY 0.0f
#define AL_ECHO_MAX_LRDELAY 0.404f
#define AL_ECHO_DEFAULT_LRDELAY 0.1f
#define AL_ECHO_MIN_DAMPING 0.0f
#define AL_ECHO_MAX_DAMPING 0.99f
#define AL_ECHO_DEFAULT_DAMPING 0.5f
#define AL_ECHO_MIN_FEEDBACK 0.0f
#define AL_ECHO_MAX_FEEDBACK 1.0f
#define AL_ECHO_DEFAULT_FEEDBACK 0.5f
#define AL_ECHO_MIN_SPREAD (-1.0f)
#define AL_ECHO_MAX_SPREAD 1.0f
#define AL_ECHO_DEFAULT_SPREAD (-1.0f)
#define AL_FLANGER_MIN_WAVEFORM 0
#define AL_FLANGER_MAX_WAVEFORM 1
#define AL_FLANGER_DEFAULT_WAVEFORM 1
#define AL_FLANGER_WAVEFORM_SINUSOID 0
#define AL_FLANGER_WAVEFORM_TRIANGLE 1
#define AL_FLANGER_MIN_PHASE (-180)
#define AL_FLANGER_MAX_PHASE 180
#define AL_FLANGER_DEFAULT_PHASE 0
#define AL_FLANGER_MIN_RATE 0.0f
#define AL_FLANGER_MAX_RATE 10.0f
#define AL_FLANGER_DEFAULT_RATE 0.27f
#define AL_FLANGER_MIN_DEPTH 0.0f
#define AL_FLANGER_MAX_DEPTH 1.0f
#define AL_FLANGER_DEFAULT_DEPTH 1.0f
#define AL_FLANGER_MIN_FEEDBACK (-1.0f)
#define AL_FLANGER_MAX_FEEDBACK 1.0f
#define AL_FLANGER_DEFAULT_FEEDBACK (-0.5f)
#define AL_FLANGER_MIN_DELAY 0.0f
#define AL_FLANGER_MAX_DELAY 0.004f
#define AL_FLANGER_DEFAULT_DELAY 0.002f
#define AL_FREQUENCY_SHIFTER_MIN_FREQUENCY 0.0f
#define AL_FREQUENCY_SHIFTER_MAX_FREQUENCY 24000.0f
#define AL_FREQUENCY_SHIFTER_DEFAULT_FREQUENCY 0.0f
#define AL_FREQUENCY_SHIFTER_MIN_LEFT_DIRECTION 0
#define AL_FREQUENCY_SHIFTER_MAX_LEFT_DIRECTION 2
#define AL_FREQUENCY_SHIFTER_DEFAULT_LEFT_DIRECTION 0
#define AL_FREQUENCY_SHIFTER_MIN_RIGHT_DIRECTION 0
#define AL_FREQUENCY_SHIFTER_MAX_RIGHT_DIRECTION 2
#define AL_FREQUENCY_SHIFTER_DEFAULT_RIGHT_DIRECTION 0
#define AL_FREQUENCY_SHIFTER_DIRECTION_DOWN 0
#define AL_FREQUENCY_SHIFTER_DIRECTION_UP 1
#define AL_FREQUENCY_SHIFTER_DIRECTION_OFF 2
#define AL_VOCAL_MORPHER_MIN_PHONEMEA 0
#define AL_VOCAL_MORPHER_MAX_PHONEMEA 29
#define AL_VOCAL_MORPHER_DEFAULT_PHONEMEA 0
#define AL_VOCAL_MORPHER_MIN_PHONEMEA_COARSE_TUNING (-24)
#define AL_VOCAL_MORPHER_MAX_PHONEMEA_COARSE_TUNING 24
#define AL_VOCAL_MORPHER_DEFAULT_PHONEMEA_COARSE_TUNING 0
#define AL_VOCAL_MORPHER_MIN_PHONEMEB 0
#define AL_VOCAL_MORPHER_MAX_PHONEMEB 29
#define AL_VOCAL_MORPHER_DEFAULT_PHONEMEB 10
#define AL_VOCAL_MORPHER_PHONEME_A 0
#define AL_VOCAL_MORPHER_PHONEME_E 1
#define AL_VOCAL_MORPHER_PHONEME_I 2
#define AL_VOCAL_MORPHER_PHONEME_O 3
#define AL_VOCAL_MORPHER_PHONEME_U 4
#define AL_VOCAL_MORPHER_PHONEME_AA 5
#define AL_VOCAL_MORPHER_PHONEME_AE 6
#define AL_VOCAL_MORPHER_PHONEME_AH 7
#define AL_VOCAL_MORPHER_PHONEME_AO 8
#define AL_VOCAL_MORPHER_PHONEME_EH 9
#define AL_VOCAL_MORPHER_PHONEME_ER 10
#define AL_VOCAL_MORPHER_PHONEME_IH 11
#define AL_VOCAL_MORPHER_PHONEME_IY 12
#define AL_VOCAL_MORPHER_PHONEME_UH 13
#define AL_VOCAL_MORPHER_PHONEME_UW 14
#define AL_VOCAL_MORPHER_PHONEME_B 15
#define AL_VOCAL_MORPHER_PHONEME_D 16
#define AL_VOCAL_MORPHER_PHONEME_F 17
#define AL_VOCAL_MORPHER_PHONEME_G 18
#define AL_VOCAL_MORPHER_PHONEME_J 19
#define AL_VOCAL_MORPHER_PHONEME_K 20
#define AL_VOCAL_MORPHER_PHONEME_L 21
#define AL_VOCAL_MORPHER_PHONEME_M 22
#define AL_VOCAL_MORPHER_PHONEME_N 23
#define AL_VOCAL_MORPHER_PHONEME_P 24
#define AL_VOCAL_MORPHER_PHONEME_R 25
#define AL_VOCAL_MORPHER_PHONEME_S 26
#define AL_VOCAL_MORPHER_PHONEME_T 27
#define AL_VOCAL_MORPHER_PHONEME_V 28
#define AL_VOCAL_MORPHER_PHONEME_Z 29
#define AL_VOCAL_MORPHER_MIN_PHONEMEB_COARSE_TUNING (-24)
#define AL_VOCAL_MORPHER_MAX_PHONEMEB_COARSE_TUNING 24
#define AL_VOCAL_MORPHER_DEFAULT_PHONEMEB_COARSE_TUNING 0
#define AL_VOCAL_MORPHER_MIN_WAVEFORM 0
#define AL_VOCAL_MORPHER_MAX_WAVEFORM 2
#define AL_VOCAL_MORPHER_DEFAULT_WAVEFORM 0
#define AL_VOCAL_MORPHER_WAVEFORM_SINUSOID 0
#define AL_VOCAL_MORPHER_WAVEFORM_TRIANGLE 1
#define AL_VOCAL_MORPHER_WAVEFORM_SAWTOOTH 2
#define AL_VOCAL_MORPHER_MIN_RATE 0.0f
#define AL_VOCAL_MORPHER_MAX_RATE 10.0f
#define AL_VOCAL_MORPHER_DEFAULT_RATE 1.41f
#define AL_PITCH_SHIFTER_MIN_COARSE_TUNE (-12)
#define AL_PITCH_SHIFTER_MAX_COARSE_TUNE 12
#define AL_PITCH_SHIFTER_DEFAULT_COARSE_TUNE 12
#define AL_PITCH_SHIFTER_MIN_FINE_TUNE (-50)
#define AL_PITCH_SHIFTER_MAX_FINE_TUNE 50
#define AL_PITCH_SHIFTER_DEFAULT_FINE_TUNE 0
#define AL_RING_MODULATOR_MIN_FREQUENCY 0.0f
#define AL_RING_MODULATOR_MAX_FREQUENCY 8000.0f
#define AL_RING_MODULATOR_DEFAULT_FREQUENCY 440.0f
#define AL_RING_MODULATOR_MIN_HIGHPASS_CUTOFF 0.0f
#define AL_RING_MODULATOR_MAX_HIGHPASS_CUTOFF 24000.0f
#define AL_RING_MODULATOR_DEFAULT_HIGHPASS_CUTOFF 800.0f
#define AL_RING_MODULATOR_MIN_WAVEFORM 0
#define AL_RING_MODULATOR_MAX_WAVEFORM 2
#define AL_RING_MODULATOR_DEFAULT_WAVEFORM 0
#define AL_RING_MODULATOR_SINUSOID 0
#define AL_RING_MODULATOR_SAWTOOTH 1
#define AL_RING_MODULATOR_SQUARE 2
#define AL_AUTOWAH_MIN_ATTACK_TIME 0.0001f
#define AL_AUTOWAH_MAX_ATTACK_TIME 1.0f
#define AL_AUTOWAH_DEFAULT_ATTACK_TIME 0.06f
#define AL_AUTOWAH_MIN_RELEASE_TIME 0.0001f
#define AL_AUTOWAH_MAX_RELEASE_TIME 1.0f
#define AL_AUTOWAH_DEFAULT_RELEASE_TIME 0.06f
#define AL_AUTOWAH_MIN_RESONANCE 2.0f
#define AL_AUTOWAH_MAX_RESONANCE 1000.0f
#define AL_AUTOWAH_DEFAULT_RESONANCE 1000.0f
#define AL_AUTOWAH_MIN_PEAK_GAIN 0.00003f
#define AL_AUTOWAH_MAX_PEAK_GAIN 31621.0f
#define AL_AUTOWAH_DEFAULT_PEAK_GAIN 11.22f
#define AL_COMPRESSOR_MIN_ONOFF 0
#define AL_COMPRESSOR_MAX_ONOFF 1
#define AL_COMPRESSOR_DEFAULT_ONOFF 1
#define AL_EQUALIZER_MIN_LOW_GAIN 0.126f
#define AL_EQUALIZER_MAX_LOW_GAIN 7.943f
#define AL_EQUALIZER_DEFAULT_LOW_GAIN 1.0f
#define AL_EQUALIZER_MIN_LOW_CUTOFF 50.0f
#define AL_EQUALIZER_MAX_LOW_CUTOFF 800.0f
#define AL_EQUALIZER_DEFAULT_LOW_CUTOFF 200.0f
#define AL_EQUALIZER_MIN_MID1_GAIN 0.126f
#define AL_EQUALIZER_MAX_MID1_GAIN 7.943f
#define AL_EQUALIZER_DEFAULT_MID1_GAIN 1.0f
#define AL_EQUALIZER_MIN_MID1_CENTER 200.0f
#define AL_EQUALIZER_MAX_MID1_CENTER 3000.0f
#define AL_EQUALIZER_DEFAULT_MID1_CENTER 500.0f
#define AL_EQUALIZER_MIN_MID1_WIDTH 0.01f
#define AL_EQUALIZER_MAX_MID1_WIDTH 1.0f
#define AL_EQUALIZER_DEFAULT_MID1_WIDTH 1.0f
#define AL_EQUALIZER_MIN_MID2_GAIN 0.126f
#define AL_EQUALIZER_MAX_MID2_GAIN 7.943f
#define AL_EQUALIZER_DEFAULT_MID2_GAIN 1.0f
#define AL_EQUALIZER_MIN_MID2_CENTER 1000.0f
#define AL_EQUALIZER_MAX_MID2_CENTER 8000.0f
#define AL_EQUALIZER_DEFAULT_MID2_CENTER 3000.0f
#define AL_EQUALIZER_MIN_MID2_WIDTH 0.01f
#define AL_EQUALIZER_MAX_MID2_WIDTH 1.0f
#define AL_EQUALIZER_DEFAULT_MID2_WIDTH 1.0f
#define AL_EQUALIZER_MIN_HIGH_GAIN 0.126f
#define AL_EQUALIZER_MAX_HIGH_GAIN 7.943f
#define AL_EQUALIZER_DEFAULT_HIGH_GAIN 1.0f
#define AL_EQUALIZER_MIN_HIGH_CUTOFF 4000.0f
#define AL_EQUALIZER_MAX_HIGH_CUTOFF 16000.0f
#define AL_EQUALIZER_DEFAULT_HIGH_CUTOFF 6000.0f
#define AL_MIN_AIR_ABSORPTION_FACTOR 0.0f
#define AL_MAX_AIR_ABSORPTION_FACTOR 10.0f
#define AL_DEFAULT_AIR_ABSORPTION_FACTOR 0.0f
#define AL_MIN_ROOM_ROLLOFF_FACTOR 0.0f
#define AL_MAX_ROOM_ROLLOFF_FACTOR 10.0f
#define AL_DEFAULT_ROOM_ROLLOFF_FACTOR 0.0f
#define AL_MIN_CONE_OUTER_GAINHF 0.0f
#define AL_MAX_CONE_OUTER_GAINHF 1.0f
#define AL_DEFAULT_CONE_OUTER_GAINHF 1.0f
#define AL_DEFAULT_METERS_PER_UNIT 1.0f

#endif
