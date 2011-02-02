	.file "transport.pp"
# Begin asmlist al_begin

.section .text
.globl	DEBUGSTART_TRANSPORT
	.type	DEBUGSTART_TRANSPORT,@object
DEBUGSTART_TRANSPORT:
	.stabs "transport.pp",100,0,0,.Lf1
.Lf1:
# End asmlist al_begin
# Begin asmlist al_stabs

.section .data
.globl	DEBUGINFO_TRANSPORT
	.type	DEBUGINFO_TRANSPORT,@object
DEBUGINFO_TRANSPORT:
# Defs - Begin unit SYSTEM has index 1
	.stabs "QWORD:t3=-32;",128,0,0,0
# Defs - End unit SYSTEM has index 1
# Defs - Begin unit OBJPAS has index 4
# Defs - End unit OBJPAS has index 4
# Defs - Begin unit UNIXTYPE has index 28
# Defs - End unit UNIXTYPE has index 28
# Defs - Begin unit KERNELDEFS has index 199
# Defs - End unit KERNELDEFS has index 199
# Defs - Begin unit KERNELIOCTL has index 200
# Defs - End unit KERNELIOCTL has index 200
# Defs - Begin unit LIBC has index 198
# Defs - End unit LIBC has index 198
# Defs - Begin unit JACKTYPES has index 192
# Defs - End unit JACKTYPES has index 192
# Defs - Begin unit TRANSPORT has index 196
	.stabs "void:t12=12",128,0,0,0
	.stabs "POINTER:t13=*12",128,0,0,0
	.stabs "PJACK_CLIENT_T:t1=*13",128,0,0,0
	.stabs "JACK_TRANSPORT_STATE_T:Tt2=eJACKTRANSPORTSTOPPED:0,JACKTRANSPORTROLLING:1,JACKTRANSPORTLOOPING:2,JACKTRANSPORTSTARTING:3,;",128,0,0,0
	.stabs "JACK_POSITION_BITS_T:Tt4=eJACKPOSITIONBBT:16,JACKPOSITIONTIMECODE:32,JACKBBTFRAMEOFFSET:64,JACKAUDIOVIDEORATIO:128,JACKVIDEOFRAMEOFFSET:256,;",128,0,0,0
	.stabs "LONGWORD:t14=r14;0;-1;",128,0,0,0
	.stabs "LONGINT:t15=r15;-2147483648;2147483647;",128,0,0,0
	.stabs "DOUBLE:t16=r15;8;0;",128,0,0,0
	.stabs "SHORTINT:t17=r17;-128;127;",128,0,0,0
	.stabs ":t18=ar17;0;6;15",128,0,0,0
	.stabs "JACK_POSITION_T:Tt5=s148UNIQUE_1:3,0,64;USECS:3,64,64;FRAME_RATE:14,128,32;FRAME:14,160,32;VALID:4,192,32;BAR:15,224,32;BEAT:15,256,32;TICK:15,288,32;BAR_START_TICK:16,320,64;BEATS_PER_BAR:16,384,64;BEAT_TYPE:16,448,64;TICKS_PER_BEAT:16,512,64;BEATS_PER_MINUTE:16,576,64;FRAME_TIME:16,640,64;NEXT_TIME:16,704,64;BBT_OFFSET:14,768,32;AUDIO_FRAMES_PER_VIDEO_FRAME:16,800,64;VIDEO_OFFSET:14,864,32;PADDING:18,896,224;UNIQUE_2:3,1120,64;;",128,0,0,0
	.stabs "PJACK_POSITION_T:t6=*5",128,0,0,0
	.stabs "JACKSYNCCALLBACK:t7=*f15",128,0,0,0
	.stabs "JACKTIMEBASECALLBACK:t8=*f12",128,0,0,0
	.stabs "JACK_TRANSPORT_BITS_T:Tt9=eJACKTRANSPORTSTATE:1,JACKTRANSPORTPOSITION:2,JACKTRANSPORTLOOP:4,JACKTRANSPORTSMPTE:8,JACKTRANSPORTBBT:16,;",128,0,0,0
	.stabs "JACK_TRANSPORT_INFO_T:Tt10=s96FRAME_RATE:14,0,32;USECS:3,32,64;VALID:9,96,32;TRANSPORT_STATE:2,128,32;FRAME:14,160,32;LOOP_START:14,192,32;LOOP_END:14,224,32;SMPTE_OFFSET:15,256,32;SMPTE_FRAME_RATE:16,288,64;BAR:15,352,32;BEAT:15,384,32;TICK:15,416,32;BAR_START_TICK:16,448,64;BEATS_PER_BAR:16,512,64;BEAT_TYPE:16,576,64;TICKS_PER_BEAT:16,640,64;BEATS_PER_MINUTE:16,704,64;;",128,0,0,0
	.stabs "PJACK_TRANSPORT_INFO_T:t11=*10",128,0,0,0
# Defs - End unit TRANSPORT has index 196
# Defs - Begin Staticsymtable
# Defs - End Staticsymtable
# Syms - Begin unit TRANSPORT has index 196
	.stabs "EXTERNAL_LIBRARY:c=s'jack';",36,0,15,0
	.stabs "PJACK_CLIENT_T:t1",128,0,18,0
	.stabs "JACK_TRANSPORT_STATE_T:Tt2",128,0,59,0
	.stabs "JACK_UNIQUE_T:t3",128,0,63,0
	.stabs "JACK_POSITION_BITS_T:Tt4",128,0,74,0
	.stabs "JACK_POSITION_MASK:c=i496;",36,0,80,0
	.stabs "JACK_POSITION_T:Tt5",128,0,138,0
	.stabs "PJACK_POSITION_T:t6",128,0,161,0
	.stabs "JACKSYNCCALLBACK:t7",128,0,204,0
	.stabs "JACKTIMEBASECALLBACK:t8",128,0,278,0
	.stabs "JACK_TRANSPORT_BITS_T:Tt9",128,0,411,0
	.stabs "JACK_TRANSPORT_INFO_T:Tt10",128,0,427,0
	.stabs "PJACK_TRANSPORT_INFO_T:t11",128,0,447,0
# Syms - End unit TRANSPORT has index 196
# Syms - Begin Staticsymtable
# Syms - End Staticsymtable
# End asmlist al_stabs
# Begin asmlist al_procedures
# End asmlist al_procedures
# Begin asmlist al_globals

.section .data
	.balign 4
.globl	THREADVARLIST_TRANSPORT
	.type	THREADVARLIST_TRANSPORT,@object
THREADVARLIST_TRANSPORT:
	.long	0
# [transport.pp]
# [481] 
.Le0:
	.size	THREADVARLIST_TRANSPORT, .Le0 - THREADVARLIST_TRANSPORT
# End asmlist al_globals
# Begin asmlist al_const
# End asmlist al_const
# Begin asmlist al_typedconsts
# End asmlist al_typedconsts
# Begin asmlist al_rotypedconsts
# End asmlist al_rotypedconsts
# Begin asmlist al_threadvars
# End asmlist al_threadvars
# Begin asmlist al_imports
# End asmlist al_imports
# Begin asmlist al_exports
# End asmlist al_exports
# Begin asmlist al_resources
# End asmlist al_resources
# Begin asmlist al_rtti

.section .data
	.balign 4
.globl	INIT_TRANSPORT_PJACK_CLIENT_T
	.type	INIT_TRANSPORT_PJACK_CLIENT_T,@object
INIT_TRANSPORT_PJACK_CLIENT_T:
	.byte	0
	.ascii	"\016Pjack_client_t"
.Le1:
	.size	INIT_TRANSPORT_PJACK_CLIENT_T, .Le1 - INIT_TRANSPORT_PJACK_CLIENT_T

.section .data
	.balign 4
.globl	RTTI_TRANSPORT_PJACK_CLIENT_T
	.type	RTTI_TRANSPORT_PJACK_CLIENT_T,@object
RTTI_TRANSPORT_PJACK_CLIENT_T:
	.byte	0
	.ascii	"\016Pjack_client_t"
.Le2:
	.size	RTTI_TRANSPORT_PJACK_CLIENT_T, .Le2 - RTTI_TRANSPORT_PJACK_CLIENT_T

.section .data
	.balign 4
.globl	INIT_TRANSPORT_JACK_TRANSPORT_STATE_T
	.type	INIT_TRANSPORT_JACK_TRANSPORT_STATE_T,@object
INIT_TRANSPORT_JACK_TRANSPORT_STATE_T:
	.byte	3
	.ascii	"\026jack_transport_state_t"
	.byte	5
	.long	0,3,0
	.byte	20
	.ascii	"JackTransportStopped"
	.byte	20
	.ascii	"JackTransportRolling"
	.byte	20
	.ascii	"JackTransportLooping"
	.byte	21
	.ascii	"JackTransportStarting"
	.byte	0
.Le3:
	.size	INIT_TRANSPORT_JACK_TRANSPORT_STATE_T, .Le3 - INIT_TRANSPORT_JACK_TRANSPORT_STATE_T

.section .data
	.balign 4
.globl	RTTI_TRANSPORT_JACK_TRANSPORT_STATE_T
	.type	RTTI_TRANSPORT_JACK_TRANSPORT_STATE_T,@object
RTTI_TRANSPORT_JACK_TRANSPORT_STATE_T:
	.byte	3
	.ascii	"\026jack_transport_state_t"
	.byte	5
	.long	0,3,0
	.byte	20
	.ascii	"JackTransportStopped"
	.byte	20
	.ascii	"JackTransportRolling"
	.byte	20
	.ascii	"JackTransportLooping"
	.byte	21
	.ascii	"JackTransportStarting"
	.byte	0
.Le4:
	.size	RTTI_TRANSPORT_JACK_TRANSPORT_STATE_T, .Le4 - RTTI_TRANSPORT_JACK_TRANSPORT_STATE_T

.section .data
	.balign 4
.globl	INIT_TRANSPORT_JACK_POSITION_BITS_T
	.type	INIT_TRANSPORT_JACK_POSITION_BITS_T,@object
INIT_TRANSPORT_JACK_POSITION_BITS_T:
	.byte	3
	.ascii	"\024jack_position_bits_t"
	.byte	5
	.long	16,256,0
	.byte	15
	.ascii	"JackPositionBBT"
	.byte	20
	.ascii	"JackPositionTimecode"
	.byte	18
	.ascii	"JackBBTFrameOffset"
	.byte	19
	.ascii	"JackAudioVideoRatio"
	.byte	20
	.ascii	"JackVideoFrameOffset"
	.byte	0
.Le5:
	.size	INIT_TRANSPORT_JACK_POSITION_BITS_T, .Le5 - INIT_TRANSPORT_JACK_POSITION_BITS_T

.section .data
	.balign 4
.globl	RTTI_TRANSPORT_JACK_POSITION_BITS_T
	.type	RTTI_TRANSPORT_JACK_POSITION_BITS_T,@object
RTTI_TRANSPORT_JACK_POSITION_BITS_T:
	.byte	3
	.ascii	"\024jack_position_bits_t"
	.byte	5
	.long	16,256,0
	.byte	15
	.ascii	"JackPositionBBT"
	.byte	20
	.ascii	"JackPositionTimecode"
	.byte	18
	.ascii	"JackBBTFrameOffset"
	.byte	19
	.ascii	"JackAudioVideoRatio"
	.byte	20
	.ascii	"JackVideoFrameOffset"
	.byte	0
.Le6:
	.size	RTTI_TRANSPORT_JACK_POSITION_BITS_T, .Le6 - RTTI_TRANSPORT_JACK_POSITION_BITS_T

.section .data
	.balign 4
.globl	INIT_TRANSPORT_JACK_POSITION_T
	.type	INIT_TRANSPORT_JACK_POSITION_T,@object
INIT_TRANSPORT_JACK_POSITION_T:
	.byte	13
	.ascii	"\017jack_position_t"
	.long	148,0
.Le7:
	.size	INIT_TRANSPORT_JACK_POSITION_T, .Le7 - INIT_TRANSPORT_JACK_POSITION_T

.section .data
	.balign 4
.globl	RTTI_TRANSPORT_DEF5
	.type	RTTI_TRANSPORT_DEF5,@object
RTTI_TRANSPORT_DEF5:
	.byte	12
	.ascii	"\000"
	.long	4,7
	.long	RTTI_SYSTEM_LONGINT
	.long	3
.Le8:
	.size	RTTI_TRANSPORT_DEF5, .Le8 - RTTI_TRANSPORT_DEF5

.section .data
	.balign 4
.globl	RTTI_TRANSPORT_JACK_POSITION_T
	.type	RTTI_TRANSPORT_JACK_POSITION_T,@object
RTTI_TRANSPORT_JACK_POSITION_T:
	.byte	13
	.ascii	"\017jack_position_t"
	.long	148,20
	.long	RTTI_SYSTEM_QWORD
	.long	0
	.long	RTTI_SYSTEM_QWORD
	.long	8
	.long	RTTI_SYSTEM_LONGWORD
	.long	16
	.long	RTTI_SYSTEM_LONGWORD
	.long	20
	.long	RTTI_TRANSPORT_JACK_POSITION_BITS_T
	.long	24
	.long	RTTI_SYSTEM_LONGINT
	.long	28
	.long	RTTI_SYSTEM_LONGINT
	.long	32
	.long	RTTI_SYSTEM_LONGINT
	.long	36
	.long	RTTI_SYSTEM_DOUBLE
	.long	40
	.long	RTTI_SYSTEM_DOUBLE
	.long	48
	.long	RTTI_SYSTEM_DOUBLE
	.long	56
	.long	RTTI_SYSTEM_DOUBLE
	.long	64
	.long	RTTI_SYSTEM_DOUBLE
	.long	72
	.long	RTTI_SYSTEM_DOUBLE
	.long	80
	.long	RTTI_SYSTEM_DOUBLE
	.long	88
	.long	RTTI_SYSTEM_LONGWORD
	.long	96
	.long	RTTI_SYSTEM_DOUBLE
	.long	100
	.long	RTTI_SYSTEM_LONGWORD
	.long	108
	.long	RTTI_TRANSPORT_DEF5
	.long	112
	.long	RTTI_SYSTEM_QWORD
	.long	140
.Le9:
	.size	RTTI_TRANSPORT_JACK_POSITION_T, .Le9 - RTTI_TRANSPORT_JACK_POSITION_T

.section .data
	.balign 4
.globl	INIT_TRANSPORT_PJACK_POSITION_T
	.type	INIT_TRANSPORT_PJACK_POSITION_T,@object
INIT_TRANSPORT_PJACK_POSITION_T:
	.byte	0
	.ascii	"\020Pjack_position_t"
.Le10:
	.size	INIT_TRANSPORT_PJACK_POSITION_T, .Le10 - INIT_TRANSPORT_PJACK_POSITION_T

.section .data
	.balign 4
.globl	RTTI_TRANSPORT_PJACK_POSITION_T
	.type	RTTI_TRANSPORT_PJACK_POSITION_T,@object
RTTI_TRANSPORT_PJACK_POSITION_T:
	.byte	0
	.ascii	"\020Pjack_position_t"
.Le11:
	.size	RTTI_TRANSPORT_PJACK_POSITION_T, .Le11 - RTTI_TRANSPORT_PJACK_POSITION_T

.section .data
	.balign 4
.globl	INIT_TRANSPORT_JACKSYNCCALLBACK
	.type	INIT_TRANSPORT_JACKSYNCCALLBACK,@object
INIT_TRANSPORT_JACKSYNCCALLBACK:
	.byte	23
	.ascii	"\020JackSyncCallback"
.Le12:
	.size	INIT_TRANSPORT_JACKSYNCCALLBACK, .Le12 - INIT_TRANSPORT_JACKSYNCCALLBACK

.section .data
	.balign 4
.globl	RTTI_TRANSPORT_JACKSYNCCALLBACK
	.type	RTTI_TRANSPORT_JACKSYNCCALLBACK,@object
RTTI_TRANSPORT_JACKSYNCCALLBACK:
	.byte	23
	.ascii	"\020JackSyncCallback"
.Le13:
	.size	RTTI_TRANSPORT_JACKSYNCCALLBACK, .Le13 - RTTI_TRANSPORT_JACKSYNCCALLBACK

.section .data
	.balign 4
.globl	INIT_TRANSPORT_JACKTIMEBASECALLBACK
	.type	INIT_TRANSPORT_JACKTIMEBASECALLBACK,@object
INIT_TRANSPORT_JACKTIMEBASECALLBACK:
	.byte	23
	.ascii	"\024JackTimebaseCallback"
.Le14:
	.size	INIT_TRANSPORT_JACKTIMEBASECALLBACK, .Le14 - INIT_TRANSPORT_JACKTIMEBASECALLBACK

.section .data
	.balign 4
.globl	RTTI_TRANSPORT_JACKTIMEBASECALLBACK
	.type	RTTI_TRANSPORT_JACKTIMEBASECALLBACK,@object
RTTI_TRANSPORT_JACKTIMEBASECALLBACK:
	.byte	23
	.ascii	"\024JackTimebaseCallback"
.Le15:
	.size	RTTI_TRANSPORT_JACKTIMEBASECALLBACK, .Le15 - RTTI_TRANSPORT_JACKTIMEBASECALLBACK

.section .data
	.balign 4
.globl	INIT_TRANSPORT_JACK_TRANSPORT_BITS_T
	.type	INIT_TRANSPORT_JACK_TRANSPORT_BITS_T,@object
INIT_TRANSPORT_JACK_TRANSPORT_BITS_T:
	.byte	3
	.ascii	"\025jack_transport_bits_t"
	.byte	5
	.long	1,16,0
	.byte	18
	.ascii	"JackTransportState"
	.byte	21
	.ascii	"JackTransportPosition"
	.byte	17
	.ascii	"JackTransportLoop"
	.byte	18
	.ascii	"JackTransportSMPTE"
	.byte	16
	.ascii	"JackTransportBBT"
	.byte	0
.Le16:
	.size	INIT_TRANSPORT_JACK_TRANSPORT_BITS_T, .Le16 - INIT_TRANSPORT_JACK_TRANSPORT_BITS_T

.section .data
	.balign 4
.globl	RTTI_TRANSPORT_JACK_TRANSPORT_BITS_T
	.type	RTTI_TRANSPORT_JACK_TRANSPORT_BITS_T,@object
RTTI_TRANSPORT_JACK_TRANSPORT_BITS_T:
	.byte	3
	.ascii	"\025jack_transport_bits_t"
	.byte	5
	.long	1,16,0
	.byte	18
	.ascii	"JackTransportState"
	.byte	21
	.ascii	"JackTransportPosition"
	.byte	17
	.ascii	"JackTransportLoop"
	.byte	18
	.ascii	"JackTransportSMPTE"
	.byte	16
	.ascii	"JackTransportBBT"
	.byte	0
.Le17:
	.size	RTTI_TRANSPORT_JACK_TRANSPORT_BITS_T, .Le17 - RTTI_TRANSPORT_JACK_TRANSPORT_BITS_T

.section .data
	.balign 4
.globl	INIT_TRANSPORT_JACK_TRANSPORT_INFO_T
	.type	INIT_TRANSPORT_JACK_TRANSPORT_INFO_T,@object
INIT_TRANSPORT_JACK_TRANSPORT_INFO_T:
	.byte	13
	.ascii	"\025jack_transport_info_t"
	.long	96,0
.Le18:
	.size	INIT_TRANSPORT_JACK_TRANSPORT_INFO_T, .Le18 - INIT_TRANSPORT_JACK_TRANSPORT_INFO_T

.section .data
	.balign 4
.globl	RTTI_TRANSPORT_JACK_TRANSPORT_INFO_T
	.type	RTTI_TRANSPORT_JACK_TRANSPORT_INFO_T,@object
RTTI_TRANSPORT_JACK_TRANSPORT_INFO_T:
	.byte	13
	.ascii	"\025jack_transport_info_t"
	.long	96,17
	.long	RTTI_SYSTEM_LONGWORD
	.long	0
	.long	RTTI_SYSTEM_QWORD
	.long	4
	.long	RTTI_TRANSPORT_JACK_TRANSPORT_BITS_T
	.long	12
	.long	RTTI_TRANSPORT_JACK_TRANSPORT_STATE_T
	.long	16
	.long	RTTI_SYSTEM_LONGWORD
	.long	20
	.long	RTTI_SYSTEM_LONGWORD
	.long	24
	.long	RTTI_SYSTEM_LONGWORD
	.long	28
	.long	RTTI_SYSTEM_LONGINT
	.long	32
	.long	RTTI_SYSTEM_DOUBLE
	.long	36
	.long	RTTI_SYSTEM_LONGINT
	.long	44
	.long	RTTI_SYSTEM_LONGINT
	.long	48
	.long	RTTI_SYSTEM_LONGINT
	.long	52
	.long	RTTI_SYSTEM_DOUBLE
	.long	56
	.long	RTTI_SYSTEM_DOUBLE
	.long	64
	.long	RTTI_SYSTEM_DOUBLE
	.long	72
	.long	RTTI_SYSTEM_DOUBLE
	.long	80
	.long	RTTI_SYSTEM_DOUBLE
	.long	88
.Le19:
	.size	RTTI_TRANSPORT_JACK_TRANSPORT_INFO_T, .Le19 - RTTI_TRANSPORT_JACK_TRANSPORT_INFO_T

.section .data
	.balign 4
.globl	INIT_TRANSPORT_PJACK_TRANSPORT_INFO_T
	.type	INIT_TRANSPORT_PJACK_TRANSPORT_INFO_T,@object
INIT_TRANSPORT_PJACK_TRANSPORT_INFO_T:
	.byte	0
	.ascii	"\026Pjack_transport_info_t"
.Le20:
	.size	INIT_TRANSPORT_PJACK_TRANSPORT_INFO_T, .Le20 - INIT_TRANSPORT_PJACK_TRANSPORT_INFO_T

.section .data
	.balign 4
.globl	RTTI_TRANSPORT_PJACK_TRANSPORT_INFO_T
	.type	RTTI_TRANSPORT_PJACK_TRANSPORT_INFO_T,@object
RTTI_TRANSPORT_PJACK_TRANSPORT_INFO_T:
	.byte	0
	.ascii	"\026Pjack_transport_info_t"
.Le21:
	.size	RTTI_TRANSPORT_PJACK_TRANSPORT_INFO_T, .Le21 - RTTI_TRANSPORT_PJACK_TRANSPORT_INFO_T
# End asmlist al_rtti
# Begin asmlist al_dwarf

.section .debug_frame
	.long	.Lc3-.Lc2
.Lc2:
	.long	-1
	.byte	1
	.byte	0
	.uleb128	1
	.sleb128	-4
	.byte	8
	.byte	12
	.uleb128	4
	.uleb128	4
	.byte	5
	.uleb128	8
	.uleb128	1
	.balign 4,0
.Lc3:
# End asmlist al_dwarf
# Begin asmlist al_dwarf_info
# End asmlist al_dwarf_info
# Begin asmlist al_dwarf_abbrev
# End asmlist al_dwarf_abbrev
# Begin asmlist al_dwarf_line
# End asmlist al_dwarf_line
# Begin asmlist al_picdata
# End asmlist al_picdata
# Begin asmlist al_resourcestrings
# End asmlist al_resourcestrings
# Begin asmlist al_end

.section .text
.globl	DEBUGEND_TRANSPORT
	.type	DEBUGEND_TRANSPORT,@object
DEBUGEND_TRANSPORT:
	.stabs "",100,0,0,.Lf2
.Lf2:
# End asmlist al_end

