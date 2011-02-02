	.file "midiport.pp"
# Begin asmlist al_begin

.section .text
.globl	DEBUGSTART_MIDIPORT
	.type	DEBUGSTART_MIDIPORT,@object
DEBUGSTART_MIDIPORT:
	.stabs "midiport.pp",100,0,0,.Lf1
.Lf1:
# End asmlist al_begin
# Begin asmlist al_stabs

.section .data
.globl	DEBUGINFO_MIDIPORT
	.type	DEBUGINFO_MIDIPORT,@object
DEBUGINFO_MIDIPORT:
# Defs - Begin unit SYSTEM has index 1
	.stabs "BYTE:t1=r1;0;255;",128,0,0,0
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
# Defs - Begin unit BASEUNIX has index 29
# Defs - End unit BASEUNIX has index 29
# Defs - Begin unit MIDIPORT has index 191
	.stabs "PJACK_MIDI_DATA_T:t2=*1",128,0,0,0
	.stabs "LONGWORD:t5=r5;0;-1;",128,0,0,0
	.stabs ":t6=*1",128,0,0,0
	.stabs "_JACK_MIDI_EVENT:Tt3=s12TIME:5,0,32;SIZE:5,32,32;BUFFER:6,64,32;;",128,0,0,0
	.stabs "PJACK_MIDI_EVENT_T:t4=*3",128,0,0,0
# Defs - End unit MIDIPORT has index 191
# Defs - Begin Staticsymtable
# Defs - End Staticsymtable
# Syms - Begin unit MIDIPORT has index 191
	.stabs "EXTERNAL_LIBRARY:c=s'jack';",36,0,15,0
	.stabs "JACK_MIDI_DATA_T:t1",128,0,50,0
	.stabs "PJACK_MIDI_DATA_T:t2",128,0,51,0
	.stabs "_JACK_MIDI_EVENT:Tt3",128,0,57,0
	.stabs "JACK_MIDI_EVENT_T:Tt3",128,0,62,0
	.stabs "PJACK_MIDI_EVENT_T:t4",128,0,63,0
# Syms - End unit MIDIPORT has index 191
# Syms - Begin Staticsymtable
# Syms - End Staticsymtable
# End asmlist al_stabs
# Begin asmlist al_procedures
# End asmlist al_procedures
# Begin asmlist al_globals

.section .data
	.balign 4
.globl	THREADVARLIST_MIDIPORT
	.type	THREADVARLIST_MIDIPORT,@object
THREADVARLIST_MIDIPORT:
	.long	0
# [midiport.pp]
# [161] 
.Le0:
	.size	THREADVARLIST_MIDIPORT, .Le0 - THREADVARLIST_MIDIPORT
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
.globl	INIT_MIDIPORT_PJACK_MIDI_DATA_T
	.type	INIT_MIDIPORT_PJACK_MIDI_DATA_T,@object
INIT_MIDIPORT_PJACK_MIDI_DATA_T:
	.byte	0
	.ascii	"\021Pjack_midi_data_t"
.Le1:
	.size	INIT_MIDIPORT_PJACK_MIDI_DATA_T, .Le1 - INIT_MIDIPORT_PJACK_MIDI_DATA_T

.section .data
	.balign 4
.globl	RTTI_MIDIPORT_PJACK_MIDI_DATA_T
	.type	RTTI_MIDIPORT_PJACK_MIDI_DATA_T,@object
RTTI_MIDIPORT_PJACK_MIDI_DATA_T:
	.byte	0
	.ascii	"\021Pjack_midi_data_t"
.Le2:
	.size	RTTI_MIDIPORT_PJACK_MIDI_DATA_T, .Le2 - RTTI_MIDIPORT_PJACK_MIDI_DATA_T

.section .data
	.balign 4
.globl	INIT_MIDIPORT__JACK_MIDI_EVENT
	.type	INIT_MIDIPORT__JACK_MIDI_EVENT,@object
INIT_MIDIPORT__JACK_MIDI_EVENT:
	.byte	13
	.ascii	"\020_jack_midi_event"
	.long	12,0
.Le3:
	.size	INIT_MIDIPORT__JACK_MIDI_EVENT, .Le3 - INIT_MIDIPORT__JACK_MIDI_EVENT

.section .data
	.balign 4
.globl	RTTI_MIDIPORT_DEF3
	.type	RTTI_MIDIPORT_DEF3,@object
RTTI_MIDIPORT_DEF3:
	.byte	0
	.ascii	"\000"
.Le4:
	.size	RTTI_MIDIPORT_DEF3, .Le4 - RTTI_MIDIPORT_DEF3

.section .data
	.balign 4
.globl	RTTI_MIDIPORT__JACK_MIDI_EVENT
	.type	RTTI_MIDIPORT__JACK_MIDI_EVENT,@object
RTTI_MIDIPORT__JACK_MIDI_EVENT:
	.byte	13
	.ascii	"\020_jack_midi_event"
	.long	12,3
	.long	RTTI_SYSTEM_LONGWORD
	.long	0
	.long	RTTI_SYSTEM_LONGWORD
	.long	4
	.long	RTTI_MIDIPORT_DEF3
	.long	8
.Le5:
	.size	RTTI_MIDIPORT__JACK_MIDI_EVENT, .Le5 - RTTI_MIDIPORT__JACK_MIDI_EVENT

.section .data
	.balign 4
.globl	INIT_MIDIPORT_PJACK_MIDI_EVENT_T
	.type	INIT_MIDIPORT_PJACK_MIDI_EVENT_T,@object
INIT_MIDIPORT_PJACK_MIDI_EVENT_T:
	.byte	0
	.ascii	"\022Pjack_midi_event_t"
.Le6:
	.size	INIT_MIDIPORT_PJACK_MIDI_EVENT_T, .Le6 - INIT_MIDIPORT_PJACK_MIDI_EVENT_T

.section .data
	.balign 4
.globl	RTTI_MIDIPORT_PJACK_MIDI_EVENT_T
	.type	RTTI_MIDIPORT_PJACK_MIDI_EVENT_T,@object
RTTI_MIDIPORT_PJACK_MIDI_EVENT_T:
	.byte	0
	.ascii	"\022Pjack_midi_event_t"
.Le7:
	.size	RTTI_MIDIPORT_PJACK_MIDI_EVENT_T, .Le7 - RTTI_MIDIPORT_PJACK_MIDI_EVENT_T
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
.globl	DEBUGEND_MIDIPORT
	.type	DEBUGEND_MIDIPORT,@object
DEBUGEND_MIDIPORT:
	.stabs "",100,0,0,.Lf2
.Lf2:
# End asmlist al_end

