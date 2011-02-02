	.file "jack.pp"
# Begin asmlist al_begin

.section .text
.globl	DEBUGSTART_JACK
	.type	DEBUGSTART_JACK,@object
DEBUGSTART_JACK:
	.stabs "jack.pp",100,0,0,.Lf1
.Lf1:
# End asmlist al_begin
# Begin asmlist al_stabs

.section .data
.globl	DEBUGINFO_JACK
	.type	DEBUGINFO_JACK,@object
DEBUGINFO_JACK:
# Defs - Begin unit SYSTEM has index 1
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
# Defs - End unit TRANSPORT has index 196
# Defs - Begin unit BASEUNIX has index 29
# Defs - End unit BASEUNIX has index 29
# Defs - Begin unit JACKTHREAD has index 197
# Defs - End unit JACKTHREAD has index 197
# Defs - Begin unit JACK has index 190
	.stabs "void:t5=5",128,0,0,0
	.stabs "POINTER:t6=*5",128,0,0,0
	.stabs "PJACK_PORT_T:t1=*6",128,0,0,0
	.stabs "JACKSTATUS:Tt7=eJACKFAILURE:1,JACKINVALIDOPTION:2,JACKNAMENOTUNIQUE:4,JACKSERVERSTARTED:8,JACKSERVERFAILED:16,JACKSERVERERROR:32,JACKNOSUCHCLIENT:64,JACKLOADFAILURE:128,JACKINITFAILURE:256,JACKSHMFAILURE:512,JACKVERSIONERROR:1024,;",128,0,0,0
	.stabs "PJACK_STATUS_T:t2=*7",128,0,0,0
	.stabs "TFUNC:t3=*f5",128,0,0,0
	.stabs "PFUNC:t4=*3",128,0,0,0
# Defs - End unit JACK has index 190
# Defs - Begin Staticsymtable
# Defs - End Staticsymtable
# Syms - Begin unit JACK has index 190
	.stabs "EXTERNAL_LIBRARY:c=s'jack';",36,0,15,0
	.stabs "PJACK_PORT_T:t1",128,0,19,0
	.stabs "PJACK_STATUS_T:t2",128,0,20,0
	.stabs "TFUNC:t3",128,0,22,0
	.stabs "PFUNC:t4",128,0,23,0
# Syms - End unit JACK has index 190
# Syms - Begin Staticsymtable
# Syms - End Staticsymtable
# End asmlist al_stabs
# Begin asmlist al_procedures
# End asmlist al_procedures
# Begin asmlist al_globals

.section .data
	.balign 4
.globl	THREADVARLIST_JACK
	.type	THREADVARLIST_JACK,@object
THREADVARLIST_JACK:
	.long	0
# [jack.pp]
# [821] 
.Le0:
	.size	THREADVARLIST_JACK, .Le0 - THREADVARLIST_JACK
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
.globl	INIT_JACK_PJACK_PORT_T
	.type	INIT_JACK_PJACK_PORT_T,@object
INIT_JACK_PJACK_PORT_T:
	.byte	0
	.ascii	"\014Pjack_port_t"
.Le1:
	.size	INIT_JACK_PJACK_PORT_T, .Le1 - INIT_JACK_PJACK_PORT_T

.section .data
	.balign 4
.globl	RTTI_JACK_PJACK_PORT_T
	.type	RTTI_JACK_PJACK_PORT_T,@object
RTTI_JACK_PJACK_PORT_T:
	.byte	0
	.ascii	"\014Pjack_port_t"
.Le2:
	.size	RTTI_JACK_PJACK_PORT_T, .Le2 - RTTI_JACK_PJACK_PORT_T

.section .data
	.balign 4
.globl	INIT_JACK_PJACK_STATUS_T
	.type	INIT_JACK_PJACK_STATUS_T,@object
INIT_JACK_PJACK_STATUS_T:
	.byte	0
	.ascii	"\016Pjack_status_t"
.Le3:
	.size	INIT_JACK_PJACK_STATUS_T, .Le3 - INIT_JACK_PJACK_STATUS_T

.section .data
	.balign 4
.globl	RTTI_JACK_PJACK_STATUS_T
	.type	RTTI_JACK_PJACK_STATUS_T,@object
RTTI_JACK_PJACK_STATUS_T:
	.byte	0
	.ascii	"\016Pjack_status_t"
.Le4:
	.size	RTTI_JACK_PJACK_STATUS_T, .Le4 - RTTI_JACK_PJACK_STATUS_T

.section .data
	.balign 4
.globl	INIT_JACK_TFUNC
	.type	INIT_JACK_TFUNC,@object
INIT_JACK_TFUNC:
	.byte	23
	.ascii	"\005Tfunc"
.Le5:
	.size	INIT_JACK_TFUNC, .Le5 - INIT_JACK_TFUNC

.section .data
	.balign 4
.globl	RTTI_JACK_TFUNC
	.type	RTTI_JACK_TFUNC,@object
RTTI_JACK_TFUNC:
	.byte	23
	.ascii	"\005Tfunc"
.Le6:
	.size	RTTI_JACK_TFUNC, .Le6 - RTTI_JACK_TFUNC

.section .data
	.balign 4
.globl	INIT_JACK_PFUNC
	.type	INIT_JACK_PFUNC,@object
INIT_JACK_PFUNC:
	.byte	0
	.ascii	"\005Pfunc"
.Le7:
	.size	INIT_JACK_PFUNC, .Le7 - INIT_JACK_PFUNC

.section .data
	.balign 4
.globl	RTTI_JACK_PFUNC
	.type	RTTI_JACK_PFUNC,@object
RTTI_JACK_PFUNC:
	.byte	0
	.ascii	"\005Pfunc"
.Le8:
	.size	RTTI_JACK_PFUNC, .Le8 - RTTI_JACK_PFUNC
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
.globl	DEBUGEND_JACK
	.type	DEBUGEND_JACK,@object
DEBUGEND_JACK:
	.stabs "",100,0,0,.Lf2
.Lf2:
# End asmlist al_end

