	.file "jackthread.pp"
# Begin asmlist al_begin

.section .text
.globl	DEBUGSTART_JACKTHREAD
	.type	DEBUGSTART_JACKTHREAD,@object
DEBUGSTART_JACKTHREAD:
	.stabs "jackthread.pp",100,0,0,.Lf1
.Lf1:
# End asmlist al_begin
# Begin asmlist al_stabs

.section .data
.globl	DEBUGINFO_JACKTHREAD
	.type	DEBUGINFO_JACKTHREAD,@object
DEBUGINFO_JACKTHREAD:
# Defs - Begin unit SYSTEM has index 1
	.stabs "void:t6=6",128,0,0,0
	.stabs "POINTER:t1=*6",128,0,0,0
# Defs - End unit SYSTEM has index 1
# Defs - Begin unit OBJPAS has index 4
# Defs - End unit OBJPAS has index 4
# Defs - Begin unit UNIXTYPE has index 28
# Defs - End unit UNIXTYPE has index 28
# Defs - Begin unit BASEUNIX has index 29
# Defs - End unit BASEUNIX has index 29
# Defs - Begin unit JACKTHREAD has index 197
	.stabs "PJACK_CLIENT_T:t2=*1",128,0,0,0
	.stabs "LONGWORD:t7=r7;0;-1;",128,0,0,0
	.stabs "PPTHREAD_T:t3=*7",128,0,0,0
	.stabs "TSTARTROUTINE:t4=*f1",128,0,0,0
	.stabs "PSTARTROUTINE:t5=*4",128,0,0,0
# Defs - End unit JACKTHREAD has index 197
# Defs - Begin Staticsymtable
# Defs - End Staticsymtable
# Syms - Begin unit JACKTHREAD has index 197
	.stabs "EXTERNAL_LIBRARY:c=s'jack';",36,0,15,0
	.stabs "JACK_CLIENT_T:t1",128,0,18,0
	.stabs "PJACK_CLIENT_T:t2",128,0,19,0
	.stabs "PPTHREAD_T:t3",128,0,20,0
	.stabs "TSTARTROUTINE:t4",128,0,22,0
	.stabs "PSTARTROUTINE:t5",128,0,23,0
	.stabs "THREAD_STACK:c=i524288;",36,0,56,0
# Syms - End unit JACKTHREAD has index 197
# Syms - Begin Staticsymtable
# Syms - End Staticsymtable
# End asmlist al_stabs
# Begin asmlist al_procedures
# End asmlist al_procedures
# Begin asmlist al_globals

.section .data
	.balign 4
.globl	THREADVARLIST_JACKTHREAD
	.type	THREADVARLIST_JACKTHREAD,@object
THREADVARLIST_JACKTHREAD:
	.long	0
# [jackthread.pp]
# [113] 
.Le0:
	.size	THREADVARLIST_JACKTHREAD, .Le0 - THREADVARLIST_JACKTHREAD
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
.globl	INIT_JACKTHREAD_PJACK_CLIENT_T
	.type	INIT_JACKTHREAD_PJACK_CLIENT_T,@object
INIT_JACKTHREAD_PJACK_CLIENT_T:
	.byte	0
	.ascii	"\016Pjack_client_t"
.Le1:
	.size	INIT_JACKTHREAD_PJACK_CLIENT_T, .Le1 - INIT_JACKTHREAD_PJACK_CLIENT_T

.section .data
	.balign 4
.globl	RTTI_JACKTHREAD_PJACK_CLIENT_T
	.type	RTTI_JACKTHREAD_PJACK_CLIENT_T,@object
RTTI_JACKTHREAD_PJACK_CLIENT_T:
	.byte	0
	.ascii	"\016Pjack_client_t"
.Le2:
	.size	RTTI_JACKTHREAD_PJACK_CLIENT_T, .Le2 - RTTI_JACKTHREAD_PJACK_CLIENT_T

.section .data
	.balign 4
.globl	INIT_JACKTHREAD_PPTHREAD_T
	.type	INIT_JACKTHREAD_PPTHREAD_T,@object
INIT_JACKTHREAD_PPTHREAD_T:
	.byte	0
	.ascii	"\012Ppthread_t"
.Le3:
	.size	INIT_JACKTHREAD_PPTHREAD_T, .Le3 - INIT_JACKTHREAD_PPTHREAD_T

.section .data
	.balign 4
.globl	RTTI_JACKTHREAD_PPTHREAD_T
	.type	RTTI_JACKTHREAD_PPTHREAD_T,@object
RTTI_JACKTHREAD_PPTHREAD_T:
	.byte	0
	.ascii	"\012Ppthread_t"
.Le4:
	.size	RTTI_JACKTHREAD_PPTHREAD_T, .Le4 - RTTI_JACKTHREAD_PPTHREAD_T

.section .data
	.balign 4
.globl	INIT_JACKTHREAD_TSTARTROUTINE
	.type	INIT_JACKTHREAD_TSTARTROUTINE,@object
INIT_JACKTHREAD_TSTARTROUTINE:
	.byte	23
	.ascii	"\015TStartRoutine"
.Le5:
	.size	INIT_JACKTHREAD_TSTARTROUTINE, .Le5 - INIT_JACKTHREAD_TSTARTROUTINE

.section .data
	.balign 4
.globl	RTTI_JACKTHREAD_TSTARTROUTINE
	.type	RTTI_JACKTHREAD_TSTARTROUTINE,@object
RTTI_JACKTHREAD_TSTARTROUTINE:
	.byte	23
	.ascii	"\015TStartRoutine"
.Le6:
	.size	RTTI_JACKTHREAD_TSTARTROUTINE, .Le6 - RTTI_JACKTHREAD_TSTARTROUTINE

.section .data
	.balign 4
.globl	INIT_JACKTHREAD_PSTARTROUTINE
	.type	INIT_JACKTHREAD_PSTARTROUTINE,@object
INIT_JACKTHREAD_PSTARTROUTINE:
	.byte	0
	.ascii	"\015PStartRoutine"
.Le7:
	.size	INIT_JACKTHREAD_PSTARTROUTINE, .Le7 - INIT_JACKTHREAD_PSTARTROUTINE

.section .data
	.balign 4
.globl	RTTI_JACKTHREAD_PSTARTROUTINE
	.type	RTTI_JACKTHREAD_PSTARTROUTINE,@object
RTTI_JACKTHREAD_PSTARTROUTINE:
	.byte	0
	.ascii	"\015PStartRoutine"
.Le8:
	.size	RTTI_JACKTHREAD_PSTARTROUTINE, .Le8 - RTTI_JACKTHREAD_PSTARTROUTINE
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
.globl	DEBUGEND_JACKTHREAD
	.type	DEBUGEND_JACKTHREAD,@object
DEBUGEND_JACKTHREAD:
	.stabs "",100,0,0,.Lf2
.Lf2:
# End asmlist al_end

