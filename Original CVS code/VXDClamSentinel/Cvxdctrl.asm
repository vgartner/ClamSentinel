;******************************************************************************
TITLE CONTROL - ControlDispatch for VxD in C
;******************************************************************************
;

    .386p

;******************************************************************************
;                I N C L U D E S
;******************************************************************************
SENTINEL_MAJOR_VERSION	EQU	1
SENTINEL_MINOR_VERSION	EQU	6

IFSFN_DELETE Equ 31
IFSFN_OPEN   Equ 36
IFSFN_RENAME Equ 37
IFSFN_READ   Equ 0 ; read a file
IFSFN_WRITE  Equ 1 ; write a file
IFSFN_CLOSE Equ 11; close handle

   .xlist
   include vmm.inc
   include ifsmgr.inc
   include vwin32.inc

; the following equate makes the VXD dynamically loadable.
SENTINEL_DYNAMIC EQU 1
;============================================================================
;        V I R T U A L   D E V I C E   D E C L A R A T I O N
;============================================================================

DECLARE_VIRTUAL_DEVICE    SENTINEL, \
            SENTINEL_MAJOR_VERSION, SENTINEL_MINOR_VERSION, \
            SENTINEL_Control, UNDEFINED_DEVICE_ID, \
            UNDEFINED_INIT_ORDER

;===========================================================================
;
;   PROCEDURE: SENTINEL_Control
;
;   DESCRIPTION:
;    Device control procedure for the CVXD VxD
;
;   ENTRY:
;    EAX = Control call ID
;
;   EXIT:
;    If carry clear then
;        Successful
;    else
;        Control call failed
;
;   USES:
;    EAX, EBX, ECX, EDX, ESI, EDI, Flags
;
;============================================================================
VxD_LOCKED_CODE_SEG
BeginProc SENTINEL_Control
    Control_Dispatch SYS_DYNAMIC_DEVICE_INIT, CVXD_Dynamic_Init, sCall
    Control_Dispatch SYS_DYNAMIC_DEVICE_EXIT, CVXD_Dynamic_Exit, sCall
    Control_Dispatch W32_DEVICEIOCONTROL,     CVXD_W32_DeviceIOControl, sCall, <ecx, ebx, edx, esi>
    clc
    ret
EndProc SENTINEL_Control
VxD_LOCKED_CODE_ENDS

END