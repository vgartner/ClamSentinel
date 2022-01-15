// IFSMGRLOCAL.H -- Local Declarations for Installable File System Manager Calls 

#ifndef IFSMGR_LOCAL_H 
#define IFSMGR_LOCAL_H 

#ifdef __cplusplus 
extern "C" { 
#endif 

#include <ifs.h>

#define IFSMGRVERSION 0x22

// IFSMgr services 

#ifndef IFSMgr_DEVICE_ID 
#define IFSMgr_DEVICE_ID 0x0040 
#endif 

#ifndef Not_VxD 

#undef IFSMgr_Service 
#define IFSMgr_Service Declare_Service 
#pragma warning(disable:4003) // not enough parameters 

Begin_Service_Table(IFSMgr) 

IFSMgr_Service(IFSMgr_Get_Version) 
IFSMgr_Service(IFSMgr_RegisterMount) 
IFSMgr_Service(IFSMgr_RegisterNet) 
IFSMgr_Service(IFSMgr_RegisterMailSlot) 
IFSMgr_Service(IFSMgr_Attach) 
IFSMgr_Service(IFSMgr_Detach) 
IFSMgr_Service(IFSMgr_Get_NetTime) 
IFSMgr_Service(IFSMgr_Get_DOSTime) 
IFSMgr_Service(IFSMgr_SetupConnection) 
IFSMgr_Service(IFSMgr_DerefConnection) 
IFSMgr_Service(IFSMgr_ServerDOSCall) 
IFSMgr_Service(IFSMgr_CompleteAsync) 
IFSMgr_Service(IFSMgr_RegisterHeap) 
IFSMgr_Service(IFSMgr_GetHeap) 
IFSMgr_Service(IFSMgr_RetHeap) 
IFSMgr_Service(IFSMgr_CheckHeap) 
IFSMgr_Service(IFSMgr_CheckHeapItem) 
IFSMgr_Service(IFSMgr_FillHeapSpare) 
IFSMgr_Service(IFSMgr_Block) 
IFSMgr_Service(IFSMgr_Wakeup) 
IFSMgr_Service(IFSMgr_Yield) 
IFSMgr_Service(IFSMgr_SchedEvent) 
IFSMgr_Service(IFSMgr_QueueEvent) 
IFSMgr_Service(IFSMgr_KillEvent) 
IFSMgr_Service(IFSMgr_FreeIOReq) 
IFSMgr_Service(IFSMgr_MakeMailSlot) 
IFSMgr_Service(IFSMgr_DeleteMailSlot) 
IFSMgr_Service(IFSMgr_WriteMailSlot) 
IFSMgr_Service(IFSMgr_PopUp) 
IFSMgr_Service(IFSMgr_printf) 
IFSMgr_Service(IFSMgr_AssertFailed) 
IFSMgr_Service(IFSMgr_LogEntry) 
IFSMgr_Service(IFSMgr_DebugMenu) 
IFSMgr_Service(IFSMgr_DebugVars) 
IFSMgr_Service(IFSMgr_GetDebugString) 
IFSMgr_Service(IFSMgr_GetDebugHexNum) 
IFSMgr_Service(IFSMgr_NetFunction) 
IFSMgr_Service(IFSMgr_DoDelAllUses) 
IFSMgr_Service(IFSMgr_SetErrString) 
IFSMgr_Service(IFSMgr_GetErrString) 
IFSMgr_Service(IFSMgr_SetReqHook) 
IFSMgr_Service(IFSMgr_SetPathHook) 
IFSMgr_Service(IFSMgr_UseAdd) 
IFSMgr_Service(IFSMgr_UseDel) 
IFSMgr_Service(IFSMgr_InitUseAdd) 
IFSMgr_Service(IFSMgr_ChangeDir) 
IFSMgr_Service(IFSMgr_DelAllUses) 
IFSMgr_Service(IFSMgr_CDROM_Attach) 
IFSMgr_Service(IFSMgr_CDROM_Detach) 
IFSMgr_Service(IFSMgr_Win32DupHandle) 
IFSMgr_Service(IFSMgr_Ring0_FileIO) 
IFSMgr_Service(IFSMgr_Win32_Get_Ring0_Handle) 
IFSMgr_Service(IFSMgr_Get_Drive_Info) 
IFSMgr_Service(IFSMgr_Ring0GetDriveInfo) 
IFSMgr_Service(IFSMgr_BlockNoEvents) 
IFSMgr_Service(IFSMgr_NetToDosTime) 
IFSMgr_Service(IFSMgr_DosToNetTime) 
IFSMgr_Service(IFSMgr_DosToWin32Time) 
IFSMgr_Service(IFSMgr_Win32ToDosTime) 
IFSMgr_Service(IFSMgr_NetToWin32Time) 
IFSMgr_Service(IFSMgr_Win32ToNetTime) 
IFSMgr_Service(IFSMgr_MetaMatch) 
IFSMgr_Service(IFSMgr_TransMatch) 
IFSMgr_Service(IFSMgr_CallProvider) 
IFSMgr_Service(UniToBCS) 
IFSMgr_Service(UniToBCSPath) 
IFSMgr_Service(BCSToUni) 
IFSMgr_Service(UniToUpper) 
IFSMgr_Service(UniCharToOEM) 
IFSMgr_Service(CreateBasis) 
IFSMgr_Service(MatchBasisName) 
IFSMgr_Service(AppendBasisTail) 
IFSMgr_Service(FcbToShort) 
IFSMgr_Service(ShortToFcb) 
IFSMgr_Service(IFSMgr_ParsePath) 
IFSMgr_Service(Query_PhysLock) 
IFSMgr_Service(_VolFlush) 
IFSMgr_Service(NotifyVolumeArrival) 
IFSMgr_Service(NotifyVolumeRemoval) 
IFSMgr_Service(QueryVolumeRemoval) 
IFSMgr_Service(IFSMgr_FSDUnmountCFSD) 
IFSMgr_Service(IFSMgr_GetConversionTablePtrs) 
IFSMgr_Service(IFSMgr_CheckAccessConflict) 
IFSMgr_Service(IFSMgr_LockFile) 
IFSMgr_Service(IFSMgr_UnlockFile) 
IFSMgr_Service(IFSMgr_RemoveLocks) 
IFSMgr_Service(IFSMgr_CheckLocks) 
IFSMgr_Service(IFSMgr_CountLocks) 
IFSMgr_Service(IFSMgr_ReassignLockFileInst) 
IFSMgr_Service(IFSMgr_UnassignLockList) 
IFSMgr_Service(IFSMgr_MountChildVolume) 
IFSMgr_Service(IFSMgr_UnmountChildVolume) 
IFSMgr_Service(IFSMgr_SwapDrives) 
IFSMgr_Service(IFSMgr_FSDMapFHtoIOREQ) 
IFSMgr_Service(IFSMgr_FSDParsePath) 
IFSMgr_Service(IFSMgr_FSDAttachSFT) 
IFSMgr_Service(IFSMgr_GetTimeZoneBias) 
IFSMgr_Service(IFSMgr_PNPEvent) 
IFSMgr_Service(IFSMgr_RegisterCFSD) 
IFSMgr_Service(IFSMgr_Win32MapExtendedHandleToSFT) 
IFSMgr_Service(IFSMgr_DbgSetFileHandleLimit) 
IFSMgr_Service(IFSMgr_Win32MapSFTToExtendedHandle) 
IFSMgr_Service(IFSMgr_FSDGetCurrentDrive) 
IFSMgr_Service(IFSMgr_InstallFileSystemApiHook) 
IFSMgr_Service(IFSMgr_RemoveFileSystemApiHook) 
IFSMgr_Service(IFSMgr_RunScheduledEvents) 
IFSMgr_Service(IFSMgr_CheckDelResource) 
IFSMgr_Service(IFSMgr_Win32GetVMCurdir) 
IFSMgr_Service(IFSMgr_SetupFailedConnection) 
IFSMgr_Service(_GetMappedErr) 
IFSMgr_Service(ShortToLossyFcb) 
IFSMgr_Service(IFSMgr_GetLockState) 
IFSMgr_Service(BcsToBcs) 
IFSMgr_Service(IFSMgr_SetLoopback) 
IFSMgr_Service(IFSMgr_ClearLoopback) 
IFSMgr_Service(IFSMgr_ParseOneElement) 
IFSMgr_Service(BcsToBcsUpper) 

End_Service_Table(IFSMgr) 

#pragma warning(default:4003) 

#endif // Not_VxD 

// Inline service function definitions

#ifndef Not_VxD 
#pragma warning(disable:4035) // missing return value 

#undef NAKED 
#define NAKED __declspec(naked) 

UINT VXDINLINE NAKED UniToBCSPath(PBYTE pBCSPath, PathElement* pUniPath, UINT maxLength, int charSet) 
{VxDJmp(UniToBCSPath)} 

ppIFSFileHookFunc VXDINLINE NAKED IFSMgr_InstallFileSystemApiHook(pIFSFileHookFunc fcn) 
{VxDJmp(IFSMgr_InstallFileSystemApiHook)} 

int VXDINLINE NAKED IFSMgr_RemoveFileSystemApiHook(pIFSFileHookFunc fcn) 
{VxDJmp(IFSMgr_RemoveFileSystemApiHook)} 

PVOID VXDINLINE NAKED IFSMgr_GetHeap(UINT size) 
{VxDJmp(IFSMgr_GetHeap)} 

VOID VXDINLINE NAKED IFSMgr_RetHeap(PVOID p) 
{VxDJmp(IFSMgr_RetHeap)} 

#endif // Not_VxD 

#ifdef __cplusplus 
} 
#endif 

#endif // IFSMGR_LOCAL_H