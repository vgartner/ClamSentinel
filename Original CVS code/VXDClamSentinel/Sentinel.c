#define WANTVXDWRAPS

#include <basedef.h>
#include <vmm.h>
#include <vxdwraps.h>
#include <vwin32.h>
#include <winerror.h>
#include <ifs.h>
#include "ifsmgrlocal.h"
#include <string.h>

#pragma VxD_LOCKED_CODE_SEG
#pragma VxD_LOCKED_DATA_SEG

#pragma intrinsic(strlen,strcat,strcpy,strcmp,memset,memcpy)

#define TRUE 1
#define FALSE 0

#define INSTALL_HOOK 1
#define GET_OUTPUT 2

typedef unsigned short USHORT;
typedef unsigned long ULONG;

typedef struct
{
    ioreq            ifsir;
    struct hndlfunc  *ifs_hndl;
    ULONG            reserved[10];
} ifsreq, *pifsreq;

#define CVXD_VERSION 0x400

typedef DIOCPARAMETERS *LPDIOC;

#define MAXOUTPUTSIZE 256*MAX_PATH

char Output[MAXOUTPUTSIZE];

#define SIZE_FILEINUSE 8*0x400
unsigned char FILES_IN_USE[SIZE_FILEINUSE];

DWORD _stdcall CVXD_W32_DeviceIOControl(DWORD, DWORD, DWORD, LPDIOC);
int   _cdecl SentinelFileHook(pIFSFunc pfn, int nFunction, int nDrive, int nResources, int CodePage, pioreq pir);

DWORD _stdcall InstallHook(DWORD dwDDB, DWORD hDevice, LPDIOC lpDIOCParms);
DWORD _stdcall GetOutput(DWORD dwDDB, DWORD hDevice, LPDIOC lpDIOCParms);

ppIFSFileHookFunc ppPrevHook;

typedef char bool;

bool IsActive;

// Semaphore for critical sections
VMM_SEMAPHORE Sem_Output;
VMM_SEMAPHORE Sem_FilesInUse;

VOID ClearFilesInUse()
{ 
  Wait_Semaphore(Sem_FilesInUse, BLOCK_SVC_INTS);
    memset(FILES_IN_USE,0,SIZE_FILEINUSE);
  Signal_Semaphore(Sem_FilesInUse);
}

VOID ClearOut()
{ Output[0] = 0; }

VOID ClearOutMutex()
{
  Wait_Semaphore(Sem_Output, BLOCK_SVC_INTS);
    ClearOut();
  Signal_Semaphore(Sem_Output);
}

/****************************************************************************
                  CVXD_W32_DeviceIOControl
****************************************************************************/
DWORD _stdcall CVXD_W32_DeviceIOControl(DWORD  dwService,
                                        DWORD  dwDDB,
                                        DWORD  hDevice,
                                        LPDIOC lpDIOCParms)
{
    DWORD dwRetVal = 0;

    *(PDWORD)lpDIOCParms->lpcbBytesReturned = 0;

    // DIOC_OPEN is sent when VxD is loaded w/ CreateFile 
    //  (this happens just after SYS_DYNAMIC_INIT)
    if (dwService == DIOC_OPEN)
    {
        // Must return 0 to tell WIN32 that this VxD supports DEVIOCTL
        ppPrevHook = 0;
        IsActive = FALSE;
        dwRetVal = 0;
        return dwRetVal;
    }
    else if(dwService==INSTALL_HOOK)
    {
        // CALL requested service
        dwRetVal = InstallHook(dwDDB, hDevice, lpDIOCParms);
        return(dwRetVal);
    }
    else if(dwService==GET_OUTPUT){
        dwRetVal = GetOutput(dwDDB, hDevice, lpDIOCParms);
        return(dwRetVal);
    }
    return(dwRetVal);
}

DWORD _stdcall InstallHook(DWORD dwDDB, DWORD hDevice, LPDIOC lpDIOCParms)
{
  ClearOutMutex();
  ClearFilesInUse();

  ppPrevHook=IFSMgr_InstallFileSystemApiHook(SentinelFileHook);

  if (!ppPrevHook)
  { return FALSE; }
  else
  {
    IsActive = TRUE;
    return(NO_ERROR);
  }
}

DWORD _stdcall GetOutput(DWORD dwDDB, DWORD hDevice, LPDIOC lpDIOCParms)
{
  if (IsActive)
  {
    // If there is an output buffer
    Wait_Semaphore(Sem_Output, BLOCK_SVC_INTS);

    if ((lpDIOCParms->lpvOutBuffer)
        && (lpDIOCParms->cbOutBuffer >= sizeof(Output)))
    {
      // Copy the string into the output buffer
      strcpy((char*)(lpDIOCParms->lpvOutBuffer), Output);
      *(PDWORD)lpDIOCParms->lpcbBytesReturned = strlen(Output);

      ClearOut();
    }
    Signal_Semaphore(Sem_Output);
  }
    return(NO_ERROR);
}

VOID ConvertPath(int drive, path_t ppath, int CodePage, PCHAR fullpathname)
{
    int  i = 0;
    int  len = 0;

    // Stick on the drive letter if we know it.
    if ((drive & 0xFF) != 0xFF) {
        // Its a volume-based path
        fullpathname[0] = drive+'A'-1;
        fullpathname[1] = ':';
        i = 2;
    }

    len = UniToBCSPath(&fullpathname[i], ppath->pp_elements, MAX_PATH, CodePage); //BCS_WANSI BCS_OEM

    if (len >= 0)
      fullpathname[len+i] = 0;
    else
      fullpathname[0] = 0;
}

VOID GetPath(PCHAR fullname, int drive, int Resource, int CodePage, pioreq pir)
{
    pIFSFunc        enumFunc;
    ifsreq          ifsr;
    path_t          fPath;
    int             retval;

    fullname[0] = 0;

    fPath = IFSMgr_GetHeap(MAX_PATH * sizeof(wchar_t) + sizeof(path_t));
    if (fPath)
    {
      memcpy(&ifsr, pir, sizeof(ifsreq));
      ifsr.ifsir.ir_flags = ENUMH_GETFILENAME;
      ifsr.ifsir.ir_ppath = fPath;
      enumFunc = ifsr.ifs_hndl->hf_misc->hm_func[HM_ENUMHANDLE];

      retval = (*ppPrevHook)(enumFunc, IFSFN_ENUMHANDLE,
                                    drive, Resource, CodePage,
                                    (pioreq) &ifsr);

      if (retval == ERROR_SUCCESS)
        ConvertPath(drive, fPath, CodePage, fullname);

      IFSMgr_RetHeap((void *) fPath);
    }
}

#pragma VxD_LOCKED_CODE_SEG

#pragma optimize("", off)

VOID AddFile(PCHAR fullpathname)
{
  if (fullpathname[0] != 0)
  {
    Wait_Semaphore(Sem_Output, BLOCK_SVC_INTS);
      if (strlen(Output) + strlen(fullpathname) < sizeof(Output)-1)
      {
        strcat(Output,fullpathname);
        strcat(Output,"|");
      }
    Signal_Semaphore(Sem_Output);
  }
}

VOID Set_FileInUse(sfn_t ir_sfn, bool b)
{
  int i;
  unsigned char c;
  unsigned char v;

  if (ir_sfn>0)
  {
    i =  ir_sfn / 8;

    c = 1 << (8-(ir_sfn % 8));
    if (c==0) c=1;

    Wait_Semaphore(Sem_FilesInUse, BLOCK_SVC_INTS);
      v = FILES_IN_USE[i];
      if (b)
        FILES_IN_USE[i] = v | c;
      else
        {
          if ((v & c) == c)
            FILES_IN_USE[i] = v ^ c;
        }
    Signal_Semaphore(Sem_FilesInUse);
  }
}

bool Get_FileInUse(sfn_t ir_sfn)
{
  unsigned char c;
  bool b = FALSE;

  if (ir_sfn>0)
  {
    c = 1 << (8-(ir_sfn % 8));
    if (c==0) c=1;

    Wait_Semaphore(Sem_FilesInUse, BLOCK_SVC_INTS);

      if ((FILES_IN_USE[ir_sfn / 8] & c) == c)
        b = TRUE;
 
    Signal_Semaphore(Sem_FilesInUse);
  }
  return b;
}

int _cdecl SentinelFileHook(pIFSFunc pfn, int nFunction, int nDrive, int nResources, int CodePage, pioreq pir)
{
  bool bWrite;
  char fullpathname[MAX_PATH];
  int iRet=0;

  fullpathname[0]=0;

  iRet=(*(*ppPrevHook))(pfn, nFunction, nDrive, nResources, CodePage, pir);

  if ((IsActive) && ((nDrive & 0xFF) != 0xFF))
  {
    switch(nFunction)
    {
      case IFSFN_OPEN:

        Set_FileInUse(pir->ir_sfn,FALSE);
        break;

      case IFSFN_RENAME:

         //Target
         ConvertPath(nDrive, pir->ir_ppath2, CodePage, fullpathname);
         AddFile(fullpathname);
         break;

      case IFSFN_CLOSE:

        if ((pir->ir_flags ) == CLOSE_FINAL)
        {
           bWrite = (Get_FileInUse(pir->ir_sfn) == TRUE);
           Set_FileInUse(pir->ir_sfn,FALSE);

           if (bWrite)
           {
             GetPath(fullpathname, nDrive, nResources, CodePage, pir);
             AddFile(fullpathname);
           }
        }
        break;

      case IFSFN_DELETE:
        break;

      case IFSFN_WRITE:

        //sometimes, a process writes zero bytes only to verify.
        if(pir->ir_length==0) break;

        Set_FileInUse(pir->ir_sfn,TRUE);
        break;

      case IFSFN_DIR:

        if (pir->ir_flags == CREATE_DIR)
        {
          ConvertPath(nDrive, pir->ir_ppath, CodePage, fullpathname);
          AddFile(fullpathname);
        }
        break;
    }
  }
  return iRet;
}

#pragma optimize("", on)

DWORD _stdcall CVXD_Dynamic_Exit(void)
{
    IsActive = FALSE;

    if (ppPrevHook)
     IFSMgr_RemoveFileSystemApiHook(SentinelFileHook);

    ClearOutMutex();

    if (Sem_Output)
    {
      Signal_Semaphore(Sem_Output);
      Destroy_Semaphore(Sem_Output);
      Sem_Output = NULL;
    }

    if (Sem_FilesInUse)
    {
      Signal_Semaphore(Sem_FilesInUse);
      Destroy_Semaphore(Sem_FilesInUse);
      Sem_FilesInUse = NULL;
    }

    return(VXD_SUCCESS);
}

DWORD _stdcall CVXD_Dynamic_Init(void)
{
    IsActive = FALSE;
    // Initialize the locks
    Sem_Output = Create_Semaphore(1);
    Sem_FilesInUse = Create_Semaphore(1);
    return(VXD_SUCCESS);
}