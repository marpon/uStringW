

/'  ' begins RC Bloc

_BEGIN_RC_  'début de bloc Rc   placer un '  devant _BEGIN_RC_     , pour ne pas prendre en compte
	#define IDD_DLG1 1000
	#define IDC_BTN2 1002
	#define IDC_EDT1 1003
	#define IDC_BTN1 1001
	#define IDC_STC1 1004
	#define Icon1 500

	IDD_DLG1 DIALOGEX 6,5,194,107
	CAPTION "test70 - pour voir !"
	FONT 9,"MS Sans Serif",2000,0,0
	STYLE 0x10CC0800
	BEGIN
		CONTROL "Annuler",IDC_BTN2,"Button",0x50010000,99,69,63,18
		CONTROL "Entrée vos saisie ici ...!",IDC_EDT1,"Edit",0x50010000,39,42,117,15,0x00000200
		CONTROL "Ok",IDC_BTN1,"Button",0x50010000,27,69,63,18
		CONTROL "Mon texte de vérification",IDC_STC1,"Static",0x50000201,12,12,174,24
	END

	Icon1 ICON DISCARDABLE "info.ico"
_END_RC_  'fin de bloc Rc     placer un '  devant _END_RC_  , pour ne pas prendre en compte

'/  ' ends RC Bloc



#Define COMPIL_NAME test34.exe      ' définition du nom exe/dll
'													placer un '  devant pour ne pas prendre en compte


#define UNICODE
#Include Once "windows.bi"


#Include once "Dyn_wstring.bi"

#define IDD_DLG1 1000
#define IDC_BTN2 1002
#define IDC_EDT1 1003
#define IDC_BTN1 1001
#define IDC_STC1 1004
#define Icon1 500




Dim Shared hIn1    AS hModule
Dim Shared hIcon1  AS hIcon

Dim Shared AS uStringW retour
Dim Shared AS uStringW Titre
Dim Shared AS uStringW Valeur
Dim Shared AS uStringW Info


Declare Function DlgProc(ByVal hI1 As hWnd, ByVal uI1 As Ulong, ByVal wP1 As wParam, ByVal lP1 As lParam) As long



Declare Function WinMain(ByVal hI1 As hInstance, _
      ByVal hP1 As hInstance, _
      ByRef CLine As String, _
      ByVal CShow As long) As long


' définition de la function initiale
End WinMain(GetModuleHandle(null), null, Command(), SW_NORMAL)
' sinon démarrage sans function

'''
''' Program start
'''
Function WinMain(ByVal hInstExe As hInstance, _
         ByVal hPrevInstance As hInstance, _
         ByRef lpCmdLine As String, _
         ByVal iCmdShow As long) As long


   Titre = "Quel Titre ?"
   Valeur = "\u604FQuelle Valeur ?"
   Info = "Quelle Info ?"

  
	hIn1 = hInstExe								  '' initialisation avec function winmain
   '' Create the Dialog
   DialogBoxParam(hIn1,  cast(wstring ptr,IDD_DLG1), NULL, @DlgProc, NULL)
	
	''
   '' Program has ended
   Return 0
End Function
''' Program end



Function DlgProc(ByVal hWin As hWnd, ByVal uMsg As Ulong, _
         ByVal wParam1 As wParam, _
         ByVal lParam1 As lParam) As long
   Dim AS long id
   Dim AS long Event1

   dim nBuffer              AS uStringW = u_space(50)

   Select Case uMsg
'		Case WM_CREATE
'			SetTimer( hWin, 1, 6000, null )
      Case WM_INITDIALOG
         SetWindowText hWin, Titre
         SetWindowText GetDlgItem(hWin, IDC_EDT1), Valeur
         SetWindowText GetDlgItem(hWin, IDC_STC1), Info
         'SetTimer( hWin, 1, 4000, null )
         hIcon1 = LoadIcon(hIn1, Cast(wString Ptr, Icon1))
         SendMessage(hWin, WM_SETICON, NULL, Cast(lParam, hIcon1))
      Case WM_CLOSE

			EndDialog(hWin, 0)

      Case WM_TIMER
			'MessageBox(hWin, "Fin du temps prévu", "Timer", MB_ICONINFORMATION)
			'Killtimer(hWin, 1)
			'EndDialog(hWin, 0)
      Case WM_COMMAND
         id = LoWord(wParam1)
         Event1 = HiWord(wParam1)
         Select Case id
            Case IDC_BTN1
               GetWindowText GetDlgItem(hWin, IDC_EDT1), nBuffer, 50
               'MessageBox(hWin, nBuffer, "Test de bouton 1", MB_ICONINFORMATION)
               retour = nBuffer
					print "nBuffer[0]",nBuffer[0]
					MessageBox(hWin, "retour = " & retour, "Retour", MB_ICONINFORMATION)
               EndDialog(hWin, 0)
            Case IDC_BTN2
               'MessageBox(hWin, "Fermeture du programme", "Bouton OK", MB_ICONSTOP)
               retour = "Annul"
               EndDialog(hWin, 0)
               '
         End Select

      Case Else
         Return FALSE
         '
   End Select
   Return TRUE

End Function




