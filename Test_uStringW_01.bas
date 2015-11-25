'compile with -console   to see more info
	 'and with -g  		 to get more verbose info on Alloc/reAlloc/Free   management


' to test input features and contatenation

#define UNICODE					'needed to verify unicode with windows API   --MessageBox--
										'but not needed for UstringW type wich is simply a wstring variable length type



#INCLUDE ONCE "Windows.bi"  'just needed to check the windows API, but can compile without !!!!
' or put here the relevant needed include  for OS different from windows

#Ifdef __windows_bi__				'to insure compatibility with old FBC versions
	#Define _INC_WINDOWS
#ENDIF

#define __VERBOSE_MODE__			'	uncomment to have mem menagement info / comment to hide

#Include once "Dyn_wstring.bi"


'The direct Dim assignment is  allowed for input with :  uStringW
'												and (casted) with :  implicitly converted  string  or wstring ptr
'The assignment is done with :  uStringW   ( a copy in fact )
'			  and (casted) with :  implicitly converted  string  or wstring ptr

' concat with    &   or    +     with :  uStringW   and also with implicitly converted  string  or wstring ptr
' concat with   &=   or   +=		with :  uStringW   and also with implicitly converted  string  or wstring ptr


'#########################################################################################################
'test code
'#########################################################################################################


'dim as uStringW ustr1
'ustr1= "ustr1  hello here unicode Euro :  €"								'OK input implicitly assigned to ustringw

dim as uStringW ustr1 = "ustr1 : hello here unicode Euro :  €"     '  OK in dim implicit assigment
'dim as uStringW ustr1 = varptr(wstr("ustr1 : hello here unicode Euro :  €"))   ' ok also but complicated way
'casted to string
	print ustr1
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr1 , "Hello" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

dim as uStringW ustr2 = u_Wstr("ustr2  hello here unicode Euro :  €") 'OK input explicitly assigned to ustringw

'casted to string
	print ustr2
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr2 , "Hello2" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

dim as string ss1= "ustr3  hello here unicode Euro :  €"

dim as uStringW ustr3 =ss1    										'ok in dim implicit assigment
'dim as uStringW ustr3 =u_Wstr(ss1)									'OK input string converted to ustringw
'dim as uStringW ustr3
'ustr3 = ss1																	'OK implicit assignment

	'casted to string
	print ustr3
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr3 , "Hello3" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

dim as wstring ptr pws1= varptr(wstr("ustr4  hello here unicode Euro :  €")) ' string input converted to wstring ptr

dim as UstringW ustr4 = pws1    										' ok in dim implicit assigment
'dim as UstringW ustr4 = u_Wstr(pws1	)							'OK input wstring ptr converted to ustringw
'dim as UstringW ustr4
'ustr4 = pws1
																'Ok implicit assignment
'casted to string
	print varptr(ustr4),ustr4
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr4 , "Hello4" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

dim as UstringW ustr5 = ustr4											'Ok normal assignment

'kill_uStringW(ustr4)				'delete ustr4 completly , to verify it is no effect on ustr5

'ustr4=varptr(wstr("ustr4 : hello here unicode Euro :  €"))  			'reassignment with new value
ustr4 &=	" -  added part "															'or concat to verify if effect on ustr5
'casted to string
	print varptr(ustr5) , ustr5
	print varptr(ustr4) , ustr4
	print varptr(ustr5) , ustr5
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr5, "Hello5" , MB_OK)
	messagebox(0 , ustr4, "Hello4" , MB_OK)
	messagebox(0 , ustr5, "Hello5" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

'you can even do Dim direct assignment and concat immediatly with ustringw or implicitly converted

dim as uStringW umixed = ustr5 & "   - to test " + chr(10) & ustr4

'casted to string
	print umixed

#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , umixed, "Hello6" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

'or concat immediatly with ustringw or implicitly converted

'umixed &= chr(10) & "To verify again : " & chr(10) & ustr5 & "   - to test " + chr(10) & ustr4  'ok
umixed += chr(10) & "To verify again : " & chr(10) & ustr5 & "   - to test " + chr(10) & ustr4   'ok
'casted to string
	print varptr(ustr5) , umixed

#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , umixed, "Hello7" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif
