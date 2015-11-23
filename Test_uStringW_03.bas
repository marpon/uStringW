'compile with -console   to see more info
	 'and with -g  		 to get more verbose info on Alloc/reAlloc/Free   management


' to test  u_Mid  , u_Left  , u_Right  , u_Instr  , u_Replace , u_Reverse , u_String , u_Parse


#define UNICODE					'needed to verify unicode with windows API   --MessageBox--
										'but not needed for UstringW type wich is simply a wstring variable length type

#INCLUDE ONCE "Windows.bi"  'just needed to check the windows API, but can compile without !!!!
' or put here the relevant needed include  for OS different from windows

#Ifdef __windows_bi__				'to insure compatibility with old FBC versions
	#Define _INC_WINDOWS
#ENDIF

'#define __VERBOSE_MODE__			'	uncomment to have mem menagement info / comment to hide

#Include once "Dyn_wstring.bi"





'#########################################################################################################
'test code
'#########################################################################################################



dim as uStringW ustr1 = "hello here unicode Euro :  €"     '  OK in dim implicit assigment


dim as uStringW  uret = u_Mid(ustr1,12)

'casted to string
	print ustr1
	print uret
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr1 & chr(10) & uret, "Hello u_Mid" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif


uret = u_Mid(ustr1,20,4)

'casted to string
	print ustr1
	print uret
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr1 & chr(10) & uret, "Hello u_Mid" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif


uret = u_Left(ustr1,24)

'casted to string
	print ustr1
	print uret
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr1 & chr(10) & uret, "Hello u_Left" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif


uret = u_Right(ustr1,22)

'casted to string
	print ustr1
	print uret
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr1 & chr(10) & uret, "Hello u_Right" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

dim as long ipos = u_Instr(ustr1, "here")
print
print "u_Instr(""" &  ustr1  & """, ""here"") = "; ipos

#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 ,"u_Instr(""" &  ustr1  & """, ""here"") = " & wstr( ipos), "Hello u_Instr" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

'dim as long ipos = u_Instr("hello here unicode Euro :  €", "here")  'ok also
'print
'print "u_Instr(""hello here unicode Euro :  €"", ""here"") = "; ipos

ipos = u_Instr(9 , ustr1 , "e" )
print
print "u_Instr(9 , """ &  ustr1  & ", ""e"") = " ; ipos

#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 ,"u_Instr(9 , """ &  ustr1  & ", ""e"") = " & wstr( ipos), "Hello u_Instr" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif


ipos = u_Instr( -20 , ustr1 , "e" )
print
print "u_Instr( -20 , """ &  ustr1  & ", ""e"") = " ; ipos

#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 ,"u_Instr( -20 , """ &  ustr1  & ", ""e"") = " & wstr( ipos), "Hello u_Instr" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif


uret = u_Replace(ustr1,"e","€")

	print ustr1
	print uret
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr1 & chr(10) & uret, "Hello u_Replace" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

uret = u_Replace(ustr1,"Euro","€ symbol")

	print ustr1
	print uret
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr1 & chr(10) & uret, "Hello u_Replace " , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif


uret = u_Reverse(ustr1)

	print ustr1
	print uret
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr1 & chr(10) & uret, "Hello u_Reverse " , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif


uret = u_String(5,8364)

	print ustr1
	print uret
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , "u_String(5,""€"")" & chr(10) & uret, "Hello u_String " , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif


uret = u_Parse(ustr1," ", 4)

	print ustr1
	print "uret = u_Parse(ustr1,"" "", 4)"
	print uret
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr1 & chr(10) & "uret = u_Parse(ustr1,"" "", 4)"  & chr(10) & uret, "Hello u_Parse " , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif
