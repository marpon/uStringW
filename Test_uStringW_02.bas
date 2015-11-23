'compile with -console   to see more info
	 'and with -g  		 to get more verbose info on Alloc/reAlloc/Free   management


' to test  input an convert functions, with explicit or explicit convertion


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

'input using core FreeBasic to make convertion : to wstring ptr

'direcly without defining the size of wstring ( possible with literal only , the compiler take care)
dim as uStringW ustr1
dim as wstring ptr pws100 = varptr(wstr("ustr1 : hello here unicode Euro :  €")) 'convert string to wstring ptr
ustr1=pws100				'assignment with implicit convertion  wstring ptr to uStringW

'casted to string
	print ustr1
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr1 , "Hello " , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

'direcly with defining the size of wstring (to be done with var wstring )
' if you use var wstring it is the way
dim as uStringW ustr2
dim as wstring *100 ws100 = wstr("ustr2 : hello here unicode Euro :  €") 'convert string to wstring
ustr2= varptr(ws100)				'assignment with implicit convertion  wstring ptr to uStringW

'casted to string
	print ustr2
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr2 , "Hello " , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

'same but putting some espace sequence in the wstr function
dim as uStringW ustr3
dim as wstring *100 ws101 = wstr("ustr3 : hello here unicode Euro :  " & !"\u20AC" )  'convert string to wstring
ustr3= varptr(ws101)				'assignment with implicit convertion  wstring ptr to uStringW

'casted to string
	print ustr3
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr3 , "Hello " , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

'same but putting some espace sequence outside  the wstr function
dim as uStringW ustr4
dim as wstring *100 ws102 = wstr("ustr4 : hello here unicode Euro :  " ) & !"\u20AC" 'convert string to wstring
ustr4= varptr(ws102)				'assignment with implicit convertion  wstring ptr to uStringW

'casted to string
	print ustr4
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr4 , "Hello " , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

'same but putting some espace sequence outside  the wstr function
'no espace sequence on the string : if put here it will be implicitly converted to ansi
dim as string ss1="ustr5 : hello here unicode Euro :  "  '
dim as uStringW ustr5
dim as wstring *100 ws103 = wstr(ss1) & !"\u20AC" 'convert string to wstring and add the needed sequence
ustr5= varptr(ws103)				'assignment with implicit convertion  wstring ptr to uStringW

'casted to string
	print ustr5
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr5 , "Hello " , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

'#########################################################################################################
'
'new function to convert    u_Wstr(wstring ptr )  explicit convertion wstring ptr to uStringW
'#########################################################################################################

'Input using specific explicit convertion from wstring ptr to uStringW with  u_Wstr function
dim as Wstring ptr stest = varptr(wstr(!"\u20AC"))  ' another way of input , always with ! for escape sequence

dim as uStringW utest= u_Wstr(stest)                ' stest is a real wstring ptr
'casted to string
	print utest
#ifdef _INC_WINDOWS
	messagebox(0 , utest , "utest" , MB_OK) 				'verification
#Else
	Print:Print "Press key to continue" : sleep
#endif


'#########################################################################################################
'
'new function to convert    u_From_CodeString(string)
'#########################################################################################################
'input using string as container to code the input
'the string will be seen as a sequence of ansi byte or special cases
'all bytes will be considered as ansi char and converted to their equivalent wstring code,
'except for some pseudo byte sequence
'	\uXXYY  	6 byte sequence : XX and YY are the hex code of unicode char <=FFFF
'  \wZZXXYY 8 byte sequence : ZZ , XX and YY are the hex code of unicode char >FFFF
' with that kind of sequence the function can convert all the unicode chars
'remember the string is a normal string but with possible pseudo byte sequence to code all the unicode chars

'create string container code_string
dim as string ss2="ustr6 : hello here unicode Euro :  \u20AC"  ' or \w0020AC with 8 sequence
dim as uStringW ustr6
ustr6= u_From_CodeString(ss2) 'assignment with explicit convertion  code_string to uStringW
'or   dim as uStringW ustr6 = u_From_CodeString(ss2)

'casted to string
	print ustr6
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr6 , "Hello " , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif


'same but using the implicit conversion code_string to uStringW
dim as uStringW ustr7
ustr7 = "ustr7 : hello here unicode Euro :  \w0020AC" 'assignment with implicit convertion  code_string to uStringW


'casted to string
	print ustr7
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr7 , "Hello " , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

'even more simple using the assignment implicit conversion code_string to uStringW
'when found a string to assign to uStringW, the string is considered as code_string and converted
dim as uStringW ustr8 = "ustr8 : hello here unicode Euro :  \w0020AC" 'assignment with implicit convertion  code_string to uStringW

'casted to string
	print ustr8
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr8 , "Hello " , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

'#########################################################################################################
'
'new function to convert    u_from_Utf8s ( string)
'#########################################################################################################
'input using string as container holding a coded UTF8 sequence of bytes
'the string will be seen as a sequence of multibyte utf coded element
'remember the string is a normal string but contain multibyte sequence

' use the function like that :     ' € in utf8  chr(226,130,172)
'
dim as string string_Utf8_composed = "ustr9 : hello here unicode Euro :  " & chr(226,130,172)
dim as uStringW ustr9
ustr9 = u_from_Utf8s ( string_Utf8_composed )    'string_Utf8_composed is the coded string container

'casted to string
	print ustr9
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr9 , "Hello " , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif


'#########################################################################################################
'
'new function to convert   u_from_String( string )
'#########################################################################################################
'input using string as container holding a  sequence of all bytes for wstring
' the string is used as a container of ubyte sequence of the wstring representation ( 2 or 4 bytes)
'remember the string is a normal string but all bytes needed by wstring

' use the function like that :

		declare function simple_wstring_bytes(st1 as string) as string
		dim as string string_of_wstring_bytes = simple_wstring_bytes("ustr10 : hello here unicode Euro :  €")

		dim as uStringW ustr10
		ustr10 = u_from_String ( string_of_wstring_bytes )    'string_of_wstring_bytes is the all bytes string container
' the string must contain 2 bytes for each wstring in windows mode / 4 bytes in linux mode

'casted to string
	print ustr10
#ifdef _INC_WINDOWS
	'casted to wstring ptr for win API
	messagebox(0 , ustr10 , "Hello " , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

function simple_wstring_bytes(st1 as string) as string  ' function to simulate sequence of wstring bytes
	dim as string stemp=""
	for x as long = 1 to len(st1)
		if asc(mid(st1,x,1))=128 THEN
			stemp &= chr(172,32)
		else
			stemp &= mid(st1,x,1) & chr(0)
      END IF

		if len(wstring)=4 then stemp &= chr(0,0)
	NEXT
	return stemp
END FUNCTION
	
