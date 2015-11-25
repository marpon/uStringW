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



'#########################################################################################################
'test code
'#########################################################################################################


'test  u_LineInput   ;  sub for reading files line/line  ok for win /linux   Ansi ,UTF8 ,UTF16LE ,UTF32BE
'  usage    Open txt For Input   As #filehandle
'				u_LineInput( filehandle,utxt,16)
'				Close #filehandle
'


Dim Filehandle As Integer
Dim utxt As uStringW



dim as ubyte ub(250)
dim as ubyte ptr pu 
dim  as long x,y

Dim txt As String 

'test for utf16le
txt = "hello_UTF16LE.bas" 
Filehandle = FreeFile
Open txt For Input   As #filehandle
	Do while EOF(Filehandle)=0
		y +=1
		u_LineInput( filehandle,utxt,16)
		Print "line " & y & " of " & txt & " is:"
		Print utxt 
		print "len of line " ;u_len(utxt)
'		print "bytes" : print
'		pu= varptr (ub(0))
'		pu = cast(ubyte ptr , u_strptr(utxt))
'		for x  = 1 to u_len(utxt) * 2
'			print x , pu[x-1] , hex(pu[x-1],2)
'		NEXT
'		erase ub
	LOOP
Close #filehandle
print: print "push any key to continue"
sleep

  'test for utf8
txt = "hello_UTF8.bas" 
Filehandle = FreeFile
Open txt For Input   As #filehandle
	Do while EOF(Filehandle)=0
		y +=1
		u_LineInput( filehandle,utxt,8)
		Print "line " & y & " of " & txt & " is:"
		Print utxt 
		print "len of line " ;u_len(utxt)
'		print "bytes" : print
'		pu= varptr (ub(0))
'		pu = cast(ubyte ptr , u_strptr(utxt))
'		for x  = 1 to u_len(utxt) * 2
'			print x , pu[x-1] , hex(pu[x-1],2)
'		NEXT
'		erase ub
	LOOP
Close #filehandle
print: print "push any key to continue"
sleep  

txt = "hello_UNC.bas"
Filehandle = FreeFile
Open txt For Input   As #filehandle
	Do while EOF(Filehandle)=0
		y +=1
		u_LineInput( filehandle,utxt,0)
		Print "line " & y & " of " & txt & " is:"
		Print utxt 
		print "len of line " ;u_len(utxt)
'		print "bytes" : print
'		pu= varptr (ub(0))
'		pu = cast(ubyte ptr , u_strptr(utxt))
'		for x  = 1 to u_len(utxt) * 2
'			print x , pu[x-1] , hex(pu[x-1],2)
'		NEXT
'		erase ub
	LOOP
Close #filehandle
print: print "push any key to continue"
sleep

txt = "hello_UTF32BE.bas"
Filehandle = FreeFile
Open txt For Input   As #filehandle
	Do while EOF(Filehandle)=0
		y +=1
		u_LineInput( filehandle,utxt,32)
		Print "line " & y & " of " & txt & " is:"
		Print utxt 
'		print "len of line " ;u_len(utxt)
'		print "bytes" : print
'		pu= varptr (ub(0))
'		pu = cast(ubyte ptr , u_strptr(utxt))
'		for x  = 1 to u_len(utxt) * 2
'			print x , pu[x-1] , hex(pu[x-1],2)
'		NEXT
'		erase ub
	LOOP
Close #filehandle
print: print "push any key to continue"
sleep
