
'compile with -console   to see the job
	 'and with -g  		 to get more verbose info on Alloc/reAlloc/Free   management

#define UNICODE					'needed to verify unicode with windows API   --MessageBox--
										'but not needed for UstringW type wich is simply a wstring variable length type



#INCLUDE ONCE "Windows.bi"  'just needed to check the windows API, but  compile without !!!!
' or put here the relevant needed include  for OS different from windows

#Ifdef __windows_bi__				'to insure compatibility with old FBC versions
	#Define _INC_WINDOWS
#ENDIF

#define __VERBOSE_MODE__

#Include once "Dyn_wstring.bi"


print "Test code"
dim    str1     as UstringW
dim    old1     as UstringW
dim    new1     as UstringW
dim   utest1     as UstringW
'input from wstring :   note the use of ! before the escape sequence \uXXXX  with XXXX is the ucode in hex
str1 = u_Wstr( "€ This is a test to verify how it works with unicode variable length type " & !"\u20AC")


'str1=u_Mid(str1,4,10)								' u_Mid like Mid

'str1=WReplaceChar(str1 , 8364 , 18300)			' WReplaceChar  with 8364 ,18300 are the decimal unicode value

print "u_Asc : ", u_Asc(str1,1)

dim as long lt1=u_Asc(u_From_CodeString("\uD834\uDD1E"))
dim as long lt2=u_Asc(u_From_CodeString("\w027128"))
print &h1D11E,lt1,&h27128,lt2

dim    str99     as UstringW = u_Chr (&h10000 )
print "u_Asc : ", u_Asc(str99) , &h10000
dim    str98     as UstringW = u_String(7,65)
messagebox(0 , u_Strptr(str98) , "str98" , MB_OK)

print "str1" , u_to_Ansi(str1) , "len uni = " ; Len(str1) 'warning Len return the size of structure UstringW so 12
old1 = u_Wstr( "est")
'old1 =  "est"
'new1 = u_Wstr( " ""mixer"" " & !"\u00E9")
new1 = " ""mixer"" " & !"\u00E9"
utest1 += old1
if utest1 = old1 THEN
	print "utest1 = old1"
else
	print "utest1 <> old1"
end if
utest1 += old1
if utest1 <> old1 THEN
	print "utest1 <> old1"
else
	print "utest1 = old1"
end if
utest1=u_Reverse(utest1)

#ifdef _INC_WINDOWS
	messagebox(0 , utest1 , "Hello" , MB_OK)
#Endif

print "all " , u_to_Ansi(str1) , u_to_Ansi(old1) , u_to_Ansi(new1)
print "len" , str1.len1 , old1.len1 , new1.len1
print "size" , str1.size1 , old1.size1 , new1.size1
print "mem" , str1.data1 , old1.data1 , new1.data1

print: print " action : replace"
dim as UstringW retour = u_Replace(str1 , old1 , new1)  'generic function for replacing or removing sub-strings

print: print "here ==="
print "return = " & retour.data1 & "  " & u_to_Ansi(retour)


print "all2 " , u_to_Ansi(str1), u_to_Ansi(old1) , u_to_Ansi(new1)
print "len2" , str1.len1 , old1.len1 , new1.len1 , retour.len1
print "size2" , str1.size1 , old1.size1 , new1.size1 , retour.size1
print "mem" , str1.data1 , old1.data1 , new1.data1 , retour.data1


#ifdef _INC_WINDOWS
	messagebox(0 , retour , "Hello" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

' test with easy unicode character : Euro  wich is 20AC in hex,  but also ascii code 128 depending codepage

'new1 = u_Wstr(!"\u20AC\u20AC_abcd_")  'similar to line after  notice the need of ! for escape sequence
new1 = u_From_CodeString( "\w010000\w01D11E\w01F456_abcd_")   'input in string type mode   no need of ! but similar \uXXXX to code
'U+10000 	{U+D800, U+DC00}
'U+1D11E 	{U+D834, U+DD1E}
'new1 = u_Wstr(!"\u20AC\u20AC_abcd_" & !"\u10000" & "_efgh" & !"\uD800\uDC00")
'new1 = u_Wstr( !"\u00010000_abcd_")
print "new1 "; *new1.data1, u_to_Ansi(new1), new1
#ifdef _INC_WINDOWS
messagebox(0 , new1 , "Hello2" , MB_OK)
#endif

'new1=u_Mid(new1,3,4)
new1=u_Reverse(new1)
new1=u_Mid(new1,4,4)
'new1=u_Right(new1,7)
print new1
print: print " action : replace"
retour = u_Replace(str1 , old1 , new1)

print "return = " & retour.data1 & "  " & u_to_Ansi(retour)

#ifdef _INC_WINDOWS
	messagebox(0 , retour , "Hello2" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

dim as Wstring ptr stest = varptr(wstr(!"\u20AC"))  ' another way of input , always with ! for escape sequence
#ifdef _INC_WINDOWS
	messagebox(0 , stest , "stest" , MB_OK) 				'verification
#endif
old1 = u_Wstr(stest)                ' stest is a real wstring ptr

new1 = u_Wstr( "nothing" & !"\n")   ' to verify the \n escape sequence  newline

print: print " action : replace"
retour = u_Replace(retour , old1 , new1)

print "return = " & retour.data1 & "  " & u_to_Ansi(retour)
print "all3 " , u_to_Ansi(str1) , u_to_Ansi(old1) , u_to_Ansi(new1)
print "len3" , str1.len1 , old1.len1 , new1.len1 , retour.len1
print "size3" , str1.size1 , old1.size1 , new1.size1 , retour.size1
print "mem" , str1.data1 , old1.data1 , new1.data1 , retour.data1

#ifdef _INC_WINDOWS
	messagebox(0 , retour , "Hello3" , MB_OK)
#Else
	Print:Print "Press key to continue" : sleep
#endif

stest = varptr(wstr( "uni"))
old1 = u_Wstr(stest)

new1 = u_Wstr( "")							'to test empty string
print: print " action : replace"
retour = u_Replace(retour , old1 , new1)


print "return = " & retour.data1 & "  " & u_to_Ansi(retour)
print "all4 " , u_to_Ansi(str1) , u_to_Ansi(old1) , u_to_Ansi(new1)
print "len4" , str1.len1 , old1.len1 , new1.len1 , retour.len1
print "size4" , str1.size1 , old1.size1 , new1.size1 , retour.size1
print "mem" , str1.data1 , old1.data1 , new1.data1 , retour.data1

#ifdef _INC_WINDOWS
   messagebox(0 , retour , "Hello4" , MB_OK)
#endif

print : print "last mem  " ; retour.data1 , u_to_Ansi(retour) : print
dim as string smen = "a" & chr(0) & "b" & chr(0) & "c" & chr(0) & "e" & chr(0) & "f" & chr(0) & "h" & chr(0)
new1=u_from_String(smen)
'new1=u_From_CodeString(smen)
'new1 = u_Wstr("abc" & !"\u20AC" & "ef")
dim as ushort u22= new1.data1[3]
dim as string tt=u_to_Ansi(new1)
Print "Test pour voir" , u_to_Ansi(new1) ,new1.data1[0] , u22, tt, u_Wdata(new1),"len "; u_Len(new1),"len() ";len(*new1.data1)
dim as string u8=u_to_Utf8s(new1)

'dim as UstringW ucopy

'ucopy = u_Wstr(*(new1.data1) & *(old1.data1))
'#ifdef _INC_WINDOWS
'messagebox 0, u_Strptr(ucopy) ,"to see",0
'#endif
dim as string st56=" ceci pour vérifier et voir € "
print "to utf8 " ; u8 , "len u8 " ;len(u8)
dim as UstringW u16
u16 &= st56 + u_from_Utf8s(u8)& st56
print "from utf8 " , *u16.data1
#ifdef _INC_WINDOWS
	messagebox 0,new1 & "   " & new1 & "    " & u16  ,tt,0
#endif
'dim as wstring ptr pwk = u_Wdata(new1)
'kill_uStringW(new1)

'print "killed mem  : " & pwk , *pwk

dim as ustringW ufil1= read_utf16_file("hello_UTF16BE.bas")
#ifdef _INC_WINDOWS
	messagebox 0,u_Strptr(ufil1),"read_utf16_file",0
#endif
print *u_Strptr(ufil1)
print "length = " & u_Len(ufil1)  , len(ufil1)
write_utf16_file("hello_UTF16LE3.bas",ufil1)

dim as ustringW ufil2= read_utf8_file("hello_UTF8.bas")
#ifdef _INC_WINDOWS
	messagebox 0,u_Strptr(ufil2),"read_utf8_file",0
#endif
print *u_Strptr(ufil2)
print "length = " & u_Len(ufil2), len(*ufil2.data1)
write_utf16_file("test_u16.txt",u16)
dim u32be as ustringw
u32be= read_utf32_file("hello_UTF32BE.bas")
#ifdef _INC_WINDOWS
	messagebox 0,u_Strptr(u32be),"read_utf32be_file",0
#endif
write_utf32_file("hello_UTF32BE4.bas",ufil2)
write_utf8_file("hello_UTF8xxx.bas",u32be)


Print : print "====   Press a key to finish   ===="
sleep
