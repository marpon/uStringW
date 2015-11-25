# uStringW
Freebasic unicode variable length type string , using wstring :  coded internally utf16 for windows / utf32 for linux

It is an attempt to make easier the way for unicode strings, and specialy not defining the length for these strings.

The process of allocated memory :
the dim  creates an allocated data of 1 wstring ( 2 or 4 bytes)
when assigned the allocation is done by steps of 16 wstrings (32 or 64 bytes) + 1 wstring (2 or 4 bytes) to insure always uStringW with null character , the allocation is growing or reducing constantly according the 16 wstrings steps. (or 1)
the core for that allocation/reallocation  is the hRealloc sub , initially from the fbc compiler source, it was litle adapted for my usage.

The uStringW type is using 
constructors to directly assignate uStringW from :
    uStringW (as it is evident)
    string  ( doing implicit conversion)
    wstring ptr (also with implicit conversion)
operators : 
    + ; & to concatenate with implicit conversion also for string ; wstring ptr ; longint ; double
    &= ; += also with implicit conversion for string ; wstring ptr
    = ; <> to compare uStringW
    let for the implicit conversion
    cast to cast to string or wstring ptr when needed
destructor:
    to free automatically the allocated mem according the uStringW scope
    
the global address list of the allocated memory is hold simply in a shared string array managed by a pseudo link list,
in fact a class type  ( thanks to Paul Squires, wich posted a very simple code to make it).
That global address list serves to manage the remaining allocated mem , and can be used to free the memory on demand.

As the automatic destructor function is in place , it can be avoided if needed 
  just comment that line   #define __U_CLEAN_MEM__  
  
the global process of allocating/reallocating/deallocating can be tracked with console messages, if you are in debug
mode or simply by putting that define   #define __VERBOSE_MODE__  before the include in your code

The "implicit conversion" for wstring ptr is very simple but not really interresting, if you already have wstring *XX
you have done the job, uStringW type is just to not define the length at compile time.
But anyhow just take the @ (varptr) of the wstring *xx to assign it direcly to uStringW  like : ustrw = @wst1
same to concat, same to direcly assign at creation   : dim as uStringW ustr = @wst1

The implicit conversion from strings is more interresting, it is done in different ways
the most evident   : 
  ustr = varptr(wstr("abcd")) wich converts string literal to wstring ptr, and implicitly converted to uStringW
  the string converted can hold special escape sequence !"\uXXYY" to hold the needed unicode direcly 
  notice that !"\uXXYY\uZZWW" that \u escape sequence with 4 Hex digits by unicode char.
you can also do the same like that   
  ustr = u_Wstr("Ansi string to convert" & !\uXXYY) with the u_Wstr new fubction.
but you can simply do that  
  ustr="Ansi string to convert" & "\uXXYY" notice the pseudo escape sequence without !
  it is because implicitly the string is converted by an encoding function 
  u_From_CodeString wich convert the ansi string to wstring but can hold also a pseudo sequence of 
  6 char \uXXYY  as the normal escape sequence do for unicode < &h10000 (max \uFFFF) but also
  8 char \wZZXXYY  for a pseudo escape sequence for unicode >&hFFFF
  
  by default the ustr = "string to convert" use that u_From_CodeString to convert ansi sequences
  
  But it also possible to use explicitly the function you want  :
  u_From_string   - the string is in fact container with a sequence of wstrings in 2 or 4 bytes     -no conversion
  u_From_ubyte    - 1 ubyte array as container as an alternative to the previous string container   -no conversion
  u_from_Utf8s    - 1 string as container for utf8 coded string   convertion done
  
  or using read_utf8_file ; read_utf16_file or read_utf32_file 
  to direcly assign to uStringW  a complete file in the different coding ways  
  ( the cr\lf , the control codes , even chr(0) are put in the uStringW
  
  For windows as said internally the coding is in UTF16  that means it has to take care of the surrogate pairs
  all the conversion function provided are managing properly that feature of surrogate pairs. 
  That also why it is included minimal manipulation uStringW function to take care of that issue.
  
  For linux is internally coded in UTF32 so minimal problem with surrogate story, just reading  with read_utf16_file
  as to take care of that to convert the possible surrogate pair to an unique wstring, 
  the provided function is managing that point also.

    
    
