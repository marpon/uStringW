
'#########################################################################################################
' Code to create VARIABLE LENGTH WSTRING type , and some helper sub/function to use it !
'
' the inspiration code is from the FreeBasic source compiler itself, i'm still do not understand
' why it was not extended on the core functions ?
' I hope it will evoluate to incorporate this important feature in simpler way.
' the core sub is : hRealloc wich makes the allocate/deallocate transparent for the user.
' it normally will work on both Windows / linux platforms taking into account the
' different size in bytes 2 for Win ; 4 for Linux.
' So internally the uStringW type will be coded as utf16 for Windows and platform using 2 bytes Wstring
' as utf32 for linux and platform using 4 bytes Wstring
' the main interest is : no need to define Wstring length , the counterpart : is problably slowing the
' process, but not noticed on average usage , most of the uStringW are allocated only once
'#########################################################################################################
' "Borrowed code" from Paul Squires : original post for linked_list example
' http://www.freebasic.net/forum/viewtopic.php?f=7&t=23902&p=211211&hilit=linked+list#p211211
' used to simply take trace of the allocated memory, and automatically to free it when prog ends.
' That part seems to be removed without risk , but better programming usage to free the allocated memory.
'#########################################################################################################
' Dyn_Wstring.bi version: 1.04 2015-November-28 original v1.00 2015/Nov/09
' by Marc Pons marpon@aliceadsl.fr
' it's free to use and modify, but please do not remove that header.
'#########################################################################################################
' evolutions : from 1.00
' 1.01 added constructors to have corrected direct assignment and with implicit converted string , wstring ptr
' 1.01 corrected bug in u_Instr function to work on reverse ustring order with negative start
' 1.02 some cosmetic cleaning : ustringw functions : use now 'function = xx' , not 'return xx'
' 1.02 modified parameter order in u_string(count , code), added more functions : case & Trim collection
' 1.03 use as much as possible subs to not create frequent memory re-allocation
' 1.03 new constructors ; operators and destructor
' 1.04 minor bugs corrected, on surrogate management
'#########################################################################################################

#Ifndef SET_USTRINGW_DYN
   #Define SET_USTRINGW_DYN
   
   #ifndef __U_CLEAN_MEM__                       ' to "free" the remaining allocated memory when program ends
      #define __U_CLEAN_MEM__                    ' if no used you can reduce around 2048 bytes on your executable
   #endif                                        ' to not compile, simply comment the define
   
   dim shared UBYTE_STRING() As ubyte            ' to work with ubyte info
   dim shared UTF32LE() as ulong                 ' to work with ulong info in utf32 coding
   
   'indirect dim just to group here with the other shared variables
   #Define SET_DWSTRING_LIST Dim shared DWSTRING_LIST_GLOB As clsKeyList
   
   
   #Define SLEEP_TIME 15000                      ' change here : wait time to exit in verbose mode
   
   #ifndef __U_WIN_OS__                          ' Windows found by the include file
      #Ifdef __windows_bi__                      ' for compatibility with old FBC versions
         #Define _INC_WINDOWS
      #ENDIF
      #ifdef _INC_WINDOWS
         #define __U_WIN_OS__
      #endif
      
      #ifndef __U_WIN_OS__                       ' Windows found by the OS define
         #ifdef __FB_WIN32__
            #define __U_WIN_OS__
         #endif
         #ifdef __FB_WIN64__                     ' probably not existing yet ? but better to be prepared to it
            #define __U_WIN_OS__
         #endif
      #Endif
      
      #ifndef __U_WIN_OS__                       ' if not Windows Wstring use 4 bytes
         #define __U_NOT_WIN_OS__
         #Print
         #Print "===		"
         #Print "===		WARNING  : be aware "
         #Print "===		uStringW type "
         #Print "===		is equivalent to UTF32 internally"
         #Print "===		"
         #Print
      #Else                                      ' if Windows Wstring use 2 bytes
         #Print
         #Print "===		"
         #Print "===		WARNING  : be aware "
         #Print "===		uStringW type "
         #Print "===		is equivalent to UTF16 internally"
         #Print "===		"
         #Print
      #ENDIF
   #ENDIF
   
   #if __FB_DEBUG__ <> 0
      #print Debug mode
      #define __VERBOSE_MODE__                   'show allocate mem information during debugg execution
   #else
      '#define __VERBOSE_MODE__ ' uncomment if needed allocate mem information in normal execution
   #endif
   
   #ifdef __VERBOSE_MODE__
      const TO_TEST_INFO = 1                     ' to show Alloc/Free information
   #else
      const TO_TEST_INFO = 0                     ' to hide Alloc/Free information
   #endif
   
   
   #define UTF16_MAX_BMP &h0000FFFF
   #define UTF16_SUR_HIGH_START &hD800
   #define UTF16_SUR_HIGH_END &hDBFF
   #define UTF16_SUR_LOW_START &hDC00
   #define UTF16_SUR_LOW_END &hDFFF
   #define LEAD_OFFSET (&hD800 - (&h10000 shr 10))
   #define SURROGATE_OFFSET ( &h10000 - (&hD800 shl 10) - &hDC00)
   
   #define IS_HIGH_UTF16_SUR(wch) (((wch) >= UTF16_SUR_HIGH_START) andalso ((wch) <= UTF16_SUR_HIGH_END))
   #define IS_LOW_UTF16_SUR(wch) (((wch) >= UTF16_SUR_LOW_START) andalso ((wch) <= UTF16_SUR_LOW_END))
   #define IS_UTF16_PAIR(hs, ls) (IS_HIGH_UTF16_SUR(hs) andalso IS_LOW_UTF16_SUR(ls))
   
   #define U32_SWAP(c) (((c) shr 24) or (((c) shl 8) and &h00FF0000) or _
         (((c) shr 8) and &h0000FF00) or ((c) shl 24))
   
   
   'generic unicode variable length type string using wide chars to hold the content
   'in windows 2 bytes/wstring so , internally the coding use UTF16 coding on 1 or 2 ushort
   'on linux 4 bytes/wstring "      " "       " using UTF32 coding on 1 ulong
   
   type uStringW
         data1                     as Wstring ptr' Data wstring (in Utf16 in windows or Utf32 in linux )
         len1                      as long       ' lenght in wstring (not bytes) including surrogate parts in win
         size1                     as long       ' size in bytes of allocated memory
         surrogate                 as long       ' Number of surrogate in the data1 wstring needed for windows
         
         Declare Operator Cast() as Wstring ptr
         Declare Operator Cast() as string
         Declare Operator &= (ByRef ust2 as uStringW)
         Declare Operator += (ByRef ust2 as uStringW)
         Declare Operator &= (ByVal wst2 as wstring ptr)
         Declare Operator += (ByVal wst2 as wstring ptr)
         Declare Operator &= (ByRef st2 as string)
         Declare Operator += (ByRef st2 as string)
         Declare operator &= (ByVal it2 as longint) 'new in v1.03
         Declare operator &= (ByVal dt2 as double) 'new in v1.03
         Declare Operator let(ByRef st2 as string)
         Declare Operator let(ByVal wst2 as wstring ptr)
         Declare Operator let(ByRef ust2 as uStringW) 'new in v1.03
         Declare Constructor(ByRef ust2 as uStringW) 'new in v1.01
         Declare Constructor(ByVal wst2 as Wstring ptr) 'new in v1.01
         Declare Constructor(ByRef st2 as string)'new in v1.01
         Declare Constructor()                   'new in v1.01
         Declare Destructor()                    'new in v1.03
   end type
   
   
   
   #ifdef __U_CLEAN_MEM__
      ' just to store/unstore the Allocate/ReAllocate/DeAllocate memory info, in order to have clean environment
      'can be deleted without big risk, to reduce the executable size , the program will free the memory at the end
      
      dim shared M_ARRAYLIST() As string         ' to store adress of the allocated memory
      dim shared M_ARRAYTEMP() As string         ' to store adress of the temp allocated memory
      
      Type clsKeyList
			Private:
            m_nGrowBy              As long       ' how big to grow the list by when needed
            m_nCount               As long       ' current number of elements in the list
            m_nCurrent             As long       ' current position in the list
            
            Declare Function _CreateNode() As long
            Declare Function _DeleteNode(ByVal nIndex As long) As long
			Public:
            m_temp                 As long = 0   'to know if list is temp or not
            Declare Function Store(ByRef sKey As Const String) As long
            Declare Function GetByIndex(ByVal nIndex As long) As long
            Declare Function GetByKey(ByRef sKey As Const String) As long
            Declare Function ClearList(byval flag as long = 0) As long
            Declare Function DeleteByIndex(ByVal nIndex As long) As long
            Declare Function DeleteByKey(ByRef sKey As Const String) As long
            Declare Function GetKeyString(ByVal nIndex As long = - 1) As String
            Declare sub FreeAll(byval flag as long = 0)
            Declare Property GrowBy(ByVal nValue As long)
            Declare Property GrowBy() As long
            Declare Property Count() As long
            Declare Constructor(ByVal nInitalGrowBy As long = - 1)
            Declare Destructor
      End Type
      ''
      ''
      SET_DWSTRING_LIST                          ' to dim shared the clsKeyList type
      
      
      Constructor clsKeyList(ByVal nInitalGrowBy As long = - 1)
         m_nGrowBy = Iif(nInitalGrowBy = - 1 , 20 , nInitalGrowBy)
         m_nCount = 0
      End Constructor
      ''
      ''
      Destructor clsKeyList
         this.ClearList(TO_TEST_INFO)
      End Destructor
      ''
      ''
      Property clsKeyList.GrowBy(ByVal nValue As long)
         If nValue <= 0 Then Exit Property
         this.m_nGrowBy = nValue
      End Property
      ''
      ''
      Property clsKeyList.GrowBy() As long
         Property = this.m_nGrowBy
      End Property
      ''
      ''
      Property clsKeyList.Count() As long
         Property = this.m_nCount
      End Property
      ''
      ''
      Private Function clsKeyList.ClearList(byval flag as long = 0) As long
         This.FreeAll(flag)
         Erase M_ARRAYLIST
         Erase UBYTE_STRING
         m_nCount = 0
         Function = 1
      End Function
      ''
      ''
      Private Function clsKeyList._CreateNode() As long
         Dim ub                As long
         ub = Ubound(M_ARRAYLIST)
         m_nCount = m_nCount + 1
         ' Determine if the lists need to be grown in order to accomodate the new node.
         If m_nCount > ub Then ReDim Preserve M_ARRAYLIST(ub + m_nGrowBy) As string
         Return m_nCount - 1                     ' zero based position in array
      End Function
      ''
      ''
      Private Function clsKeyList.Store(ByRef sKey As Const String) As long
         ' If key already exists in this list then simply update the
         ' node with the new data, otherwise create the new node with
         ' data at the end of the list.
         If this.GetByKey(sKey) Then
            ' m_nCurrent is already set in GetByKey if found successfully
         Else
            m_nCurrent = this._CreateNode()
         End If
         M_ARRAYLIST(m_nCurrent) = sKey
         Return 1
      End Function
      ''
      ''
      Private Function clsKeyList.GetByIndex(ByVal nIndex As long) As long
         If (nIndex >= 0) And (nIndex < m_nCount) Then
            m_nCurrent = nIndex
            Return 1
         End If
         Return 0
      End function
      ''
      ''
      Private Function clsKeyList.GetByKey(ByRef sKey As Const String) As long
         For i As long = LBound(M_ARRAYLIST) To Ubound(M_ARRAYLIST)
            If M_ARRAYLIST(i) = sKey Then
               m_nCurrent = i
               Return 1
            End If
         Next
         Return 0
      End Function
      ''
      ''
      Private Function clsKeyList.GetKeyString(ByVal nIndex As long = - 1) As String
         If nIndex <> - 1 Then this.GetByIndex(nIndex)
         If (m_nCount > 0) And (m_nCurrent >= LBound(M_ARRAYLIST)) And _
               (m_nCurrent <= Ubound(M_ARRAYLIST)) Then
            Return M_ARRAYLIST(m_nCurrent)
         End If
      End Function
      ''
      ''
      Private Function clsKeyList._DeleteNode(ByVal nIndex As long) As long
         ' Private function that compresses the array
         For i As long = nIndex To Ubound(M_ARRAYLIST) - 1
            M_ARRAYLIST(i) = M_ARRAYLIST(i + 1)
         Next
         m_nCount = m_nCount - 1
         Return 1
      End Function
      ''
      ''
      Private Function clsKeyList.DeleteByIndex(ByVal nIndex As long) As long
         If this.GetByIndex(nIndex) Then
            this._DeleteNode(nIndex)
            Return 1
         End If
         Return 0
      End Function
      ''
      ''
      Private Function clsKeyList.DeleteByKey(ByRef sKey As Const String) As long
         If this.GetByKey(sKey) Then
            this._DeleteNode(m_nCurrent)
            Return 1
         End If
         Return 0
      End Function
      ''
      ''
      Private sub clsKeyList.FreeAll(byval flag as long = 0)
         dim         as long ci
         dim as string sdata1 , collect = ""
         dim         as wstring ptr p
         
         For ci = 0 To This.Count - 1
            If this.GetByIndex(ci) Then
               sdata1 = this.GetKeyString(ci)
               p = cast(wstring ptr , cint(sdata1))
               #ifdef __VERBOSE_MODE__
                  collect &= sdata1 & chr(10)
                  if flag = 1 then print "Index:" ; ci , "Key: " ; sdata1 , *p
               #endif
               Deallocate(p)
               p = 0
            End If
         Next
         #ifdef __VERBOSE_MODE__
            if flag = 1 then
               print : print : print "====   Press a key to exit  USTRINGW_DYN  Verbose Mode   ===="
               sleep SLEEP_TIME
            end if
         #endif
      end sub
      
   #Endif                                        '__U_CLEAN_MEM__
   
   
   ''::::: internal sub to manage allocated mem
   private sub hRealloc(byval s as uStringW ptr , _
            byval chars as long , _
            byval dopreserve as long)
      dim         as long newsize
      dim         as long oldlen
      dim         as long newsize2
      dim         as any ptr p
      dim         as string sp2
      dim         as long sur
      
      if chars = 0 then
         newsize = 0                             'just initialized
      else
         newsize = (chars + 15) and not 15       ' alloc every 16-chars remember chars are 2 or 4 bytes
      end if
      newsize2 = newsize * len(wstring)
      if chars = 0 and (s -> data1 <> 0) and s -> len1 = 0 THEN exit sub
      if ((s -> data1 = 0) or (s -> data1 <> 0 and newsize2 <> s -> size1)) then
         if (dopreserve = 0) then
            sur = 0
            if (s -> data1 = 0) then
               sp2 = ""
               s -> data1 = allocate(newsize2 + len(wstring))
               if (s -> data1 = 0) then          '' failed? try again
                  s -> data1 = allocate(newsize2 + len(wstring))
                  if (s -> data1 = 0) then error(4)
               end if
            else
               p = s -> data1
               sp2 = str(p)
               s -> data1 = reallocate(p , newsize2 + len(wstring))
               if (s -> data1 = 0) then          '' failed? try again
                  s -> data1 = reallocate(p , newsize2 + len(wstring))
                  if (s -> data1 = 0) then error(4)
               end if
            end if
            if s -> data1 <> 0 then
               Clear(*(s -> data1) , 0 , newsize2 + len(wstring)) 'put 0 in all bytes
               #ifdef __U_CLEAN_MEM__
                  '**** unstore the previous pointer
                  if sp2 <> "" then DWSTRING_LIST_GLOB.DeleteByKey(sp2)
                  '**** store the pointer to be able to deallocate
                  DWSTRING_LIST_GLOB.Store(str(s -> data1))
               #endif
               #ifdef __VERBOSE_MODE__
                  if TO_TEST_INFO = 1 and sp2 <> "" then
                     print "Free_N  " ; sp2
                     print "ReAlloc_N  " ; s -> data1 , newsize2 + len(wstring)
                  elseif TO_TEST_INFO = 1 then
                     print "Alloc_New  " ; s -> data1 , newsize2 + len(wstring)
                  end if
               #Endif
            end if
         else                                    '' preserve..
            p = s -> data1
            sp2 = str(p)
            oldlen = s -> len1
            sur = s -> surrogate
            s -> data1 = reallocate(p , newsize2 + len(wstring))
            '' failed? try again
            if (s -> data1 = 0) then
               s -> data1 = reallocate(p , newsize2 + len(wstring))
               if (s -> data1 = 0) then error(4)
            end if
            if s -> data1 <> 0 then
               Clear(*(s -> data1 + chars) , 0 , newsize2 + len(wstring) - chars * len(wstring)) 'put 0 in all remaining bytes
               #ifdef __U_CLEAN_MEM__
                  '**** unstore the previous pointer
                  DWSTRING_LIST_GLOB.DeleteByKey(sp2)
                  '**** store the pointer to be able to deallocate
                  DWSTRING_LIST_GLOB.Store(str(s -> data1))
               #endif
               #ifdef __VERBOSE_MODE__
                  if TO_TEST_INFO = 1 then print "Free_P  " ; sp2
                  if TO_TEST_INFO = 1 then print "ReAlloc_P  " ; s -> data1 , newsize2 + len(wstring)
               #endif
            end if
         end if
         s -> size1 = newsize2
      else
         Clear(*(s -> data1 + chars) , 0 , newsize2 + len(wstring) - chars * len(wstring)) 'put 0 in all remaining bytes
      end if
      s -> len1 = chars
      if len(wstring) = 4 then
         s -> surrogate = 0
      else
         if chars = 0 THEN
            s -> surrogate = 0
         else
            s -> surrogate = sur
         end if
      end if
   end sub
   
   private sub u_reset(byref uret as ustringW)
      hRealloc(@uret , 0 , 0)
   end sub
   
   private Function u_Empty() as uStringW
      dim dst               as uStringW
      hRealloc(@dst , 0 , 0)
      return dst
   End function
   
   ''::::: to force free/clean an uStringW
   private sub kill_uStringW(byref dst as uStringW)
      if dst.data1 <> 0 THEN
         #ifdef __U_CLEAN_MEM__
            '**** unstore the pointer
            DWSTRING_LIST_GLOB.DeleteByKey(str(dst.data1))
            #ifdef __VERBOSE_MODE__
               print "FreeW " ; dst.data1 , *dst.data1 'v1.02
            #endif
         #endif
         clear(*(dst.data1) , 0 , dst.size1 + len(wstring))
         Deallocate(dst.data1)
      END IF
      dst.data1 = 0
      dst.len1 = 0
      dst.size1 = 0
      dst.surrogate = 0
   end sub
   
   ''::::: v1.02 to force free/clean an uStringW array
   private sub u_Erase(uArray() as uStringW)
      for ipos as long = lbound(uArray) to ubound(uArray)
         kill_uStringW(uArray(ipos))
      NEXT
   end sub
   
   ''::::: external to count surrogate elements in uStringW ( needed for windows only)
   private function u_surX(byref src as uStringW) as long
      dim         as long x
      dim         as long y
      dim         as long z
      dim         as ushort us1
      if len(wstring) = 4 or src.len1 = 0 THEN return 0
      z = src.len1
      y = 0 : x = 0
      Do While x < z
         x += 1
         if IS_HIGH_UTF16_SUR(src.data1[x - 1]) then 'asc(src.data1[x-1])) then
            if x > z - 1 THEN return x * - 1     'return negative position to show where error
            x += 1
            if IS_LOW_UTF16_SUR(src.data1[x - 1]) then 'asc(src.data1[x-1])) then
               y += 1
            else
               return x * - 1                    'return negative position to show where error
            end if
         END IF
      LOOP
      function = y
   end function
   
   private function u_NextSurr(byref src as uStringW , istart as long = 1) as long
      dim         as long x
      dim         as long y
      dim         as long z
      dim         as ushort us1
      
      if len(wstring) = 4 or src.len1 = 0 or istart > src.len1 or istart < 1 or src.surrogate = 0 THEN return 0
      z = len(src.len1)
      y = 0 : x = istart - 1
      Do While x < z
         x += 1
         if IS_HIGH_UTF16_SUR(src.data1[x - 1]) then 'asc(src.data1[x-1])) then
            if x > z - 1 THEN return x * - 1     'return negative position to show where error
            x += 1
            if IS_LOW_UTF16_SUR(src.data1[x - 1]) then 'asc(src.data1[x-1])) then
               function = x - 1
            else
               return x * - 1                    'return negative position to show where error
            end if
         END IF
      LOOP
      function = 0
   end function
   
   private function u_SurrCount(byref src as uStringW) as long
      return src.surrogate
   end function
   
   ''::::: internal sub , because risk with surrogate story
   private sub wReplaceCharA(byref ini as uStringW , byval old as ulong , byval us1 as ulong)
      dim p                 as long
      
      p = 0
      do
         p = instr(p + 1 , *ini.data1 , *cast(wstring ptr , varptr(old)))
         if p = 0 then exit sub
         *(ini.data1 + p - 1) = us1
      loop
      if len(wstring) = 4 then
         ini.surrogate = 0
      else
         ini.surrogate = u_surX(ini)
      end if
   end sub
   
   ''::::: internal sub
   private sub DWstrAssign(byref dst as uStringW , byval src as Wstring ptr , _
            byval itemp as long = 0)
      dim         as long src_len
      
      if src = 0 THEN
         src_len = 0
      else
         src_len = len(*src)
      END IF
      if (src_len = 0) then
         hRealloc(@dst , 0 , 0)
         exit sub
      end if
      hRealloc(@dst , src_len , 0)
      if (dst.data1 <> 0) then
         *dst.data1 = *src
         '#if __FB_VERSION__ < "0.90.0"
         wReplaceCharA(dst , 128 , 8364)         ' trap for bad coding € ,at least on western europe countries
         '#endif
         if len(wstring) = 4 then
            dst.surrogate = 0
         else
            dst.surrogate = u_surX(dst)
         end if
      end if
   end sub
   
   ''::::: external function to input wstring into uString ( accept escape sequence also like !"\uXXYY")
   private function u_Wstr(byval src as Wstring ptr) as uStringW
      dim dst               as uStringW
      
      DWstrAssign(dst , src)
      function = dst
   end function
   
   ''::::: internal sub to duplicate uStringW
   private sub DWstrDup(byref dst as uStringW , byref src as uStringW)
      hRealloc(@dst , src.len1 , 0)
      if (dst.data1 <> 0) then
         *dst.data1 = *src.data1
         dst.surrogate = src.surrogate
      end if
   end sub
   
   ''::::: internal sub to Concat&Assign uStringW
   private sub DWstrConcatAssign(byref dst as uStringW , byval src as Wstring ptr)
      dim dst_len           as long
      dim src_len           as long
      
      if src = 0 THEN
         src_len = 0
      else
         src_len = len(*src)
      END IF
      if dst.data1 <> 0 THEN
         if src_len = 0 THEN exit sub
         dst_len = dst.len1
         hRealloc(@dst , dst_len + src_len , 1)
         if (dst.data1 <> 0) then
            *(dst.data1 + dst_len) = *src
            dst.surrogate = u_surX(dst)
         end if
      else
         dst = u_Wstr(src)
      end if
   end sub
   
   ''::::: internal sub to Concat&Assign uStringW with existing one
   private sub uConcatAssign(byref dst as uStringW , byref src as uStringW)
      dim dst_len           as long
      dim src_len           as long
      
      src_len = src.len1
      if src_len = 0 THEN exit sub
      if dst.data1 <> 0 THEN
         dst_len = dst.len1
         hRealloc(@dst , dst_len + src_len , 1)
         if (dst.data1 <> 0) then
            *(dst.data1 + dst_len) = *src.data1
            dst.surrogate += src.surrogate
         end if
      end if
   end sub
   
   ''::::: external function to concat 2 uStringW into new uStringW
   private function u_Concat(byref dst as uStringW , byref src as uStringW) as uStringW
      dim as wstring ptr pw1 = src.data1
      dim retu              as uStringW
      DWstrDup(retu , dst)                       'DWstrDup(dst)'modified in v1.02 not needed to duplicate
      DWstrConcatAssign(retu , pw1)
      function = retu
   end function
   
   ''::::: internal function to replace/remove substrings
   private function wReplace(byref ini as uStringW , _
            byref oldtext as uStringW , _
            byref newtext as uStringW _
            ) as uStringW
      dim oldlen            as long
      dim newlen            as long
      dim p                 as long
      dim remtext           as uStringW
      
      DWstrDup(remtext , ini)
      oldlen = oldtext.len1
      newlen = newtext.len1
      if oldlen = 0 or ini.len1 = 0 or oldlen > ini.len1 THEN
         function = remtext
         exit function
      end if
      
      dim text              as uStringW
      
      do
         p = instr(1 , *remtext.data1 , *oldtext.data1)
         if (p = 0) then
            DWstrConcatAssign(text , remtext.data1)
            exit do
         end if
         DWstrConcatAssign(text , left(*remtext.data1 , p - 1))
         if newlen > 0 then DWstrConcatAssign(text , newtext.data1)
         DWstrAssign(remtext , mid(*remtext.data1 , p + oldlen))
      loop
      function = text
   end function
   
   ''::::: external function can make : char or substring Replace or Remove win or linux ok
   private Function u_Replace(byref ini as uStringW , byref oldtext as uStringW , byref newtext as uStringW) as uStringW
      dim p                 as long
      dim dst               as uStringW
      
      if ini.len1 = 0 THEN
         function = dst
         exit function
      END IF
      if oldtext.len1 = 0 THEN
         DWstrDup(dst , ini)
         function = dst
         exit function
      END IF
      if oldtext.len1 > 0 and newtext.len1 <> 1 THEN
         dst = wReplace(ini , oldtext , newtext)
         function = dst
         exit function
      end if
      DWstrDup(dst , ini)
      wReplaceCharA(dst , asc(*oldtext.data1) , asc(*newtext.data1))
      function = dst
   end Function
   
   'internal function
   private function surrogate_byt(tmp as string , flag16 as long = 16) as string
      dim         as ulong c
      dim         as ushort u1
      dim         as ushort u2
      dim as string stemp = ""
      
      u1 = asc(mid(tmp , 1 , 1)) + asc(mid(tmp , 2 , 1)) *&h100
      u2 = asc(mid(tmp , 3 , 1)) + asc(mid(tmp , 4 , 1)) *&h100
      
      if flag16 = 16 THEN
         c = u1 + u2 * &h10000
         if c > &h10FFFF THEN return ""
         if c < &h10000 THEN
            stemp = mid(tmp , 1 , 1) & mid(tmp , 2 , 1) & chr(0) & chr(0)
         else
            u1 = LEAD_OFFSET + (c shr 10)
            stemp = chr(HiByte(u1)) & chr(LoByte(u1))
            u2 = &hDC00 + (c and &h3FF)
            stemp &= chr(HiByte(u1)) & chr(LoByte(u2))
         END IF
      else
         if (u1 >= UTF16_SUR_HIGH_START) and (u1 <= UTF16_SUR_HIGH_END) then
            c = ((u1 - &HD800) *&H400) + (u2 - &HDC00) + &H10000
            stemp = chr(lobyte(loword(c))) & chr(hibyte(loword(c))) & chr(lobyte(Hiword(c))) & chr(hibyte(hiword(c)))
         else
            stemp = tmp
         end if
      END IF
      function = stemp
   end function
   
   ' ''::::: internal converts char > FFFF to 1 surrogate pair
   ' function coding(U1 as Uinteger) as string
   ' dim hi as Ulong
   ' dim lo as Ulong
   ' if (U1 >= &H10000 and U1 <= &H10FFFF) then
   ' hi = ((U1 - &H10000) / &H400) + &HD800
   ' lo = ((U1 - &H10000) mod &H400) + &HDC00
   ' end if
   ' print "U1 = " ; hex(U1)
   ' print "hi = " ; hex(hi) , "lo = " ; hex(lo)
   ' function = hex(hi) & " _ " & hex(lo)
   ' END FUNCTION
   
   ' ''::::: internal converts 1 surrogate pair to char > FFFF
   ' function decoding(hi as Uinteger , lo as Uinteger) as string
   ' dim S as Uinteger
   ' if hi >= &HD800 and hi <= &HDBFF and lo >= &HDC00 and lo <= &HDFFF then
   ' s = ((hi - &HD800) *&H400) + (lo - &HDC00) + &H10000
   ' end if
   
   ' print "hi = " ; hex(hi) , "lo = " ; hex(lo)
   ' print "S = " ; hex(S)
   ' return hex(s)
   ' return hex(s)
   ' end function
   
   
   ''::::: internal function to code uStringW from string definition ok for win or linux
   ' can accept \uXXYY 4 hex digits coding or added \wZZXXYY 6 hex digits coding
   private function Ucode_str_A(src as string , byref ilon2 as long , byref isur as long) as any ptr
      dim ilen as long = len(src)
      dim isize as long = 0
      dim idim as long = 8 * len(wstring)
      dim ityp as long = len(wstring) - 2        'to complete the extra bytes
      dim inot              as long
      dim c                 as ulong
      dim u1                as Ushort
      dim tmp4              as string
      redim as ubyte UBYTE_STRING(0 to idim - 1)
      dim tmp               as string
      dim model as string = "0123456789ABCDEF"
      dim x as long = 1
      
      do while(x < ilen + 1)
         if ucase(mid(src , x , 2)) = "\U" and x < ilen - 4 THEN ' pseudo escape sequence as normal \uYYYY
            tmp = ucase(mid(src , x + 2 , 4))
            for y as long = 1 to 4
               if instr(model , mid(tmp , y , 1)) = 0 THEN tmp = "003F" 'not understood force ?"
            NEXT
            isize = isize + len(wstring)
            if isize > idim - len(wstring) then
               idim = idim * 2
               redim preserve as ubyte UBYTE_STRING(0 to idim - 1)
            end if
            ' ok with windows surrogate pairs , but not with linux wich do not need 2 wstrings for that
            ' must be treated as exeption my proposal is to add a pseudo escape sequence \wZZYYXX
            ' but if input as surrogate has to be counted here and converted t for linux
            c = valint( "&H" & tmp)
            'dim as string cod16 = coding (c) ' to verify
            if IS_HIGH_UTF16_SUR(c) then         'surrogate pair?
               if x + 11 > ilen or mid(src , x + 6 , 2) <> "\U" THEN exit do
               tmp4 = ucase(mid(src , x + 8 , 4))
               u1 = valint( "&H" & tmp4)
               if IS_LOW_UTF16_SUR(u1) then
                  if len(wstring) = 2 THEN
                     isur += 1
                  else
                     c = ((c - &HD800) *&H400) + (u1 - &HDC00) + &H10000
                     UBYTE_STRING(isize - 4 - ityp) = lobyte(loword(c))
                     UBYTE_STRING(isize - 3 - ityp) = hibyte(loword(c))
                     UBYTE_STRING(isize - 2 - ityp) = lobyte(Hiword(c))
                     UBYTE_STRING(isize - 1 - ityp) = hibyte(hiword(c))
                     x += 6
                     goto escape1
                  end if
               else
                  exit do
               END IF
            end if
            UBYTE_STRING(isize - 1 - ityp) = valint( "&H" & left(tmp , 2))
            UBYTE_STRING(isize - 2 - ityp) = valint( "&H" & right(tmp , 2))
            if ityp > 0 THEN                     ' only if len(wstring) =4
               UBYTE_STRING(isize - 1) = 0
               UBYTE_STRING(isize - 2) = 0
            END IF
escape1:
            x += 6
         elseif ucase(mid(src , x , 2)) = "\W" and x < ilen - 6 THEN ' new pseudo escape sequence as \wZYYXX
            ' print:print "Ucode_str_A new sequ \wZZYYXX ": print
            ' to be able to get the unicode > 2 bytes codes directly
            tmp = ucase(mid(src , x + 2 , 6))
            inot = 0
            for y as long = 1 to 6
               if instr(model , mid(tmp , y , 1)) = 0 THEN
                  tmp = "003F"                   'not understood force ?
                  inot = 1
                  isize = isize + len(wstring)
                  if isize > idim - len(wstring) then
                     idim = idim * 2
                     redim preserve as ubyte UBYTE_STRING(0 to idim - 1)
                  end if
                  UBYTE_STRING(isize - 1 - ityp) = valint( "&H" & left(tmp , 2))
                  UBYTE_STRING(isize - 2 - ityp) = valint( "&H" & right(tmp , 2))
                  if ityp > 0 THEN               ' only if len(wstring) =4
                     UBYTE_STRING(isize - 1) = 0
                     UBYTE_STRING(isize - 2) = 0
                  END IF
                  exit for
               end if
            NEXT
            if inot = 0 THEN
               ' 2 cases win coded as utf16LE or linux coded as utf32LE
               if ityp > 0 THEN                  ' linux case
                  isize = isize + len(wstring)
                  if isize > idim - len(wstring) then
                     idim = idim * 2
                     redim preserve as ubyte UBYTE_STRING(0 to idim - 1)
                  end if
                  UBYTE_STRING(isize - 3) = valint( "&H" & mid(tmp , 2 , 2))
                  UBYTE_STRING(isize - 4) = valint( "&H" & right(tmp , 2))
                  UBYTE_STRING(isize - 2) = valint( "&H" & left(tmp , 2))
                  UBYTE_STRING(isize - 1) = 0
               Else
                  c = valint( "&H" & tmp)
                  'dim as string cod16 = coding (c) ' to verify
                  if (c > UTF16_MAX_BMP) then    'surrogate pair
                     isize = isize + len(wstring) *2
                     if isize > idim - len(wstring) *2 then
                        idim = idim * 2
                        redim preserve as ubyte UBYTE_STRING(0 to idim - 1)
                     end if
                     u1 = LEAD_OFFSET + (c shr 10)
                     UBYTE_STRING(isize - 3) = HiByte(u1)
                     UBYTE_STRING(isize - 4) = LoByte(u1)
                     'print "first : " ; hex(HiByte(u1)),hex(LoByte(u1))
                     u1 = &hDC00 + (c and &h3FF)
                     UBYTE_STRING(isize - 1) = HiByte(u1)
                     UBYTE_STRING(isize - 2) = LoByte(u1)
                     isur += 1
                     'print "second : " ; hex(HiByte(u1)),hex(LoByte(u1))
                  else                           ' correction
                     tmp = ucase(mid(src , x + 4 , 4))
                     isize = isize + len(wstring)
                     if isize > idim - len(wstring) then
                        idim = idim * 2
                        redim preserve as ubyte UBYTE_STRING(0 to idim - 1)
                     end if
                     UBYTE_STRING(isize - 1) = valint( "&H" & left(tmp , 2))
                     UBYTE_STRING(isize - 2) = valint( "&H" & right(tmp , 2))
                  end if
               end if
            END IF
            x += 8
         else
            isize = isize + len(wstring)
            if isize > idim - len(wstring) then
               idim = idim * 2
               redim preserve as ubyte UBYTE_STRING(0 to idim - 1)
            end if
            
            UBYTE_STRING(isize - 2 - ityp) = asc(mid(src , x , 1))
            UBYTE_STRING(isize - 1 - ityp) = 0
            
            if ityp > 0 THEN                     ' only if len(wstring) =4
               UBYTE_STRING(isize - 1) = 0
               UBYTE_STRING(isize - 2) = 0
            END IF
            '#if __FB_VERSION__ < "0.90.0" ' Ç€ü¼¬¼
            if UBYTE_STRING(isize - 2 - ityp) = 128 and UBYTE_STRING(isize - 1 - ityp) = 0 THEN ' correct €
               UBYTE_STRING(isize - 2 - ityp) = 172
               UBYTE_STRING(isize - 1 - ityp) = 32
            END IF
            '#endif
            x += 1
         END IF
      LOOP
      isize += len(wstring)
      redim preserve as ubyte UBYTE_STRING(0 to isize - 1)
      UBYTE_STRING(isize - 1 - ityp) = 0
      UBYTE_STRING(isize - 2 - ityp) = 0
      if ityp > 0 THEN                           ' only if len(wstring) =4
         UBYTE_STRING(isize - 1) = 0
         UBYTE_STRING(isize - 2) = 0
      END IF
      ilon2 = (isize - len(wstring)) / len(wstring)
      return cast(any ptr , varptr(UBYTE_STRING(0)))
   end function
   
   
   private sub u_From_CodeString overload(st1 as string , byref remtext as uStringW)
      if (len(st1) = 0) then
         hRealloc(@remtext , 0 , 0)
         exit sub
      end if
      dim as long isur = 0
      dim as long ilen = 0
      dim as wstring ptr wtem = Ucode_str_A(st1 , ilen , isur)
      'print "ilen : ";ilen,*wtem,"st1",st1,len(st1)
      hRealloc(@remtext , ilen , 0)
      if (remtext.data1 <> 0) then
         *remtext.data1 = *wtem                  '& chr(0) & chr(0)
         if len(wstring) = 4 then
            remtext.surrogate = 0
         else
            remtext.surrogate = isur
         end if
      end if
   end sub
   
   ''::::: external function to code uStringW from string definition, can accept escape sequences without !"
   ' use \uXXYY 4 hex digits coding or added \wZZXXYY 6 hex digits coding not less then 6 digits
   ' win or linux ok
   private function u_From_CodeString overload(st1 as string) as uStringW
      dim remtext           as uStringW
      
      u_From_CodeString(st1 , remtext)
      function = remtext
   end function
   
   
   
   ''::::: external function win or linux ok
   private Function u_ReplaceChar(byref ini as uStringW , byval old as uinteger , byval us1 as uinteger) as uStringW
      dim p                 as long
      dim hi                as Uinteger
      dim lo                as Uinteger
      dim stemp             as string
      dim         as string s1
      dim         as string s2
      dim dst               as uStringW
      
      if ini.len1 = 0 THEN function = dst
      
      dim so1               as uStringW
      dim sn1               as uStringW
      
      DWstrDup(dst , ini)
      if old > &H10FFFF or us1 > &H10FFFF THEN
         function = dst
         exit function
      end if
      if (old >= &H10000 and old <= &H10FFFF) THEN
         s1 = hex(old , 6)
         stemp = "\w" & s1
      else
         s1 = hex(old , 4)
         stemp = "\u" & s1
      end if
      u_From_CodeString(stemp , so1)
      if (us1 >= &H10000 and us1 <= &H10FFFF) THEN
         s2 = hex(us1 , 6)
         stemp = "\w" & s2
      else
         s2 = hex(us1 , 4)
         stemp = "\u" & s2
      end if
      u_From_CodeString(stemp , sn1)
      
      dst = u_Replace(ini , so1 , sn1)
      function = dst
   end function
   
   ''::::: carrefull in windows (utf16) counts also surrogate parts ( adding 1 wstring ) its more chain size
   private function u_Wlen_simple(ByRef src as Const uStringW) as long
      if src.data1 = 0 THEN
         Function = 0
      else
         Function = src.len1
      END IF
   end function
   
   private Function u_String(icount as ulong , ucode as uinteger = 0) as uStringW ' v1.02 modified parameter order & more
      dim         as string stemp
      dim         as string s1
      dim         as ulong ulen
      dim         as ulong u1
      dim udest             as uStringW
      
      if ucode > &H10FFFF or icount < 1 THEN
         function = udest
         exit function
      end if
      if ucode = 0 then
         function = udest
         exit function
      elseif (ucode >= &H10000 and ucode <= &H10FFFF) THEN
         s1 = hex(ucode , 6)
         stemp = "\w" & s1
      else
         s1 = hex(ucode , 4)
         stemp = "\u" & s1
      end if
      dim uNew1             as uStringW
      u_From_CodeString(stemp , uNew1)
      ulen = uNew1.len1
      hRealloc(@udest , ulen * icount , 0)
      for x as long = 0 to(icount - 1) *ulen step ulen
         u1 = uNew1.data1[0]
         udest.data1[x] = u1
         if ulen = 2 THEN
            u1 = uNew1.data1[1]
            udest.data1[x + 1] = u1
         end if
      NEXT
      function = udest
   End function
   
   private function u_Space(icount as ulong = 1) as uStringW
      dim as uStringW dst = u_String(icount , 32)
      
      function = dst
   end function
   
   
   private Function u_reserv(icount as ulong) as uStringW ' v1.02 new
      dim udest             as uStringW
      if icount < 1 THEN
         'hRealloc(@udest , 0 , 0)
      else
         hRealloc(@udest , icount , 0)
      end if
      function = udest
   End function
   
   
   private function u_Equal(ByRef src as Const uStringW , ByRef src2 as Const uStringW) as long
      if src.data1 = 0 and src2.data1 = 0 THEN return 1
      if src.len1 <> src2.len1 or src.data1 = 0 or src2.data1 = 0 THEN return 0
      if instr(*(src.data1) , *(src2.data1)) = 1 and _
            len(*(src.data1)) = len(*(src2.data1)) THEN return 1
   end function
   
   
   
   private function u_W_U16Start(ByRef src as Const uStringW , posi as long) as long
      dim         as long x
      dim         as long y
      dim         as ushort us1
      dim         as ushort us2
      
      if src.data1 = 0 or src.len1 = 0 or posi < 1 or posi > src.len1 THEN return 0
      if len(wstring) = 4 THEN return posi
      x = 0 : y = 0
      Do While y < posi and x < src.len1
         x += 1
         us1 = src.data1[x - 1]
         if IS_HIGH_UTF16_SUR(us1) then          'asc(src.data1[x-1])) then
            if x > src.len1 - 1 THEN return x * - 1 'return negative position to show where error
            x += 1
            us2 = src.data1[x - 1]
            if IS_LOW_UTF16_SUR(us2) then        'asc(src.data1[x-1])) then
               y += 1
               if y = posi THEN return x - 1
            else
               return x * - 1                    'return negative position to show where error
            end if
         else
            y += 1
            if y = posi THEN return x
         END IF
      LOOP
      function = 0
   end function
   
   
   
   private function u_W_U16len(ByRef src as Const uStringW , byref posi as long , pos2 as long = 134217725) as long
      dim         as long x
      dim         as long y
      dim         as long z
      dim         as ushort us1
      dim         as ushort us2
      
      if src.data1 = 0 or src.len1 = 0 or posi < 1 or posi > src.len1 or pos2 < 1 THEN return 0
      if posi > 1 then
         z = u_W_U16Start(src , posi)
         if z < 1 THEN return 0
      else
         z = 1
      end if
      posi = z
      y = 0 : x = z
      Do While y < pos2 and x <= src.len1
         us1 = src.data1[x - 1]
         if IS_HIGH_UTF16_SUR(us1) then          'asc(src.data1[x-1])) then
            if x > src.len1 - 1 THEN return x * - 1 'return negative position to show where error
            x += 1
            us2 = src.data1[x - 1]
            if IS_LOW_UTF16_SUR(us2) then        'asc(src.data1[x-1])) then
               y += 1
               if y = pos2 THEN return x - z + 1
            else
               return x * - 1                    'return negative position to show where error
            end if
         else
            y += 1
            if y = pos2 THEN return x - z + 1
         END IF
         x += 1
      LOOP
      function = x - z
   end function
   
   ''::::: give number of surrogate into uStringW for win only
   private function u_Exist_Sur(ByRef src as Const uStringW , posi as long = 1) as long
      if src.data1 = 0 or src.len1 = 0 or posi = 0 THEN return 0
      function = src.surrogate
   end function
   
   
   
   
   
   ''::::: equivalent to len(string) but counts units no wstrings : important for windows ; for linux not
   private function u_Len(ByRef src as Const uStringW) as long
      if src.len1 = 0 THEN
         return 0
      elseif src.surrogate = 0 then
         return src.len1
      END IF
      function = src.len1 - src.surrogate
   end function
   
   ''::::: equivalent to Strptr for uStringW
   private Function u_Strptr(ByRef src as uStringW) as wstring ptr
      function = src.data1
   END FUNCTION
   
   private sub u_PutLen(ByRef src as uStringW)
      hRealloc(@src , len(*src.data1) , 1)
   END sub
   
   ''::::: equivalent to Mid for uStringW, but carrefull in windows (utf16) surrogate parts ( adding 1 wstring )
   private Function u_Mid(ByRef str1 as Const uStringW , ByVal start as long , ByVal nb as long = 268435455) as uStringW
      dim dest              as uStringW
      if str1.data1 = 0 or str1.len1 = 0 or nb < 1 or start < 1 or start > str1.len1 THEN
         function = dest
         exit function
      END IF
      if nb > str1.len1 - (start - 1) THEN nb = str1.len1 - (start - 1)
      if str1.surrogate <> 0 then
         nb = u_W_U16len(str1 , start , nb)
      end if
      DWstrAssign(dest , mid(*str1.data1 , start , nb))
      function = dest
   END FUNCTION
   
   ''::::: reverse uStringW, taking care in windows (utf16) of surrogate parts
   private Function u_Reverse(ByRef str1 as Const uStringW) as uStringW
      dim dest              as uStringW
      dim         as long x
      dim         as long y
      dim         as ulong us2
      dim         as ulong us1
      if str1.data1 = 0 or str1.len1 = 0 THEN
         function = dest
         exit function
      END IF
      y = str1.len1
      hRealloc(@dest , y , 0)
      if str1.surrogate = 0 then
         for x = y - 1 to 0 step - 1
            us1 = str1.data1[x]
            dest.data1[y - x - 1] = us1
         NEXT
      else
         for x = y - 1 to 0 step - 1
            us2 = str1.data1[x]
            if IS_LOW_UTF16_SUR(us2) THEN
               if x - 1 < 0 THEN exit for
               us1 = str1.data1[x - 1]
               if IS_HIGH_UTF16_SUR(us1) THEN
                  dest.data1[y - x] = us2
                  dest.data1[y - x - 1] = us1
                  x -= 1
               else
                  exit for
               end if
            else
               dest.data1[y - x - 1] = us2
            END IF
         NEXT
      end if
      dest.surrogate = str1.surrogate
      function = dest
   END FUNCTION
   
   ''::::: equivalent to Left for uStringW, taking care in windows (utf16) of surrogate parts
   private Function u_Left(ByRef str1 as uStringW , ByVal nb as long) as uStringW
      dim dest              as uStringW
      dim         as long l1
      if str1.data1 = 0 or str1.len1 = 0 or nb < 1 THEN
         function = dest
         exit function
      END IF
      if nb > str1.len1 THEN
         DWstrDup(dest , str1)
         function = dest
         exit function
      end if
      l1 = 1
      if str1.surrogate <> 0 then nb = u_W_U16len(str1 , l1 , nb)
      DWstrAssign(dest , left(*str1.data1 , nb))
      function = dest
   END FUNCTION
   
   ''::::: equivalent to Right for uStringW, taking care in windows (utf16) of surrogate parts
   private Function u_Right(ByRef str1 as uStringW , ByVal nb as long) as uStringW
      dim dest              as uStringW
      dim temp              as uStringW
      dim temp2             as uStringW
      dim         as long l1
      if str1.data1 = 0 or str1.len1 = 0 or nb < 1 THEN
         function = dest
         exit function
      END IF
      if nb > str1.len1 THEN
         DWstrDup(dest , str1)
         function = dest
         exit function
      end if
      if str1.surrogate <> 0 then
         l1 = 1
         temp = u_Reverse(str1)
         nb = u_W_U16len(temp , l1 , nb)
         DWstrAssign(temp2 , left(*temp.data1 , nb))
         dest = u_Reverse(temp2)
      else
         DWstrAssign(dest , right(*str1.data1 , nb))
      end if
      function = dest
   END FUNCTION
   
   '''::::: equivalent to Instr for uStringW, taking care in windows (utf16) of surrogate parts
   private Function u_Instr Overload(ByRef str1 as uStringW , ByRef sub1 as uStringW) as long
      if str1.len1 = 0 or sub1.len1 = 0 then
         function = 0
      else
         function = instr(*(str1.data1) , *(sub1.data1))
      end if
   END FUNCTION
   
   ' v1.02 bug corrected
   '''::::: equivalent to Instr for uStringW (second form) can accept negative start point for test from the end
   private Function u_Instr Overload(ByVal start As long , ByRef str1 as uStringW , ByRef sub1 as uStringW) as long
      dim stemp             as uStringW
      dim stemp2            as uStringW
      
      if str1.len1 = 0 or sub1.len1 = 0 or start = 0 then return 0
      if str1.surrogate = 0 and sub1.surrogate = 0 THEN
         if start > 0 THEN
            return instr(start , *(str1.data1) , *(sub1.data1))
         else
            stemp = u_Reverse(str1)
            stemp2 = u_Reverse(sub1)
            start = start * - 1
            start = instr(start , *(stemp.data1) , *(stemp2.data1))
            function = start                     'str1.len1 - start + 1
         END IF
      elseif str1.surrogate = 0 and sub1.surrogate > 0 then
         return 0
      else
         if start > 0 THEN
            start = u_W_U16Start(str1 , start)
            return instr(start , *(str1.data1) , *(sub1.data1))
         else
            stemp = u_Reverse(str1)
            stemp2 = u_Reverse(sub1)
            start = u_W_U16Start(stemp , start * - 1)
            start = instr(start , *(stemp.data1) , *(stemp2.data1))
            function = start                     'str1.len1 - start + 1
         END IF
      end if
   END FUNCTION
   
   '''::::: equivalent to Asc for uStringW
   private Function u_Asc(ByRef str1 as Const uStringW , ByVal pos1 As long = 1) as ulong
      if str1.data1 = 0 or str1.len1 < pos1 then return 0
      if len(wstring) = 4 then                   'not windows
         function = asc(*(str1.data1) , pos1)
      Else
         dim hi as ulong = asc(*(str1.data1) , pos1)
         if hi >= &HD800 and hi <= &HDBFF THEN   'check surrogate
            dim lo as ulong = asc(*(str1.data1) , pos1 + 1)
            if lo >= &HDC00 and lo <= &HDFFF THEN
               function = ((hi - &HD800) *&H400) + (lo - &HDC00) + &H10000
            else
               function = 0
            END IF
         else
            function = hi
         end if
      End if
   END FUNCTION
   
   '''::::: equivalent to wChr for uStringW , only 1 code
   private Function u_Chr(icode as ulong) as uStringW
      dim str1              as uStringW
      dim stemp             as string
      if icode = 0 or icode > &h10FFFF THEN
         function = str1
         exit function
      END IF
      if len(wstring) = 4 then
         hRealloc(@str1 , 1 , 0)
         *str1.data1 = icode
      else
         if icode > &hFFFF THEN
            dim as ushort u1 = LEAD_OFFSET + (icode shr 10)
            dim as ushort u2 = &hDC00 + (icode and &h3FF)
            hRealloc(@str1 , 2 , 0)
            *str1.data1 = u1
            *(str1.data1 + 1) = u2
            str1.surrogate = 1
         else
            hRealloc(@str1 , 1 , 0)
            *str1.data1 = icode
         end if
      end if
      function = str1
   end function
   
   '''::::: equivalent to Val for uStringW
   private Function u_Val(ByRef str1 as Const uStringW) as double
      if str1.data1 = 0 then return 0
      function = val(*(str1.data1))
   END FUNCTION
   
   '''::::: v1.02 to get the index delimited substring for uStringW
   Private FUNCTION u_Parse(ByRef source as ustringW , ByRef delimiter as ustringW , index as long) as ustringW
      Dim         As Long i
      Dim         As Long s
      Dim         As Long c
      Dim         As Long l
      dim str1              as uStringW
      
      l = delimiter.len1
      s = 1
      do
         If c = index - 1 then
            str1 = u_mid(source , s , u_instr(s , source , delimiter) - s)
            function = str1
            exit function
         end if
         i = u_instr(s , source , delimiter)
         If i > 0 then
            c += 1
            s = i + l
         end if
      loop until i = 0
      function = str1
   End Function
   
   'v1.02 u_Ucase ; u_Lcase ; u_Trim ; u_Ltrim ; u_Rtrim
   Private FUNCTION u_Ucase(ByRef source as ustringW) as ustringW
      dim str1              as uStringW
      DWstrAssign(str1 , Ucase(*source.data1))
      function = str1
   End Function
   
   Private FUNCTION u_Lcase(ByRef source as ustringW) as ustringW
      dim str1              as uStringW
      DWstrAssign(str1 , lcase(*source.data1))
      function = str1
   End Function
   
   Private FUNCTION u_Trim(ByRef source as ustringW , utt as ustringW = " ") as ustringW
      dim str1              as uStringW
      DWstrAssign(str1 , trim(*source.data1 , any * utt.data1))
      function = str1
   End Function
   
   Private FUNCTION u_Ltrim(ByRef source as ustringW , utt as ustringW = " ") as ustringW
      dim str1              as uStringW
      DWstrAssign(str1 , Ltrim(*source.data1 , any * utt.data1))
      function = str1
   End Function
   
   Private FUNCTION u_Rtrim(ByRef source as ustringW , utt as ustringW = " ") as ustringW
      dim str1              as uStringW
      DWstrAssign(str1 , Rtrim(*source.data1 , any * utt.data1))
      function = str1
   End Function
   
   '''::::: same as uStringW ( more friendly form) ?
   private function u_Wdata(ByRef ini1 as uStringW) as wstring ptr
      function = ini1.data1
   end function
   
   ''::::: ok for win or linux
   private function u_to_Ansi(ByRef ini1 as uStringW) as string
      dim         as ulong c
      dim as long chars = 0
      dim as string temp = ""
      
      if ini1.data1 = 0 or ini1.len1 = 0 THEN return temp
      do while(chars < ini1.len1)
         c = ini1.data1[chars]
         if (c > 255) then
            temp &= "?"
            if (IS_HIGH_UTF16_SUR(c)) and len(wstring) = 2 then '' surrogate?
               if chars + 1 < ini1.len1 THEN
                  if (IS_LOW_UTF16_SUR(ini1.data1[chars + 1])) then chars += 1
               END IF
            end if
         else
            temp &= chr(c)
         end if
         chars += 1
      loop
      function = temp
   end function
   
   ''::::: ok for win or linux
   private function u_to_Utf32le_ar(ByRef ini1 as uStringW) as long
      dim         as ulong c
      dim         as ulong wc
      dim         as ulong u1
      dim         as long chars
      dim as long x = 0
      
      redim as ulong UTF32LE(1)
      chars = 0
      if ini1.data1 = 0 or ini1.len1 = 0 THEN
         UTF32LE(0) = 0
         return 0
      end if
      redim as ulong UTF32LE(0 to ini1.len1)
      do while(chars < ini1.len1)
         wc = ini1.data1[chars]
         x += 1
         if ini1.surrogate > 0 THEN
            if (wc >= UTF16_SUR_HIGH_START) and len(wstring) = 2 then '' surrogate?
               if (wc <= UTF16_SUR_HIGH_END) then
                  chars += 1
                  if chars > ini1.len1 THEN exit do
                  c = ini1.data1[chars]
                  u1 = wc
                  wc = (u1 shl 10) + c + SURROGATE_OFFSET
               end if
            end if
         END IF
         UTF32LE(x) = wc
         chars += 1
      loop
      redim preserve as ulong UTF32LE(0 to chars)
      UTF32LE(0) = chars
      function = chars
   end function
   
   ''::::: ok for win or linux
   private function u_to_Utf32be_ar(ByRef ini1 as uStringW) as long
      dim         as ulong c
      dim         as ulong c1
      dim         as long i
      dim         as long chars
      
      redim as ulong UTF32LE(1)
      if ini1.data1 = 0 or ini1.len1 = 0 THEN
         UTF32LE(0) = 0
         return 0
      end if
      chars = u_to_Utf32le_ar(ini1)
      for i = 1 to chars
         c = UTF32LE(i)
         c1 = U32_SWAP(c)
         UTF32LE(i) = c1
      next
      function = chars
   end function
   
   
   private sub u_from_String Overload(ByRef src as string , byref dest as uStringW)
      if len(src) = 0 THEN
         hRealloc(@dest , 0 , 0)
      else
         hRealloc(@dest , len(src) / len(wstring) , 0)
         if len(wstring) = 4 THEN
            src = src & chr(0) & chr(0) & chr(0) & chr(0)
         else
            src = src & chr(0) & chr(0)
         END IF
         src = src
         *dest.data1 = *(cast(wstring ptr , strptr(src)))
         if len(wstring) = 4 THEN
            dest.surrogate = 0
         else
            dest.Surrogate = u_surX(dest)
         END IF
      END IF
   end sub
   
   ''::::: ok for win or linux : no conversion interresting to get the string from an unicode file
   ' the string is used as a container of ubyte sequence of the wstring representation ( 2 or 4 bytes)
   private function u_from_String Overload(ByRef src as string) as uStringW
      dim dest              as uStringW
      
      u_from_String(src , dest)
      function = dest
   end function
   
   
   
   
   ''::::: ok for win or linux : no conversion interresting to get the bytes from an unicode file
   ' the ubyte array has to have all the bytes needed by the internal len(wstring) ( 2 or 4 bytes)
   ' and do the redim ( 1 to last byte of the wstring ) not more
   private function u_from_Ubyte(bsrc() as ubyte) as uStringW
      dim dest              as uStringW
      dim ilong             as long
      
      if (ubound(bsrc) = 0 and lbound(bsrc) = 0) or ubound(bsrc) < lbound(bsrc) THEN
         ilong = 0
      else
         ilong = ubound(bsrc) - lbound(bsrc) + 1
      END IF
      redim as ubyte bsrc(1 to ilong + len(wstring))
      if len(wstring) = 4 then
         bsrc(ilong + 3) = 0
         bsrc(ilong + 4) = 0
      end if
      bsrc(ilong + 1) = 0
      bsrc(ilong + 2) = 0
      if ilong = 0 THEN
         '
      else
         hRealloc(@dest , ilong / len(wstring) , 0)
         *dest.data1 = *(cast(wstring ptr , varptr(bsrc(0))))
         if len(wstring) = 4 THEN
            dest.surrogate = 0
         else
            dest.Surrogate = u_surX(dest)
         END IF
      END IF
      function = dest
   end function
   
   ' '::::::::::::::::::::::::::::::::::::
   ' UTF-8 coding / decoding
   ' '::::::::::::::::::::::::::::::::::::
   dim shared as ubyte utf8_trailingTb(0 to 255) => _
         {0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , _
         0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , _
         0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , _
         0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , _
         0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , _
         0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , _
         0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , _
         0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , _
         0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , _
         0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , _
         0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , _
         0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , _
         1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , _
         1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , _
         2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , _
         3 , 3 , 3 , 3 , 3 , 3 , 3 , 3 , 4 , 4 , 4 , 4 , 5 , 5 , 5 , 5}
   
   
   dim shared as ulong utf8_offsetsTb(0 to 5) => _
         {&h00000000 , &h00003080 , &h000E2080 , &h03C82080 , &hFA082080 , &h82082080}
   
   ''::::: ok for win internal coding as utf16 , and ok for linux internal coding as utf32
   private sub u_from_Utf8s overload(ByRef utf8s as string , byref dest as uStringW)
      dim         as ubyte src(0 to 6)
      dim         as ubyte ptr p
      dim         as ulong c
      dim         as long chars
      dim         as long extbytes
      dim         as long i
      dim         as long ilen
      dim         as long x
      dim dst               as wstring ptr
      
      ilen = len(utf8s)
      if ilen = 0 THEN
         exit sub
      else
         hRealloc(@dest , ilen , 0)
         dst = dest.data1
      end if
      chars = 0 : x = 0
      do while(x < ilen)
         src(0) = asc(mid(utf8s , x + 1 , 1))
         extbytes = utf8_trailingTb(src(0))
         c = 0
         p = @src(0)
         if (extbytes > 0) then
            if x + extbytes + 1 > ilen THEN exit do
            for i = 1 to extbytes
               src(i) = asc(mid(utf8s , x + 1 + i , 1))
            NEXT
            i = extbytes
            do
               c += *p
               p += 1
               c shl= 6
               i -= 1
            loop while(i > 0)
            x = x + extbytes
         end if
         c += *p
         c -= utf8_offsetsTb(extbytes)
         if (c > UTF16_MAX_BMP) and len(wstring) = 2 then ' win surrogate
            if (x < ilen) then
               *dst = LEAD_OFFSET + (c shr 10)
               dst += 1
               chars += 1
            end if
            c = &hDC00 + (c and &h3FF)
         end if
         *dst = c
         dst += 1
         chars += 1
         x += 1
      loop
      if ilen > chars then hRealloc(@dest , chars , 1)
      if len(wstring) = 4 THEN
         dest.surrogate = 0
      else
         dest.Surrogate = u_surX(dest)
      END IF
   end sub
   
   
   ''::::: ok for win internal coding as utf16 , and ok for linux internal coding as utf32
   private function u_from_Utf8s overload(ByRef utf8s as string) as uStringW
      dim dest              as uStringW
      
      u_from_Utf8s(utf8s , dest)
      function = dest
   end function
   
   'enum UTF_ENCOD
   ' UTF_ENCOD_ASCII =0
   ' UTF_ENCOD_UTF8 =1
   ' UTF_ENCOD_UTF16 =2
   ' UTF_ENCOD_UTF32 =3
   'end enum
   
   extern "C"
      'generic function to convert from wstrings, extracted from utf_conv.bi
      declare function WChar_UTF alias "fb_WCharToUTF" _
            (byval encod as long , _                   '' UTF_ENCOD 1 for utf8
            byval src as wstring ptr , _
            byval chars as long , _
            byval dst as any ptr , _
            byval bytes as long ptr _
            ) as any ptr
   end extern
   
   ''::::: ok for win , on linux ?
   private function u_to_Utf8s(ByRef ini1 as uStringW) as string
      dim         as long bytes
      dim         as long ilen
      dim         as string pw1
      
      if ini1.data1 = 0 or ini1.len1 = 0 THEN return ""
      ilen = ini1.len1
      WChar_UTF(1 , ini1.data1 , ilen , strptr(pw1) , @bytes) ' first to get the size
      pw1 = String(bytes , 0)                    ' allocating the minimum size
      WChar_UTF(1 , ini1.data1 , ilen , strptr(pw1) , @bytes) ' second to convert
      function = pw1
   end function
   
   
   
   
   ''::::: ok for win , and linux
   private function read_utf16_file(ByRef file as string , endian as long = 1) as uStringW
      dim         as long x
      dim         as long z
      dim         as long y
      dim         as long r
      dim         as long t
      dim as long f1 = freefile
      dim         as string stemp
      dim         as string stemp2
      dim         as ubyte ubit
      dim         as ushort wc
      dim         as uStringW dest
      
      if open(file for binary Access Read as #f1) = 0 then
         x = LOF(f1)
         'print "file x = " ; x
         If x > 0 Then
            stemp = String(x , 0)
            Get #f1 , , stemp
         else
            Close #f1
            function = dest
            exit Function
         End If
         Close #f1
      else
         function = dest
         exit function
      end if
      z = 0
      if asc(left(stemp , 1)) = 255 and asc(mid(stemp , 2 , 1)) = 254 THEN
         stemp = mid(stemp , 3)
         z = 1 : x = x - 2
      elseif asc(left(stemp , 1)) = 254 and asc(mid(stemp , 2 , 1)) = 255 THEN
         stemp = mid(stemp , 3)
         z = 2 : x = x - 2
      end if
      if z = 0 THEN                              ' test space
         r = instr(stemp , chr(32) & chr(0))
         if r > 0 THEN
            t = r mod 2
            if t = 1 THEN z = 1
            if t = 0 THEN z = 2
         end if
      END IF
      if z = 0 THEN z = endian                   'forced by default to LE because more frequent
      if len(stemp) = 0 THEN
         function = dest
         exit function
      end if
      if z = 1 and len(wstring) = 2 THEN
         u_from_String(stemp , dest)
         function = dest
         exit function
      end if
      stemp2 = ""
      if z = 2 then
         for y = 1 to x step 2
            stemp2 &= mid(stemp , y + 1 , 1) & mid(stemp , y , 1)
         NEXT
         if len(wstring) = 2 THEN
            u_from_String(stemp2 , dest)
            function = dest
            exit function
         end if
         stemp = stemp2
      end if
      stemp2 = ""
      for y = 1 to x step 2
         wc = asc(mid(stemp , y , 1)) + asc(mid(stemp , y + 1 , 1)) *256
         if (wc >= UTF16_SUR_HIGH_START) and (wc <= UTF16_SUR_HIGH_END) then '' surrogate?
            if y + 2 > x THEN exit for
            y += 2
            stemp2 &= surrogate_byt(mid(stemp , y , 4) , 32)
         else
            stemp2 &= mid(stemp , y , 2) & chr(0) & chr(0)
         END IF
      next
      u_from_String(stemp2 , dest)
      function = dest
   end function
   
   ''::::: ok for win and linux creates UTF16LE file with BOM
   private function write_utf16_file(ByRef file as string , ByRef content as uStringW , rewrite as long = 1) as long
      dim         as long x
      dim         as long y
      dim         as long z
      dim as long f1 = freefile
      dim as zstring ptr pc1 = cast(zstring ptr , u_Strptr(content))
      dim         as ubyte by1
      dim         as string sdef
      dim         as string sb1
      dim         as string sb2
      dim         as string sb3
      dim         as string sb4
      x = content.len1
      x = x * len(wstring)
      If x > 0 Then
         y = open(file for binary Access Read as #f1)
         if y = 0 then
            z = LOF(f1)
            close #f1
         end if
         if rewrite = 1 and y = 0 then
            kill file
         elseif rewrite <> 1 and y = 0 and z > 0 then
            return 0
         elseif y = 0 then
            kill file
         end if
         f1 = freefile
         if open(file for binary as #f1) = 0 then
            by1 = 255
            Put #f1 , 1 , by1 , 1
            by1 = 254
            Put #f1 , 2 , by1 , 1
            if len(wstring) = 2 THEN
               for y = 0 to x - 1
                  by1 = asc(pc1[y])
                  Put #f1 , y + 3 , by1 , 1
               NEXT
            else
               sdef = ""
               For y = 0 to x - 1 step 4
                  sb1 = pc1[y]
                  sb2 = pc1[y + 1]
                  sb3 = pc1[y + 2]
                  sb4 = pc1[y + 3]
                  sdef &= surrogate_byt(sb1 & sb2 & sb3 & sb4)
               NEXT
               Put #f1 , 4 , sdef
            END IF
            Close #f1
         else
            return 0
         end if
      else
         return 0
      end if
      return 1
   end function
   
   ''::::: ok for win or linux
   private function read_utf8_file(ByRef file as string) as uStringW
      dim         as long x
      dim as long f1 = freefile
      dim         as string stemp
      dim         as uStringW dest
      if open(file for binary Access Read as #f1) = 0 then
         x = LOF(f1)
         If x > 0 Then
            stemp = String(x , 0)
            Get #f1 , , stemp
         else
            Close #f1
            function = dest
            exit function
         End If
         Close #f1
      else
         function = dest
         exit function
      end if
      if asc(left(stemp , 1)) = 239 and asc(mid(stemp , 2 , 1)) = 187 and asc(mid(stemp , 3 , 1)) = 191 THEN stemp = mid(stemp , 4)
      if len(stemp) = 0 THEN
         '
      else
         u_from_Utf8s(stemp , dest)
      end if
      function = dest
   end function
   
   ' Win or linux ok
   private function read_utf32_file(ByRef file as string , endian as long = 1) as uStringW
      dim         as long x
      dim         as long y
      dim         as long z
      dim         as long r
      dim         as long t
      dim as long f1 = freefile
      dim         as string stemp
      dim         as string sb1
      dim         as string sb2
      dim         as string sb3
      dim         as string sb4
      dim         as string sdef
      dim         as ulong c
      dim         as ushort hs
      dim         as ushort ls
      dim         as uStringW dest
      if open(file for binary Access Read as #f1) = 0 then
         y = LOF(f1)
         If y > 0 Then
            stemp = String(y , 0)
            Get #f1 , , stemp
         else
            Close #f1
            function = dest
            exit function
         End If
         Close #f1
      else
         function = dest
         exit function
      end if
      z = 0
      if asc(left(stemp , 1)) = 0 and asc(mid(stemp , 2 , 1)) = 0 _
            and asc(mid(stemp , 3 , 1)) = 254 and asc(mid(stemp , 4 , 1)) = 255 THEN 'with bom BE
         stemp = mid(stemp , 5)
         y = y - 4
         z = 1
      elseif asc(left(stemp , 1)) = 255 and asc(mid(stemp , 2 , 1)) = 254 _
            and asc(mid(stemp , 3 , 1)) = 0 and asc(mid(stemp , 4 , 1)) = 0 THEN 'with bom LE
         stemp = mid(stemp , 5)
         y = y - 4
         z = 2
      end if
      if z = 0 THEN                              ' test space
         r = instr(stemp , chr(0) & chr(0) & chr(0) &chr(32))
         if r > 0 THEN
            t = r mod 4
            if t = 2 THEN z = 2
         end if
      END IF
      if z = 0 THEN z = endian                   'forced by default to BE because more frequent
      if y = 0 THEN
         function = dest
         exit function
      end if
      if z = 1 then                              'swap bytes and reduce bytes
         sdef = ""
         For x = 1 to y step 4
            sb1 = mid(stemp , x , 1)
            sb2 = mid(stemp , x + 1 , 1)
            sb3 = mid(stemp , x + 2 , 1)
            sb4 = mid(stemp , x + 3 , 1)
            if sb1 = chr(0) and sb2 = chr(0) and len(wstring) = 2 THEN ' win no surrogate
               sdef &= sb4 & sb3
            else
               if len(wstring) = 2 then          'win surrogate for utf16
                  sdef &= surrogate_byt(sb1 & sb2 & sb3 & sb4)
               else                              ' linux
                  sdef &= sb4 & sb3 & sb2 & sb1
               end if
            END IF
         NEXT
      else                                       'reduce bytes
         sdef = ""
         For x = 1 to y step 4
            sb1 = mid(stemp , x , 1)
            sb2 = mid(stemp , x + 1 , 1)
            sb3 = mid(stemp , x + 2 , 1)
            sb4 = mid(stemp , x + 3 , 1)
            if sb3 = chr(0) and sb4 = chr(0) and len(wstring) = 2 THEN ' win no surrogate
               sdef &= sb1 & sb2
            else
               if len(wstring) = 2 then          'win surrogate for utf16
                  sdef &= surrogate_byt(sb4 & sb3 & sb2 & sb1)
               else                              ' linux
                  sdef &= sb1 & sb2 & sb3 & sb1
               end if
            END IF
         NEXT
      end if
      if len(sdef) = 0 THEN
         'function = dest
      else
         u_from_String(sdef , dest)
      end if
      function = dest
   end function
   
   ''::::: ok for win , linux ? in utf32BE format with BOM
   private function write_utf32_file(ByRef file as string , ByRef content as uStringW , rewrite as long = 1) as long
      dim         as long x
      dim         as long y
      dim         as long z
      dim as long f1 = freefile
      dim         as ubyte by1
      dim         as ulong char
      redim as ulong UTF32LE(0)
      x = u_to_Utf32be_ar(content)
      If x > 0 Then
         y = open(file for binary Access Read as #f1)
         if y = 0 then
            z = LOF(f1)
            close #f1
         end if
         if rewrite = 1 and y = 0 then
            kill file
         elseif rewrite <> 1 and y = 0 and z > 0 then
            erase UTF32LE
            return 0
         elseif y = 0 then
            kill file
         end if
         f1 = freefile
         if open(file for binary as #f1) = 0 then
            by1 = 0
            Put #f1 , 1 , by1 , 1
            Put #f1 , 2 , by1 , 1
            by1 = 254
            Put #f1 , 3 , by1 , 1
            by1 = 255
            Put #f1 , 4 , by1 , 1
            Put #f1 , 5 , UTF32LE()
            Close #f1
         else
            erase UTF32LE
            return 0
         end if
      end if
      erase UTF32LE
      return 1
   end function
   
   ''::::: ok for win , linux ? WITH BOM
   private function write_utf8_file(ByRef file as string , ByRef content as uStringW , rewrite as long = 1) as long
      dim         as long x
      dim         as long y
      dim         as long z
      dim as long f1 = freefile
      dim as string spc1 = u_to_Utf8s(content)
      dim as zstring ptr pc1 = strptr(spc1)
      dim         as ubyte by1
      x = content.len1
      If x > 0 Then
         y = open(file for binary Access Read as #f1)
         if y = 0 then
            z = LOF(f1)
            close #f1
         end if
         if rewrite = 1 and y = 0 then
            kill file
         elseif rewrite <> 1 and y = 0 and z > 0 then
            return 0
         elseif y = 0 then
            kill file
         end if
         f1 = freefile
         if open(file for binary as #f1) = 0 then
            by1 = &hEF
            Put #f1 , 1 , by1 , 1
            by1 = &hBB
            Put #f1 , 2 , by1 , 1
            by1 = &hBF
            Put #f1 , 3 , by1 , 1
            for y = 0 to x - 1
               by1 = asc(pc1[y])
               Put #f1 , y + 4 , by1 , 1
            NEXT
            Close #f1
         else
            return 0
         end if
      else
         return 0
      end if
      return 1
   end function
   
   ''::::: external sub for reading files line/line ok for win /linux Ansi ,UTF8 ,UTF16LE ,UTF32BE
   ' usage Open txt For Input As #filehandle
   ' u_LineInput( filehandle,utxt,16)
   ' Close #filehandle
   '
   private sub u_LineInput(byref Filehandle As Integer , byref y as uStringW , iflag as long = 0)
      dim         as string x
      dim         as string sdef
      dim         as string sb1
      dim         as string sb2
      dim         as string sb3
      dim         as string sb4
      dim         as ubyte ubt1
      dim         as long i
      
      if EOF(Filehandle) = 0 then
         Line Input #Filehandle , x
      else
         u_reset(y)
         exit sub
      end if
      if iflag = 0 THEN
         if x = "" THEN
            u_reset(y)
            exit sub
         END IF
         u_From_CodeString(x , y)
      elseif iflag = 32 THEN                     'assume its utf32BE
         if left(x , 4) = chr(0 , 0 , 254 , 255) then x = mid(x , 5)
         if right(x , 7) = chr(0 , 0 , 0 , 13 , 0 , 0 , 0) then x = left(x , len(x) - 7)
         if x = "" THEN
            u_reset(y)
            exit sub
         END IF
         sdef = ""
         For i = 1 to len(x) step 4
            sb1 = mid(x , i , 1)
            sb2 = mid(x , i + 1 , 1)
            sb3 = mid(x , i + 2 , 1)
            sb4 = mid(x , i + 3 , 1)
            if sb1 = chr(0) and sb2 = chr(0) and len(wstring) = 2 THEN ' win no surrogate
               sdef &= sb4 & sb3
            else
               if len(wstring) = 2 then          'win surrogate for utf16
                  sdef &= surrogate_byt(sb1 & sb2 & sb3 & sb4)
               else                              ' linux
                  sdef &= sb4 & sb3 & sb2 & sb1
               end if
            END IF
         NEXT
         u_from_String(sdef , y)
      elseif iflag = 16 THEN                     'assume its utf16LE
         if EOF(Filehandle) = 0 then get #Filehandle , , ubt1
         if left(x , 2) = chr(255 , 254) then x = mid(x , 3)
         if right(x , 2) = chr(13 , 0) then x = left(x , len(x) - 2)
         if x = "" THEN
            u_reset(y)
            exit sub
         END IF
         u_from_String(x , y)
      elseif iflag = 8 THEN
         if asc(left(x , 1)) = 239 and asc(mid(x , 2 , 1)) = 187 and asc(mid(x , 3 , 1)) = 191 THEN x = mid(x , 4)
         if x = "" THEN
            u_reset(y)
            exit sub
         END IF
         u_from_Utf8s(x , y)
      END IF
   END sub
   
   
   '==============================================================================================
   ' uStringW Operators ; constructors and destructor
   '==============================================================================================
   
   'to cast as wstring ptr
   operator uStringW.cast() as wstring ptr
      return this.data1
   end operator
   
   'to cast as string
   operator uStringW.cast() as string
      return u_to_Ansi(this)
   end operator
   
   'to concat 2 uStringW into new uStringW _ temp uStringW
   Operator &(ByRef ust1 as uStringW , ByRef ust2 as uStringW) as uStringW
      return u_Concat(ust1 , ust2)
   end operator
   'to concat 2 uStringW into new uStringW (second form) _ temp uStringW
   Operator + (ByRef ust1 as uStringW , ByRef ust2 as uStringW) as uStringW
      return u_Concat(ust1 , ust2)
   end operator
   
   'to concat 1 uStringW and 1 wstring ptr into new uStringW _ temp uStringW
   Operator &(ByRef ust1 as uStringW , ByVal wst2 as wstring ptr) as uStringW
      return u_Concat(ust1 , u_Wstr(wst2))
   end operator
   'to concat 1 uStringW and 1 wstring ptr into new uStringW (second form) _ temp uStringW
   Operator + (ByRef ust1 as uStringW , ByVal wst2 as wstring ptr) as uStringW
      return u_Concat(ust1 , u_Wstr(wst2))
   end operator
   
   'to concat 1 uStringW and 1 string into new uStringW _ temp uStringW
   Operator &(ByRef ust1 as uStringW , ByRef st2 as string) as uStringW
      return u_Concat(ust1 , u_From_CodeString(st2))
   end operator
   'to concat 1 uStringW and 1 string into new uStringW (second form) _ temp uStringW
   Operator + (ByRef ust1 as uStringW , ByRef st2 as string) as uStringW
      return u_Concat(ust1 , u_From_CodeString(st2))
   end operator
   
   Operator &(ByRef ust1 as uStringW , Byval it2 as longint) as uStringW
      return u_Concat(ust1 , u_From_CodeString(str(it2)))
   end operator
   
   Operator &(ByRef ust1 as uStringW , Byval dt2 as double) as uStringW
      return u_Concat(ust1 , u_From_CodeString(str(dt2)))
   end operator
   
   'to concat 1 uStringW at the end of existing uStringW
   operator uStringW.&= (ByRef ust2 as uStringW)
      uConcatAssign(this , ust2)
   end operator
   'to concat 1 uStringW at the end of existing uStringW (second form)
   operator uStringW.+= (ByRef ust2 as uStringW)
      uConcatAssign(this , ust2)
   end operator
   
   'to concat 1 wstring ptr at the end of existing uStringW
   operator uStringW.&= (ByVal wst2 as wstring ptr)
      DWstrConcatAssign(this , wst2)
   end operator
   'to concat 1 wstring ptr at the end of existing uStringW (second form)
   operator uStringW.+= (ByVal wst2 as Wstring ptr)
      DWstrConcatAssign(this , wst2)
   end operator
   
   operator uStringW.&= (ByVal it2 as longint)
      DWstrConcatAssign(this , u_From_CodeString(str(it2)))
   end operator
   
   operator uStringW.&= (ByVal dt2 as double)
      DWstrConcatAssign(this , u_From_CodeString(str(dt2)))
   end operator
   
   'to concat 1 string at the end of existing uStringW
   operator uStringW.&= (ByRef st2 as string)
      dim ust2              as uStringW
      u_From_CodeString(st2 , ust2)
      uConcatAssign(this , ust2)
   end operator
   'to concat 1 string at the end of existing uStringW (second form)
   operator uStringW.+= (ByRef st2 as string)
      dim ust2              as uStringW
      u_From_CodeString(st2 , ust2)
      uConcatAssign(this , ust2)
   end operator
   
   'to get uStringW from uStringW
   Operator uStringW.let(ByRef ust2 as uStringW)
      DWstrdup(this , ust2)
   end operator
   
   'to get uStringW from string
   Operator uStringW.let(ByRef st2 as string)
      u_From_CodeString(st2 , this)
   end operator
   
   'to get uStringW from wstring ptr
   Operator uStringW.let(ByVal wst2 as wstring ptr)
      DWstrAssign(this , wst2)
   end operator
   
   
   'to compare 2 uStringW : equal
   Operator = (ByRef ust1 as uStringW , ByRef ust2 as uStringW) as long
		return u_Equal(ust1 , ust2)
	end operator

	'to compare 2 uStringW : different
	Operator <> (ByRef ust1 as uStringW , ByRef ust2 as uStringW) as long
		if u_Equal(ust1 , ust2) = 1 then
			return 0
		else
			return 1
		end if
	end operator

	'to Dim empty uStringW
	constructor uStringW()
		hRealloc(@this , 0 , 0)
	end constructor

	'to Dim uStringW from ustringw
	constructor uStringW(ByRef ust2 as uStringW)
		DWstrdup(this , ust2)
	end constructor

	'to Dim uStringW from wstring ptr including normal + escape sequence !"\uXXYY"
	constructor uStringW(ByVal wst2 as wstring ptr)
		DWstrAssign(this , wst2)
	end constructor

	'to Dim uStringW from coded string including pseudo + escape sequence \uXXYY or \wZZXXYY
	constructor uStringW(ByRef st2 as string)
		u_From_CodeString(st2 , this)
	end constructor


	Destructor uStringW()                            'v1.03
		if this.data1 <> 0 THEN
			#ifdef __U_CLEAN_MEM__
				'**** unstore the pointer
				DWSTRING_LIST_GLOB.DeleteByKey(str(this.data1))
				#ifdef __VERBOSE_MODE__
					print "Automatic FreeW " ; this.data1 , *this.data1
				#endif
			#endif
			clear(*(this.data1) , 0 , this.size1 + len(wstring))
			Deallocate(this.data1)
		END IF
		this.data1 = 0
		this.len1 = 0
		this.size1 = 0
		this.surrogate = 0
	End Destructor
#ENDIF                                           ' SET_USTRING_DYN





