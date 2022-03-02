#Inclib "Linked_Lists"

Declare Function SelectElement(ByVal list As Any Ptr,ByVal  element As integer) as Any Ptr
Declare sub AddElement_B(ByVal lis As Any Ptr,ByVal  znach As Byte)
Declare Sub AddElement_Sh(ByVal lis As Any Ptr,ByVal  znach As Short)
Declare Sub AddElement_S(ByVal list As Any Ptr,ByVal  znach As String)
Declare Sub AddElement_L(ByVal list As Any Ptr,ByVal  znach As longint)
Declare Sub AddElement_In(ByVal list As Any Ptr,ByVal  znach As integer)
Declare Sub AddElement_D(ByVal list As Any Ptr,ByVal  znach As Double)
Declare Function NewList() As Any Ptr
Declare Sub DeleteElement(ByVal list As Any Ptr,ByVal  element As integer)
Declare Function SizeList(ByVal lis As Any Ptr ) As Integer
Declare Sub DeleteList(ByVal lis As Any Ptr )
Declare Sub ClearList(ByVal lis As Any Ptr )
Declare Function LastElement(ByVal lis As Any Ptr ) As Integer
Declare Function FirstElement(ByVal lis As Any Ptr ) As Integer
Declare Function NextElement(ByVal lis As Any Ptr ) As Integer
Declare Function PrevElement(ByVal lis As Any Ptr ) As Integer
Declare Sub SetValueElement_SH(ByVal lis As Any Ptr ,ByVal element As Integer,ByVal znachenie As Short )
Declare Sub SetValueElement_B(ByVal lis As Any Ptr ,ByVal element As Integer,ByVal znachenie As Byte )
Declare Sub SetValueElement_In(ByVal lis As Any Ptr ,ByVal element As Integer,ByVal znachenie As Integer )
Declare Sub SetValueElement_S(ByVal lis As Any Ptr ,ByVal element As Integer,ByVal znachenie As String)
Declare Sub SetValueElement_D(ByVal lis As Any Ptr ,ByVal element As Integer,ByVal znachenie As Double )
Declare Sub SetValueElement_L(ByVal lis As Any Ptr ,ByVal element As Integer,ByVal znachenie As LongInt )
Declare Function GetList_SH(ByVal lis As Any Ptr) As Short
Declare Function GetList_B(ByVal lis As Any Ptr  ) As Byte 
Declare Function GetList_D(ByVal lis As Any Ptr) As Double
Declare Function GetList_L(ByVal lis As Any Ptr) As LongInt
Declare Function GetList_S(ByVal lis As Any Ptr) As String
Declare Function GetList_In(ByVal lis As Any Ptr) As Integer
Declare Sub SetList_SH(ByVal lis As Any Ptr,ByVal znachenie As Short)
Declare Sub SetList_B(ByVal lis As Any Ptr,ByVal znachenie As Byte)
Declare Sub SetList_In(ByVal lis As Any Ptr,ByVal znachenie As Integer)
Declare Sub SetList_S(ByVal lis As Any Ptr,ByVal znachenie As String)
Declare Sub SetList_D(ByVal lis As Any Ptr,ByVal znachenie As Double)
Declare Sub SetList_L(ByVal lis As Any Ptr,ByVal znachenie As LongInt)
Declare Function GetValueElement_S(ByVal list As Any Ptr,ByVal  element As integer) as String
Declare Function GetValueElement_In(ByVal list As Any Ptr,ByVal  element As integer) as Integer
Declare Function GetValueElement_L(ByVal list As Any Ptr,ByVal  element As integer) as LongInt
Declare Function GetValueElement_D(ByVal list As Any Ptr,ByVal  element As integer) As Double
Declare Function GetValueElement_B(ByVal list As Any Ptr,ByVal  element As integer) as Byte
Declare Function GetValueElement_SH(ByVal list As Any Ptr,ByVal  element As integer) As Short
Declare Sub InsertElement_S(ByVal list As Any Ptr,ByVal item As UInteger, byval  znach As String)
Declare Sub InsertElement_In(ByVal list As Any Ptr,ByVal item As UInteger, byval  znach As Integer)
Declare Sub InsertElement_L(ByVal list As Any Ptr,ByVal item As UInteger, byval  znach As LongInt)
Declare Sub InsertElement_B(ByVal list As Any Ptr,ByVal item As UInteger, byval  znach As Byte)
Declare Sub InsertElement_SH(ByVal list As Any Ptr,ByVal item As UInteger, byval  znach As Short)
Declare Sub InsertElement_D(ByVal list As Any Ptr,ByVal item As UInteger, byval  znach As Double)
Declare Sub AddElementHead_B(ByVal list As Any Ptr,ByVal  znach As Byte)
Declare Sub AddElementHead_S(ByVal list As Any Ptr,ByVal  znach As String)
Declare Sub AddElementHead_L(ByVal list As Any Ptr,ByVal  znach As LongInt)
Declare Sub AddElementHead_D(ByVal list As Any Ptr,ByVal  znach As Double)
Declare Sub AddElementHead_In(ByVal list As Any Ptr,ByVal  znach As Integer)
Declare Sub AddElementHead_SH(ByVal list As Any Ptr,ByVal  znach As Short)
Declare Sub MoveElement(ByVal lis As Any Ptr,ByVal from_ As UInteger, byval in_ As UInteger)



