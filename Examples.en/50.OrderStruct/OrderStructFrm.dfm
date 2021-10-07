object OrderStructForm: TOrderStructForm
  Left = 0
  Top = 0
  Caption = 'Order Struct. create by.qq600585'
  ClientHeight = 331
  ClientWidth = 993
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 24
    Top = 16
    Width = 945
    Height = 241
    Lines.Strings = (
      'Orderstruct is a sequence structure driven framework located in the kernel library. Its idea is based on the push / pop of assembly language'
      'Orderstruct gets rid of the influence of linked list and array, and directly accesses the structure. In the queue mechanism and parallel programs, the performance and simplicity are better than linked list'
      ''
      'TOrderStruct<T_>'#65306' Example of general sequence structure framework, < t_ > It can be a class, a record, or an atomic variable'
      
        'TOrderPtrStruct<T_>'#65306' Sequence structure framework instance of automation pointer, < T > If the record / atomic variable, it will be automatically converted to a pointer' +
        'Manage memory'
      
        'TCriticalOrderStruct<T_>'#65306' (thread safe) general sequence structure framework instance, < T > It can be a class, a record, or a' +
        'Therefore, atomic variables are the preferred data queue framework for parallel / multiline programs'
      
        'TCriticalOrderPtrStruct<T_>'#65306' (thread safety) sequence structure framework instance of automation pointer, < T > If record / original' +
        'Sub variables will be automatically converted into pointers to automatically manage memory'
      ''
      'Orderstruct is currently widely used in parallel and multithreaded data optimization in the background of AI'
      ''
      'by.qq600585'
      '')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 24
    Top = 272
    Width = 129
    Height = 25
    Caption = 'TOrderPtrStruct<T>'
    TabOrder = 1
    OnClick = Button1Click
  end
end
