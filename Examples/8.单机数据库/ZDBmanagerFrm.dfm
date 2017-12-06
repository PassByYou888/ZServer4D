object ZDBmanagerForm: TZDBmanagerForm
  Left = 0
  Top = 0
  Caption = 'ZDB Local...'
  ClientHeight = 387
  ClientWidth = 1108
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object buildTempDataButton: TButton
    Left = 16
    Top = 8
    Width = 91
    Height = 33
    Caption = 'generate 500k'
    TabOrder = 0
    OnClick = buildTempDataButtonClick
  end
  object Memo1: TMemo
    Left = 121
    Top = 8
    Width = 952
    Height = 257
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object QueryButton: TButton
    Left = 16
    Top = 47
    Width = 75
    Height = 25
    Caption = 'query'
    TabOrder = 2
    OnClick = QueryButtonClick
  end
  object InsertButton: TButton
    Left = 16
    Top = 128
    Width = 75
    Height = 25
    Caption = 'insert'
    TabOrder = 3
    OnClick = InsertButtonClick
  end
  object DeleteButton: TButton
    Left = 16
    Top = 159
    Width = 75
    Height = 25
    Caption = 'delete'
    TabOrder = 4
    OnClick = DeleteButtonClick
  end
  object ModifyButton: TButton
    Left = 16
    Top = 190
    Width = 75
    Height = 25
    Caption = 'modify'
    TabOrder = 5
    OnClick = ModifyButtonClick
  end
  object CompressButton: TButton
    Left = 16
    Top = 240
    Width = 75
    Height = 25
    HelpType = htKeyword
    Caption = 'Compress'
    TabOrder = 6
    OnClick = CompressButtonClick
  end
  object RecacheButton: TButton
    Left = 16
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Recache'
    TabOrder = 7
    OnClick = RecacheButtonClick
  end
  object ListBox1: TListBox
    Left = 120
    Top = 272
    Width = 953
    Height = 97
    ItemHeight = 13
    TabOrder = 8
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 416
    Top = 176
  end
  object Timer2: TTimer
    OnTimer = Timer2Timer
    Left = 552
    Top = 200
  end
end
