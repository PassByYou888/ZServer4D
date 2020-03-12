object StringTranslateForm: TStringTranslateForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 15
  Caption = 'declaration translate..'
  ClientHeight = 393
  ClientWidth = 1138
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  ScreenSnap = True
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 498
    Height = 393
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Memo2: TMemo
    Left = 640
    Top = 0
    Width = 498
    Height = 393
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object Hex2AsciiButton: TButton
    Left = 504
    Top = 70
    Width = 130
    Height = 25
    Caption = 'hex 2 ascii ->'
    TabOrder = 2
    OnClick = Hex2AsciiButtonClick
  end
  object Ascii2HexButton: TButton
    Left = 504
    Top = 39
    Width = 130
    Height = 25
    Caption = '<- ascii 2 hex'
    TabOrder = 3
    OnClick = Ascii2HexButtonClick
  end
  object Ascii2DeclButton: TButton
    Left = 504
    Top = 144
    Width = 130
    Height = 25
    Caption = '<- ascii 2 declaration'
    TabOrder = 4
    OnClick = Ascii2DeclButtonClick
  end
  object Ascii2PascalDeclButton: TButton
    Left = 504
    Top = 175
    Width = 130
    Height = 25
    Caption = '<- ascii 2 pascal'
    TabOrder = 5
    OnClick = Ascii2PascalDeclButtonClick
  end
  object PascalDecl2AsciiButton: TButton
    Left = 504
    Top = 206
    Width = 130
    Height = 25
    Caption = 'pascal 2 ascii ->'
    TabOrder = 6
    OnClick = PascalDecl2AsciiButtonClick
  end
  object Ascii2cButton: TButton
    Left = 504
    Top = 279
    Width = 130
    Height = 25
    Caption = '<- ascii 2 c'
    TabOrder = 7
    OnClick = Ascii2cButtonClick
  end
  object c2AsciiButton: TButton
    Left = 504
    Top = 310
    Width = 130
    Height = 25
    Caption = 'c 2 ascii ->'
    TabOrder = 8
    OnClick = c2AsciiButtonClick
  end
end
