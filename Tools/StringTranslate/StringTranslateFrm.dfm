object StringTranslateForm: TStringTranslateForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderWidth = 15
  Caption = 'declaration translate..'
  ClientHeight = 393
  ClientWidth = 1095
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 498
    Height = 393
    Lines.Strings = (
      
        '      $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34' +
        ', $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $65, $63, $34, $2' +
        '0, $5F, $72, $65, $74, $5F, $30, $3B, $0D, $0A, $76, $65, $63, $' +
        '34, $20, $5F,'
      
        '      $54, $4D, $50, $30, $3B, $0D, $0A, $76, $65, $63, $32, $20' +
        ', $5F, $63, $30, $30, $30, $34, $3B, $0D, $0A, $75, $6E, $69, $6' +
        '6, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $5F, $4F, $' +
        '70, $61, $63,'
      
        '      $69, $74, $79, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72' +
        ', $6D, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $5' +
        'F, $74, $65, $78, $74, $75, $72, $65, $30, $3B, $0D, $0A, $76, $' +
        '6F, $69, $64,'
      
        '      $20, $6D, $61, $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A' +
        ', $20, $20, $20, $20, $5F, $63, $30, $30, $30, $34, $20, $3D, $2' +
        '0, $76, $65, $63, $32, $28, $54, $45, $58, $30, $2E, $78, $2C, $' +
        '20, $31, $2E,'
      
        '      $30, $20, $2D, $20, $54, $45, $58, $30, $2E, $79, $29, $3B' +
        ', $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $30, $20, $3' +
        'D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $' +
        '74, $65, $78,'
      
        '      $74, $75, $72, $65, $30, $2C, $20, $5F, $63, $30, $30, $30' +
        ', $34, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $65, $7' +
        '4, $5F, $30, $20, $3D, $20, $5F, $54, $4D, $50, $30, $2A, $5F, $' +
        '4F, $70, $61,'
      
        '      $63, $69, $74, $79, $3B, $0D, $0A, $20, $20, $20, $20, $67' +
        ', $6C, $5F, $46, $72, $61, $67, $43, $6F, $6C, $6F, $72, $20, $3' +
        'D, $20, $5F, $72, $65, $74, $5F, $30, $3B, $0D, $0A, $20, $20, $' +
        '20, $20, $72,'
      '      $65, $74, $75, $72, $6E, $3B, $0D, $0A, $7D, $20, $0D, $0A')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Memo2: TMemo
    Left = 624
    Top = 0
    Width = 471
    Height = 393
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object Button1: TButton
    Left = 504
    Top = 8
    Width = 105
    Height = 25
    Caption = 'hex 2 ascii ->'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 504
    Top = 39
    Width = 105
    Height = 25
    Caption = '<- ascii 2 hex'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 504
    Top = 95
    Width = 105
    Height = 25
    Caption = '<- ascii 2 decl'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 504
    Top = 175
    Width = 105
    Height = 25
    Caption = '<- ascii 2 pascal'
    TabOrder = 5
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 504
    Top = 206
    Width = 105
    Height = 25
    Caption = 'pascal 2 decl ->'
    TabOrder = 6
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 504
    Top = 279
    Width = 105
    Height = 25
    Caption = '<- ascii 2 c'
    TabOrder = 7
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 504
    Top = 310
    Width = 105
    Height = 25
    Caption = 'c 2 decl ->'
    TabOrder = 8
    OnClick = Button7Click
  end
end
