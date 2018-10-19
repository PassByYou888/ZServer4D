object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #21508#31181'MD5'#31639#27861#27979#35797
  ClientHeight = 385
  ClientWidth = 507
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 296
    Top = 38
    Width = 26
    Height = 19
    Caption = 'lbl1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btn1: TButton
    Left = 32
    Top = 32
    Width = 98
    Height = 25
    Caption = 'HASH'#25991#20214
    TabOrder = 0
    OnClick = btn1Click
  end
  object edt1: TEdit
    Left = 32
    Top = 80
    Width = 441
    Height = 21
    TabOrder = 1
    Text = 'edt1'
  end
  object edt2: TEdit
    Left = 32
    Top = 120
    Width = 441
    Height = 21
    TabOrder = 2
    Text = 'edt1'
  end
  object edt3: TEdit
    Left = 32
    Top = 160
    Width = 441
    Height = 21
    TabOrder = 3
    Text = 'edt3'
  end
  object edt4: TEdit
    Left = 32
    Top = 200
    Width = 441
    Height = 21
    TabOrder = 4
    Text = 'edt3'
  end
  object btn2: TButton
    Left = 152
    Top = 32
    Width = 98
    Height = 25
    Caption = 'HASH'#23383#31526#20018
    TabOrder = 5
    OnClick = btn2Click
  end
  object edt5: TEdit
    Left = 32
    Top = 240
    Width = 441
    Height = 21
    TabOrder = 6
    Text = 'edt3'
  end
  object edt6: TEdit
    Left = 32
    Top = 280
    Width = 441
    Height = 21
    TabOrder = 7
    Text = 'edt3'
  end
  object edt7: TEdit
    Left = 32
    Top = 320
    Width = 441
    Height = 21
    TabOrder = 8
    Text = 'edt3'
  end
  object dlgOpen1: TOpenDialog
    Left = 336
    Top = 8
  end
end
