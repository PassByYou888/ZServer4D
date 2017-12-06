object fRenameMM: TfRenameMM
  Left = 334
  Top = 159
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Rename MM'
  ClientHeight = 75
  ClientWidth = 255
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 20
    Top = 12
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object eMMName: TEdit
    Left = 56
    Top = 8
    Width = 185
    Height = 21
    TabOrder = 0
  end
  object bOK: TBitBtn
    Left = 88
    Top = 36
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object bCancel: TBitBtn
    Left = 164
    Top = 36
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
end
