object NewDBOptForm: TNewDBOptForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 20
  Caption = 'New Database Options.'
  ClientHeight = 168
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object FixedStringEdit: TLabeledEdit
    Left = 134
    Top = 95
    Width = 41
    Height = 21
    EditLabel.Width = 125
    EditLabel.Height = 13
    EditLabel.Caption = 'fixed string size:(10..255)'
    LabelPosition = lpLeft
    TabOrder = 0
    Text = '65'
  end
  object OkButton: TButton
    Left = 3
    Top = 143
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 84
    Top = 143
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 401
    Height = 89
    Lines.Strings = (
      
        'Fixed string is directory,item name and description name of leng' +
        'th.'
      'fixed string size is a one-time parameter that'
      'cannot be modified after creation!'
      ''
      'default fixed string size: 65')
    ReadOnly = True
    TabOrder = 3
  end
end
