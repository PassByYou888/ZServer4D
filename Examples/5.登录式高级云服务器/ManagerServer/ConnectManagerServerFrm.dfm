object ConnectManagerServerForm: TConnectManagerServerForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 50
  Caption = 'registered server...'
  ClientHeight = 209
  ClientWidth = 345
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object RegNameEdit: TLabeledEdit
    Left = 120
    Top = 0
    Width = 225
    Height = 21
    EditLabel.Width = 75
    EditLabel.Height = 13
    EditLabel.Caption = 'Registed name:'
    LabelPosition = lpLeft
    TabOrder = 0
    Text = '127.0.0.1'
  end
  object RegServerHostEdit: TLabeledEdit
    Left = 120
    Top = 27
    Width = 225
    Height = 21
    EditLabel.Width = 71
    EditLabel.Height = 13
    EditLabel.Caption = 'Registed Host:'
    LabelPosition = lpLeft
    TabOrder = 1
    Text = '127.0.0.1'
  end
  object ManagerServerHostEdit: TLabeledEdit
    Left = 120
    Top = 80
    Width = 225
    Height = 21
    EditLabel.Width = 105
    EditLabel.Height = 13
    EditLabel.Caption = 'manager server Host:'
    LabelPosition = lpLeft
    TabOrder = 2
    Text = '127.0.0.1'
  end
  object ConnectButton: TButton
    Left = 0
    Top = 184
    Width = 75
    Height = 25
    Caption = 'connect'
    ModalResult = 1
    TabOrder = 3
  end
  object CancelButton: TButton
    Left = 81
    Top = 184
    Width = 75
    Height = 25
    Caption = '&Cancel (ESC)'
    ModalResult = 2
    TabOrder = 4
  end
end
