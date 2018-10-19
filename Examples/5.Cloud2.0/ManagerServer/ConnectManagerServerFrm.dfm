object ConnectManagerServerForm: TConnectManagerServerForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 10
  Caption = 'registered server...'
  ClientHeight = 209
  ClientWidth = 227
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
    Left = 89
    Top = 0
    Width = 138
    Height = 21
    EditLabel.Width = 75
    EditLabel.Height = 13
    EditLabel.Caption = 'Registed name:'
    LabelPosition = lpLeft
    TabOrder = 0
  end
  object RegServerHostEdit: TLabeledEdit
    Left = 89
    Top = 27
    Width = 137
    Height = 21
    EditLabel.Width = 71
    EditLabel.Height = 13
    EditLabel.Caption = 'Registed Host:'
    LabelPosition = lpLeft
    TabOrder = 1
  end
  object ServerManagerHostEdit: TLabeledEdit
    Left = 90
    Top = 96
    Width = 137
    Height = 21
    EditLabel.Width = 80
    EditLabel.Height = 13
    EditLabel.Caption = 'manager server:'
    LabelPosition = lpLeft
    TabOrder = 2
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
  object ManCliRecvPortEdit: TLabeledEdit
    Left = 89
    Top = 123
    Width = 56
    Height = 21
    EditLabel.Width = 65
    EditLabel.Height = 13
    EditLabel.Caption = 'Receive Port:'
    LabelPosition = lpLeft
    TabOrder = 5
  end
  object ManCliSendPortEdit: TLabeledEdit
    Left = 89
    Top = 150
    Width = 56
    Height = 21
    EditLabel.Width = 51
    EditLabel.Height = 13
    EditLabel.Caption = 'Send Port:'
    LabelPosition = lpLeft
    TabOrder = 6
  end
end
