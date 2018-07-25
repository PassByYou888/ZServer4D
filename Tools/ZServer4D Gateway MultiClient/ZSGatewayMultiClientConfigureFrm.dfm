object ZSGatewayMultiClientConfigureForm: TZSGatewayMultiClientConfigureForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 20
  Caption = 'ZSGateway Multi Client V1.0 create by qq600585'
  ClientHeight = 610
  ClientWidth = 1332
  Color = clBtnFace
  DoubleBuffered = True
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
  object RemoteIPEdit: TLabeledEdit
    Left = 0
    Top = 26
    Width = 214
    Height = 21
    EditLabel.Width = 136
    EditLabel.Height = 13
    EditLabel.Caption = 'Remote Configure IP or DNS'
    TabOrder = 0
    Text = '192.168.111.132'
    OnKeyUp = RemoteIPEditKeyUp
  end
  object RemotePortEdit: TLabeledEdit
    Left = 220
    Top = 26
    Width = 41
    Height = 21
    EditLabel.Width = 20
    EditLabel.Height = 13
    EditLabel.Caption = 'Port'
    TabOrder = 1
    Text = '4797'
    OnKeyUp = RemoteIPEditKeyUp
  end
  object GetConfigureButton: TButton
    Left = 267
    Top = 22
    Width = 97
    Height = 25
    Caption = 'Refresh Server'
    TabOrder = 2
    OnClick = GetConfigureButtonClick
  end
  object ControlPanel: TPanel
    Left = 0
    Top = 53
    Width = 385
    Height = 557
    BevelOuter = bvNone
    TabOrder = 3
    Visible = False
    object Label1: TLabel
      Left = 0
      Top = 491
      Width = 103
      Height = 13
      Caption = 'gateway architecture'
    end
    object IPV6CheckBox: TCheckBox
      Left = 0
      Top = 467
      Width = 162
      Height = 17
      Caption = 'Activted IPV6 Gateway'
      TabOrder = 0
    end
    object X86RadioButton: TRadioButton
      Left = 113
      Top = 490
      Width = 49
      Height = 17
      Caption = 'X86'
      TabOrder = 1
    end
    object x64RadioButton: TRadioButton
      Left = 168
      Top = 490
      Width = 49
      Height = 17
      Caption = 'X64'
      TabOrder = 2
    end
    object AboutButton: TButton
      Left = 136
      Top = 514
      Width = 54
      Height = 41
      Caption = 'About'
      TabOrder = 4
      OnClick = AboutButtonClick
    end
    object StartClientListenButton: TButton
      Left = 0
      Top = 514
      Width = 130
      Height = 41
      Caption = 'Start NAT Client'
      ElevationRequired = True
      TabOrder = 3
      OnClick = StartClientListenButtonClick
    end
    object TabControl: TTabControl
      Left = 0
      Top = 0
      Width = 385
      Height = 456
      MultiLine = True
      TabOrder = 5
      OnChange = TabControlChange
      object RemoteInfoLabel: TLabel
        Left = 14
        Top = 82
        Width = 355
        Height = 56
        AutoSize = False
        Caption = 'No Catpure infomation'
        Layout = tlCenter
        WordWrap = True
      end
      object OpenWebButton: TButton
        Left = 14
        Top = 51
        Width = 96
        Height = 25
        Caption = 'Visit Statistics'
        TabOrder = 0
        OnClick = OpenWebButtonClick
      end
      object TokenEdit: TLabeledEdit
        Left = 146
        Top = 55
        Width = 223
        Height = 21
        EditLabel.Width = 30
        EditLabel.Height = 13
        EditLabel.Caption = 'token '
        LabelPosition = lpLeft
        TabOrder = 1
      end
      object ListView: TListView
        Left = 14
        Top = 144
        Width = 355
        Height = 297
        Checkboxes = True
        Columns = <
          item
            AutoSize = True
            Caption = 'NAT Tunnel Name'
          end
          item
            Caption = 'Remote Map'
            Width = 80
          end
          item
            Caption = 'Local Port'
            Width = 80
          end>
        HideSelection = False
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        PopupMenu = ListPopupMenu
        TabOrder = 2
        ViewStyle = vsReport
      end
    end
  end
  object Memo: TMemo
    Left = 391
    Top = 0
    Width = 941
    Height = 610
    BevelOuter = bvRaised
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
    Visible = False
    WantTabs = True
    WordWrap = False
  end
  object sysProcessTimer: TTimer
    OnTimer = sysProcessTimerTimer
    Left = 252
    Top = 116
  end
  object NetworkTimer: TTimer
    Interval = 100
    OnTimer = NetworkTimerTimer
    Left = 200
    Top = 192
  end
  object ListPopupMenu: TPopupMenu
    Left = 220
    Top = 357
    object ModifyLocalPort1: TMenuItem
      Caption = 'Modify Local Port'
      OnClick = ModifyLocalPort1Click
    end
  end
end
