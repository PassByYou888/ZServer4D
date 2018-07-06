object zsGatewayMiniServConfigureForm: TzsGatewayMiniServConfigureForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 20
  Caption = 'ZServer4D Gateway configure with Mini Service...'
  ClientHeight = 617
  ClientWidth = 1277
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 552
    Width = 103
    Height = 13
    Caption = 'gateway architecture'
  end
  object CfgPageControl: TPageControl
    Left = 2
    Top = 128
    Width = 342
    Height = 369
    ActivePage = ServTabSheet
    TabOrder = 6
    object ServTabSheet: TTabSheet
      Caption = 'Local NAT Service'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label2: TLabel
        Left = 19
        Top = 17
        Width = 51
        Height = 13
        Caption = 'Share Port'
      end
      object SharePortComboBox: TComboBox
        Left = 76
        Top = 14
        Width = 89
        Height = 21
        TabOrder = 0
        Text = '80'
      end
      object ListView: TListView
        Left = 14
        Top = 71
        Width = 299
        Height = 257
        Columns = <
          item
            AutoSize = True
            Caption = 'NAT Tunnel Name'
          end
          item
            Caption = 'Share Port'
            Width = 80
          end>
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 4
        ViewStyle = vsReport
      end
      object AddItmButton: TButton
        Left = 14
        Top = 40
        Width = 67
        Height = 25
        Caption = 'Add'
        TabOrder = 1
        OnClick = AddItmButtonClick
      end
      object DeleteItmButton: TButton
        Left = 87
        Top = 40
        Width = 67
        Height = 25
        Caption = 'Delete'
        TabOrder = 2
        OnClick = DeleteItmButtonClick
      end
      object saveButton: TButton
        Left = 246
        Top = 40
        Width = 67
        Height = 25
        Caption = 'Save'
        TabOrder = 3
        OnClick = saveButtonClick
      end
    end
  end
  object NatPortEdit: TLabeledEdit
    Left = 144
    Top = 32
    Width = 59
    Height = 21
    EditLabel.Width = 113
    EditLabel.Height = 13
    EditLabel.Caption = 'NAT Bridge Listen Port  '
    LabelPosition = lpLeft
    TabOrder = 2
    Text = '4799'
  end
  object WebPortEdit: TLabeledEdit
    Left = 144
    Top = 59
    Width = 59
    Height = 21
    EditLabel.Width = 141
    EditLabel.Height = 13
    EditLabel.Caption = 'State with Web Service Port  '
    LabelPosition = lpLeft
    TabOrder = 3
    Text = '4798'
  end
  object StartServerListenButton: TButton
    Left = 2
    Top = 576
    Width = 130
    Height = 41
    Caption = 'Start NAT Service'
    ElevationRequired = True
    TabOrder = 10
    OnClick = StartServerListenButtonClick
  end
  object AboutButton: TButton
    Left = 138
    Top = 576
    Width = 54
    Height = 41
    Caption = 'About'
    TabOrder = 11
    OnClick = AboutButtonClick
  end
  object TokenEdit: TLabeledEdit
    Left = 144
    Top = 86
    Width = 145
    Height = 21
    EditLabel.Width = 76
    EditLabel.Height = 13
    EditLabel.Caption = 'security Token  '
    LabelPosition = lpLeft
    TabOrder = 4
  end
  object RandomTokenButton: TButton
    Left = 295
    Top = 84
    Width = 43
    Height = 25
    Caption = 'Rnd'
    TabOrder = 5
    OnClick = RandomTokenButtonClick
  end
  object X86RadioButton: TRadioButton
    Left = 123
    Top = 551
    Width = 49
    Height = 17
    Caption = 'X86'
    TabOrder = 8
  end
  object x64RadioButton: TRadioButton
    Left = 178
    Top = 551
    Width = 49
    Height = 17
    Caption = 'X64'
    TabOrder = 9
  end
  object Memo: TMemo
    Left = 350
    Top = 0
    Width = 927
    Height = 617
    BorderStyle = bsNone
    OEMConvert = True
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 13
    Visible = False
    WantTabs = True
    WordWrap = False
  end
  object RemoteConfigurePortEdit: TLabeledEdit
    Left = 144
    Top = 5
    Width = 59
    Height = 21
    EditLabel.Width = 129
    EditLabel.Height = 13
    EditLabel.Caption = 'Remote configure Service  '
    LabelPosition = lpLeft
    TabOrder = 0
    Text = '4797'
  end
  object EnabledRemoteConfigureCheckBox: TCheckBox
    Left = 218
    Top = 8
    Width = 101
    Height = 17
    Caption = 'Enabled'
    TabOrder = 1
  end
  object IPV6CheckBox: TCheckBox
    Left = 10
    Top = 519
    Width = 162
    Height = 17
    Caption = 'Activted IPV6 Gateway'
    TabOrder = 7
  end
  object MinimizedToTaskButton: TButton
    Left = 269
    Top = 576
    Width = 75
    Height = 41
    Caption = 'Minimized'
    TabOrder = 12
    OnClick = MinimizedToTaskButtonClick
  end
  object sysProcessTimer: TTimer
    Enabled = False
    OnTimer = sysProcessTimerTimer
    Left = 252
    Top = 116
  end
  object GatewayTrayIcon: TTrayIcon
    Hint = 'ZServer4D Gateway configure with Mini Service...'
    BalloonHint = 'ZServer4D Gateway configure with Mini Service...'
    BalloonTitle = 'ZServer4D Gateway configure with Mini Service...'
    OnClick = GatewayTrayIconClick
    Left = 248
    Top = 256
  end
  object NetworkTimer: TTimer
    Enabled = False
    Interval = 10
    OnTimer = NetworkTimerTimer
    Left = 200
    Top = 192
  end
end
