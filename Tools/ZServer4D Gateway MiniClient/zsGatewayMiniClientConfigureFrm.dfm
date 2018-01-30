object zsGatewayMiniClientConfigureForm: TzsGatewayMiniClientConfigureForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 20
  Caption = 'ZServer4D Gateway with Mini Client...'
  ClientHeight = 610
  ClientWidth = 1065
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
  object Memo: TMemo
    Left = 359
    Top = 0
    Width = 706
    Height = 610
    BevelOuter = bvRaised
    OEMConvert = True
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 4
    Visible = False
    WantTabs = True
    WordWrap = False
  end
  object RemoteIPEdit: TLabeledEdit
    Left = 0
    Top = 26
    Width = 214
    Height = 21
    EditLabel.Width = 136
    EditLabel.Height = 13
    EditLabel.Caption = 'Remote Configure IP or DNS'
    TabOrder = 0
    Text = '127.0.0.1'
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
  end
  object GetConfigureButton: TButton
    Left = 267
    Top = 22
    Width = 86
    Height = 25
    Caption = 'Get Configure'
    TabOrder = 2
    OnClick = GetConfigureButtonClick
  end
  object ControlPanel: TPanel
    Left = 0
    Top = 53
    Width = 342
    Height = 557
    BevelOuter = bvNone
    Caption = 'ControlPanel'
    TabOrder = 3
    Visible = False
    object Label1: TLabel
      Left = 0
      Top = 494
      Width = 103
      Height = 13
      Caption = 'gateway architecture'
    end
    object CfgPageControl: TPageControl
      Left = 0
      Top = 0
      Width = 342
      Height = 449
      ActivePage = ServTabSheet
      TabOrder = 0
      object ServTabSheet: TTabSheet
        Caption = 'Remote NAT'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object RemoteInfoLabel: TLabel
          Left = 14
          Top = 34
          Width = 299
          Height = 70
          AutoSize = False
          Caption = 'No Catpure infomation'
          Layout = tlCenter
          WordWrap = True
        end
        object ListView: TListView
          Left = 14
          Top = 110
          Width = 299
          Height = 298
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
          TabOrder = 1
          ViewStyle = vsReport
        end
        object OpenWebButton: TButton
          Left = 14
          Top = 3
          Width = 96
          Height = 25
          Caption = 'Visit Statistics'
          TabOrder = 0
          OnClick = OpenWebButtonClick
        end
        object TokenEdit: TLabeledEdit
          Left = 146
          Top = 7
          Width = 167
          Height = 21
          EditLabel.Width = 30
          EditLabel.Height = 13
          EditLabel.Caption = 'token '
          LabelPosition = lpLeft
          TabOrder = 2
        end
      end
    end
    object IPV6CheckBox: TCheckBox
      Left = 0
      Top = 470
      Width = 162
      Height = 17
      Caption = 'Activted IPV6 Gateway'
      TabOrder = 1
    end
    object X86RadioButton: TRadioButton
      Left = 113
      Top = 493
      Width = 49
      Height = 17
      Caption = 'X86'
      TabOrder = 2
    end
    object x64RadioButton: TRadioButton
      Left = 168
      Top = 493
      Width = 49
      Height = 17
      Caption = 'X64'
      TabOrder = 3
    end
    object AboutButton: TButton
      Left = 136
      Top = 516
      Width = 54
      Height = 41
      Caption = 'About'
      TabOrder = 5
      OnClick = AboutButtonClick
    end
    object StartClientListenButton: TButton
      Left = 0
      Top = 516
      Width = 130
      Height = 41
      Caption = 'Start NAT Client'
      ElevationRequired = True
      TabOrder = 4
      OnClick = StartClientListenButtonClick
    end
    object MinimizedToTaskButton: TButton
      Left = 263
      Top = 516
      Width = 75
      Height = 41
      Caption = 'Minimized'
      TabOrder = 6
      OnClick = MinimizedToTaskButtonClick
    end
  end
  object sysProcessTimer: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = sysProcessTimerTimer
    Left = 252
    Top = 116
  end
  object TrayIcon: TTrayIcon
    Hint = 'ZServer4D Gateway with Mini Client...'
    BalloonHint = 'ZServer4D Gateway with Mini Client...'
    BalloonTitle = 'ZServer4D Gateway with Mini Client...'
    OnClick = TrayIconClick
    Left = 264
    Top = 272
  end
  object ListPopupMenu: TPopupMenu
    Left = 220
    Top = 357
    object ModifyLocalPort1: TMenuItem
      Caption = 'Modify Local Port'
      OnClick = ModifyLocalPort1Click
    end
  end
  object NetworkTimer: TTimer
    Enabled = False
    Interval = 10
    OnTimer = NetworkTimerTimer
    Left = 200
    Top = 192
  end
end
