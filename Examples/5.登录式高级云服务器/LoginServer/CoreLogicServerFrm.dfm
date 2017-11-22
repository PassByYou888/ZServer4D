object CoreLogicServerForm: TCoreLogicServerForm
  Left = 0
  Top = 0
  Caption = 'Core Logic service...'
  ClientHeight = 212
  ClientWidth = 584
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 584
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 0
    object Bevel1: TBevel
      Left = 193
      Top = 5
      Width = 24
      Height = 31
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 185
      ExplicitTop = 4
    end
    object Bevel3: TBevel
      Left = 345
      Top = 5
      Width = 24
      Height = 31
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 395
      ExplicitTop = 4
    end
    object Bevel2: TBevel
      Left = 481
      Top = 5
      Width = 24
      Height = 31
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 507
      ExplicitTop = 4
    end
    object StartServiceButton: TButton
      Left = 5
      Top = 5
      Width = 116
      Height = 31
      Align = alLeft
      Caption = 'Start Service'
      TabOrder = 0
      OnClick = StartServiceButtonClick
    end
    object StopServiceButton: TButton
      Left = 121
      Top = 5
      Width = 72
      Height = 31
      Align = alLeft
      Caption = 'Stop'
      TabOrder = 1
      OnClick = StopServiceButtonClick
    end
    object connectButton: TButton
      Left = 217
      Top = 5
      Width = 128
      Height = 31
      Align = alLeft
      Caption = 'Registered server'
      TabOrder = 2
      OnClick = connectButtonClick
    end
    object RefreshServerListButton: TButton
      Left = 369
      Top = 5
      Width = 112
      Height = 31
      Align = alLeft
      Caption = 'Refresh Server Tree'
      TabOrder = 3
      OnClick = RefreshServerListButtonClick
    end
    object StatusCheckBox: TCheckBox
      Left = 505
      Top = 5
      Width = 56
      Height = 31
      Align = alLeft
      Caption = 'Status'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 41
    Width = 584
    Height = 171
    ActivePage = StatusTabSheet
    Align = alClient
    TabOrder = 1
    object StatusTabSheet: TTabSheet
      Caption = 'Log Status'
      object Memo: TMemo
        Left = 0
        Top = 0
        Width = 576
        Height = 143
        Align = alClient
        Color = clBlack
        Font.Charset = ANSI_CHARSET
        Font.Color = clLime
        Font.Height = -12
        Font.Name = 'Consolas'
        Font.Style = [fsBold]
        Lines.Strings = (
          'ZServer4D'#26159#19968#27454#26381#21153#22120#20013#38388#20214
          #27492'Demo'#28436#31034#20102#26080#38480#20113#37096#32626#30340#26381#21153#22120#31995#32479#26426#21046#65292#35814#35265#38468#38543#25216#26415#25991#26723)
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object OptTabSheet: TTabSheet
      Caption = 'Options'
      ImageIndex = 2
      object BindIPEdit: TLabeledEdit
        Left = 136
        Top = 11
        Width = 89
        Height = 21
        EditLabel.Width = 78
        EditLabel.Height = 13
        EditLabel.Caption = 'Bind IP Address '
        LabelPosition = lpLeft
        TabOrder = 0
        Text = '0.0.0.0'
      end
      object RecvPortEdit: TLabeledEdit
        Left = 136
        Top = 38
        Width = 53
        Height = 21
        EditLabel.Width = 129
        EditLabel.Height = 13
        EditLabel.Caption = 'Logic Receive Service port '
        LabelPosition = lpLeft
        TabOrder = 1
        Text = '3339'
      end
      object SendPortEdit: TLabeledEdit
        Left = 136
        Top = 65
        Width = 53
        Height = 21
        EditLabel.Width = 114
        EditLabel.Height = 13
        EditLabel.Caption = 'Logic Send service port '
        LabelPosition = lpLeft
        TabOrder = 2
        Text = '3338'
      end
    end
    object ConnectTreeTabSheet: TTabSheet
      Caption = 'connect tree'
      ImageIndex = 1
      object TreeView: TTreeView
        Left = 0
        Top = 0
        Width = 576
        Height = 143
        Align = alClient
        Indent = 19
        TabOrder = 0
      end
    end
  end
  object ProgressTimer: TTimer
    Interval = 100
    OnTimer = ProgressTimerTimer
    Left = 200
    Top = 128
  end
  object AntiIDLETimer: TTimer
    Interval = 5000
    OnTimer = AntiIDLETimerTimer
    Left = 72
    Top = 128
  end
  object AppEvents: TApplicationEvents
    OnException = AppEventsException
    Left = 320
    Top = 128
  end
end
