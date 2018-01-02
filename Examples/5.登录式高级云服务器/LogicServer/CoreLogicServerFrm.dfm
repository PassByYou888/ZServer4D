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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
          #30331#24405#24335#39640#32423#20113#26381#21153#22120
          ''
          ''
          #20351#29992
          ''
          'managerServer'#24517#39035#39318#20808#21551#21160#26381#21153#65292#27599#20010#25805#20316#31995#32479#21482#33021#21551#21160#19968#21488#65292#21487#20197#37096#32626#22810#21488'ManagerServer'#22312#22810#21488#20113#26381#21153#22120#20013
          'DBServer,LoginServer'#21487#20197#19981#29992#25353#39034#24207#21551#21160#65292#20294#26159#21551#21160#21518#24517#39035#35201#21521'ManagerServer'#27880#20876#25165#33021#29983#25928
          ''
          
            #25105#20204#21487#20197#37096#32626#26080#25968#21488'DBServer+LoginServer'#26469#36798#21040#20113#36127#36733#38656#27714#65292#32780#23458#25143#31471#30331#24405#65292#21482#38656#35201#30693#36947#20854#20013#19968#21488'ManagerSer' +
            'ver'#30340'IP'#22320#22336' or '#22495#21517#65292#36825#23545#21160#24577#32452#32593#26377#24040#22823#30340#24110#21161
          #20113#26381#21153#22120#25216#26415#20307#31995#65292#23545#38450#27490#20725#23608#32593#32476'DDos'#65292'CC'#25915#20987#26377#22825#28982#30340#25239#24615
          #22240#20026#19968#20010#26376#30340'DDos'#38450#24481#25104#26412#39640#36798'20'#19975#20197#19978#65292#24517#39035#20351#29992'BGP+'#20113#22564#65292#20854#20013#20113#22564#19968#20010'IP'#30340#31199#37329#23601#20250#30772'10'#19975
          ''
          #22312#20351#29992#26102#65292#24212#35813#20351#29992#20840#21629#20196#34892#30340#25209#22788#29702#26041#24335#26469#21551#21160#21508#20010#19981#21516#30340#26381#21153#22120#65292#20197#33410#30465#26102#38388#24320#38144
          ''
          #30331#24405#24335#39640#32423#20113#26381#21153#22120#65292#23646#20110#21830#19994#26381#21153#22120#26694#26550#65292#24314#35758#22823#23478#22810#33457#26102#38388#30740#31350#36825#37324#30340#23454#29616#29615#33410#65292#26377#19981#28165#26970#65292#21487#20197#21040'QQ'#32676#32473#25105#30041#35328
          ''
          ''
          'LoginServer'#30340#20316#29992
          
            'LoginServer'#21487#20197#22312#25805#20316#31995#32479#20351#29992#25209#22788#29702#21629#20196#34892#27169#24335#19968#27425#21551#21160#22810#21488#65292#24182#33258#21160#27880#20876#21040'ManagerServer'#65292'LoginServ' +
            'er'#30456#24403#20110#19968#21488#19994#21153#26381#21153#22120#65292#21487#20197#21551#21160#26080#25968#21488'LoginServer'#26469#36798#21040#19994#21153#36127#36733#38656#27714
          ''
          ''
          'DBServer'#30340#20316#29992
          'DBServer'#26159#20010#29615#24418#32593#32476#65292#21482#23545'LoginServer'#25552#20379#26381#21153#65292#19968#27425#21487#20197#21551#21160#22810#21488
          
            #26377#29992#25143#30331#24405#26102' LoginServer'#20250#20174'DBServer'#30340#29615#24418#32593#32476#20013' '#23547#25214#26102#38388#26368#25509#36817#30340#29992#25143#26723#26696'CheckOut'#24182'Lock'#65292#28982#21518#21457 +
            #36865#32473'LoginServer'#65292#25509#19979#26469'LoginServer'#25165#33021#22788#29702#30331#24405#35831#27714
          #27880#24847#65306#24403#29992#25143#34987'CheckOut'#26102#65292#29992#25143#23558#19981#33021#30331#24405#65292#31616#21333#26469#29702#35299#23601#26159#19968#20010#24080#21495#19968#27425#21482#33021#30331#24405#19968#21517#29992#25143
          
            #24403#29992#25143#31163#32447#26102#65292'LoginServer'#20250#23558#26412#27425#29992#25143#25805#20316#22788#29702#30340#26723#26696#25171#21253#65292#21457#32473#26102#38388#26368#36828#30340'DBServer'#23384#26723#65292#24182#19988#35299#38145#21018#25165'Lock'#22312'D' +
            'Bserver'#30340#29992#25143'ID'
          ''
          ''
          #20026#20160#20040#35201#29992#29615#24418'DB'#32593#32476
          '1,DBServer'#21482#36127#36131#23384#20648#21644#25552#20379#25968#25454#65292#19981#22788#29702#19994#21153#65292#20551#22914#25105#20204#22312#21518#21488#37096#32626'10'#21488'DBServer'#65292#20415#31435#21051#20855#22791#20102'10'#21488#22791#20221#24674#22797#33021#21147
          '2,DBServer'#19981#19968#23450#22312#26412#22320#65292#20063#26377#21487#33021#22312#36828#31243#65292#20219#20309#19968#20010#20114#32852#32593#21040#36798#30340#22320#26041#65292#29978#33267#22312#20844#21496#20869#37096#65292#22240#20026#36825#26159#20113#26694#26550
          ''
          'byQQ600585'
          '2017-11-24')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object OptTabSheet: TTabSheet
      Caption = 'Options'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
