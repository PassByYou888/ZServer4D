object zsGatewayConfigureForm: TzsGatewayConfigureForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 20
  Caption = 'ZServer4D Gateway configure...'
  ClientHeight = 497
  ClientWidth = 1025
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
  object CfgPageControl: TPageControl
    Left = 0
    Top = 92
    Width = 342
    Height = 317
    ActivePage = ServTabSheet
    TabOrder = 4
    object ServTabSheet: TTabSheet
      Caption = 'NAT Service'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label2: TLabel
        Left = 51
        Top = 41
        Width = 51
        Height = 13
        Caption = 'Share Port'
      end
      object Label4: TLabel
        Left = 14
        Top = 14
        Width = 88
        Height = 13
        Caption = 'Remote Service IP'
      end
      object SharePortComboBox: TComboBox
        Left = 108
        Top = 38
        Width = 89
        Height = 21
        TabOrder = 0
        Text = '80'
      end
      object ListView: TListView
        Left = 14
        Top = 103
        Width = 299
        Height = 178
        Columns = <
          item
            AutoSize = True
            Caption = 'NAT Tunnel Name'
          end
          item
            Caption = 'Listen Port'
            Width = 80
          end>
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 6
        ViewStyle = vsReport
      end
      object IPComboBox: TComboBox
        Left = 108
        Top = 11
        Width = 139
        Height = 21
        TabOrder = 2
        Text = '127.0.0.1'
      end
      object AddItmButton: TButton
        Left = 14
        Top = 72
        Width = 67
        Height = 25
        Caption = 'Add'
        TabOrder = 3
        OnClick = AddItmButtonClick
      end
      object BrowseButton: TButton
        Left = 253
        Top = 9
        Width = 60
        Height = 25
        Caption = 'Browse'
        TabOrder = 1
        OnClick = BrowseButtonClick
      end
      object DeleteItmButton: TButton
        Left = 87
        Top = 72
        Width = 67
        Height = 25
        Caption = 'Delete'
        TabOrder = 4
        OnClick = DeleteItmButtonClick
      end
      object saveButton: TButton
        Left = 246
        Top = 72
        Width = 67
        Height = 25
        Caption = 'Save'
        TabOrder = 5
        OnClick = saveButtonClick
      end
    end
  end
  object NatPortEdit: TLabeledEdit
    Left = 148
    Top = 0
    Width = 59
    Height = 21
    EditLabel.Width = 113
    EditLabel.Height = 13
    EditLabel.Caption = 'NAT Bridge Listen Port  '
    LabelPosition = lpLeft
    TabOrder = 0
    Text = '4799'
  end
  object WebPortEdit: TLabeledEdit
    Left = 148
    Top = 27
    Width = 59
    Height = 21
    EditLabel.Width = 141
    EditLabel.Height = 13
    EditLabel.Caption = 'State with Web Service Port  '
    LabelPosition = lpLeft
    TabOrder = 1
    Text = '4798'
  end
  object StartServerListenButton: TButton
    Left = 0
    Top = 456
    Width = 130
    Height = 41
    Caption = 'Start NAT Service'
    ElevationRequired = True
    TabOrder = 7
    OnClick = StartServerListenButtonClick
  end
  object StartClientListenButton: TButton
    Left = 136
    Top = 456
    Width = 130
    Height = 41
    Caption = 'Start NAT Client'
    ElevationRequired = True
    TabOrder = 8
    OnClick = StartClientListenButtonClick
  end
  object AboutButton: TButton
    Left = 288
    Top = 471
    Width = 54
    Height = 26
    Caption = 'About'
    TabOrder = 9
    OnClick = AboutButtonClick
  end
  object TokenEdit: TLabeledEdit
    Left = 148
    Top = 54
    Width = 145
    Height = 21
    EditLabel.Width = 76
    EditLabel.Height = 13
    EditLabel.Caption = 'security Token  '
    LabelPosition = lpLeft
    TabOrder = 2
  end
  object RandomTokenButton: TButton
    Left = 299
    Top = 52
    Width = 43
    Height = 25
    Caption = 'Rnd'
    TabOrder = 3
    OnClick = RandomTokenButtonClick
  end
  object X86RadioButton: TRadioButton
    Left = 8
    Top = 416
    Width = 113
    Height = 17
    Caption = 'X86 architecture'
    TabOrder = 5
  end
  object x64RadioButton: TRadioButton
    Left = 138
    Top = 416
    Width = 113
    Height = 17
    Caption = 'X64 architecture'
    TabOrder = 6
  end
  object Memo: TMemo
    Left = 348
    Top = 0
    Width = 677
    Height = 497
    BorderStyle = bsNone
    OEMConvert = True
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 10
    Visible = False
    WantTabs = True
    WordWrap = False
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 212
    Top = 188
  end
end
