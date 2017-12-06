object fGraphs: TfGraphs
  Left = 243
  Top = 158
  AutoScroll = False
  Caption = 
    'Graphs - Scaled so that best score is 100%, Longer bars are bett' +
    'er (Faster and/or lower address space usage)'
  ClientHeight = 718
  ClientWidth = 937
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 800
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object pBottom: TPanel
    Left = 0
    Top = 686
    Width = 937
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      937
      32)
    object lGraph: TLabel
      Left = 588
      Top = 8
      Width = 32
      Height = 13
      Anchors = [akRight, akBottom]
      Caption = 'MM(s):'
    end
    object lResults: TLabel
      Left = 262
      Top = 8
      Width = 44
      Height = 13
      Anchors = [akRight, akBottom]
      Caption = 'Result(s):'
    end
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 68
      Height = 13
      Caption = 'Benchmark(s):'
    end
    object bbClose: TBitBtn
      Left = 864
      Top = 4
      Width = 67
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 3
      TabStop = False
      Kind = bkClose
    end
    object cbMemoryManager: TComboBox
      Left = 622
      Top = 5
      Width = 153
      Height = 21
      AutoComplete = False
      Style = csDropDownList
      Anchors = [akRight, akBottom]
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 2
      Text = 'All Memory Managers'
      OnChange = GraphOptionChange
      Items.Strings = (
        'All Memory Managers')
    end
    object cbResults: TComboBox
      Left = 310
      Top = 5
      Width = 193
      Height = 21
      AutoComplete = False
      Style = csDropDownList
      Anchors = [akRight, akBottom]
      ItemHeight = 13
      ItemIndex = 3
      TabOrder = 1
      Text = 'Speed, Memory & Combined Score'
      OnChange = GraphOptionChange
      Items.Strings = (
        'Speed Score'
        'Memory Usage Score'
        'Combined Weighted Score'
        'Speed, Memory & Combined Score')
    end
    object bSaveResults: TBitBtn
      Left = 792
      Top = 4
      Width = 67
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Save'
      TabOrder = 4
      OnClick = bSaveResultsClick
    end
    object cbBenchmarks: TComboBox
      Left = 80
      Top = 5
      Width = 164
      Height = 21
      AutoComplete = False
      Style = csDropDownList
      Anchors = [akLeft, akRight, akBottom]
      DropDownCount = 20
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'All Benchmarks'
      OnChange = GraphOptionChange
      Items.Strings = (
        'All Benchmarks')
    end
    object cbResultType: TComboBox
      Left = 502
      Top = 5
      Width = 73
      Height = 21
      Style = csDropDownList
      Anchors = [akRight, akBottom]
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 5
      Text = 'Summary'
      OnChange = GraphOptionChange
      Items.Strings = (
        'Summary'
        'Detail')
    end
  end
  object Chart: TChart
    Left = 0
    Top = 0
    Width = 937
    Height = 686
    AllowPanning = pmNone
    AllowZoom = False
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    MarginBottom = 0
    MarginTop = 3
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    BottomAxis.Grid.Color = clSilver
    BottomAxis.Grid.SmallDots = True
    BottomAxis.MinorGrid.Style = psDot
    BottomAxis.MinorGrid.SmallDots = True
    BottomAxis.MinorTickCount = 1
    LeftAxis.ExactDateTime = False
    LeftAxis.Grid.Color = clBlack
    LeftAxis.Grid.Style = psSolid
    LeftAxis.GridCentered = True
    LeftAxis.Inverted = True
    Legend.ColorWidth = 30
    Legend.DividingLines.Color = clGray
    Legend.DividingLines.Style = psDot
    Legend.DividingLines.Visible = True
    Legend.HorizMargin = 10
    Legend.Inverted = True
    Legend.LegendStyle = lsSeries
    Legend.ShadowColor = clSilver
    Legend.ShadowSize = 2
    Legend.TextStyle = ltsPlain
    View3D = False
    Align = alClient
    BevelOuter = bvNone
    BevelWidth = 0
    TabOrder = 1
  end
  object SaveDialog: TSaveDialog
    FileName = 'MMBenchmarkResults.txt'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 508
    Top = 472
  end
end
