object DBViewCliForm: TDBViewCliForm
  Left = 0
  Top = 0
  Caption = 'DB Viewer...'
  ClientHeight = 433
  ClientWidth = 870
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    870
    433)
  PixelsPerInch = 96
  TextHeight = 13
  object connectButton: TButton
    Left = 16
    Top = 8
    Width = 75
    Height = 25
    Caption = 'connect'
    TabOrder = 0
    OnClick = connectButtonClick
  end
  object DisconnectButton: TButton
    Left = 16
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 1
    OnClick = DisconnectButtonClick
  end
  object PageControl: TPageControl
    Left = 112
    Top = 8
    Width = 740
    Height = 407
    ActivePage = DBTabSheet
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object LogTabSheet: TTabSheet
      Caption = 'Log'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo: TMemo
        Left = 0
        Top = 0
        Width = 732
        Height = 379
        Align = alClient
        TabOrder = 0
      end
    end
    object DBTabSheet: TTabSheet
      Caption = 'DB'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Splitter1: TSplitter
        Left = 201
        Top = 0
        Height = 379
        AutoSnap = False
        ExplicitLeft = 376
        ExplicitTop = 112
        ExplicitHeight = 100
      end
      object DBListView: TListView
        Left = 0
        Top = 0
        Width = 201
        Height = 379
        Align = alLeft
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Name'
          end>
        ShowColumnHeaders = False
        TabOrder = 0
        ViewStyle = vsReport
        OnClick = DBListViewClick
      end
      object DBStringGrid: TStringGrid
        Left = 204
        Top = 0
        Width = 528
        Height = 379
        Align = alClient
        BorderStyle = bsNone
        ColCount = 2
        DefaultColWidth = 128
        DoubleBuffered = True
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing, goTabs]
        ParentColor = True
        ParentDoubleBuffered = False
        TabOrder = 1
        ColWidths = (
          172
          211)
        RowHeights = (
          24
          24)
      end
    end
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 424
    Top = 208
  end
end
