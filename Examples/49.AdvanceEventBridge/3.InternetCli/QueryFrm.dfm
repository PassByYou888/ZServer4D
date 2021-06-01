object QueryForm: TQueryForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 10
  Caption = 'Query.'
  ClientHeight = 321
  ClientWidth = 1317
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmExplicit
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 896
    Top = 7
    Width = 98
    Height = 13
    Caption = 'Format Json source:'
  end
  object NameEdit: TLabeledEdit
    Left = 40
    Top = 2
    Width = 41
    Height = 21
    EditLabel.Width = 31
    EditLabel.Height = 13
    EditLabel.Caption = 'Name:'
    LabelPosition = lpLeft
    TabOrder = 0
  end
  object PhoneEdit: TLabeledEdit
    Left = 128
    Top = 2
    Width = 41
    Height = 21
    EditLabel.Width = 34
    EditLabel.Height = 13
    EditLabel.Caption = 'Phone:'
    LabelPosition = lpLeft
    TabOrder = 1
  end
  object CommentEdit: TLabeledEdit
    Left = 232
    Top = 2
    Width = 41
    Height = 21
    EditLabel.Width = 49
    EditLabel.Height = 13
    EditLabel.Caption = 'Comment:'
    LabelPosition = lpLeft
    TabOrder = 2
  end
  object QueryButton: TButton
    Left = 359
    Top = 0
    Width = 75
    Height = 25
    Caption = 'Query'
    TabOrder = 4
    OnClick = QueryButtonClick
  end
  object ListView: TListView
    Left = 0
    Top = 31
    Width = 513
    Height = 290
    Columns = <
      item
        Caption = 'Name'
        Width = 150
      end
      item
        Caption = 'Phone'
        Width = 150
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 5
    ViewStyle = vsReport
    OnCreateItemClass = ListViewCreateItemClass
    OnSelectItem = ListViewSelectItem
  end
  object infoPanel: TPanel
    Left = 519
    Top = 1
    Width = 369
    Height = 319
    BorderStyle = bsSingle
    TabOrder = 6
    object Label1: TLabel
      Left = 19
      Top = 62
      Width = 47
      Height = 13
      Caption = 'comment:'
    end
    object Label2: TLabel
      Left = 29
      Top = 143
      Width = 37
      Height = 13
      Caption = 'Picture:'
    end
    object InfoNameEdit: TLabeledEdit
      Left = 72
      Top = 8
      Width = 121
      Height = 21
      Color = clBtnFace
      EditLabel.Width = 31
      EditLabel.Height = 13
      EditLabel.Caption = 'Name:'
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 0
    end
    object InfoPhoneEdit: TLabeledEdit
      Left = 72
      Top = 35
      Width = 145
      Height = 21
      Color = clBtnFace
      EditLabel.Width = 34
      EditLabel.Height = 13
      EditLabel.Caption = 'Phone:'
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 1
    end
    object InfoCommentMemo: TMemo
      Left = 72
      Top = 62
      Width = 289
      Height = 75
      Color = clBtnFace
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 2
      WordWrap = False
    end
    object PicturePanel: TPanel
      Left = 72
      Top = 143
      Width = 289
      Height = 162
      BorderStyle = bsSingle
      TabOrder = 3
      TabStop = True
      object Image: TImage
        Left = 1
        Top = 1
        Width = 283
        Height = 156
        Align = alClient
        ParentShowHint = False
        Proportional = True
        ShowHint = False
        ExplicitLeft = 56
        ExplicitTop = 80
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
  end
  object ReverseCheckBox: TCheckBox
    Left = 279
    Top = 4
    Width = 66
    Height = 17
    Caption = 'Reverse'
    TabOrder = 3
  end
  object jsonMemo: TMemo
    Left = 894
    Top = 26
    Width = 423
    Height = 290
    ScrollBars = ssBoth
    TabOrder = 7
    WordWrap = False
  end
  object DeleteButton: TButton
    Left = 438
    Top = 0
    Width = 59
    Height = 25
    Caption = 'Delete'
    TabOrder = 8
    OnClick = DeleteButtonClick
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 416
    Top = 215
  end
end
