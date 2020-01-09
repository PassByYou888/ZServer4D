object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 440
  ClientWidth = 896
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 896
    Height = 440
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'symbol list'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo1: TMemo
        Left = 16
        Top = 19
        Width = 369
        Height = 337
        Lines.Strings = (
          #39#26631#35760#31526#34920#39
          'abc'
          '"'#26631#35760#31526#34920'"'
          '123'
          '-2.020952E+00 * (10 - 7.000000E+00) - 9.516623E+00')
        TabOrder = 0
      end
      object Button1: TButton
        Left = 391
        Top = 41
        Width = 75
        Height = 25
        Caption = 'pascal style'
        TabOrder = 1
        OnClick = Button1Click
      end
      object Memo2: TMemo
        Left = 472
        Top = 19
        Width = 393
        Height = 337
        TabOrder = 2
      end
      object Button2: TButton
        Left = 391
        Top = 72
        Width = 75
        Height = 25
        Caption = 'c style'
        TabOrder = 3
        OnClick = Button2Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'pascal proc list'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo3: TMemo
        Left = 24
        Top = 27
        Width = 369
        Height = 337
        Lines.Strings = (
          'program test;'
          ''
          'function test;'
          'procedure abc;'
          ''
          'begin'
          'end.')
        TabOrder = 0
      end
      object Button3: TButton
        Left = 399
        Top = 49
        Width = 75
        Height = 25
        Caption = 'pascal style'
        TabOrder = 1
        OnClick = Button3Click
      end
      object Memo4: TMemo
        Left = 480
        Top = 27
        Width = 393
        Height = 337
        TabOrder = 2
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'zExpression Demo'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo5: TMemo
        Left = 0
        Top = 49
        Width = 888
        Height = 363
        Align = alClient
        TabOrder = 0
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 888
        Height = 49
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Button4: TButton
          Left = 8
          Top = 10
          Width = 75
          Height = 25
          Caption = 'demo1'
          TabOrder = 0
          OnClick = Button4Click
        end
        object Button5: TButton
          Left = 89
          Top = 10
          Width = 75
          Height = 25
          Caption = 'demo2'
          TabOrder = 1
          OnClick = Button5Click
        end
        object Button6: TButton
          Left = 170
          Top = 10
          Width = 75
          Height = 25
          Caption = 'demo3'
          TabOrder = 2
          OnClick = Button6Click
        end
        object Button7: TButton
          Left = 251
          Top = 10
          Width = 75
          Height = 25
          Caption = 'demo4'
          TabOrder = 3
          OnClick = Button7Click
        end
        object Button8: TButton
          Left = 332
          Top = 10
          Width = 75
          Height = 25
          Caption = 'demo5'
          TabOrder = 4
          OnClick = Button8Click
        end
        object Button9: TButton
          Left = 413
          Top = 10
          Width = 100
          Height = 25
          Caption = 'special function'
          TabOrder = 5
          OnClick = Button9Click
        end
      end
    end
  end
end
