object MHMainForm: TMHMainForm
  Left = 0
  Top = 0
  Caption = 'MemoryHook demo'
  ClientHeight = 680
  ClientWidth = 1261
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 1261
    Height = 600
    Align = alClient
    BorderStyle = bsNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      
        'There are 4 memory hook copies in ZS project, which can span all' +
        ' platforms and have excellent performance. The principle is to c' +
        'ontrol the memory overhead of our program by hooking the memory ' +
        'management unit and recording the allocated address'
      ''
      
        'Scenario 1: when we create a class, we hook it and monitor the m' +
        'emory overhead of the class'
      ''
      
        'Scenario 2: we can hook a function in the server. When the funct' +
        'ion exits, we can calculate the allocated space of the function.' +
        ' Finally, we can judge whether there is a leak in the function'
      ''
      
        'Scenario 3: a large number of memory hooks are used in zdbengine' +
        ' to manage cache memory overhead'
      ''
      'Mh.pas memory hook for batch management'
      'MH_ 1. PAS hooked for the first time'
      'MH_ 2. PAS second hook'
      'MH_ 3. PAS third hook'
      ''
      'The following is dostatus information')
    ParentColor = True
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    ExplicitWidth = 1080
  end
  object Panel1: TPanel
    Left = 0
    Top = 600
    Width = 1261
    Height = 80
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 1080
    object Button1: TButton
      Left = 8
      Top = 16
      Width = 178
      Height = 41
      Caption = 'Function call monitoring'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 192
      Top = 16
      Width = 330
      Height = 41
      Caption = 'Calculate the real memory size of the record and release it'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 528
      Top = 16
      Width = 378
      Height = 41
      Caption = 
        'High performance hook optimization for high frequency memory sta' +
        'tistics'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 912
      Top = 16
      Width = 170
      Height = 41
      Caption = 'Record mass memory requests'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 1088
      Top = 16
      Width = 153
      Height = 41
      Caption = 'Strict memory monitoring'
      TabOrder = 4
      OnClick = Button5Click
    end
  end
end
