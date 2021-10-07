object ParallelForm: TParallelForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderWidth = 10
  Caption = 'Parallel Demo. Create by.qq600585'
  ClientHeight = 551
  ClientWidth = 721
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StateLabel: TLabel
    Left = 113
    Top = 280
    Width = 25
    Height = 13
    Caption = 'state'
  end
  object ParaAddButton: TButton
    Left = 0
    Top = 14
    Width = 107
    Height = 25
    Caption = 'Parallel Add'
    TabOrder = 0
    OnClick = ParaAddButtonClick
  end
  object Memo1: TMemo
    Left = 113
    Top = 0
    Width = 608
    Height = 50
    Lines.Strings = (
      'Parallel add demonstrates four ways to manipulate integer atomic variables')
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 113
    Top = 56
    Width = 608
    Height = 50
    Lines.Strings = (
      'Parallel lock demonstrates two kinds of security state machines, string')
    TabOrder = 2
  end
  object ParaLockButton: TButton
    Left = 0
    Top = 70
    Width = 107
    Height = 25
    Caption = 'Parallel lock'
    TabOrder = 3
    OnClick = ParaLockButtonClick
  end
  object Memo3: TMemo
    Left = 113
    Top = 112
    Width = 608
    Height = 50
    Lines.Strings = (
      'Parallel 19937 demonstrates the unity of 19937 random numbers in parallel and multithreaded programs')
    TabOrder = 4
  end
  object Para19937Button: TButton
    Left = 0
    Top = 126
    Width = 107
    Height = 25
    Caption = 'Parallel 19937'
    TabOrder = 5
    OnClick = Para19937ButtonClick
  end
  object Memo4: TMemo
    Left = 113
    Top = 168
    Width = 608
    Height = 50
    Lines.Strings = (
      'Parallel trondom demonstrates the unity of trondom in parallel and multithreaded programs, and its performance is better than using 19937 functions directly')
    TabOrder = 6
  end
  object ParallelTRandomButton: TButton
    Left = 0
    Top = 182
    Width = 107
    Height = 25
    Caption = 'Parallel TRandom'
    TabOrder = 7
    OnClick = ParallelTRandomButtonClick
  end
  object Memo5: TMemo
    Left = 113
    Top = 224
    Width = 608
    Height = 50
    Lines.Strings = (
      'Delphi trondom demonstrates how to replace the built-in random in Delphi, so that it has the unity of parallel and multithreaded programs and realizes large-scale algorithm transplantation. This method only supports xe10.3 or later versions, but xe10.2 is not supported. See the code remarks for details')
    TabOrder = 8
  end
  object ParaDelphiRandomButton: TButton
    Left = 0
    Top = 238
    Width = 107
    Height = 25
    Caption = 'delphi Random'
    TabOrder = 9
    OnClick = ParaDelphiRandomButtonClick
  end
  object Memo6: TMemo
    Left = 113
    Top = 302
    Width = 608
    Height = 249
    Lines.Strings = (
      'Tcompute is a thread instance of large-scale computing, which automatically manages thread pool, computing queue, granularity parallelism and so on'
      'The threads in the tcompute thread pool will only be idle for 1 second. When our thread reaches 1 second, the thread will be released'
      'Tcompute thread pool can be nested layer by layer, such as thread set parallelism and then set parallelism inside'
      'Tcompute and parallel programs share thread pools and thread instances'
      'There are many tcompute state machines, most of which are internal state machines, which are obtained through the state machine functions provided by tcompute'
      'Tcompute provides local near procedure for FPC and anonymous procedure for Delphi')
    TabOrder = 10
  end
  object ComputeThreadButton: TButton
    Left = 0
    Top = 302
    Width = 107
    Height = 25
    Caption = 'TCompute Thread'
    TabOrder = 11
    OnClick = ComputeThreadButtonClick
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 232
    Top = 456
  end
end
