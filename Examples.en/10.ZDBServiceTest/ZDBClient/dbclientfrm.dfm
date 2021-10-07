object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Zdb Client demo'
  ClientHeight = 961
  ClientWidth = 1253
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
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 1253
    Height = 169
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 4
      Top = 52
      Width = 97
      Height = 64
      Caption = 'connect'
      TabOrder = 0
      OnClick = Button1Click
    end
    object build100DataButton: TButton
      Left = 121
      Top = 17
      Width = 129
      Height = 134
      Caption = 'build 100k data'
      TabOrder = 1
      OnClick = build100DataButtonClick
    end
    object QueryG300Button: TButton
      Left = 256
      Top = 17
      Width = 178
      Height = 64
      Caption = 'query -300 to 300 value'
      TabOrder = 2
      OnClick = QueryG300ButtonClick
    end
    object QueryG700Button: TButton
      Left = 455
      Top = 17
      Width = 160
      Height = 64
      Caption = 'query -700 to -710 value'
      TabOrder = 3
      OnClick = QueryG700ButtonClick
    end
    object QueryG700AndDeleteButton: TButton
      Left = 455
      Top = 87
      Width = 160
      Height = 64
      Caption = 'delete -700 to -710'
      TabOrder = 4
      OnClick = QueryG700AndDeleteButtonClick
    end
    object QueryG300AndDeleteButton: TButton
      Left = 255
      Top = 87
      Width = 178
      Height = 64
      Caption = 'delete -300 to 300 '
      TabOrder = 5
      OnClick = QueryG300AndDeleteButtonClick
    end
    object CompressButton: TButton
      Left = 858
      Top = 17
      Width = 93
      Height = 64
      Caption = 'compress'
      TabOrder = 6
      OnClick = CompressButtonClick
    end
    object ResetDataButton: TButton
      Left = 957
      Top = 17
      Width = 156
      Height = 64
      Caption = 'reset LocalTestDB data'
      TabOrder = 7
      OnClick = ResetDataButtonClick
    end
    object ModifyG300AsG700Button: TButton
      Left = 636
      Top = 17
      Width = 192
      Height = 64
      Caption = 'modify -300 value as -700'
      TabOrder = 8
      OnClick = ModifyG300AsG700ButtonClick
    end
  end
  object ListBox1: TListBox
    Left = 843
    Top = 169
    Width = 410
    Height = 792
    Align = alRight
    ItemHeight = 13
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 169
    Width = 843
    Height = 792
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'state'
      object Memo: TMemo
        Left = 0
        Top = 0
        Width = 835
        Height = 764
        Align = alClient
        Color = clBlack
        Ctl3D = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clWhite
        Font.Height = -15
        Font.Name = 'Consolas'
        Font.Style = []
        Lines.Strings = (
          'ZDB is a very violent large database processing engine, which is now being used in a large and medium-sized project. In the near future, ZDB will combine clustering algorithm to solve the problem of in-depth data processing. Using ZDB as an application for small and medium-sized enterprises is killing chickens with ox knives. If you want to apply it to the field of small and medium-sized enterprises, please rest assured to use ZDB'
          'For the security of important data, please use the corecipher library to encrypt and decrypt by yourself (remember to turn on the parallel encoding switch of zdefine.inc)'
          'ZDB queries are fed back in the form of fragments, which can be sent from 1000 lines. Fragments can still have correct feedback results'
          'ZDB database can work in two media: disk file and operating system memory. Because it does not support virtual memory, when using ZDB, pay attention to controlling cache parameters according to its own background configuration'
          
            'ZDB temporary database will use memory copy. For commercial use, please prepare one of fastmm, tcmalloc, jemalloc and nexusdb professional database' +
            'A memory support library, and please read the technical paper in a certain memory optimization field'
          'Running ZDB does not require array support. ZDB only requires medium-sized memory (for example, running tens of millions of entries for query + analysis, using 128G memory + 2-core CPU). Note: after ZDB database entries exceed tens of millions, the memory throughput in parallel lines is very large. If the memory is not enough, please reduce the cache limit parameter, In exchange for stability at the expense of performance note: do not run ZDB server on X86 platform note: for Windows server running ZDB, please select server2012 and above systems, and open the parallel option at zdefine.inc. note: the server running ZDB needs to perform compress operation at least once a week to improve disk life and query efficiency. Note the above four points, ZDB is a very violent large data engine'
          ''
          'The operation method of ZDB client is consistent with that of single machine'
          'ZDB network clients work in zero blocking asynchronous mode'
          ''
          'The demo provides basic addition, modification, deletion, query and other demonstration methods'
          'There are 2 sets of ZDB network services, 1. With authentication and 2. Without authentication. The demo uses the ZDB server model without authentication'
          ''
          'The characteristics of ZDB network service are very clear. The query can be operated manually, and there is no need to submit and modify the stream at high speed with SQL statements'
          'Support big data cutting, big data analysis and professional HPC'
          'The single IO concurrency mechanism supports the annealing cache in the middle and low end cloud background: the more parallel queries, the faster. After the query task is completed, ZDB will automatically anneal, which can force the limit of big data throughput'
          'ZDB'#39's network services are carried out in the background. Zero blocking is often done in the background. ZDB compression is safe. There is no memory leakage in the network and background databases. There is no need to install controls and set a directory to compile ZDB'
          'ZDB is not only competent for rough and heavy data processing, but also can be used for the companion data system attached to the main database system. The database data transmission is encrypted, and the data storage is not encrypted (please solve the storage encryption manually). The disk pre reading and write back support is provided at the bottom of the database. Using ZDB does not need to consider the problem of synchronous and asynchronous concurrency, but only needs to write data matching judgment and data processing'
          ''
          'There is almost transparent information about the internal working state of ZDB on the right side of the server and client. If it is not enough, vmmap, deskmonitor and other tools can be used to observe the memory' +
            'Due to the time relationship with the IO status of the hard disk, the demo will not do much. For example, for the operation method based on JSON, please refer to tdataframeengine (do not save JSON to t)' +
            'In the dataframeengine, ZDB can be operated directly (Jason). The author'#39's maintenance time for ZDB is fragmented. If you find any problems, please try to contact me by email, QQ email 600585 qq.com'
          ''
          '')
        ParentCtl3D = False
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object ProgressTimer: TTimer
    Interval = 1
    OnTimer = ProgressTimerTimer
    Left = 440
    Top = 176
  end
  object Timer1: TTimer
    Interval = 2000
    OnTimer = Timer1Timer
    Left = 336
    Top = 184
  end
end
