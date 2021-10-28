object ZDBmanagerForm: TZDBmanagerForm
  Left = 0
  Top = 0
  Caption = 'ZDB Local...'
  ClientHeight = 857
  ClientWidth = 1350
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
    1350
    857)
  PixelsPerInch = 96
  TextHeight = 13
  object buildTempDataButton: TButton
    Left = 16
    Top = 8
    Width = 113
    Height = 45
    Caption = 'generate 50k'
    TabOrder = 0
    OnClick = buildTempDataButtonClick
  end
  object QueryButton: TButton
    Left = 16
    Top = 55
    Width = 113
    Height = 45
    Caption = 'query'
    TabOrder = 1
    OnClick = QueryButtonClick
  end
  object InsertButton: TButton
    Left = 16
    Top = 710
    Width = 75
    Height = 25
    Caption = 'easy insert'
    TabOrder = 2
    OnClick = InsertButtonClick
  end
  object DeleteButton: TButton
    Left = 16
    Top = 741
    Width = 75
    Height = 25
    Caption = 'easy delete'
    TabOrder = 3
    OnClick = DeleteButtonClick
  end
  object ModifyButton: TButton
    Left = 16
    Top = 772
    Width = 75
    Height = 25
    Caption = 'easy modify'
    TabOrder = 4
    OnClick = ModifyButtonClick
  end
  object CompressButton: TButton
    Left = 16
    Top = 354
    Width = 75
    Height = 25
    HelpType = htKeyword
    Caption = 'Compress'
    TabOrder = 5
    OnClick = CompressButtonClick
  end
  object StopButton: TButton
    Left = 16
    Top = 323
    Width = 75
    Height = 25
    Caption = 'stop'
    TabOrder = 6
    OnClick = StopButtonClick
  end
  object Panel1: TPanel
    Left = 176
    Top = 8
    Width = 1155
    Height = 831
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 7
    object Splitter1: TSplitter
      Left = 0
      Top = 541
      Width = 1155
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 0
      ExplicitWidth = 264
    end
    object Memo1: TMemo
      Left = 0
      Top = 0
      Width = 1155
      Height = 541
      Align = alClient
      Lines.Strings = (
        
          'ZDB local database demo provides demonstration methods for local' +
          'ization, basic addition, modification, deletion, query, etc'
        ''
        ''
        
          'ZDB is a very violent large database processing engine, which is' +
          ' now being used in a large and medium-sized project. In the near' +
          ' future, ZDB will combine clustering algorithm to solve the prob' +
          'lem of in-depth data processing. Using ZDB as an application for' +
          ' small and medium-sized enterprises is killing chickens with ox ' +
          'knives. If you want to apply it to the field of small and medium' +
          '-sized enterprises, please rest assured to use ZDB'
        
          'For the security of important data, please use the corecipher li' +
          'brary to encrypt and decrypt by yourself (remember to turn on th' +
          'e parallel encoding switch of zdefine.inc)'
        
          'ZDB queries are fed back in the form of fragments, which can be ' +
          'sent from 1000 lines. Fragments can still have correct feedback ' +
          'results'
        
          'ZDB database can work in two media: disk file and operating syst' +
          'em memory. Because it does not support virtual memory, when usin' +
          'g ZDB, pay attention to controlling cache parameters according t' +
          'o its own background configuration'
        
          'ZDB temporary database will use memory copy. For commercial use,' +
          ' please prepare one of fastmm, tcmalloc, jemalloc and nexusdb pr' +
          'ofessional databaseA memory support library, and please read the' +
          ' technical paper in a certain memory optimization field'
        
          'Running ZDB does not require array support. ZDB only requires me' +
          'dium-sized memory (for example, running tens of millions of entr' +
          'ies for query + analysis, using 128G memory + 2-core CPU). Note:' +
          ' after ZDB database entries exceed tens of millions, the memory ' +
          'throughput in parallel lines is very large. If the memory is not' +
          ' enough, please reduce the cache limit parameter, In exchange fo' +
          'r stability at the expense of performance note: do not run ZDB s' +
          'erver on X86 platform note: for Windows server running ZDB, plea' +
          'se select server2012 and above systems, and open the parallel op' +
          'tion at zdefine.inc. note: the server running ZDB needs to perfo' +
          'rm compress operation at least once a week to improve disk life ' +
          'and query efficiency. Note the above four points, ZDB is a very ' +
          'violent large data engine'
        ''
        
          'The characteristics of ZDB network service are very clear. The q' +
          'uery can be operated manually, and there is no need to submit an' +
          'd modify the stream at high speed with SQL statements'
        'Support big data cutting, big data analysis and professional HPC'
        
          'The single IO concurrency mechanism supports the annealing cache' +
          ' in the middle and low end cloud background: the more parallel q' +
          'ueries, the faster. After the query task is completed, ZDB will ' +
          'automatically anneal, which can force the limit of big data thro' +
          'ughput'
        
          'ZDB'#39's network services are carried out in the background. Zero b' +
          'locking is often done in the background. ZDB compression is safe' +
          '. There is no memory leakage in the network and background datab' +
          'ases. There is no need to install controls and set a directory t' +
          'o compile ZDB'
        
          'ZDB is not only competent for rough and heavy data processing, b' +
          'ut also can be used for the companion data system attached to th' +
          'e main database system. The database data transmission is encryp' +
          'ted, and the data storage is not encrypted (please solve the sto' +
          'rage encryption manually). The disk pre reading and write back s' +
          'upport is provided at the bottom of the database. Using ZDB does' +
          ' not need to consider the problem of synchronous and asynchronou' +
          's concurrency, but only needs to write data matching judgment an' +
          'd data processing'
        ''
        
          'There is almost transparent information about the internal worki' +
          'ng state of ZDB on the right side of the server and client. If i' +
          't is not enough, vmmap, deskmonitor and other tools can be used ' +
          'to observe the memoryDue to the time relationship with the IO st' +
          'atus of the hard disk, the demo will not do much. For example, f' +
          'or the operation method based on JSON, please refer to tdatafram' +
          'eengine (do not save JSON to t)In the dataframeengine, ZDB can b' +
          'e operated directly (Jason). The author'#39's maintenance time for Z' +
          'DB is fragmented. If you find any problems, please try to contac' +
          't me by email, QQ email 600585 qq.com'
        ''
        ''
        '')
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object ListBox1: TListBox
      Left = 0
      Top = 544
      Width = 1155
      Height = 287
      Align = alBottom
      ItemHeight = 13
      TabOrder = 1
    end
  end
  object PrintButton: TButton
    Left = 16
    Top = 803
    Width = 75
    Height = 25
    Caption = 'print'
    TabOrder = 8
    OnClick = PrintButtonClick
  end
  object ReverseQueryButton: TButton
    Left = 16
    Top = 106
    Width = 113
    Height = 45
    Caption = 'Reverse query'
    TabOrder = 9
    OnClick = ReverseQueryButtonClick
  end
  object QueryAndDeleteButton: TButton
    Left = 16
    Top = 157
    Width = 113
    Height = 45
    Caption = 'query and delete'
    TabOrder = 10
    OnClick = QueryAndDeleteButtonClick
  end
  object QueryAndModifyButton: TButton
    Left = 16
    Top = 208
    Width = 113
    Height = 45
    Caption = 'query and modify'
    TabOrder = 11
    OnClick = QueryAndModifyButtonClick
  end
  object QueryAndAnalysisButton: TButton
    Left = 16
    Top = 259
    Width = 113
    Height = 45
    Caption = 'query and Analysis'
    TabOrder = 12
    OnClick = QueryAndAnalysisButtonClick
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 416
    Top = 176
  end
  object Timer2: TTimer
    OnTimer = Timer2Timer
    Left = 552
    Top = 200
  end
end
