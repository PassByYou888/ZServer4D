object ZDBBatchDataServiceForm: TZDBBatchDataServiceForm
  Left = 0
  Top = 0
  Caption = 'Dataset service'
  ClientHeight = 744
  ClientWidth = 1402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusMemo: TMemo
    Left = 0
    Top = 0
    Width = 937
    Height = 744
    Align = alClient
    Lines.Strings = (
      'ZDB dataset service: take Jason as the data structure to demonstrate that the ZDB interface used by the actual ZDB to the dataset service in the project is an authentication interface. The client needs to log in. The default login account has two users: test1 password: test1'
      'User: test2 password: test2'
      ''
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
        'In the dataframeengine, ZDB can be operated directly (Jason). The author'#39's maintenance time for ZDB is fragmented. If you find any problems, please try to contact me by email, QQ email 600585 qq.com')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object WatchMemo: TMemo
    Left = 937
    Top = 0
    Width = 465
    Height = 744
    Align = alRight
    ReadOnly = True
    TabOrder = 1
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 472
    Top = 328
  end
  object Timer2: TTimer
    OnTimer = Timer2Timer
    Left = 592
    Top = 376
  end
end
