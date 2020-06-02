### 重大bug修复,请大家一定要更新到该版本

- **修复BigStream发送后,立即发送一条响应指令,这时候出现收不到响应的bug,因为缺少了一个信号处理机制,导致了BigStream机制无法应用**
- 该问题属于重大级bug,漏了一个逻辑处理机制,一定要更新到该版本
- 升级项目有任何问题给我留言,小窗qq我,见到即刻回复

## 最后一更新日志

- 移除最大并行线程限制
- 新增大规模分片式负载技术:当服务器的连接过多,主循环方法progress一次不会全部遍历完IO,而是按时间一次遍历一部分
- TCompute新增线程级的Post方法:因为ZServer4D的客户端高并发库要求主线程工作,这些库包括(DIOCP/CrossSocket),因此在线程中很不好控制ZServer的主动连接/收发命令,TCompute.PostXX方法主要解决该问题.客户端使用Indy/ics接口可以直接后台线程兼容.
- P2PVM机制优化:本次更新后的P2PVM可以支持构建应用级P2PVM:目前已经在zAI中针对机器学习服务器大规模应用了P2PVM技术
- 内部命名小调整:不影响堆现有项目和demo的编译+运行

- P2PVM断开时将会触发远程通知: **p2pVM在物理连接断线以后都是直接断线,一般不做逻辑断线处理,所以一直没有做断线消息,今天,我自己测试p2pVM模拟日常通讯,发现断线问题.于是,紧急修复.**
- 新增折叠并行机制: **相比delphi内置的TParallel.For以及fpc内置的mtprocs更加优化的折叠地址并行**


## 全面支持XE10.3.2

- 新增Demo: zExpression对自然句法支持度的深度测试以及运算能力完整的评估和技术支持
- 新增Demo: 演示了使用TextParsing文本解析引擎机器化对pascal语言进行%二进制类申明翻译
- 新增Demo：针对代码机器人,大规模代码翻译的技术支持,它演示了对pascal语言的uses依赖关系的解析和排序,它可以解析出uses过哪些库,以及$I过那些源码,并且排序

- 新增若干文档，请参考documents

- 修复bug：修复ListEngine中IgnoreCase变量赋值问题，感谢蝈蝈qq56571173


## 修复xNAT代理http长连接的问题

- XNAT做http穿透时需要使用长连接方式:HTTP Keep-Alive
- 修复crossSocket接口断开连接时小概率不触发DoDisconnect的问题


## 2019-4-15 新增并行化压缩器，新增两个Demo

- 性能：通讯内核引入并行化压缩技术：buff尺寸越大，提升越明显，已经test passed
- 示范：新增Demo：并行压缩性能测试
- 示范：新增Demo：抗量子密码的使用范例
- bug修复：修复ListEngine的IgnoreCase
- bug修复：修复TextParsing对备注信息的符号还原问题
- bug修复：修复在Json数据集Demo中使用application.processmessages会造成卡死的问题
- 近段时间作者会考虑万兆以太的性能问题
- 近段时间作者会考虑zs对paas架构的支持

## 2019-3-18 新增一个DLL技术范例

- 详见 "**33.DLL_API**" 的Demo，使用D7调用无需使用FastMM这类工具来检测内存泄漏，FastMM的内存泄漏检测与XE Runtime不兼容
- 忙于升级zAI，尚未与CorssSocket和DIOCP做最新代码合并，代码合并容易，测试很麻烦，待zAI升级完成.

## 2019-3-2 底层库大幅更新

**注意：本次底层库大幅更新，有安全性协议更新不兼容之前的ZS协议，老项目升级请小心对待**

**本次更新的通讯内核已经历过zAI商业项目的预热期考验，请放心使用**

- 深度安全:由于公网探头太多了，ZS需要封闭的安全协议，本次更新在ZS的最底层直接屏蔽裸流传输，所有的传输都会以序列包进行，并且每个序列包会验证正确性。服务器只要在公网开机运行，野生客户端即使连接进来，也无法获取任何信息。
- 深度性能:由于ZS在最底层统一采用了序列包验证协议，这会导致传输性能下降，为此，ZS在内核中也开辟了无卡TComputeThread计算机制，简单来说，如果使用本次对通讯安全的大更新，通讯性能会下降5%，对于单进程高密度计算的程序，需要使用TComputeThread来分担CPU负载，从而把更多CPU资源共享给安全传输处理。
- 性能:HPC的API工艺会自动挂接使用本次更新的TComputeThread内核，如：RunStreamWithDelayThreadP 这类函数
- bug修复:修复了底层 UnicodeMixedLib.pas 中的 umlGetFileName, umlGetFilePath，umlCombineFileName等等函数
- 小升级：新增了FPC泛型库支持，完全兼容IOT方向以及Delphi，并且以后准备大规模应用泛型模式编程
- IO内核新增BigStream的传输进度事件，收发有进度信息，截获该事件后，我们可以更方便在ProgressBar之类的东西上显示状态
- 在 THashStringList 和 THashVariantLsit 可以使用zExpression来赋值，写法为，list['value']='e"1+1"' 结果 list['value']=2

**由于最近我的精力都忙于zAI的开发[https://zpascal.net](https://zpascal.net "https://zpascal.net")，圈内的CrossSocket和DIOCP两个重量级接口库，我还没有来得及使用最新的代码合并，因为我不确定CrossSocket内存泄漏问题是否已经解决了，如果你有时间请自行使用Winmerge合并一下新库的代码来测试，如果有结果，请反馈一下**

**本次更新需要向大家说明一个小小信息：ZS有独特的P2PVM和StableIO，因为我就是希望ZS是独特的，以后都不能被替代，我不敢说ZS是圈内第一通讯库，至少保证它也是独特的存在。当然。它必须是免费的，因为通讯库都是地基项目，地基库是不能有半点水分的。**

## 2018-12-22 BigStream能支持在非物理隧道进行大规模并行传输

**建议将工程都升级到本次更新的版本，本次更新已经数小时，上百个通讯程序的严格测试，更加稳定**

- 大改:重做BigStream的协议机制，新的BigStream协议为反馈续传：我们在发送一个大型文件时，内核会按小块一次一个进行发送，当收到远程信号后，才会发送下一个。不再是在Progress中对物理隧道做WriteBufferEmpty进行下一个小块发送。
- 新功能:BigStream支持压缩选项，我们打开了SendDataCompressed开关后，BigStream会自动对小数据块进行压缩，在实际后台工作，它对cpu的消耗很小，可以忽略不计
- 新功能:新增全自动化KeepAlive机制：只要我们在服务器设置了TimeOutIDLE这类参数（超时断线），系统就会自动判断掉线，可以稳定保活连接，无需自行在外部去实现AntiIDLE的机制了。KeepAlive不再依靠操作系统的非标准KeepAlive，这是非标准api，各个系统很不稳定。
- 修复:修复P2PVM-Client中IO释放的BUG
- 修复:取消了SequencePacket的编译选项，默认情况下，SequencePacket模式都会打开，而我们开关SequencePacket模式不用再通过编译选项，注意：在大规模并发时，会让cpu性能会掉1-5%左右，假如你觉得这样不好，可以关闭Server.SequencePacketActivted的开关，但是会失去自动化KeepAlive机制功能。
- 修改:将 IO 中的CheckIOBusy检查更名为IOBusy，外部程序如果有使用该方法请替换一下名字

## 2018-11-29 P2PVM支持了IO对穿

- 新功能:XNAT可以对穿,XNAT服务器可以是客户端，并且仍然可以断线重连，XNAT客户端可以是服务器，对穿就是正向代理和反向代理我们可以自由切换
- 改进:XNAT可以设置TimeOut时间
- 新Demo:新增p2pVM的对穿Demo
- 优化:ZDB优化了书写规则

## 2018-11-25 日常更新

- 新增:XNAT技术体系的VirtualServer Demo，该Demo可以在IOT以及手机设备带起操作2000连接的并发服务器，并且支持断线重连技术StableIO
- 修复:CrossSocket报告socket无效问题
- 修复:在zDefine.inc中，有三个原子锁开关，CriticalSimulateAtomic，SoftCritical，ANTI_DEAD_ATOMIC_LOCK，现在可以自由打开，程序不会报错
- 修复:在IOT设备，大幅降低高并程序发对cpu的使用（干掉后置事件引擎的LockObject死板机制，改用互斥区做锁）
- 测试通过:所有Demo和服务器模型运行亲测通过

## 2018-11-21 想做个大更，但是，因为新增了StableIO，引发了无数多小问题，所以，最近更新总是不断，真烦!!

- 修改:对每个服务器提供PrefixName+Name，Print时我们会知道哪个服务器在Log
- 修复:大修MemoryHook的Delphi+FPC实现，此改动影响ZDB
- 修复:zExpression识别-(，会认为是数字的bug
- 修复:Synapse在IOT的reuse_addr的问题，感谢longbow qq47324905

**服务器不做最大连接数限制都是不安全的，你的服务器无限制，但是操作系统会有限制**

- CrossSocket最大连接被限制到20000(小幅修改CrossSocket的IOCP,EPoll,KQueue内核)
- DIOCP最大连接被限制到20000
- ICS最大连接被限制到500
- INDY最大连接被限制到20
- Syanpse最大连接被限制到100
- P2PVM最大连接无限制（不受操作系统影响）

## 2018-11-20

- 修改:独立了一个PhysicsIO.pas单元
- 修改:处于方便排查问题，StableIO默认将会打开物理连接的Log信息 
- 修复:在IOT平台，如果FPC编译器打开优化，会出现堆栈异常导致某些复杂的程序无法运行，通过修改zDefine.inc已关闭fpc编译器的优化开关
- 修复:TimeTick默认从第三天凌晨开始计数
- 修复:运算符需求，将TimeTick类型从UInt64更变为Int64
- 修复:MemoryHook使用的ThreadVar修饰符更变为var
- 提示:在xnat高频率触发物理断线后，超时1分钟就会暴力关闭连接，在暴力关闭连接中，crossSocket会发生error这类提示，因为物理链接已经断开，接收和发送都会出现error，不用管它
- 修复:暂时未修复的问题：因为windows有最大连接数限制，服务器超过最大连接，就会停止侦听，这种情况也会发生在，连接断开了以后句柄没有立即释放，比如在1分钟内，有3万个连接发过来，并且全都accept，然后3万连接突然全部物理断线，然后又发3万连接过来，这时候，就会超出最大连接限制。因为crossSocket底层没有提供accept事件，我要修改HandeAccept，比较烦麻烦，我在考虑要不要把CrossSocket独立出来做个专属接口项目

## 2018-11-16

- 修复:socket取替了优雅IO的关闭方式，直接close，这一改动对物理断线后的关闭有效
- 修复:xnat的物理连接会在5分钟超时后暴力关闭
- 修改:重写了ICS server接口，现在ICS server为单线程服务器
- 修改:在StableIO的客户端增加了 StopCommunicationTimeTick 参数，无论是否在线和离线，StopCommunicationTimeTick都能准确给出无通讯响应的时间，在开发IOT设备时，该参数可用于重启预检测

## 2018-11-9

- 新功能:IO内核新增序列包机制
- 新功能:IO内核新增不怕断线的StableIO系统
- 新功能:重做时间刻度支持，新版本的时间刻度可以让服务器开机到硬件报废
- 新功能:新增两个Demo：基于Dataset的sql查询演示，聊天室，这两个Demo都使用了StableIO技术，了解StableIO使用可以参考他们，因为使用StableIO太简单了，不需要编写专门的Demo
- 新功能:新增两个文档：[基于序列包的断线重连系统StableIO](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9F%BA%E4%BA%8E%E5%BA%8F%E5%88%97%E5%8C%85%E7%9A%84%E6%96%AD%E7%BA%BF%E9%87%8D%E8%BF%9E%E7%B3%BB%E7%BB%9FStableIO.pdf)  [Zserver中的序列包机制详解](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/Zserver%E4%B8%AD%E7%9A%84%E5%BA%8F%E5%88%97%E5%8C%85%E6%9C%BA%E5%88%B6%E8%AF%A6%E8%A7%A3.pdf)
- 更新:所有内核的指令全部以__@开头和外部指令进行区分
- 修复:之前，ZExpression会把and or xor shr shl div这些关键字当成一个ascii来处理，现在会将pascal关键字and or xor shr shl div当成符号来处理

## 2018-10-30 重做底层原子锁系统

- 重新支持了IOT后台：在IOT后台，Syanpse能稳定对接各种接口
- 重新制作了使用互斥机制替代原子锁的模拟器，互斥机制替代原子锁，会让性能有小幅下降，不会有感觉
- 新作一套使用软件方式实现线程互斥锁的机制，软件模拟互斥机制会更消耗CPU，也会让服务器更耗电，但是可以帮助我们解决原子锁的安全布置问题
- 将所有服务器，全部改成了同步化的数据处理模式：保证了ICS,Indy,CrossSocket,DIOCP,Synapse，这5个接口都能稳定应用于商业后台项目
- XNAT的外网服务器系统，均能跑在ICS,Indy,CrossSocket,DIOCP,Synapse这5个接口下
- Indy服务器接口改造，异步方式只用数据接收和发送，处理数据全部使用同步方式
- ICS服务器接口改造，异步方式只用数据接收和发送，处理数据全部使用同步方式
- DIOCP服务器接口改造，异步方式只用数据接收和发送，处理数据全部使用同步方式
- Synapse服务器接口改造，异步方式只用数据接收和发送，处理数据全部使用同步方式


## 2018-10-28 内核大修

- 保证机制不变的前提，大幅修改内核流程，几乎重做内核
- 本次更新后的内核工作流程的稳定性会有前所未有的提高
- 内核会区分使用带有异步性质的程序（多线程并发+原子锁+同步回调），和非异步程序（单线程往往有更高的稳定性和可维护性）
- CrossSocket服务器本次更新后，使用内核的工作模式为非异步程序，底层为异步，接口后，以非异步方式处理
- CrossSocket客户端本次更新后，使用内核的工作模式为非异步程序，底层为异步，接口后，以非异步方式处理
- DIOCP服务器本次更新后，使用内核的工作模式为异步程序
- DIOCP客户端本次更新后，使用内核的工作模式为异步程序
- ICS服务器本次更新后，使用内核的工作模式为异步程序
- ICS客户端本次更新后，使用内核的工作模式为非异步程序
- INDY服务器本次更新后，使用内核的工作模式为异步程序
- INDY客户端本次更新后，使用内核的工作模式为非异步程序
- Synapse服务器本次更新后，使用内核的工作模式为异步程序
- Synapse客户端本次更新后，使用内核的工作模式为非异步程序
- 优化数据吞吐能力
- 修复CrossSocket底层在EPOLL+KQUEUE中的发送异步回调事件的死锁问题
- 内置了TimeOut检查机制
- 以ab为主，用高达20亿次请求在EPOLL(linux)+KQUEUE(unix)+IOCP(win)压测XNAT通过，整个过程持续了1天
- 重新命名Json库，以ZS_JsonDataObjects.pas代替原来的JsonDataObjects.pas，感谢黑夜杀手建议


## 2018-10-23

> 小幅度更新

- 修复:CrossSocket接口在Linux发送大数据块卡死的问题
- 修复:在Linux环境部署XNAT公网服务器后，用AB严格测试，发现互斥锁会锁不住某些线程(rt底层会线程越权)，改用delphi内置的原子锁，解决该问题，经历15分钟，1000万请求，无任何报错。XNAT只在delphi测试通过。fpc未测试。
- 升级:XNAT支持负载均衡，由外网服务器自动化全权调度，内网穿透的使用无变化
- 演示:新增指令序列化，以及5种Stream收发的Demo
- 文档:新增指令序列化的机制详解
- 修复:DelayClose缺少时间参数的问题
 
## 2018-10-22

> 小幅度更新

- XNAT使用RD远程桌面速度太慢的解决方案：已将后置式的代理转发机制，改为了无延迟的直接转发，负载性能会下降，但是实时性更好
- 重做XNAT内置的IPV6地址生成方式：现在会雪崩式生成无重复的IPV6地址
- XNAT新增一个基于Mapping直接构建ServerFramework的接口以及Demo，都在XNAT的演示目录
- 过去，XNAT的Data交换是后置式的，现在更改为即时交换
- CompleteBuffer增加一个压缩开关，默认使用FastCompress函数(该函数位于MemorySteam64.pas)


## 2018-10-20


> 小幅度更新

- 升级:合并了最近更新的DIOCP库
- 优化:重做地基库中的Base64编码
- 优化:内核的状态计数器不再直接使用inc操作，全部改用原子模式AtomInc，Delphi为原生的原子整数操作，因为在fpc不支持通用原子函数使用互斥锁解决
- 优化:Android平台不再考虑对雷电这类使用kvm加速模拟器支持，开发安卓系统，请使用真实设备
- 优化:调整了几处正常人类看起来不太顺眼的命名
- 工艺:在底层IO开发参考代码中编写了详细备注，看一眼即可明白接口含义, CommunicationFramework_Client_Refrence.pas,CommunicationFramework_Server_Refrence.pas
- 工艺:核心库的加密规则调整:不再使用固定加密算法，内核会随机使用一种加密算法进行工作，
- 工艺:ZDB新增对基础数据结构THashStringList(比THashVariantList更快)支持
- 工艺:ZDB改进网络服务的数据传输规则:凡是涉及到数据内容传输的地方，均会自动使用NIST认可的加密算法之一对其进行加密
- 工艺:将Examples中的所有的目录名全部改成英文
- 升级:本次升级已经测试通过若干已经上线项目，如不出意外，应该是大更新前最后一个小更新


## 2018-10-16

- 修复:修复zExpression无法对1.0e-2浮点识别的bug
- 修复:crossSocket接口反复释放的一个严重bug
- 修复:xNat内创穿透断线重连不稳定的bug，新版本的xNat公网服务器，只会在内网连接后，才会侦听端口，两端运行中现在会极其稳定
- 优化:xNat在手机也可以做内网穿透服务，优化了wiki或则4G,3G连接不稳定造成的通讯问题
- 安全:新增傻瓜化使用的抗量子密码支持(sha3)，在密码货币系统非常常见，经过验证已经与wiki一致， https://en.wikipedia.org/wiki/SHA-3
- 安全:因为sha3有大量的迭代计算，在摩尔定律使用秀儿算法是无法被破解的，同时sha3的计算性能也远远不如fastmd5，也许有数千倍差异，但是，使用sha3来存储和验证密码会万无一失

**重新支持了5大美国国家标准技术研究所(NIST)高级加密标准算法，如下**

- 安全:深度测试rc6加密，通讯协议支持 https://en.wikipedia.org/wiki/RC6
- 安全:重做Twofish加密，通讯协议支持 https://en.wikipedia.org/wiki/Twofish
- 安全:通讯协议支持Serpent加密 https://en.wikipedia.org/wiki/Serpent_(cipher)
- 安全:通讯协议支持Mars加密 https://en.wikipedia.org/wiki/MARS_(cipher)
- 安全:通讯协议支持Rijndael加密 https://en.wikipedia.org/wiki/Advanced_Encryption_Standard

## 2018-9-29

- 修复:在DataFrameEngine重做了Variant类型的读写支持，统一多平台兼容性，不再使用RT库自带的Variant写入方法
- 修复:FPC中Enum为4 byte定义会丢失符号位的问题
- 修复:Synapse的接口在连接失败时，会尝试切换IPV4+IPV6重新连接
- 修复:Syanpse客户端在连接失败时不返回状态
- 工艺:新增FPC泛型支持库，FPCGenericStructlist.pas
- 工艺:新增线性事件支持库，LinerAction.pas
- 工艺:新增查询服务器支持（类似dns，主要针对万物互联需求：物联网设备太多，调度服务器不够用，可使用查询服务器分担），HostQuery.pas
- 工艺:兼容基于FPC对IOT的支持：从底层到高级，大规模统一调整命名，此项调整会影响很多工程的代码细节

```delphi

// 本项目中的回调分为3种
// call:   直接指针回调，fpc+delphi有效
// method: 方法回调，会继承一个方法宿主的地址，fpc+delphi有效
// proc:   匿名过程回调，只有delphi有效

// 如果本项调整对于改造现有工程有一定的工作量，请使用字符串批量处理工具
// 在任何有回调重载的地方，方法与函数，均需要在后缀曾加回调类型首字母说明

// 如
RunOp 变更为 RunOpP() // 后缀加P表示匿名类型回调
RunOp 变更为 RunOpM() // 后缀加M表示方法类型的回调
RunOp 变更为 RunOpC() // 后缀加C表示指针类型的回调

SendStreamCmd 变更为 SendStreamCmdP() // 后缀加P表示匿名类型回调
SendStreamCmd 变更为 SendStreamCmdM() // 后缀加M表示方法类型的回调

```


## 2018-9-21

兼容 fpc 3.0.0，可正常编译，建议使用fpc 3.0.4 or last

### 2018-9-18

新平台，新平台测试通过，在新平台树莓派3B+，操作系统 Ubuntu16.04 Mate 下测试成功

新增几个技术文档

**DoStatusIO.pas库**

- 优化，干掉后台线程刷新，改用DoStatus方法替代线程，最简单的使用方法:在你的主循环中，加一句DoStatus不要给参数
- 优化，减少对某些库的依赖性

**其它优化**

- 优化，FPC3.0.4编译出来的程序，整体性能向前提升10%
- 优化，Delphi编译出来的程序，整体性能向前提升10%
- 优化，精细调整服务器主循环：特别说明，在Console模式下后台服务器程序中，必须加上线程同步检查：CheckThreadSynchronize，否则系统Console后台不能正常工作

### 2018-9-15

本次更新，大幅提升底层库的稳定性
================

**重大更新**

- 新接口，同时支持fpc+delphi:新增基于Synapse的通讯接口
- 内网穿透核心技术，同时支持fpc+delphi:新增内网穿透的开发库，由xNatService.pas,xNatClient.pas,xNATPhysics.pas三个小库组成，只需5行代码即可驱动，可使用ZServer支持的任意通讯接口工作。已通过5万ip/每分钟连续4小时的压力穿透测试。
- IoT物联网，只限fpc：对IoT平台的支持，基于fpc在各平台完整支持了Synapse通讯接口，包括：ARM Linux(IoT需求)，Linux x86+x64，OSX x86+x64，Win x86+x64
- 稳定和安全，大规模取替底层库使用inline的机制

**CommunicationFramework.pas库及周边支持**

- 优化，稳定性提升，深度考虑安全性
- 安全，重做了p2pVM的验证系统（每个p2pVM握手时，都会用不同的验证方式）
- 安全，p2pVM第一次握手时，必须有验证码
- 工艺，新增2种协议模式：cpZServer(原来的通讯协议),cpCustom(外部自定义的通讯协议)
- 工艺，重做外部自定义通讯协议的开发工艺：开发自定义通讯协议时，不用再考虑同步异步问题
- 工艺，ProgressBackground全部统一替换成Progress
- 工艺，TCommunicationFrameworkServer服务器触发DoClientConnectAfter会区分协议，cpZServer,cpCustom会有各自处理机制
- 安全，在TCommunicationFramework中以性能换取了Progress的稳定性，客户端+服务器在高并发环境下不会再在这个地方出现异常报告了
- 周边：极小概率bug，修复CrossSocket的连接池释放时发生异常的问题(delphi)
- 周边：控制台模式服务器bug，修复ICS,Syanpse,Indy服务器在Console应用模式中不触发线程同步的问题(delphi)
- 周边：小概率bug，修复ICS服务器使用StopService偶发性的出现卡死的bug
- 周边：合并了最新更新的CrossSocket内核
- 周边：优化，在MemoryStream64.pas库中对ZLib使用解压时，会预先分配内存或则文件空间，避免因为MemoryManager频繁Realloc造成性能耗损(在FPC方向程序中，性能可以相对提供10%)


**TextParsing.pas库及其周边支持**

- 大幅提升的解析性能，使用不变，相较以前，性能向前提升90%
- 新增可替代蚂蚁机制的文本探头技术
- 优化大规模解析程序的复杂度：降低50%
- 修复对123{abc}这种写法的误判行为

**TextDataEngine.pas库**

- 重做数据结构支持，使用与以前不变
- 新增了对THashStringList的内置支持(相对THashVariantList，性能更加优异)

**ListEngine.pas库**

- 安全：对已生成的Hash会使用安全校验措施(我实测Hash的事故率为0，但是有网友报告说Hash会莫名其妙报错，我现在加了一个安全措施，如果还遇到hash无法命中，请检查自己的bug)


**百度翻译api服务器**

- 由于百度不再提供免费翻译api，我们首次运行百度翻译api服务器时，会生成一个配置文件，它会指引你如何注册翻译api的账号

**本次新增两套Demo**

- ZServer4D对大规模后台验证服务器的工艺，比如你后台有Oracle这类大型数据库，该Demo详细描述了三方验证工艺
- XNat内网穿透库的Demo


## 2019-4-15 新增并行化压缩器，新增两个Demo

- 性能：通讯内核引入并行化压缩技术：buff尺寸越大，提升越明显，已经test passed
- 示范：新增Demo：并行压缩性能测试
- 示范：新增Demo：抗量子密码的使用范例
- bug修复：修复ListEngine的IgnoreCase
- bug修复：修复TextParsing对备注信息的符号还原问题
- bug修复：修复在Json数据集Demo中使用application.processmessages会造成卡死的问题
- 近段时间作者会考虑万兆以太的性能问题
- 近段时间作者会考虑zs对paas架构的支持

## 2019-3-18 新增一个DLL技术范例

- 详见 "**33.DLL_API**" 的Demo，使用D7调用无需使用FastMM这类工具来检测内存泄漏，FastMM的内存泄漏检测与XE Runtime不兼容
- 忙于升级zAI，尚未与CorssSocket和DIOCP做最新代码合并，代码合并容易，测试很麻烦，待zAI升级完成.

## 2019-3-2 底层库大幅更新

**注意：本次底层库大幅更新，有安全性协议更新不兼容之前的ZS协议，老项目升级请小心对待**

**本次更新的通讯内核已经历过zAI商业项目的预热期考验，请放心使用**

- 深度安全:由于公网探头太多了，ZS需要封闭的安全协议，本次更新在ZS的最底层直接屏蔽裸流传输，所有的传输都会以序列包进行，并且每个序列包会验证正确性。服务器只要在公网开机运行，野生客户端即使连接进来，也无法获取任何信息。
- 深度性能:由于ZS在最底层统一采用了序列包验证协议，这会导致传输性能下降，为此，ZS在内核中也开辟了无卡TComputeThread计算机制，简单来说，如果使用本次对通讯安全的大更新，通讯性能会下降5%，对于单进程高密度计算的程序，需要使用TComputeThread来分担CPU负载，从而把更多CPU资源共享给安全传输处理。
- 性能:HPC的API工艺会自动挂接使用本次更新的TComputeThread内核，如：RunStreamWithDelayThreadP 这类函数
- bug修复:修复了底层 UnicodeMixedLib.pas 中的 umlGetFileName, umlGetFilePath，umlCombineFileName等等函数
- 小升级：新增了FPC泛型库支持，完全兼容IOT方向以及Delphi，并且以后准备大规模应用泛型模式编程
- IO内核新增BigStream的传输进度事件，收发有进度信息，截获该事件后，我们可以更方便在ProgressBar之类的东西上显示状态
- 在 THashStringList 和 THashVariantLsit 可以使用zExpression来赋值，写法为，list['value']='e"1+1"' 结果 list['value']=2

**由于最近我的精力都忙于zAI的开发[https://zpascal.net](https://zpascal.net "https://zpascal.net")，圈内的CrossSocket和DIOCP两个重量级接口库，我还没有来得及使用最新的代码合并，因为我不确定CrossSocket内存泄漏问题是否已经解决了，如果你有时间请自行使用Winmerge合并一下新库的代码来测试，如果有结果，请反馈一下**

**本次更新需要向大家说明一个小小信息：ZS有独特的P2PVM和StableIO，因为我就是希望ZS是独特的，以后都不能被替代，我不敢说ZS是圈内第一通讯库，至少保证它也是独特的存在。当然。它必须是免费的，因为通讯库都是地基项目，地基库是不能有半点水分的。**

## 2018-12-22 BigStream能支持在非物理隧道进行大规模并行传输

**建议将工程都升级到本次更新的版本，本次更新已经数小时，上百个通讯程序的严格测试，更加稳定**

- 大改:重做BigStream的协议机制，新的BigStream协议为反馈续传：我们在发送一个大型文件时，内核会按小块一次一个进行发送，当收到远程信号后，才会发送下一个。不再是在Progress中对物理隧道做WriteBufferEmpty进行下一个小块发送。
- 新功能:BigStream支持压缩选项，我们打开了SendDataCompressed开关后，BigStream会自动对小数据块进行压缩，在实际后台工作，它对cpu的消耗很小，可以忽略不计
- 新功能:新增全自动化KeepAlive机制：只要我们在服务器设置了TimeOutIDLE这类参数（超时断线），系统就会自动判断掉线，可以稳定保活连接，无需自行在外部去实现AntiIDLE的机制了。KeepAlive不再依靠操作系统的非标准KeepAlive，这是非标准api，各个系统很不稳定。
- 修复:修复P2PVM-Client中IO释放的BUG
- 修复:取消了SequencePacket的编译选项，默认情况下，SequencePacket模式都会打开，而我们开关SequencePacket模式不用再通过编译选项，注意：在大规模并发时，会让cpu性能会掉1-5%左右，假如你觉得这样不好，可以关闭Server.SequencePacketActivted的开关，但是会失去自动化KeepAlive机制功能。
- 修改:将 IO 中的CheckIOBusy检查更名为IOBusy，外部程序如果有使用该方法请替换一下名字

## 2018-11-29 P2PVM支持了IO对穿

- 新功能:XNAT可以对穿,XNAT服务器可以是客户端，并且仍然可以断线重连，XNAT客户端可以是服务器，对穿就是正向代理和反向代理我们可以自由切换
- 改进:XNAT可以设置TimeOut时间
- 新Demo:新增p2pVM的对穿Demo
- 优化:ZDB优化了书写规则

## 2018-11-25 日常更新

- 新增:XNAT技术体系的VirtualServer Demo，该Demo可以在IOT以及手机设备带起操作2000连接的并发服务器，并且支持断线重连技术StableIO
- 修复:CrossSocket报告socket无效问题
- 修复:在zDefine.inc中，有三个原子锁开关，CriticalSimulateAtomic，SoftCritical，ANTI_DEAD_ATOMIC_LOCK，现在可以自由打开，程序不会报错
- 修复:在IOT设备，大幅降低高并程序发对cpu的使用（干掉后置事件引擎的LockObject死板机制，改用互斥区做锁）
- 测试通过:所有Demo和服务器模型运行亲测通过

## 2018-11-21 想做个大更，但是，因为新增了StableIO，引发了无数多小问题，所以，最近更新总是不断，真烦!!

- 修改:对每个服务器提供PrefixName+Name，Print时我们会知道哪个服务器在Log
- 修复:大修MemoryHook的Delphi+FPC实现，此改动影响ZDB
- 修复:zExpression识别-(，会认为是数字的bug
- 修复:Synapse在IOT的reuse_addr的问题，感谢longbow qq47324905

**服务器不做最大连接数限制都是不安全的，你的服务器无限制，但是操作系统会有限制**

- CrossSocket最大连接被限制到20000(小幅修改CrossSocket的IOCP,EPoll,KQueue内核)
- DIOCP最大连接被限制到20000
- ICS最大连接被限制到500
- INDY最大连接被限制到20
- Syanpse最大连接被限制到100
- P2PVM最大连接无限制（不受操作系统影响）

## 2018-11-20

- 修改:独立了一个PhysicsIO.pas单元
- 修改:处于方便排查问题，StableIO默认将会打开物理连接的Log信息 
- 修复:在IOT平台，如果FPC编译器打开优化，会出现堆栈异常导致某些复杂的程序无法运行，通过修改zDefine.inc已关闭fpc编译器的优化开关
- 修复:TimeTick默认从第三天凌晨开始计数
- 修复:运算符需求，将TimeTick类型从UInt64更变为Int64
- 修复:MemoryHook使用的ThreadVar修饰符更变为var
- 提示:在xnat高频率触发物理断线后，超时1分钟就会暴力关闭连接，在暴力关闭连接中，crossSocket会发生error这类提示，因为物理链接已经断开，接收和发送都会出现error，不用管它
- 修复:暂时未修复的问题：因为windows有最大连接数限制，服务器超过最大连接，就会停止侦听，这种情况也会发生在，连接断开了以后句柄没有立即释放，比如在1分钟内，有3万个连接发过来，并且全都accept，然后3万连接突然全部物理断线，然后又发3万连接过来，这时候，就会超出最大连接限制。因为crossSocket底层没有提供accept事件，我要修改HandeAccept，比较烦麻烦，我在考虑要不要把CrossSocket独立出来做个专属接口项目

## 2018-11-16

- 修复:socket取替了优雅IO的关闭方式，直接close，这一改动对物理断线后的关闭有效
- 修复:xnat的物理连接会在5分钟超时后暴力关闭
- 修改:重写了ICS server接口，现在ICS server为单线程服务器
- 修改:在StableIO的客户端增加了 StopCommunicationTimeTick 参数，无论是否在线和离线，StopCommunicationTimeTick都能准确给出无通讯响应的时间，在开发IOT设备时，该参数可用于重启预检测

## 2018-11-9

- 新功能:IO内核新增序列包机制
- 新功能:IO内核新增不怕断线的StableIO系统
- 新功能:重做时间刻度支持，新版本的时间刻度可以让服务器开机到硬件报废
- 新功能:新增两个Demo：基于Dataset的sql查询演示，聊天室，这两个Demo都使用了StableIO技术，了解StableIO使用可以参考他们，因为使用StableIO太简单了，不需要编写专门的Demo
- 新功能:新增两个文档：[基于序列包的断线重连系统StableIO](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9F%BA%E4%BA%8E%E5%BA%8F%E5%88%97%E5%8C%85%E7%9A%84%E6%96%AD%E7%BA%BF%E9%87%8D%E8%BF%9E%E7%B3%BB%E7%BB%9FStableIO.pdf)  [Zserver中的序列包机制详解](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/Zserver%E4%B8%AD%E7%9A%84%E5%BA%8F%E5%88%97%E5%8C%85%E6%9C%BA%E5%88%B6%E8%AF%A6%E8%A7%A3.pdf)
- 更新:所有内核的指令全部以__@开头和外部指令进行区分
- 修复:之前，ZExpression会把and or xor shr shl div这些关键字当成一个ascii来处理，现在会将pascal关键字and or xor shr shl div当成符号来处理

## 2018-10-30 重做底层原子锁系统

- 重新支持了IOT后台：在IOT后台，Syanpse能稳定对接各种接口
- 重新制作了使用互斥机制替代原子锁的模拟器，互斥机制替代原子锁，会让性能有小幅下降，不会有感觉
- 新作一套使用软件方式实现线程互斥锁的机制，软件模拟互斥机制会更消耗CPU，也会让服务器更耗电，但是可以帮助我们解决原子锁的安全布置问题
- 将所有服务器，全部改成了同步化的数据处理模式：保证了ICS,Indy,CrossSocket,DIOCP,Synapse，这5个接口都能稳定应用于商业后台项目
- XNAT的外网服务器系统，均能跑在ICS,Indy,CrossSocket,DIOCP,Synapse这5个接口下
- Indy服务器接口改造，异步方式只用数据接收和发送，处理数据全部使用同步方式
- ICS服务器接口改造，异步方式只用数据接收和发送，处理数据全部使用同步方式
- DIOCP服务器接口改造，异步方式只用数据接收和发送，处理数据全部使用同步方式
- Synapse服务器接口改造，异步方式只用数据接收和发送，处理数据全部使用同步方式


## 2018-10-28 内核大修

- 保证机制不变的前提，大幅修改内核流程，几乎重做内核
- 本次更新后的内核工作流程的稳定性会有前所未有的提高
- 内核会区分使用带有异步性质的程序（多线程并发+原子锁+同步回调），和非异步程序（单线程往往有更高的稳定性和可维护性）
- CrossSocket服务器本次更新后，使用内核的工作模式为非异步程序，底层为异步，接口后，以非异步方式处理
- CrossSocket客户端本次更新后，使用内核的工作模式为非异步程序，底层为异步，接口后，以非异步方式处理
- DIOCP服务器本次更新后，使用内核的工作模式为异步程序
- DIOCP客户端本次更新后，使用内核的工作模式为异步程序
- ICS服务器本次更新后，使用内核的工作模式为异步程序
- ICS客户端本次更新后，使用内核的工作模式为非异步程序
- INDY服务器本次更新后，使用内核的工作模式为异步程序
- INDY客户端本次更新后，使用内核的工作模式为非异步程序
- Synapse服务器本次更新后，使用内核的工作模式为异步程序
- Synapse客户端本次更新后，使用内核的工作模式为非异步程序
- 优化数据吞吐能力
- 修复CrossSocket底层在EPOLL+KQUEUE中的发送异步回调事件的死锁问题
- 内置了TimeOut检查机制
- 以ab为主，用高达20亿次请求在EPOLL(linux)+KQUEUE(unix)+IOCP(win)压测XNAT通过，整个过程持续了1天
- 重新命名Json库，以ZS_JsonDataObjects.pas代替原来的JsonDataObjects.pas，感谢黑夜杀手建议


## 2018-10-23

> 小幅度更新

- 修复:CrossSocket接口在Linux发送大数据块卡死的问题
- 修复:在Linux环境部署XNAT公网服务器后，用AB严格测试，发现互斥锁会锁不住某些线程(rt底层会线程越权)，改用delphi内置的原子锁，解决该问题，经历15分钟，1000万请求，无任何报错。XNAT只在delphi测试通过。fpc未测试。
- 升级:XNAT支持负载均衡，由外网服务器自动化全权调度，内网穿透的使用无变化
- 演示:新增指令序列化，以及5种Stream收发的Demo
- 文档:新增指令序列化的机制详解
- 修复:DelayClose缺少时间参数的问题
 
## 2018-10-22

> 小幅度更新

- XNAT使用RD远程桌面速度太慢的解决方案：已将后置式的代理转发机制，改为了无延迟的直接转发，负载性能会下降，但是实时性更好
- 重做XNAT内置的IPV6地址生成方式：现在会雪崩式生成无重复的IPV6地址
- XNAT新增一个基于Mapping直接构建ServerFramework的接口以及Demo，都在XNAT的演示目录
- 过去，XNAT的Data交换是后置式的，现在更改为即时交换
- CompleteBuffer增加一个压缩开关，默认使用FastCompress函数(该函数位于MemorySteam64.pas)


## 2018-10-20


> 小幅度更新

- 升级:合并了最近更新的DIOCP库
- 优化:重做地基库中的Base64编码
- 优化:内核的状态计数器不再直接使用inc操作，全部改用原子模式AtomInc，Delphi为原生的原子整数操作，因为在fpc不支持通用原子函数使用互斥锁解决
- 优化:Android平台不再考虑对雷电这类使用kvm加速模拟器支持，开发安卓系统，请使用真实设备
- 优化:调整了几处正常人类看起来不太顺眼的命名
- 工艺:在底层IO开发参考代码中编写了详细备注，看一眼即可明白接口含义, CommunicationFramework_Client_Refrence.pas,CommunicationFramework_Server_Refrence.pas
- 工艺:核心库的加密规则调整:不再使用固定加密算法，内核会随机使用一种加密算法进行工作，
- 工艺:ZDB新增对基础数据结构THashStringList(比THashVariantList更快)支持
- 工艺:ZDB改进网络服务的数据传输规则:凡是涉及到数据内容传输的地方，均会自动使用NIST认可的加密算法之一对其进行加密
- 工艺:将Examples中的所有的目录名全部改成英文
- 升级:本次升级已经测试通过若干已经上线项目，如不出意外，应该是大更新前最后一个小更新

## 2018-10-16

- 修复:修复zExpression无法对1.0e-2浮点识别的bug
- 修复:crossSocket接口反复释放的一个严重bug
- 修复:xNat内创穿透断线重连不稳定的bug，新版本的xNat公网服务器，只会在内网连接后，才会侦听端口，两端运行中现在会极其稳定
- 优化:xNat在手机也可以做内网穿透服务，优化了wiki或则4G,3G连接不稳定造成的通讯问题
- 安全:新增傻瓜化使用的抗量子密码支持(sha3)，在密码货币系统非常常见，经过验证已经与wiki一致， https://en.wikipedia.org/wiki/SHA-3
- 安全:因为sha3有大量的迭代计算，在摩尔定律使用秀儿算法是无法被破解的，同时sha3的计算性能也远远不如fastmd5，也许有数千倍差异，但是，使用sha3来存储和验证密码会万无一失

**重新支持了5大美国国家标准技术研究所(NIST)高级加密标准算法，如下**

- 安全:深度测试rc6加密，通讯协议支持 https://en.wikipedia.org/wiki/RC6
- 安全:重做Twofish加密，通讯协议支持 https://en.wikipedia.org/wiki/Twofish
- 安全:通讯协议支持Serpent加密 https://en.wikipedia.org/wiki/Serpent_(cipher)
- 安全:通讯协议支持Mars加密 https://en.wikipedia.org/wiki/MARS_(cipher)
- 安全:通讯协议支持Rijndael加密 https://en.wikipedia.org/wiki/Advanced_Encryption_Standard

## 2018-9-29

- 修复:FPC中Enum为4 byte定义会丢失符号位的问题
- 修复:Synapse的接口在连接失败时，会尝试切换IPV4+IPV6重新连接
- 修复:Syanpse客户端在连接失败时不返回状态
- 工艺：新增线性事件支持库，LinerAction.pas
- 工艺：新增查询服务器支持（类似dns，主要针对万物互联需求：物联网设备太多，调度服务器不够用，可使用查询服务器分担），HostQuery.pas
- 工艺：兼容基于FPC对IOT的支持：从底层到高级，大规模统一调整命名，此项调整会影响很多工程的代码细节

```delphi

// 本项目中的回调分为3种
// call:   直接指针回调，fpc+delphi有效
// method: 方法回调，会继承一个方法宿主的地址，fpc+delphi有效
// proc:   匿名过程回调，只有delphi有效

// 如果本项调整对于改造现有工程有一定的工作量，请使用字符串批量处理工具
// 在任何有回调重载的地方，方法与函数，均需要在后缀曾加回调类型首字母说明

// 如
RunOp 变更为 RunOpP() // 后缀加P表示匿名类型回调
RunOp 变更为 RunOpM() // 后缀加M表示方法类型的回调
RunOp 变更为 RunOpC() // 后缀加C表示指针类型的回调

SendStreamCmd 变更为 SendStreamCmdP() // 后缀加P表示匿名类型回调
SendStreamCmd 变更为 SendStreamCmdM() // 后缀加M表示方法类型的回调

```


## 2018-9-21

兼容 fpc 3.0.0，可正常编译，建议使用fpc 3.0.4 or last

### 2018-9-18

新平台，新平台测试通过，在新平台树莓派3B+，操作系统 Ubuntu16.04 Mate 下测试成功

新增几个技术文档

**DoStatusIO.pas库**

- 优化，干掉后台线程刷新，改用DoStatus方法替代线程，最简单的使用方法:在你的主循环中，加一句DoStatus不要给参数
- 优化，减少对某些库的依赖性

**其它优化**

- 优化，FPC3.0.4编译出来的程序，整体性能向前提升10%
- 优化，Delphi编译出来的程序，整体性能向前提升10%
- 优化，精细调整服务器主循环：特别说明，在Console模式下后台服务器程序中，必须加上线程同步检查：CheckThreadSynchronize，否则系统Console后台不能正常工作

### 2018-9-15

本次更新，大幅提升底层库的稳定性
================

**重大更新**

- 新接口，同时支持fpc+delphi:新增基于Synapse的通讯接口
- 内网穿透核心技术，同时支持fpc+delphi:新增内网穿透的开发库，由xNatService.pas,xNatClient.pas,xNATPhysics.pas三个小库组成，只需5行代码即可驱动，可使用ZServer支持的任意通讯接口工作。已通过5万ip/每分钟连续4小时的压力穿透测试。
- IoT物联网，只限fpc：对IoT平台的支持，基于fpc在各平台完整支持了Synapse通讯接口，包括：ARM Linux(IoT需求)，Linux x86+x64，OSX x86+x64，Win x86+x64
- 稳定和安全，大规模取替底层库使用inline的机制

**CommunicationFramework.pas库及周边支持**

- 优化，稳定性提升，深度考虑安全性
- 安全，重做了p2pVM的验证系统（每个p2pVM握手时，都会用不同的验证方式）
- 安全，p2pVM第一次握手时，必须有验证码
- 工艺，新增2种协议模式：cpZServer(原来的通讯协议),cpCustom(外部自定义的通讯协议)
- 工艺，重做外部自定义通讯协议的开发工艺：开发自定义通讯协议时，不用再考虑同步异步问题
- 工艺，ProgressBackground全部统一替换成Progress
- 工艺，TCommunicationFrameworkServer服务器触发DoClientConnectAfter会区分协议，cpZServer,cpCustom会有各自处理机制
- 安全，在TCommunicationFramework中以性能换取了Progress的稳定性，客户端+服务器在高并发环境下不会再在这个地方出现异常报告了
- 周边：极小概率bug，修复CrossSocket的连接池释放时发生异常的问题(delphi)
- 周边：控制台模式服务器bug，修复ICS,Syanpse,Indy服务器在Console应用模式中不触发线程同步的问题(delphi)
- 周边：小概率bug，修复ICS服务器使用StopService偶发性的出现卡死的bug
- 周边：合并了最新更新的CrossSocket内核
- 周边：优化，在MemoryStream64.pas库中对ZLib使用解压时，会预先分配内存或则文件空间，避免因为MemoryManager频繁Realloc造成性能耗损(在FPC方向程序中，性能可以相对提供10%)


**TextParsing.pas库及其周边支持**

- 大幅提升的解析性能，使用不变，相较以前，性能向前提升90%
- 新增可替代蚂蚁机制的文本探头技术
- 优化大规模解析程序的复杂度：降低50%
- 修复对123{abc}这种写法的误判行为

**TextDataEngine.pas库**

- 重做数据结构支持，使用与以前不变
- 新增了对THashStringList的内置支持(相对THashVariantList，性能更加优异)

**ListEngine.pas库**

- 安全：对已生成的Hash会使用安全校验措施(我实测Hash的事故率为0，但是有网友报告说Hash会莫名其妙报错，我现在加了一个安全措施，如果还遇到hash无法命中，请检查自己的bug)


**百度翻译api服务器**

- 由于百度不再提供免费翻译api，我们首次运行百度翻译api服务器时，会生成一个配置文件，它会指引你如何注册翻译api的账号

**本次新增两套Demo**

- ZServer4D对大规模后台验证服务器的工艺，比如你后台有Oracle这类大型数据库，该Demo详细描述了三方验证工艺
- XNat内网穿透库的Demo


### 2018-7-6
- 大幅修正底层库的命名规则
- 对fpc/86/64平台支持，全部基础库支持Linux下的无故障编译和运行
- 对fpc编译器3.1.1全面支持
- 新增对mips,树莓派的大小字节序支持
- 修复对32位fpc编译器不认for用Int64的问题
- 修复字符串在fpc编译器运行于linux发生异常的问题
- 新增pascal预编译工具，将pascal代码规范成c风格的全部统一大小写，全面兼容Linux区分大小写文件名的机制
- 本次更新未对树莓派支持
- 本次更新未对fpc的跨平台服务器体系支持(synapse)
- 本次更新未对双通道的用户身份验证事件的回调做支持
- 本次更新未对反向代理更新，反向代理工具系列将留在下次更新一并升级


### 2018-5-26 基于云服务器的开发工艺在本次已经升级

- 删除了云服务器模型1.0及其Demo
- 原商业云服务器框架2.0被移动至Demo中
- 新版本云服务器模型并入到了 ServiceApiModel 中
- 新版本的云服务器模型具备低门槛的技术基因，支持了在Linux系统架设云服务器后台，整个模型只有一个300行代码的群集调度服务器，外加两个附属Demo和一份调度服务器的用法详解文档
- 新增一个HPC服务器的使用Demo
- 新增技术文档，BatchStream机制详解.pdf
- 新增技术文档，HPC服务器的工作机制详解.pdf
- 新增技术文档，延迟反馈机制详解.pdf
- 新增技术文档，云调度服务器用法详解.pdf
- 本次更新未对反向代理更新，反向代理工具系列将留在下次更新一并升级


### 2018-5-26 基于云服务器的开发工艺在本次已经升级

- 删除了云服务器模型1.0及其Demo
- 原商业云服务器框架2.0被移动至Demo中
- 新版本云服务器模型并入到了 ServiceApiModel 中
- 新版本的云服务器模型具备低门槛的技术基因，支持了在Linux系统架设云服务器后台，整个模型只有一个300行代码的群集调度服务器，外加两个附属Demo和一份调度服务器的用法详解文档
- 新增一个HPC服务器的使用Demo
- 新增技术文档，BatchStream机制详解.pdf
- 新增技术文档，HPC服务器的工作机制详解.pdf
- 新增技术文档，延迟反馈机制详解.pdf
- 新增技术文档，云调度服务器用法详解.pdf
- 本次更新未对反向代理更新，反向代理工具系列将留在下次更新一并升级


### 2018-5-21

- 完全移除Pasmp，并行线程的触发面积现在是根据Cpu*2进行展开，安全并行化仍然保持支持Linux，IOS，Android，windows，OSX等主流系统
- 新增HPC内核（延迟线程技术）
- HPC内核暂无Demo，稍等几天会补充上Demo
- 新增10种云服务器种类，稍等几天会补充上Demo
- CrossSocket客户端的连接池现在会自动释放
- ICS客户端在关闭时，会自动释放内存（检测内存泄漏可以用ICS，无泄漏后，再换成DIOCP or Cross）
- 已测试过所有服务器，无内存泄漏，ICSSever,IndyServer,DIOCP,CrossSocketServer
- 修复CrossSocket服务器的连接在关闭后，仍然可以获取到客户端IP的问题
- TDataFrameEngine新增对TListPascalString，TListString, THashStringList支持
- ZDB新增压缩+Copy信息反馈机制，当我们大量使用StorePos在记录条目位置，这时候ZDB被压缩后，StorePos将会变化，ZDB现在能够反馈这一变化
- ZDB网络数据库同步支持ZDB的StorePos变化反馈机制，当ZDB被压缩或则Copy时，客户端将搜到StorePos变化事件
- TPascalString的GetChar机制更改：当Index超过字符串长度时，返回#0
- TUPascalString面向FPC设计的Unicode字符串同步更新GetChar机制，与TPascalString一致
- CoreClasses中的更新：LockObject+LockID，兼容所有平台与并行化调用
- Geometry2DUnit，2D相关支持的几何库大量更新
- 本次所有更新均已在FPC(3.0.4)+Laz(1.8)下测试通过
- 本次更新了大量内核基础库，已做过系统测试，建议更新
- 本次更新并没有新增Demo

### 2018-4-27

本次主要更新基础库，对服务器框架无任何修改
- 内核ListEngine单元支持了巨型链表，巨型Text文件的存储（可以支持上亿行的文本格式读取）
- ZDB新增CSV文件数据导入
- 小幅修改部分函数命名
- 默认情况下，zServer会关闭并行接口，打开并行接口请自行修改zDefine.inc
- 在雷电模拟器测试通过了所有手机端的数据收发
- TPascalString字符串现在拥有更严格的内存分配

本次更新已做过深度测试。如果在开发中的项目，正在使用zServer服务器，可以选择更新


### 2018-4-12

修复内核中的内存越界bug：该bug的症状为无故提示内存无法访问，通过正常debug很难排除，这是是内存越界时所造成的bug

修复TwoFish加密算法bug

已合并近期更新的crossSocket

因为FastKDTree属于统计学领域，在此次更新已移除，如果如果需要使用FastKDTree请访问

https://github.com/PassByYou888/zAnalysis

### 2018-3-13

修复 17.外部协议实现范式 在关闭窗口时出现的异常问题。感谢人在旅途qq563791686

### 2018-3-6

新增地基库: 高速K维空间树(FastKDTree)，最大可以支持2048个纬度的2分空间搜索，FastKDTree支持数据存储和读取，支持货币/单/双浮点/整型 等等单位


修复SmithWaterman算法漏洞

小幅提升内核字符串处理性能

修复部分编译错误


### 2018-3-1

在TPascalString内核中新增模糊字符串对比函数（SmithWaterman），优化与测试完成

该算法属于生物基因工程学科 Smith-Waterman的维基百科地址 https://en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm


### 2018-2-28

为新项目增加了备注信息

修复了zExpression对c字符串的转义处理


### 2018-2-26

修复在三方雷电模拟器中客户端收发数据发生崩溃的问题

重制了内置原子锁，新版本的内置原子锁现在可以支持高并发，支持Linux。( 使用内置原子锁需要在zDefine.inc打开 CriticalSimulateAtomic )

修复zExpression切割分段不正确问题


### 2018-2-25

zExpression新增自定义表达式符号支持

zExpression新增自定义表达式符号的演示

zExpression修复嵌套函数参数不能正确展开接口的问题

zExpression修复解析引擎的数字探头不能识别16进制自然数和函数问题

zExpression修复字符串和数字匹配联合的问题

zExpression支持c代码风格0x16进制语法


### 2018-2-16

小幅提审字符串相关的处理性能


### 2018-2-9

新增百度翻译后台服务模型，支持最新的Ubuntu16.04LTS服务器版，同样也支持Windows服务器

已并入CrossSocket新版本接口，CrossSocket新版本接口的详细情况已在代码中备注，我修改过的Cross内容也已备注(尊重开源)

小幅修了zExpression的OpCache问题

在UnicodeMixedLib中新增了umlURLEncode


### 2018-2-7

项目内置的CrossSocket接口现在已经支持linux，我实测的Linux服务器版本为Ubunto16.04 LTS，感谢CrossSocket作者，感谢群友

修正Linux HelloWorld服务器主循环吃cpu资源问题



### 2018-2-6

Tools新增pascal,c,c++字符串申明互转，字符串10/16进制互转

在 Exameples/19.词法引擎TextParsing的用法演示/ 中新增zExpression的用法，包括if实现，html和文本宏替换，二进制保存和读取op

新增zExpression到主线工程 开源地址 https://github.com/PassByYouOfTeam/zExpression

### zExpression 基本用法演示

```Delphi
var
  rt: TOpCustomRunTime;
  v : Variant;
begin
  // rt为ze的运行函数支持库
  rt := TOpCustomRunTime.Create;
  rt.RegOp('myAddFunction', function(var Param: TOpParam): Variant
    // (a+b)*0.5
    begin
      Result := (Param[0] + Param[1]) * 0.5;
    end);
  rt.RegOp('myStringFunction', function(var Param: TOpParam): Variant
    begin
      Result := Format('字符串长度为:%d', [Length(VarToStr(Param[0]) + VarToStr(Param[1]))]);
    end);

  // 简单数学表达式
  v := EvaluateExpressionValue(False, '1000+{ 这里是备注 ze可以识别pascal和c的备注以及字符串写法 } myAddFunction(1+1/2*3/3.14*9999, 599+2+2*100 shl 3)', rt);
  DoStatus(VarToStr(v));

  // 简单字符串表达式，ze的默认文本处理格式为Pascal
  v := EvaluateExpressionValue(False, 'myStringFunction('#39'abc'#39', '#39'123'#39')', rt);
  DoStatus(VarToStr(v));

  // 简单字符串表达式，我们使用c的文本格式
  v := EvaluateExpressionValue(tsC, 'myStringFunction("abc", "123")', rt);
  DoStatus(VarToStr(v));

  disposeObject(rt);
end;

```



### 2018-2-5

修复使用json传输时因为utf8无法解码的bug,感谢 清风qq274838061

新增zExpression句法引擎


### 2018-2-3

新增一套部署Ubuntu服务器开发环境文档

新增基于Ubuntu16.04 LTS 版本的Linux支持，新增一套Linux下的服务器Demo

新增一个词法Demo，简单演示了怎样分析函数，怎样处理声明（后续会继续增加词法程序范式）


### 2018-2-1

ZDB网络数据库现在支持基于CompleteBuffer的FastPost，可以支持到每秒10000条以上记录量通过服务器存储

删除数据条目的机制改为安全队列

删除了Tools目录中的EXE


### 2018-1-31

在CoreClasses.pas中新增一个原子锁的后背支持功能：CriticalSimulateAtomic，用临界区替代Atomic，在delphi下打开它会让并行功能效率降低，但是它可以兼容所有平台和fpc

修复了p2pVM中的echo，用于模拟tcp链接中的keepalive


### 2018-1-30

新增了P2PVM第二篇技术文档，简单编写，没画图

新增了一份基于FastMD5的性能对比测试(在Examples中可以找到，需要安装Mormot感谢南极土著qq4499972)

最近的新版本尚未在Linux测试，请尽量使用Windows做后台



此次更新以调优为主，调优内容：

修改了通讯框架的数据结构，以Inline方式优化，小幅减少函数调用频率

修改了基于TDataFrameEngine的Json存储格式，此格式以后可以恒定下来

所有的演示内容重新测试和编译

云框架2.0的内容重新测试和编译

Tools目录中的所有工程，已追加了.OXC(FRP的编译打包文件)

将TPeerClient更名为TPeerIO，仍然留有老的TPeerClient指向，不会影响项目编译和升级

在TPeerIO创建时，增加了一个更加安全的原子锁，已测试调优过Cross,ICS,Indy,DIOCP等等接口

取消了AllowPrintCommand属性，以QuietMode代替

用一个已经跑过百万在线的后台项目重新编了一次新版本，并且测试全部通过





### 2018-1-29

通讯新增DIOCP接口 DIOCP是国人所开发的稳定服务器项目 因为呼声较高 所以我今天做了DIOCP的底层接口并且测试通过

新增帮助开发者自行开发网络接口的范例库(source\developerRefrence)

大幅优化TPascalStrings库

新增FastMD5 [FastMD5开源地址](https://github.com/PassByYou888/FastMD5/)

通讯数据结构 TDataFrameEngine 已支持Json打包，Json引擎基于优秀开源项目 [JsonDataObjects](https://github.com/ahausladen/JsonDataObjects)

修复一个小型bug 在云服务器框架2.0中 我们基于雾服务器做表达式计算时 写1*2.1 会出现表达式错误的问题

新增一个制作底层通讯库IO接口的小文档



### 2018-1-25

修复一处不常见的内存泄漏：泄漏症状为，服务器发送一条待反馈的消息SendStreamCMD，在等待反馈中，客户端断线，这时候发生泄漏

对多媒体机制的CompleteBuffer现在可以兼容匿名函数

新增一处实现定制化协议的标准范式Demo，内附详细使用说明，"Examples\17.外部协议实现范式"

p2pVM隧道性能小幅提升10%，在100万压测Demo下能明显感觉性能提高



### 2018-1-24

新增多媒体机制支持 [多媒体通讯](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9C%A8ZS%E4%B8%AD%E4%BD%BF%E7%94%A8%E5%A4%9A%E5%AA%92%E4%BD%93%E9%80%9A%E8%AE%AF%E6%9C%BA%E5%88%B6CompleteBuffer.pdf)

新增p2pVM隧道技术体系，并且新增基于隧道的服务器开发工艺 [p2pVM隧道技术说明](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/ZS%E4%B8%AD%E7%9A%84p2pVM%E9%9A%A7%E9%81%93%E6%8A%80%E6%9C%AF.pdf)

新增两套p2pVM隧道技术体系的服务器Demo，分别是可以带100万压测的Demo，改造认证双通道为p2pVM技术体系的Demo，内容太多，这里就不贴图和源码了

新增两套文档，分别是p2pVM隧道技术体系介绍和多媒体支持的CompleteBuffer介绍文档

优化indy通讯对手机平台的容错性(ipv4+ipv6)



### 2018-1-19

在内网穿透工具中加入了NAT端口非对称映射功能（感谢阿木和十指间的挥洒）

在MH库中新增的上下文分析，并且在内存分析钩子的Demo中也增加该功能的演示代码

内核会自行判断ipv4和ipv6的地址版本，如果使用域名，会根据定义自动优先级选择AAAA和A记录，100%支持苹果ipv6过审

重做了indy的connect方法，ics,indy,cross均新增异步链接（类似6万并发，零阻塞，高频率触发），双通道现在均支持异步链接

indy的connect阻塞被缩短在1秒内

重做wait机制，在indy中，现在使用心跳检测可以用零阻塞机制，和keepalive的效率相同

小幅调整FilePackageWithZDB的UI样式，小幅修正FilePackageWithZDB的ui

在"4.登陆式双向交互服务器框架"中，新增了精确时间戳同步的演示

在Demo中新增了双通道和单向链接的异步方法，这些方法都有写明使用

修改了几个UnicodeMixedLib的核心函数

小幅优化PascalString内核


### 2018-1-16

提交了内网穿透Shell工具全部的源代码，编译时必须包含ZServer4D的服务器框架，在源码目录有OXC包的下载地址

新增了内网穿透的自动化工具，分别是ZSGatewayMiniClient，ZSGatewayMiniServ，相当于内网穿透工具的2.0版本

重做了内网穿透工具的使用说明文档

FilePackageWithZDB现在可以打开OXC压缩文件



### 2018-1-16

新增内网穿透 [宅服架设说明](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9F%BA%E4%BA%8E%E5%86%85%E7%BD%91%E7%A9%BF%E9%80%8F%E5%B7%A5%E5%85%B7ZSGateway%E6%90%AD%E5%BB%BA%E5%AE%85%E6%9C%8D.pdf)

新增云服务器框架2.0 [云服务器框架2.0说明](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%95%86%E4%B8%9A%E4%BA%91%E6%9C%8D%E5%8A%A1%E5%99%A8%E6%A1%86%E6%9E%B62.0%20%E6%A1%86%E6%9E%B6%E8%AF%B4%E6%98%8E.pdf)

新增单元库的介绍文档，[单元库介绍](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/ZServer4D%E5%8D%95%E5%85%83%E5%BA%93%E8%AF%B4%E6%98%8E.pdf)

新增DoStatusIO线程无关性支持

新增数模引擎

新增文件打包工具，开放源码

新增将文件编码成pascal代码的工具，开放源码

新增.drp源码排序工具，开放源码

维护ObjectData，修改了部分方法

现在SendStreamCmd时DataFrameEngine参数可以为nil
	  
```Delphi
client.SendStreamCmd('xx cmd', nil, procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
begin
end) 
```


### 2018-1-3

在演示源码中说明部分使用中的坑（感谢好人一生平安路）

修改了物理断线检测机制

Inline函数可以通过zDefine.inc进行定义




### 2017-12-30


重做压力测试客户端的链接方法，6万并发压力测试客户端性能向前提高1000倍，1秒可以完成6万并发连接

优化了压力测试服务器性能，抗上万压测只会消耗单cpu 10%资源

核心压缩库已被并入主线工程，并且提供了和Delphi/fpc可以进行比较的Demo工程（支持ARM）

服务器端有个大改动：不再支持用户遍历方法，同时服务器内核不在使用链表来管理客户端，所有客户端全部改用Hash数组来管理

此类方法已被替代为匿名函数来工作，同时服务器对链接实例的管理也不再支持链表，从内核到外围，全部使用哈希数组进行管理，这些升级和改动，都是为下一步1000万并发做的铺垫

```Delphi
	// 曾经的老写法很不安全，已废弃
	LockClient;
	for i:=0 to client.count-1 do
	  client.sendcommand(xx) 
	UnLockClient;
```

新的遍历方法
```Delphi
// 后台安全写法1，
Server.ProgressPerClient(Procedure(peerClient:TPeerClient)
begin
  client.sendcommand(xx)
end);
// 后台安全写法2
RecvTunnel.GetClientIDPool(IDPool);
for pcid in IDPool do
  if RecvTunnel.Exists(pcid) then
   begin
	 RecvTunnel.ClientFromID[pcid].sendcommand(xx);
   end;
```

新增服务器的内存Hook库（傻瓜，暴力，非常暴力的释放和管理内存），同时也新增了内存管理领域开发工艺Demo，MH库支持FPC和Delphi

```Delphi
type
  PMyRec = ^TMyRec;

  TMyRec = record
    s1: string;
    p: PMyRec;
  end;

var
  p : PMyRec;
  i : Integer;
  hl: TPointerHashNativeUIntList;
begin
  // 200万次的大批量记录内存申请，最后一次性释放
  // 这种场景情况，可以用于批量释放泄漏的内存

  // 我们内建20万个Hash数组进行存储
  // BeginMemoryHook的参数越大，面对对大批量存储的高频率记录性能就越好，但也越消耗内存
  MH_3.BeginMemoryHook(200000);

  for i := 0 to 200 * 10000 do
    begin
      new(p);
      new(p^.p);
      // 模拟字符串赋值，高频率触发Realloc调用
      p^.s1 := '111111111111111';
      p^.s1 := '1111111111111111111111111111111111';
      p^.s1 := '11111111111111111111111111111111111111111111111111111111111111';
      p^.p^.s1 := '1';
      p^.p^.s1 := '11111111111111111111';
      p^.p^.s1 := '1111111111111111111111111111111111111';
      p^.p^.s1 := '11111111111111111111111111111111111111111111111111111111111111111111111111';

      if i mod 99999 = 0 then
        begin
          // 这里是迭代调用，我们不记录，将MH_3.MemoryHooked设置为False即可
          MH_3.MemoryHooked := False;
          Button1Click(nil);
          Application.ProcessMessages;
          // 继续记录内存申请
          MH_3.MemoryHooked := True;
        end;
    end;
  MH_3.EndMemoryHook;

  DoStatus('总共内存分配 %d 次 占用 %s 空间，地址跨度为：%s ', [MH_3.HookPtrList.Count, umlSizeToStr(MH_3.GetHookMemorySize).Text,
    umlSizeToStr(NativeUInt(MH_3.GetHookMemoryMaximumPtr) - NativeUInt(MH_3.GetHookMemoryMinimizePtr)).Text]);

  MH_3.HookPtrList.Progress(procedure(NPtr: Pointer; uData: NativeUInt)
    begin
      // 现在我们可以释放该地址
      FreeMem(NPtr);
    end);
  MH_3.HookPtrList.PrintHashReport;
  MH_3.HookPtrList.SetHashBlockCount(0);
end;
```


新增的内存Hook库已在ZDB内部有效应用



### 2017-12-20


修复链接池重复链接的Bug（感谢好人一生平安路）

新增LZ77哈夫曼和BRRC压缩算法，请自行对比ZLib性能及压缩率

因为是公有服务器框架，处于安全考虑，通讯包协议在首尾各增加4byte作为验证标记，发行时可以有80亿的独特协议包数据（你只需要从80亿的数值选择一个独特的验证序，别人就算有ZS源码也无法访问你的服务器）



### 2017-12-15

修复了底层hash函数库因为没有传递对象，导致云服务器的状态列表不更新的问题



### 2017-12-15

新增6万压测Demo

ZDB新增一组提交数据条目和大图片捆绑的Demo，每张图片各2M，此机制可用于任何Stream数据，请参考并且依次类推

ZDB新增安全缓存，其工作机制为：当触发数据库写操作时，开始计时，如果5秒内没有第二次写操作，就会将回写缓存立即写入文件。主要用于保护数据库文件损坏。

ZDB和ZServer均已在Linux下测试通过




### 2017-12-10

新增ZDB服务器Demo，新增ZDB IN FMX Demo，已在所有手机测试通过，所有的详细说明都内附在Demo源码中

将ZDB数据库的查询能力向前优化了15倍，内存消耗也因此提高了10%，默认单库体量为500万条，如果服务器内存在16G以上，需要自行在ZDBEngine中修改最大Cache和退火参数

修复了单通道客户端Indy,ICS,CrossSocket在发出链接后的ID不更新问题(感谢AK47)

在EZServer增加了使用Special实例的方法(感谢AK47)



### 2017-12-8

新增大数据库引擎ZDB的网络服务器Demo，并且内附了详细功能说明 [Demo说明](https://github.com/PassByYou888/ZServer4D/tree/master/Examples/11.ZDB%E6%95%B0%E6%8D%AE%E9%9B%86%E6%9C%8D%E5%8A%A1%EF%BC%88%E5%8C%85%E5%90%AB%E6%8F%90%E4%BA%A4%E5%9B%BE%E7%89%87%EF%BC%89)

在ZDB中新增压缩，拷贝，替换等等底层功能

将ZDB的退火系统改成了自动化功能，只需要设置几个参数即可

新增带有验证机制的ZDB服务模型



### 2017-12-6

新增单机数据引擎,新增分布式和网络数据引擎

新增单机数据库Demo

新增20k链接压力测试，服务器光速响应，并且无内存泄漏

通讯内核新增批次化BigStream支持

通讯内核新增special接口

通讯内核新增了很多状态机

修复了CrossSocket在退出时报异常的问题

优化内核Hash性能

优化了文件读写内核，内部集成了回写和预读缓存机制，小幅降低IO消耗频率

群集中心服务器可以一键开发和部署



### 2017-11-29

修复fpc对CommunicationFrameworkDoubleTunnelIO.pas单元的兼容性

匿名函数在异步回调系统是非常好的机制，Zserver4d新增匿名函数支持，同时新增了一套匿名函数的支持Demo

处于安全考虑，在ICS,Indy,CrossSocket服务器端从现在开始，均不支持WaitSend的阻塞通讯模式，客户端仍然支持WaitSend


### 2017-11-27

修复indy接口的服务器，新版的Zserver4D在IndyServer将不再支持WaitSend阻塞化模式，必须使用异步，在CrossSocket，ICS在服务器仍然可以使用WaitSend，Indy,ics,CorossSocket的客户端支持保持不变，新版的IndyServer会更加稳定

因为某些低版本的Windows不支持精确计时器，CoreCipher取消了并行化编码库支持，改为定义，在没有定义使用异步编码时，将不会使用PasMP

修复了MD5支持算法，在x86,x64,arm下现在超过4G的md5计算均能一致

针对freepascal编译器改动：将ListEngine的string定义全部更新成了SystemString，完全兼容UnicodeString



### 2017-11-25

测底修复了BitStream的内存拷贝问题，现在收发大型Stream均不会拷贝内存，能做到以最低开销承载1000个用户同时下载2G以上的文件

修复了所有的服务器的内存泄漏（注意：但是客户端仍然会有泄漏，因为客户端设计就是采用不考虑回收的粗犷方式）

内部机理做了小幅调整，处理内部工作某些函数重新命名

新增了一套基于Indy接口在移动平台做服务器的使用Demo



### 2017-11-24

新增登录式高级云服务器的作用介绍

更新了在IOCP中使用WSASend因为缓冲区问题发送失败的bug 感谢ak47的测试回报
