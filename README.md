## ZServer4D 是一套从商业项目(游戏类)剥离而出的云服务器中间件，可以承载百万级在线的分布式长连接负载
 
 
## 介绍

支持运行平台Android,IOS,Win32/64,Linux,OSX,物联网IOT(ARM Linux 18.04或则更高版，包括树莓1-3代，香橙，高通，三星)

支持编译器：FPC3.0.4以及DelphiXE10.2和以后的版本

并行计算支持HPC服务器，并行深度参数服务器可配置

良好支持轻量云主机，腾讯云，阿里云，亚马逊云，均有数百台使用ZServer4D的服务器在运行中

支持内置的Pascal语系的内网穿透核心库XNat，非FRP外部支持

支持基于FRP的内网穿透，在公司或家里自己架设宅服 [宅服架设说明](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9F%BA%E4%BA%8E%E5%86%85%E7%BD%91%E7%A9%BF%E9%80%8F%E5%B7%A5%E5%85%B7ZSGateway%E6%90%AD%E5%BB%BA%E5%AE%85%E6%9C%8D.pdf)

ZServer4D的前后台均支持苹果要求的IPV6审核条件，支持AAAA,A记录秒切，支持所有IPV6的云主机

内置高级加密系统，一万在线客户端会有一万把密钥，并且能动态定时更换密钥（请参考ZServer4D的附属开源项目 https://github.com/PassByYou888/CoreCipher ）

架构设计可以轻松实现IP池和入口网络秒切，非常利于在国内商业环境中防止对手DDos攻击

支持去中心化网络群集，支持去中心化网络群集一键对接

全面支持Linux服务器开发(fpc方向)

内置NoSQL并行化内核，良好支持大数据，良好支持聚类分析，支持分布式数据库负载，支持分布式数据查询结果汇集（NoSQL技术体系从11月初开始一直处于整理中，工程较大，可能短期不能完成，但是未来会以开源形式为Delphi国内带来前沿的数据库支持体系）

(NoSQL并行化内核已经完成，聚类分析和分布式负载已完成50%文档)

## 开发平台支持

- Delphi及IDE要求：Delphi Rad studio XE10.2.1 or Last
- FPC编译器支持:FPC3.0.4 or last
- CodeTyphon 6.0 or last（尽量使用Online更新到最新的Cross工具链+相关库）

## 平台支持，test with Delphi 10.2 upate 1 Tokyo and FPC 3.0.4 with Lazarus1.8

- Windows: delphi-CrossSocket(C/S OK), delphi-DIOCP(C/S OK), delphi-ICS(C/S OK), delphi-Indy(C/S OK),delphi+fpc Synapse(C/S OK)
- Android:Indy(C/S OK), CrossSocket(Only Client)
- IOS Device: Indy(C/S OK), CrossSocket(Only Client)
- IOS Simulaor: n/a
- OSX: Indy(C/S OK)，ICS(未测试), CrossSocket(C/S OK)
- Ubuntu16.04 x64 server: Indy(C/S OK), CrossSocket(C/S OK)
- Ubuntu18.04 x86+x64 Desktop:only fpc3.0.4 Synapse(C/S OK)
- Ubuntu18.04 x86+x64 Server:only fpc3.0.4 Synapse(C/S OK) 
- Ubuntu18.04 arm32+arm neon Server:only fpc3.0.4 Synapse(C/S OK)
- Ubuntu18.04 arm32+arm neon desktop:only fpc3.0.4 compile ok,no test on run.  


## 文档

[编译指南](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/zServer4D%E7%BC%96%E8%AF%91%E6%8C%87%E5%8D%97.pdf)
 [日常问题](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/ZServer4D%E6%97%A5%E5%B8%B8%E9%97%AE%E9%A2%98%E6%B1%87%E6%80%BB.pdf)
 [库说明](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/ZServer4D%E5%8D%95%E5%85%83%E5%BA%93%E8%AF%B4%E6%98%8E.pdf)

[宅服架设](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9F%BA%E4%BA%8E%E5%86%85%E7%BD%91%E7%A9%BF%E9%80%8F%E5%B7%A5%E5%85%B7ZSGateway%E6%90%AD%E5%BB%BA%E5%AE%85%E6%9C%8D.pdf)

[多媒体通讯](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9C%A8ZS%E4%B8%AD%E4%BD%BF%E7%94%A8%E5%A4%9A%E5%AA%92%E4%BD%93%E9%80%9A%E8%AE%AF%E6%9C%BA%E5%88%B6CompleteBuffer.pdf)

[p2pVM隧道技术](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/ZS%E4%B8%AD%E7%9A%84p2pVM%E9%9A%A7%E9%81%93%E6%8A%80%E6%9C%AF.pdf)
 [p2pVM第二篇机理说明](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9F%BA%E4%BA%8EZS%E7%9A%84%20p2pVM%E7%AC%AC%E4%BA%8C%E7%AF%87%E6%9C%BA%E7%90%86%E8%AF%B4%E6%98%8E.pdf)

[云服务器框架](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%95%86%E4%B8%9A%E4%BA%91%E6%9C%8D%E5%8A%A1%E5%99%A8%E6%A1%86%E6%9E%B62.0%20%E6%A1%86%E6%9E%B6%E8%AF%B4%E6%98%8E.pdf)

[怎样开发基于ZS的底层通讯IO接口](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9F%BA%E4%BA%8EZServer4D%E5%BC%80%E5%8F%91%E5%BA%95%E5%B1%82%E9%80%9A%E8%AE%AFIO%E6%8E%A5%E5%8F%A3%E7%9A%84%E6%96%B9%E6%B3%95.pdf)

[部署Ubuntu服务器的开发环境(delphi方向)](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E9%83%A8%E7%BD%B2Ubuntu%E6%9C%8D%E5%8A%A1%E5%99%A8%E7%9A%84Delphi%E5%BC%80%E5%8F%91%E7%8E%AF%E5%A2%83.pdf)

[Linux桌面开发指南(fpc方向)](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/Linux%E6%A1%8C%E9%9D%A2%E5%BC%80%E5%8F%91%E6%8C%87%E5%8D%97.pdf)

[BatchStream机制详解](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/BatchStream%E6%9C%BA%E5%88%B6%E8%AF%A6%E8%A7%A3.pdf)

[HPC服务器的工作机制详解](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/HPC%E6%9C%8D%E5%8A%A1%E5%99%A8%E7%9A%84%E5%B7%A5%E4%BD%9C%E6%9C%BA%E5%88%B6%E8%AF%A6%E8%A7%A3.pdf)

[延迟反馈机制详解](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%BB%B6%E8%BF%9F%E5%8F%8D%E9%A6%88%E6%9C%BA%E5%88%B6%E8%AF%A6%E8%A7%A3.pdf)

[云调度服务器用法详解](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E4%BA%91%E8%B0%83%E5%BA%A6%E6%9C%8D%E5%8A%A1%E5%99%A8%E7%94%A8%E6%B3%95%E8%AF%A6%E8%A7%A3.pdf)

[百度翻译服务后台(支持Ubuntu16.04LTS服务器)](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E7%99%BE%E5%BA%A6%E7%BF%BB%E8%AF%91%E6%9C%8D%E5%8A%A1%E7%9A%84%E5%90%8E%E5%8F%B0%E5%AE%9E%E7%8E%B0%E8%8C%83%E5%BC%8F.pdf)
  [百度翻译服务API(支持Ubuntu16.04LTS服务器)](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E7%99%BE%E5%BA%A6%E7%BF%BB%E8%AF%91%E6%9C%8D%E5%8A%A1%E7%9A%84API%E8%B0%83%E7%94%A8%E8%8C%83%E4%BE%8B.pdf) 
 


## 通讯接口支持(开发平台需求 Delphi Rad studio 10.2或则更高版本，低版本不支持)

1.indy 阻塞模式的通讯组件，已在ZServer4D内部集成

(open source) http://www.indyproject.org/


2.CrossSocket 异步式通讯组件，已在ZServer4D内部集成

(open source) https://github.com/winddriver/Delphi-Cross-Socket


3.ICS异步式通讯组件，已在ZServer4D内部集成

(open source) http://www.overbyte.be


4.DIOCP 国人所开发的稳定DIOCP通讯库
 
(Open source) https://github.com/ymofen/diocp-v5


## 通讯接口支持(FreePascal 3.0.4 or last with Lazarus，低版本不支持)

1.synapse4(open source) 已经在ZServer4D内部集成，主要支持fpc，同时也兼容delphi

synapse是支持ssl的优秀开源项目


## zServer4D究竟是个什么东西？

从技术来说：ZSERVER4D是一个可以穿越Delphi,FPC,在所有平台中工作的服务器+客户端中间件

从生产来说：ZSERVER4D是一套后台系统的开发工艺，也可以说是一套系统标准，

从程序和技术来说，iocp,epoll,kqueue它们只是一套从操作系统提供出来的服务器开发接口，拥有这类接口的后台服务器现在多如牛毛，而我们基于这些接口要做系统级开发，是件非常艰深的工作：优化内存池，服务器系统安全，处理并发后台的同步和异步，处理各种错误，新增通讯接口，兼容手机，Linux，Windows等等平台。

ZSERVER4D是作为系统的生产工艺，用标准开发模型解决了这些技术问题。


## 关于物联网IoT平台

ZServer4D对IoT平台的开发要求必须使用FPC编译器，ZServer4D对物联网的支持的标准系统暂时只有ARM Ubuntu18.04，要求最低FPC编译器版本为3.0.4（需要和它对应的RT内核库）


## 关于处理机架构和大小端字节序

早期的PPC处理器架构都是大端字节序，这也造成了，早期的网络通讯标准，都是大端，它一直在影响我们使用。但是后来，到现在，大端字节序已经慢慢消失，主流的Intel处理器架构，包括ARM，X86，现在都采用了小端字节序。因此，在ZServer中，所有的二进制收发，都是以小端字节序工作的。假如你在后台需要处理大端字节序，使用外部自定义协议模式即可。

大端字节序的典型场景：比如在Indy的通讯接口中，我们发送Integer时，如果打开转换参数，它会被转换成大端字节序。


## 关于内存泄漏

ZServer4D内置的服务器有：Indy, ICS, CrossSocket, DIOCP, Synapse所有的服务器均无内存泄漏

ZServer4D内置的客户端接口，某些库采用的是用完抛弃的设计方式，这是针对应用程序使用的客户端库，并不是后台使用，这会有少量内存泄漏，它们是：indy，DIOCP(客户端)

**有内存泄漏行为的客户端接口**

- TCommunicationFramework_Client_Indy，用完抛弃
- TCommunicationFramework_Client_DIOCP，用完抛弃

**无内存泄漏行为的安全客户端**

- TCommunicationFramework_Client_ICS，安全回收，无泄漏
- TCommunicationFramework_Client_CrossSocket，安全回收，无泄漏
- TCommunicationFramework_Client_Synapse，安全回收，无泄漏

**在ZServer4D中所捆绑的类，包括编解码，链表，数据库，均无内存泄漏**



## 关于压力测试

压力测试如果链接超过6万，Windows系统会自动关闭侦听端口，具体原因不详，压测请尽量保持在6万以内，超过6万服务器侦听端口会自动关闭，只需要将服务器重开一次即可


## 关于切入和使用

使用ZServer4D前，请仔细阅读本项目所提供的Demo和文档，做到对基于ZServer4D的开发范式非常了解

参考 [库说明](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/ZServer4D%E5%8D%95%E5%85%83%E5%BA%93%E8%AF%B4%E6%98%8E.pdf)

[日常问题汇总](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/ZServer4D%E6%97%A5%E5%B8%B8%E9%97%AE%E9%A2%98%E6%B1%87%E6%80%BB.pdf)


## 最后一更新日志

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
- 对已生成的Hash会使用安全校验措施

**百度翻译api服务器**

- 由于百度不再提供免费翻译api，我们首次运行百度翻译api服务器时，会生成一个配置文件，它会指引你如何注册翻译api的账号


[更多更新日志](https://github.com/PassByYou888/ZServer4D/update.md)

## 注意

REST,BAAS等等单项式的HTTP服务请自行在服务器开发和集成，ZServer4D不提供外部http支持

如果你在使用ZServer4D，并且对开发有疑问，请加群去寻找答案（请不要直接联系作者）

qq群490269542

**请支持ZServer4D的后续开发** [支付宝转账](https://github.com/PassByYou888/ZServer4D/raw/master/alipay.jpg)



