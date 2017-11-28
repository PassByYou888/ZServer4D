### ZServer4D 是一套从商业项目剥离而出的云服务器中间件，可以承载百万级在线的分布式长连接负载

## 介绍

支持运行平台Android,IOS,Win32/64,Linux,OSX

支持开发平台：FPC以及DelphiXE2以后的版本

并行计算支持HPC服务器，并行深度参数服务器可配置

良好支持轻量云主机，腾讯云，阿里云，亚马逊云，均有数百台使用ZServer4D的服务器在运行中

ZServer4D的前后台均支持苹果要求的IPV6审核条件，支持AAAA,A记录秒切，支持所有IPV6的云主机

内置高级加密系统，一万在线客户端会有一万把密钥，并且能动态定时更换密钥（请参考ZServer4D的附属开源项目 https://github.com/PassByYou888/CoreCipher ）

架构设计可以轻松实现IP池和入口网络秒切，非常利于在国内商业环境中防止对手DDos攻击

内置NoSQL并行化内核，良好支持大数据，良好支持聚类分析，支持分布式数据库负载，支持分布式数据查询结果汇集（NoSQL技术体系从11月初开始一直处于整理中，工程较大，可能短期不能完成，但是未来会以开源形式为Delphi国内带来前沿的数据库支持体系）

支持中心化网络群集，支持中心化网络群集一键对接



## 通讯接口支持(只限Delphi)

1.indy(open source) http://www.indyproject.org/
id是阻塞模式的通讯组件，已在ZServer4D内部集成


2.CrossSocket(open source) https://github.com/winddriver/Delphi-Cross-Socket
异步式通讯组件，已在ZServer4D内部集成


3.ICS(open source) www.overbyte.be
异步式通讯组件，已在ZServer4D内部集成
 
 
qq群490269542
  
 
作者qq600585（不接受加人）


## 注意

REST,BAAS等等单项式的HTTP服务请自行在服务器开发和集成，ZServer4D不提供外部http支持

如果你在使用ZServer4D，并且对开发有疑问，请加群去寻找答案（请不要直接联系作者，因为作者一般不在线）


## 关于内存泄漏

ZServer4D内置的服务器有3种：Indy，ICS，CrossSocket，所有的服务器均无内存泄漏

ZServer4D内置的客户端采用的是抛弃式链接，每次链接登录服务器都会抛弃现有链接类，并且重建新接口链接，这里不会释放老的链接（这样干是保证客户端的正常工作）

在ZServer4D中所捆绑的类，包括编解码，链表，数据库，均无内存泄漏


## 关于切入和使用

使用ZServer4D前，请仔细阅读本项目所提供的Demo，做到对基于ZServer4D的开发范式非常了解

技术调研包括

了解大型FileStream的和大型MemoryStream的收发，并且了解服务器在这方面的处理机制，如何使用这些功能

了解带有反馈机制的SendStream和SendConsole，了解反馈接口的使用

了解WaitSendStream,WaitSendConsole的使用范式

了解DirectSendStream，DirectSendConsole的使用范式

了解服务器延迟响应模式的使用范式

了解如何使用不同的网络接口做服务器和客户端，网络接口包括Indy，ICS，CrossSocket

以上只是我个人建议，如果有时间把ZServer的核心读一次最好



## 更新日志

2017-11-29

修复fpc对CommunicationFrameworkDoubleTunnelIO.pas单元的兼容性

匿名函数在异步回调系统是非常好的机制，Zserver4d新增匿名函数支持，同时新增了一套匿名函数的支持Demo

处于安全考虑，在ICS,Indy,CrossSocket服务器端从现在开始，均不支持WaitSend的阻塞通讯模式，客户端仍然支持WaitSend


2017-11-27

修复indy接口的服务器，新版的Zserver4D在IndyServer将不再支持WaitSend阻塞化模式，必须使用异步，在CrossSocket，ICS在服务器仍然可以使用WaitSend，Indy,ics,CorossSocket的客户端支持保持不变，新版的IndyServer会更加稳定

因为某些低版本的Windows不支持精确计时器，CoreCipher取消了并行化编码库支持，改为定义，在没有定义使用异步编码时，将不会使用PasMP

修复了MD5支持算法，在x86,x64,arm下现在超过4G的md5计算均能一致

针对freepascal编译器改动：将ListEngine的string定义全部更新成了SystemString，完全兼容UnicodeString



2017-11-25

测底修复了BitStream的内存拷贝问题，现在收发大型Stream均不会拷贝内存，能做到以最低开销承载1000个用户同时下载2G以上的文件

修复了所有的服务器的内存泄漏（注意：但是客户端仍然会有泄漏，因为客户端设计就是采用不考虑回收的粗犷方式）

内部机理做了小幅调整，处理内部工作某些函数重新命名

新增了一套基于Indy接口在移动平台做服务器的使用Demo



2017-11-24

新增登录式高级云服务器的作用介绍

更新了在IOCP中使用WSASend因为缓冲区问题发送失败的bug 感谢ak47的测试回报
