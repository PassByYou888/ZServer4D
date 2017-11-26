### ZServer4D 是一套从商业项目剥离而出的云服务器中间件，可以承载百万级的分布式负载服务

## 介绍

支持运行平台Android,IOS,Win32/64,Linux,OSX

支持开发平台：FPC以及DelphiXE2以后的版本


因为时间关系，ZServer4D只提供了基础Demo，不提供性能测试，并发测试，REST，BAAS等等高级Demo，只提供简单使用文档


如果你在使用ZServer4D，并且对开发有疑问，请加群去寻找答案（请不要直接联系作者，因为作者一般不在线）



## 通讯接口支持

1.indy(open source) http://www.indyproject.org/
id是阻塞模式的通讯组件，已在ZServer4D内部集成


2.CrossSocket(open source) https://github.com/winddriver/Delphi-Cross-Socket
异步式通讯组件，已在ZServer4D内部集成


3.ICS(open source) www.overbyte.be
异步式通讯组件，已在ZServer4D内部集成

 
 
qq群490269542
  
 
作者qq600585（不接受加人）

-----------------------------------------
## 关于内存泄漏

ZServer4D内置的服务器有3种：Indy，ICS，CrossSocket，所有的服务器均无内存泄漏

ZServer4D内置的客户端采用的是抛弃式链接，每次链接登录服务器都会抛弃现有链接类，并且重建新接口链接，这里不会释放老的链接（这样干是保证客户端的正常工作）

在ZServer4D中所捆绑的类，包括编解码，链表，数据库，均无内存泄漏


-----------------------------------------
## 关于切入和使用

使用ZServer4D前，请仔细阅读本项目所提供的Demo，做到对基于ZServer4D的开发范式非常了解

技术调研包括

了解大型FileStream的和大型MemoryStream的收发，并且了解服务器在这方面的处理机制，如何使用这些功能

了解带有反馈机制的SendStream和SendConsole，了解反馈接口的使用

了解WaitSendStream,WaitSendConsole的使用范式

了解DirectSendStream，DirectSendConsole的使用范式

了解服务器延迟响应模式的使用范式

了解如何使用不同的网络接口做服务器和客户端，网络接口包括Indy，ICS，CrossSocket


以上只是我个个人建议，如果有时间，花上一周以上时间把ZServer的核心读一次再使用



-----------------------------------------
## 更新日志

2017-11-27

修复indy接口的服务器，新版的Zserver4D在IndyServer将不再支持WaitSend阻塞化模式，必须使用异步，在CrossSocket，ICS在服务器仍然可以使用WaitSend，Indy,ics,CorossSocket的客户端支持保持不变

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
