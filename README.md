## ZServer4D 是一套从商业项目(游戏类)剥离而出的云服务器中间件，可以承载百万级在线的分布式长连接负载

 
## 介绍

支持运行平台Android,IOS,Win32/64,Linux,OSX

支持开发平台：FPC以及DelphiXE10和以后的版本

并行计算支持HPC服务器，并行深度参数服务器可配置

良好支持轻量云主机，腾讯云，阿里云，亚马逊云，均有数百台使用ZServer4D的服务器在运行中

支持内网穿透，在公司或家里自己架设宅服 [宅服架设说明](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9F%BA%E4%BA%8E%E5%86%85%E7%BD%91%E7%A9%BF%E9%80%8F%E5%B7%A5%E5%85%B7ZSGateway%E6%90%AD%E5%BB%BA%E5%AE%85%E6%9C%8D.pdf)

ZServer4D的前后台均支持苹果要求的IPV6审核条件，支持AAAA,A记录秒切，支持所有IPV6的云主机

内置高级加密系统，一万在线客户端会有一万把密钥，并且能动态定时更换密钥（请参考ZServer4D的附属开源项目 https://github.com/PassByYou888/CoreCipher ）

架构设计可以轻松实现IP池和入口网络秒切，非常利于在国内商业环境中防止对手DDos攻击

支持中心化网络群集，支持中心化网络群集一键对接

内置NoSQL并行化内核，良好支持大数据，良好支持聚类分析，支持分布式数据库负载，支持分布式数据查询结果汇集（NoSQL技术体系从11月初开始一直处于整理中，工程较大，可能短期不能完成，但是未来会以开源形式为Delphi国内带来前沿的数据库支持体系）

(NoSQL并行化内核已经完成，聚类分析和分布式负载已完成50%文档)


## 操作系统和设备支持一览，test with Delphi 10.2 upate 1 Tokyo

Windows: CrossSocket(C/S OK), DIOCP(C/S OK), ICS(C/S OK), Indy(C/S OK)

Linux(X64): Indy(C/S OK), CrossSocket(C/S OK)

Android:Indy(C/S OK), CrossSocket(Only Client)

IOS Device: Indy(C/S OK), CrossSocket(Only Client)

IOS Simulaor: n/a

OSX: Indy(C/S OK)，ICS(未测试), CrossSocket(C/S OK)


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

[部署Ubuntu服务器的开发环境](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E9%83%A8%E7%BD%B2Ubuntu%E6%9C%8D%E5%8A%A1%E5%99%A8%E7%9A%84Delphi%E5%BC%80%E5%8F%91%E7%8E%AF%E5%A2%83.pdf)

[百度翻译服务后台(支持Ubuntu16.04LTS服务器)](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E7%99%BE%E5%BA%A6%E7%BF%BB%E8%AF%91%E6%9C%8D%E5%8A%A1%E7%9A%84%E5%90%8E%E5%8F%B0%E5%AE%9E%E7%8E%B0%E8%8C%83%E5%BC%8F.pdf)
  [百度翻译服务API(支持Ubuntu16.04LTS服务器)](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E7%99%BE%E5%BA%A6%E7%BF%BB%E8%AF%91%E6%9C%8D%E5%8A%A1%E7%9A%84API%E8%B0%83%E7%94%A8%E8%8C%83%E4%BE%8B.pdf) 
 


## 通讯接口支持(Delphi 10或则更高版本，低版本未测试)

1.indy 阻塞模式的通讯组件，已在ZServer4D内部集成

(open source) http://www.indyproject.org/


2.CrossSocket 异步式通讯组件，已在ZServer4D内部集成

(open source) https://github.com/winddriver/Delphi-Cross-Socket


3.ICS异步式通讯组件，已在ZServer4D内部集成

(open source) http://www.overbyte.be


4.DIOCP 国人所开发的稳定DIOCP通讯库
 
(Open source) https://github.com/ymofen/diocp-v5



## 通讯接口支持(FreePascal with Lazarus 计划支持中)

1.synapse4(open source) 计划支持

支持ssl的优秀开源项目


2.fcl-net(open source) 计划支持

freepascal内置的网络库


## zServer4D究竟是个什么东西？
从架构来说，ZSERVER4D是一个中间件。

从生产来说，ZSERVER4D是一套后台系统的开发工艺，也可以说是一套系统标准。

从程序和技术来说，iocp,epoll,kqueue它们只是一套从操作系统提供出来的服务器开发接口，拥有这类接口的后台服务器现在多如牛毛，而我们基于这些接口要做系统级开发，是件非常艰深的工作：优化内存池，服务器系统安全，处理并发后台的同步和异步，处理各种错误，新增通讯接口，兼容手机，Linux，Windows等等平台。

ZSERVER4D是作为系统的生产工艺，用标准开发模型解决了这些技术问题。

## 注意

REST,BAAS等等单项式的HTTP服务请自行在服务器开发和集成，ZServer4D不提供外部http支持

如果你在使用ZServer4D，并且对开发有疑问，请加群去寻找答案（请不要直接联系作者，作者不爱交网友）

qq群490269542
 
作者qq600585（不接受加人）



## 关于内存泄漏

ZServer4D内置的服务器有4种：Indy，ICS，CrossSocket，DIOCP所有的服务器均无内存泄漏

ZServer4D内置的客户端采用的是用完抛弃的工作方式，会有少量内存泄漏，诸如：indy，DIOCP(客户端)

在ZServer4D中所捆绑的类，包括编解码，链表，数据库，均无内存泄漏



## 关于压力测试

压力测试如果链接超过6万，Windows系统会自动关闭侦听端口，具体原因不详，压测请尽量保持在6万以内，超过6万服务器侦听端口会自动关闭，只需要将服务器重开一次即可


## 关于切入和使用

使用ZServer4D前，请仔细阅读本项目所提供的Demo和文档，做到对基于ZServer4D的开发范式非常了解

参考 [库说明](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/ZServer4D%E5%8D%95%E5%85%83%E5%BA%93%E8%AF%B4%E6%98%8E.pdf)

[日常问题汇总](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/ZServer4D%E6%97%A5%E5%B8%B8%E9%97%AE%E9%A2%98%E6%B1%87%E6%80%BB.pdf)



