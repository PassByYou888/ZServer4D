## 介绍

ZSERVER4D是一套高级通讯系统的地基平台，它偏向于开发工艺和多平台支持

**注意：ZS以后不再维护(见ZNet中的说明)，更新请转ZNet（自带ZS升级程序，可一键升级到ZNet）** https://github.com/PassByYou888/ZNet

 
## 功能

**ZServer4D为SAAS自动化群集构建的基础支持库**，详见 https://github.com/PassByYou888/zCloud

支持运行平台Android,IOS,Win32/64,Linux,OSX,物联网IOT(任意版本的linux均能支持，包括树莓1-3代，香橙，高通，三星，小序列cpu mips linux)

支持编译器：FPC3.0.4以及DelphiXE10.2和以后的版本

并行计算支持HPC服务器，并行深度参数服务器可配置

良好支持轻量云主机，腾讯云，阿里云，亚马逊云，均有数千台使用ZServer4D的服务器在运行中(2019一季度状态)

支持内置的Pascal语系的内网穿透稳定核心库XNat(直接内核支持，非外部支持)

支持基于FRP的内网穿透(外部shell方式支持)，在公司或家里自己架设宅服 [宅服架设说明](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9F%BA%E4%BA%8E%E5%86%85%E7%BD%91%E7%A9%BF%E9%80%8F%E5%B7%A5%E5%85%B7ZSGateway%E6%90%AD%E5%BB%BA%E5%AE%85%E6%9C%8D.pdf)

ZServer4D的前后台均支持苹果要求的IPV6审核条件，支持AAAA,A记录秒切，支持所有IPV6的云主机

内置高级加密系统，一万在线客户端会有一万把密钥，并且能动态定时更换密钥（请参考ZServer4D的附属开源项目 https://github.com/PassByYou888/CoreCipher ）

支持去中心化网络群集，支持去中心化网络群集一键对接

内置抗量子密码支持 https://en.wikipedia.org/wiki/SHA-3

**支持了5大美国国家标准技术研究所(NIST)高级加密标准算法**

- rc6加密，通讯协议支持 https://en.wikipedia.org/wiki/RC6
- Twofish加密，通讯协议支持 https://en.wikipedia.org/wiki/Twofish
- Serpent加密，通讯协议支持 https://en.wikipedia.org/wiki/Serpent_(cipher)
- Mars加密，通讯协议支持 https://en.wikipedia.org/wiki/MARS_(cipher)
- Rijndael加密，通讯协议支持 https://en.wikipedia.org/wiki/Advanced_Encryption_Standard

**支持5G万兆以太**: 需要先升级服务器的CPU和内存，zServer内置高速CompleteBuffer，可以让后台平滑过渡至万兆以太，音频+视频+图片+文件均可使用CompleteBuffer

架构设计可以轻松实现IP池和入口网络秒切，非常利于在国内商业环境中防止对手DDos攻击

全面支持Linux服务器开发(fpc方向)

内置NoSQL并行化内核，良好支持大数据，良好支持聚类分析，支持分布式数据库负载，支持分布式数据查询结果汇集（NoSQL技术体系从11月初开始一直处于整理中，工程较大，可能短期不能完成，但是未来会以开源形式为Delphi国内带来前沿的数据库支持体系）

## 开发平台支持

- Delphi及IDE要求：Delphi Rad studio XE10.3.1 or Last
- FPC编译器支持:FPC3.0.4 or last,可参看本项目随附的[IOT入手指南](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%85%A5%E6%89%8BIOT%E7%9A%84%E5%AE%8C%E5%85%A8%E6%94%BB%E7%95%A5.pdf)将FPC升级至github最新的版本
- CodeTyphon 6.0 or last（尽量使用Online更新到最新的Cross工具链+相关库）

## 平台支持，test with Delphi 10.3 update 2 and FPC 3.0.4

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
- Ubuntu16.04 Mate arm32 desktop:only fpc3.0.4 compile ok, test passed  
- Raspberry Pi 3 Debian linux armv7 desktop,only fpc 3.0.4,test passed.
- wince(arm eabi hard flaot),windows 10 IOT,only fpc 3.3.1,test passed.

## CPU架构支持，test with Delphi 10.3 update 2 and FPC 3.0.4

- MIPS(fpc-little endian), soft float, test pass on QEMU 
- intel X86(fpc-x86), soft float
- intel X86(delphi+fpc), hard float,80386,PENTIUM,PENTIUM2,PENTIUM3,PENTIUM4,PENTIUMM,COREI,COREAVX,COREAVX2
- intel X64(fpc-x86_64), soft float
- intel X64(delphi+fpc), hard float,ATHLON64,COREI,COREAVX,COREAVX2
- ARM(fpc-arm32-eabi,soft float):ARMV3,ARMV4,ARMV4T,ARMV5,ARMV5T,ARMV5TE,ARMV5TEJ
- ARM(fpc-arm32-eabi,hard float):ARMV6,ARMV6K,ARMV6T2,ARMV6Z,ARMV6M,ARMV7,ARMV7A,ARMV7R,ARMV7M,ARMV7EM
- ARM(fpc-arm64-eabi,hard float):ARMV8，aarch64


## 文档

**必读:**

[编译指南](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/zServer4D%E7%BC%96%E8%AF%91%E6%8C%87%E5%8D%97.pdf)

[IOT完全攻略](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%85%A5%E6%89%8BIOT%E7%9A%84%E5%AE%8C%E5%85%A8%E6%94%BB%E7%95%A5.pdf)

**内网穿透:**

[关于XNAT内网穿透库](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/XNat%E5%86%85%E7%BD%91%E7%A9%BF%E9%80%8F%E6%A0%B8%E5%BF%83%E5%BA%93.pdf)

[宅服架设(FRP外壳支持)](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9F%BA%E4%BA%8E%E5%86%85%E7%BD%91%E7%A9%BF%E9%80%8F%E5%B7%A5%E5%85%B7ZSGateway%E6%90%AD%E5%BB%BA%E5%AE%85%E6%9C%8D.pdf)

**内核:**

[zDefine过程定义详解](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/zDefine%E8%BF%87%E7%A8%8B%E5%AE%9A%E4%B9%89%E8%AF%A6%E8%A7%A3.pdf)

[BigStream机制详解](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/BigStream%E6%9C%BA%E5%88%B6%E8%AF%A6%E8%A7%A3.pdf)

[多媒体通讯CompleteBuffer](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9C%A8ZS%E4%B8%AD%E4%BD%BF%E7%94%A8%E5%A4%9A%E5%AA%92%E4%BD%93%E9%80%9A%E8%AE%AF%E6%9C%BA%E5%88%B6CompleteBuffer.pdf) 

[BatchStream机制详解](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/BatchStream%E6%9C%BA%E5%88%B6%E8%AF%A6%E8%A7%A3.pdf)

[HPC服务器的工作机制详解](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/HPC%E6%9C%8D%E5%8A%A1%E5%99%A8%E7%9A%84%E5%B7%A5%E4%BD%9C%E6%9C%BA%E5%88%B6%E8%AF%A6%E8%A7%A3.pdf)

[延迟反馈机制详解](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%BB%B6%E8%BF%9F%E5%8F%8D%E9%A6%88%E6%9C%BA%E5%88%B6%E8%AF%A6%E8%A7%A3.pdf)

[序列化的命令队列机制详解](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%BA%8F%E5%88%97%E5%8C%96%E7%9A%84%E5%91%BD%E4%BB%A4%E9%98%9F%E5%88%97%E6%9C%BA%E5%88%B6%E8%AF%A6%E8%A7%A3.pdf)

**组合技术:**

[基于序列包的断线重连系统StableIO](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9F%BA%E4%BA%8E%E5%BA%8F%E5%88%97%E5%8C%85%E7%9A%84%E6%96%AD%E7%BA%BF%E9%87%8D%E8%BF%9E%E7%B3%BB%E7%BB%9FStableIO.pdf)

[Zserver中的序列包机制详解](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/Zserver%E4%B8%AD%E7%9A%84%E5%BA%8F%E5%88%97%E5%8C%85%E6%9C%BA%E5%88%B6%E8%AF%A6%E8%A7%A3.pdf)

**组合技术:**

[双通道机制详解](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%8F%8C%E9%80%9A%E9%81%93%E6%9C%BA%E5%88%B6%E8%AF%A6%E8%A7%A3.pdf)

[p2pVM隧道技术](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/ZS%E4%B8%AD%E7%9A%84p2pVM%E9%9A%A7%E9%81%93%E6%8A%80%E6%9C%AF.pdf)

[p2pVM第二篇机理说明](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9F%BA%E4%BA%8EZS%E7%9A%84%20p2pVM%E7%AC%AC%E4%BA%8C%E7%AF%87%E6%9C%BA%E7%90%86%E8%AF%B4%E6%98%8E.pdf) 

**必读:** 

[部署Ubuntu服务器的开发环境(delphi方向)](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E9%83%A8%E7%BD%B2Ubuntu%E6%9C%8D%E5%8A%A1%E5%99%A8%E7%9A%84Delphi%E5%BC%80%E5%8F%91%E7%8E%AF%E5%A2%83.pdf)

[Linux桌面开发指南(fpc方向)](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/Linux%E6%A1%8C%E9%9D%A2%E5%BC%80%E5%8F%91%E6%8C%87%E5%8D%97.pdf)

[在各开源项目中，为什么DisposeObject会比Free使用频率更高](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9C%A8%E5%90%84%E5%BC%80%E6%BA%90%E9%A1%B9%E7%9B%AE%E4%B8%AD%EF%BC%8C%E4%B8%BA%E4%BB%80%E4%B9%88DisposeObject%E4%BC%9A%E6%AF%94Free%E4%BD%BF%E7%94%A8%E9%A2%91%E7%8E%87%E6%9B%B4%E9%AB%98.pdf)

**问答:** 

[解疑：为什么通过网络传任何文件都要验证](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E8%A7%A3%E7%96%91%EF%BC%9A%E4%B8%BA%E4%BB%80%E4%B9%88%E9%80%9A%E8%BF%87%E7%BD%91%E7%BB%9C%E4%BC%A0%E4%BB%BB%E4%BD%95%E6%96%87%E4%BB%B6%E9%83%BD%E8%A6%81%E9%AA%8C%E8%AF%81.pdf)

[解疑：为什么ZServer4D不能在线程中使用](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E8%A7%A3%E7%96%91%EF%BC%9A%E4%B8%BA%E4%BB%80%E4%B9%88ZServer4D%E4%B8%8D%E8%83%BD%E5%9C%A8%E7%BA%BF%E7%A8%8B%E4%B8%AD%E4%BD%BF%E7%94%A8.pdf)

**ZDB:** 

[使用ZDB：1.认识ZDB](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E4%BD%BF%E7%94%A8ZDB%EF%BC%9A1.%E8%AE%A4%E8%AF%86ZDB.pdf)

[使用ZDB：2.查询工作原理](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E4%BD%BF%E7%94%A8ZDB%EF%BC%9A2.%E6%9F%A5%E8%AF%A2%E5%B7%A5%E4%BD%9C%E5%8E%9F%E7%90%86.pdf)

[使用ZDB：3.数据策略](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E4%BD%BF%E7%94%A8ZDB%EF%BC%9A3.%E6%95%B0%E6%8D%AE%E7%AD%96%E7%95%A5.pdf)

**其它**

[双通道多线下载技术](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%8F%8C%E9%80%9A%E9%81%93%E5%A4%9A%E7%BA%BF%E4%B8%8B%E8%BD%BD%E6%8A%80%E6%9C%AF.pdf)

[云调度服务器用法详解](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E4%BA%91%E8%B0%83%E5%BA%A6%E6%9C%8D%E5%8A%A1%E5%99%A8%E7%94%A8%E6%B3%95%E8%AF%A6%E8%A7%A3.pdf)

[百度翻译服务后台(支持Ubuntu16.04LTS服务器)](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E7%99%BE%E5%BA%A6%E7%BF%BB%E8%AF%91%E6%9C%8D%E5%8A%A1%E7%9A%84%E5%90%8E%E5%8F%B0%E5%AE%9E%E7%8E%B0%E8%8C%83%E5%BC%8F.pdf)

[百度翻译服务API(支持Ubuntu16.04LTS服务器)](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E7%99%BE%E5%BA%A6%E7%BF%BB%E8%AF%91%E6%9C%8D%E5%8A%A1%E7%9A%84API%E8%B0%83%E7%94%A8%E8%8C%83%E4%BE%8B.pdf) 
 
[云服务器框架](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%95%86%E4%B8%9A%E4%BA%91%E6%9C%8D%E5%8A%A1%E5%99%A8%E6%A1%86%E6%9E%B62.0%20%E6%A1%86%E6%9E%B6%E8%AF%B4%E6%98%8E.pdf) [怎样开发基于ZS的底层通讯IO接口](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9F%BA%E4%BA%8EZServer4D%E5%BC%80%E5%8F%91%E5%BA%95%E5%B1%82%E9%80%9A%E8%AE%AFIO%E6%8E%A5%E5%8F%A3%E7%9A%84%E6%96%B9%E6%B3%95.pdf) [console模式的后台程序开发](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/console%E6%A8%A1%E5%BC%8F%E5%90%8E%E5%8F%B0%E5%BC%80%E5%8F%91.pdf)

[CodeTyphon多架构及多平台开发陷阱](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9C%A8CodeTyphon%E7%BC%96%E8%AF%91%E5%92%8C%E4%BD%BF%E7%94%A8ZServer4D.pdf)

[在Lazarus或则CodeTyphon编译时出现缺失mtprocs库的解决办法](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/%E5%9C%A8Lazarus%E6%88%96%E5%88%99CodeTyphon%E7%BC%96%E8%AF%91%E6%97%B6%E5%87%BA%E7%8E%B0%E7%BC%BA%E5%A4%B1mtprocs%E5%BA%93%E7%9A%84%E8%A7%A3%E5%86%B3%E5%8A%9E%E6%B3%95.pdf)

[日常问题](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/ZServer4D%E6%97%A5%E5%B8%B8%E9%97%AE%E9%A2%98%E6%B1%87%E6%80%BB.pdf)

[库说明](https://github.com/PassByYou888/ZServer4D/blob/master/Documents/ZServer4D%E5%8D%95%E5%85%83%E5%BA%93%E8%AF%B4%E6%98%8E.pdf)


## 通讯接口支持(开发平台需求 Delphi Rad studio 10.2或则更高版本，低版本不支持)

1.indy 阻塞模式的通讯组件，已在ZServer4D内部集成(客户端兼容性好，服务器质量差强人意)

(open source) http://www.indyproject.org/


2.CrossSocket 异步式通讯组件，已在ZServer4D内部集成(服务器，客户端两边的质量都极好)

(open source) https://github.com/winddriver/Delphi-Cross-Socket


3.ICS异步式通讯组件，已在ZServer4D内部集成(质量很好)

(open source) http://www.overbyte.be


4.DIOCP 国人所开发的稳定DIOCP通讯库(服务器端的质量极好)
 
(Open source) https://github.com/ymofen/diocp-v5


## 通讯接口支持(FreePascal 3.0.4 or last with Lazarus，低版本不支持)

1.synapse4(open source) 已经在ZServer4D内部集成，主要支持fpc，同时也兼容delphi(客户端的兼容性好，服务器端质量很好)

synapse是支持ssl的优秀开源项目

在ZServer4D中使用Synapse的最大连接数被限制为100.


## 关于5G后台：万兆以太

万兆以太构建多使用**CompleteBuffer**机制即可,该机制可以适应未来5G的后台场景,视频+图片+大文件，均可平滑过度至5G，无需改动


## 关于物联网IoT平台

ZServer4D对IoT平台的开发要求必须使用FPC编译器，ZServer4D对物联网的支持的标准系统包含一切Linux系统，要求最低FPC编译器版本为3.0.4（需要和它对应的RT内核库）

关于IoT平台的开发测试机：本文提及到的IOT开发板都可以通过网购获取，自己动手diy Linux需要一定的耐心，懒人建议使用CodeTyphon，或则直接apt安装内置的fpc+Lazarus


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

必须先通过注册表开放windows的socket限制，然后再用PerformanceServer+PerformanceClient进行测试

压力测试如果链接超过6万，Windows系统会自动关闭侦听端口，具体原因不详，压测请尽量保持在6万以内，超过6万服务器侦听端口会自动关闭，只需要将服务器重开一次即可


## 关于切入和使用

ZServer4D有点偏向多平台通讯架构类型，目前主流架构技术是p2pVM上面做应用模型，因为长时间维护和升级导致之前遗留许多演示和框架，例如云服务器。在应用到项目之前，可与qq600585来个消息印证下使用思路。**因为对某些网络模型理解不到位可能导致项目架错，不是开玩笑的**。

ZServer4D是系统化的生产工艺地基，它并不像VCL那样傻瓜，可以拿来就用，你需要自行动手进行需求的提炼，简单来说，你必须自己动手封装，然后再使用。ZServer4D有丰富Demo和文档提供技术参考。

**注意：2020/12月期间项目繁多，我在项目开发中使用10.3.3在ARM平台ARC会自动释放我的变量，调试一天无解。如果使用ZS开发手机，请换10.4或则以后的版本。开发Win32/Linux可以继续使用现有工具**

**在linux使用fpc时需要外挂一个MM库，jemalloc/tcmalloc均可，自行编译接口**

## 技术方向建议

- **所有的通讯模型往前看都建议采用p2pVM**
- 普通且单一CS结构:往p2pVM靠
- 复杂网络通讯模型:文件,数据库,视频,实时聊天,凡是集合了这类复杂通讯功能,使用p2pVM
- 接口三方语言:例如接口c/c++/java/oc,都建议使用单通道但是双工的异步通讯模型
- IOT类开发板和自定义板:往p2pVM方向靠,记得要上jem/tcm这类MM库
- 大规模的后台通讯模型:搞这类程序,多关心编程上堆积代码的工作范式,重要地方是统一标准,具体技术可其次
- ZServer4D有很多计算机科学类领域的内容,对每个人用户我都建议多关注基本编程:线程和数据结构,避免自己水自己
- 面对不了解,正确做法是多提问,**不要闭门造车**,ZServer4D是高级框架,重点都在设计和应用思路,从直观技术面理解容易踩雷

by.qq600585

## SAAS体系与Delphi11支持

- SAAS支持体系详见 https://github.com/PassByYou888/zCloud

## 最后更新日志

- **新增支持ZJson库**：fps/delphi一致化Json操作方式支持，Json在别的开发平台都是对象化使用方式，对象化也是ZJson库的工作方式，统一fpc/delphi便于程序在多平台移植。
- **更新：BigStream如果为TMemoryStream/TMemoryStream64，可以自动化支持磁盘空间交换（使用临时文件替代内存块）**
- BigStream的文件转存机制：如果BigStream在队列中，同时它达到了尺寸要求，并且它的类型为TMemoryStream/TMemoryStream64，启动转存机制
- 新增一个标准三层范式，以名片的增删查改为中心功能：DBService->InternetService->Client ，该Demo也包括了服务器间通讯的事件桥机制Demo，我在代码中已备注中文
- 新增2个OrderStruct相关Demo，同时也演示了多线程的程序范式.

## 前一版本更新

- 规范了双通道释放机制：必须先释放个别的RecvTunnel/SendTunnel，然后再去释放双通道框架
- 更新：修复CoreClasses库中OrderStruct中destroy没有override导致内存泄露的问题
- 更新：修复RunHPC_Stream这类方法中destroy没有override导致内存泄露的问题
- 更新：修正部分ZDB1.0体系中的笼统命名
- 更新：修正部分ZDB1.0中cache的普适性
- 更新：datastore service支持体系中的加解密机制
- 更新：datastore service支持体系可以自动化解码数据库stream
- 重大机制更新：新增支持p2pVM支持连接克隆机制，以后有空我会补充该机制demo
- 重大机制更新：新增支持服务器间自动化数据转发桥接器机制，以后有空我会补充该机制demo



[更多更新日志](https://github.com/PassByYou888/ZServer4D/update.md)


## 注意

REST,BAAS等等单项式的HTTP服务请自行在服务器开发和集成，ZServer4D不提供外部http支持

如果你在使用ZServer4D，并且对开发有疑问，请加群去寻找答案（请不要直接联系作者）

qq群490269542

**支持ZServer4D的后续开发** [支付宝转账](https://github.com/PassByYou888/ZServer4D/raw/master/alipay.jpg)

不转帐也没事,遇上问题及时反馈一下.你们都是我的用户：〉

