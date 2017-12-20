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

(NoSQL并行化内核已经完成，聚类分析和分布式负载已完成50%文档)

支持中心化网络群集，支持中心化网络群集一键对接



## 通讯接口支持(只限Delphi)

1.indy(open source) http://www.indyproject.org/
id是阻塞模式的通讯组件，已在ZServer4D内部集成


2.CrossSocket(open source) https://github.com/winddriver/Delphi-Cross-Socket
异步式通讯组件，已在ZServer4D内部集成


3.ICS(open source) www.overbyte.be
异步式通讯组件，已在ZServer4D内部集成
 

## 注意

REST,BAAS等等单项式的HTTP服务请自行在服务器开发和集成，ZServer4D不提供外部http支持

如果你在使用ZServer4D，并且对开发有疑问，请加群去寻找答案（请不要直接联系作者，因为作者一般不在线）


qq群490269542
 
 
作者qq600585（不接受加人）



## 关于内存泄漏

ZServer4D内置的服务器有3种：Indy，ICS，CrossSocket，所有的服务器均无内存泄漏

ZServer4D内置的客户端采用的是抛弃式链接，每次链接登录服务器都会抛弃现有链接类，并且重建新接口链接，这里不会释放老的链接（这样干是保证客户端的正常工作）

在ZServer4D中所捆绑的类，包括编解码，链表，数据库，均无内存泄漏



## 关于切入和使用

使用ZServer4D前，请仔细阅读本项目所提供的Demo，做到对基于ZServer4D的开发范式非常了解

多读代码!



## 更新日志


2017-12-20


修复链接池重复链接的Bug（感谢好人一生平安路）

新增LZ77哈夫曼和BRRC压缩算法，请自行对比ZLib性能及压缩率

因为是公有服务器框架，处于安全考虑，通讯包协议在首尾各增加4byte作为验证标记，发行时可以有80亿的独特协议包数据（你只需要从80亿的数值选择一个独特的验证序，别人就算有ZS源码就也无法访问你的服务器）


2017-12-15

修复了底层hash函数库因为没有传递对象，导致云服务器的状态列表不更新的问题



2017-12-15

新增6万压测Demo，附截图



ZDB新增一组提交数据条目和大图片捆绑的Demo，每张图片各2M，此机制可用于任何Stream数据，请参考并且依次类推

ZDB新增安全缓存，其工作机制为：当触发数据库写操作时，开始计时，如果5秒内没有第二次写操作，就会将回写缓存立即写入文件。主要用于保护数据库文件损坏。

ZDB和ZServer均已在Linux下测试通过



2017-12-10

新增ZDB服务器Demo，新增ZDB IN FMX Demo，已在所有手机测试通过，所有的详细说明都内附在Demo源码中

将ZDB数据库的查询能力向前优化了15倍，内存消耗也因此提高了10%，默认单库体量为500万条，如果服务器内存在16G以上，需要自行在ZDBEngine中修改最大Cache和退火参数

修复了单通道客户端Indy,ICS,CrossSocket在发出链接后的ID不更新问题(感谢AK47)

在EZServer增加了使用Special实例的方法(感谢AK47)


2017-12-8

新增大数据库引擎ZDB的网络服务器Demo，并且内附了详细功能说明 [Demo说明](https://github.com/PassByYou888/ZServer4D/tree/master/Examples/10.ZDB%E6%95%B0%E6%8D%AE%E6%9C%8D%E5%8A%A1%E5%99%A8)

在ZDB中新增压缩，拷贝，替换等等底层功能

将ZDB的退火系统改成了自动化功能，只需要设置几个参数即可

新增带有验证机制的ZDB服务模型



2017-12-6

新增单机数据引擎,新增分布式和网络数据引擎

新增单机数据库Demo

新增20k链接压力测试，服务器光速响应，并且无内存泄漏 [20k长连接测试](https://github.com/PassByYou888/ZServer4D/tree/master/Examples/9.%E9%95%BF%E8%BF%9E%E6%8E%A5%E5%8E%8B%E6%B5%8B)

通讯内核新增批次化BigStream支持

通讯内核新增special接口

通讯内核新增了很多状态机

修复了CrossSocket在退出时报异常的问题

优化内核Hash性能

优化了文件读写内核，内部集成了回写和预读缓存机制，小幅降低IO消耗频率

群集中心服务器可以一键开发和部署



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
