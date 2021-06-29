# tcmalloc4p
tcmalloc for pascal (tcmalloc - google performance tools)

## 系统支持

- ARM Linux
- MIPS Linux
- IA32 Linux
- X64 Linux
- win32
- win64
- server2012
- server2016
- server2019
- ROS

## 编译器和IDE平台支持

- delphiXE10以上的版本
- fpc
- codetyphon


## 制作tcmalloc4p的导火索

- 顺手做的MM,过程和jemalloc差不多的东西,依赖项有点变化


### windows 预编译下列了动态库

- 32位系统使用 **libtcmalloc_minimal_ia32.dll**
- 64位系统使用 **libtcmalloc_minimal_x64.dll** 
- **提示：windows预编译库需要vcruntime140依赖库(vs2017)**

### Linux 编译后才能用

- linux在没有对号版本的情况下,不要直接复制.so动态库,linux是开源操作系统,没有预编译项目就自己编译吧
- gperftools内置了面向开发者调试的程序,所以会引用一些三方库,不可以直接复制so,不要用jemalloc的方式装tcm
- 在工程引入tcmalloc4p.pas后,按下列方式编译gperftools即可使用
- iot系统已在树莓派4p测试通过

```batch
git clone https://github.com/gperftools/gperftools
cd gperftools
./autoconf
./configure
make -j4
sudo make install PREFIX=/usr/lib
```


### OSX and IOS 编译后才能用

- 如果使用IOS静态链接库,需要自己编译
- 如果使用OSX动态库,需要安装
- **我今天做项目时,macos坏掉了,所以osx我没有测试,你自己测试一下吧,有问题给我来消息**

```batch
$ git clone https://github.com/gperftools/gperftools
$ cd gperftools
$ ./autoconf
$ ./configure
$ make -j2
$ make install
```


by.qq600585

2021-6
