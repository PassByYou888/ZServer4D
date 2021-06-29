# jemalloc for pascal
jemalloc for delphi/fpc compatible(win/linux/ros)

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


## 制作jemalloc4p的导火索

- **fpc-jemalloc** 这是已知的一个fpc开源库,它对delphi支持不友好,并且不提供windows动态库,**开源jemalloc库但不支持delphi,可以当成针对delphi的做法,不是开源**
- **fastmm5** 原生MM库,个人一直很喜欢,但是对于fpc和多平台支持不好
- 我的开源项目 **ZServer4D/ZAI/zAnaylsis** 这些库如果放到FPC,MM是个大问题
- **jemalloc** 目前是FPC程序可挂载MM库的最佳选择


## 构建检查

- 建议编译前把 https://github.com/jemalloc/jemalloc 拉下来对比jemalloc.h的函数原型
- 原型如果发生变化,可能jemalloc做了革命性更新,这时可以提个issus,或则自己根据原型重构一下jemalloc4p.pas

### windows 预编译下列了动态库

- 32位系统使用 **jemalloc_IA32.dll**
- 64位系统使用 **jemalloc_X64.dll** 
- windows已在引入代码自动区分系统,把jemalloc4p.pas包含进工程后自动生效
- **提示：windows预编译库需要vcruntime140依赖库(vs2017)**


### Linux 编译后才能用

- linux编译会根据系统自动生成,树莓派4p大概2分钟左右完成
- 编译后把**libjemalloc.so**考到工程目录
	- 用nm命令查看一下导出符号,**nm ./libjemalloc.so**,不同环境会影响构建出来的导出符号
	- 在工程引入jemalloc4p.pas后编译,如果发现undefined refence xxx这类东西,需要修改一下导出符号
+ **ZServer4D**如果用fpc在linux跑,上了jemalloc4p,不会再出现收发速度慢的问题
- 使用**ZServer4D**开发IOT板子,jemalloc4p是必备库,不多解释

```batch
git clone https://github.com/jemalloc/jemalloc
cd jemalloc
./autoconf
./configure
make -j4
sudo make install PREFIX=/usr/lib
```

### OSX and IOS 编译后才能用

- 如果使用IOS静态链接库,需要自己编译,弄完以后把libjemalloc.a考到delphi工程来链接
- 如果使用OSX动态库,把libjemalloc.dylib考到delphi工程中一起打包

```batch
$ git clone https://github.com/jemalloc/jemalloc
$ cd jemalloc
$ ./autoconf
$ ./configure
$ make -j2
$ make install
```


by.qq600585

2021-6
