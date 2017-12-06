内存越界单元DEMO

1：在工程中增加单元：QMMErrorUtils.pas （注意，如果在工程顺序较后，则无法检查其它单元中，启动中[initialization代码]的内存错误）

2：等待错误文件的产生
   产生的文件名为： Application-path\mem-error\Project1.threadid=8804.txt
       即每线程产生个文件。

   产生的内存越界错误数据如下：

[2014-07-20 20:48:57]memory error:
"Memory.Free", address: 004F105C, mem size: 4, instance data type: unknow class
stack trace: $4097A8 -> $4A7DF8 -> $47C2A5 -> $4596A1 -> $47C3F8 -> $47C2A5 -> $49C7DD -> $47B943
hex data:
B0 04 02 00 
[2014-07-20 20:48:57]memory error:
"Memory.Free", address: 004F0A0C, mem size: 24, instance data type: TErrorObject
stack trace: $4043C6 -> $47C2A5 -> $4596A1 -> $47C3F8 -> $47C2A5 -> $49C7DD -> $47B943 -> $4403DA
hex data:
A4 7D 4A 00 00 00 00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 

   只做了简单检测类名的处理（字符串检测未处理）请自行根据stack trace地址，进行定位到源码检查

-------
end.
20140720 by qiusonglin