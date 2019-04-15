program QuantumEncryptDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  DoStatusIO,
  MemoryStream64,
  DataFrameEngine,
  CoreCipher;

procedure QuantumSecurity_DataFrameEngine;
var
  d: TDataFrameEngine;
  m64: TMemoryStream64;
begin
  // 该函数示范了抗量子密码怎样在TDataFrameEngine中使用
  // ZS的DataFrameEninge在收发前都可以使用该方法进行加密
  // 注意：抗量子密码性能无法达到实时，我们可以选择加密一些重要的数据

  d := TDataFrameEngine.Create;
  d.WriteString('hello world');

  DoStatus('DataFrameEngine 加密前的数据:%s', [umlMD5ToStr(d.GetMD5(True)).Text]);

  // 以密码"password123456"生成 Rijndael 加密算法的key
  // 然后以Rijndael加密算法对缓冲区做1024次重叠加密
  // 密码将会以2次sha3-512位进行编码
  // 为什么说这么干是抗量子破解的？
  // 因为sha3就是抗力量子设计，而安全的密码则保证了多次Rijndael加密是安全的
  // 在双重安全机制下，Encrypt方法可以抵御未来的量子攻击
  m64 := TMemoryStream64.Create;
  d.Encrypt(m64, True, 1024, TPascalString('password123456').Bytes);

  d.Clear;
  m64.Position := 0;
  // 在不知道密码的情况下，m64中的数据永远不可能被解密出来
  if d.Decrypt(m64, TPascalString('password123456').Bytes) then
      DoStatus('成功解密')
  else
      DoStatus('密码错误');
  DoStatus('DataFrameEngine 解密后的数据:%s', [umlMD5ToStr(d.GetMD5(True)).Text]);

  DisposeObject([d, m64]);
end;

procedure QuantumSecurity_Buffer;
var
  sour: TMemoryStream64;
  m64: TMemoryStream64;
begin
  // 该函数示范了怎样对stream数据进行抗量子加解密
  // stream可以用于zs的completeBuffer,bigStream,BatchStream等等机制用
  // 注意：抗量子密码性能无法达到实时，我们可以选择加密一些重要的数据
  sour := TMemoryStream64.Create;
  sour.WriteString('hello world');
  DoStatus('buffer 加密前的数据:%s', [umlStreamMD5String(sour).Text]);

  m64 := TMemoryStream64.Create;

  sour.Position := 0;
  // 以密码"password123456"生成 Rijndael 加密算法的key
  // 然后以Rijndael加密算法对缓冲区做1024次重叠加密
  // 密码将会以2次sha3-512位进行编码
  // 为什么说这么干是抗量子破解的？
  // 因为sha3就是抗力量子设计，而安全的密码则保证了多次Rijndael加密是安全的
  // 在双重安全机制下，Encrypt方法可以抵御未来的量子攻击
  CoreCipher.QuantumEncrypt(sour, m64, 1024, TPascalString('password123456').Bytes);

  sour.Clear;
  m64.Position := 0;
  // 在不知道密码的情况下，m64中的数据永远不可能被解密出来
  if CoreCipher.QuantumDecrypt(m64, sour, TPascalString('password123456').Bytes) then
      DoStatus('成功解密')
  else
      DoStatus('密码错误');
  DoStatus('buffer 解密后的数据:%s', [umlStreamMD5String(sour).Text]);

  DisposeObject([sour, m64]);
end;

begin
  QuantumSecurity_DataFrameEngine;
  QuantumSecurity_Buffer;
  readln;

end.
