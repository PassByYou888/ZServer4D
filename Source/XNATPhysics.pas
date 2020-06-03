{ ****************************************************************************** }
{ * XNAT tunnel Physics interface, written by QQ 600585@qq.com                 * }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
unit XNATPhysics;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PhysicsIO;

type
  TXPhysicsServer = TPhysicsServer;
  TXPhysicsClient = TPhysicsClient;
  TXNAT_PHYSICS_MODEL = (XNAT_PHYSICS_SERVICE, XNAT_PHYSICS_CLIENT);

procedure BuildBuff(buff: PByte; siz: NativeInt; local_id, remote_id: Cardinal; var NewSiz: NativeInt; var NewBuff: PByte);
procedure FillBuff(sour: PByte; siz: NativeInt; var local_id, remote_id: Cardinal; var destSiz: NativeInt; var destBuff: PByte);

const
  C_RequestListen = '__@RequestListen';
  C_Connect_request = '__@connect_request';
  C_Disconnect_request = '__@disconnect_request';
  C_Data = '__@data';
  C_Connect_reponse = '__@connect_reponse';
  C_Disconnect_reponse = '__@disconnect_reponse';
  C_Workload = '__@workload';
  C_IPV6Listen = '__@IPv6Listen';

implementation

procedure BuildBuff(buff: PByte; siz: NativeInt; local_id, remote_id: Cardinal; var NewSiz: NativeInt; var NewBuff: PByte);
var
  nb: PByte;
begin
  NewSiz := siz + 8;
  nb := System.GetMemory(NewSiz);
  NewBuff := nb;
  PCardinal(nb)^ := local_id;
  inc(nb, 4);
  PCardinal(nb)^ := remote_id;
  inc(nb, 4);
  CopyPtr(buff, nb, siz);
end;

procedure FillBuff(sour: PByte; siz: NativeInt; var local_id, remote_id: Cardinal; var destSiz: NativeInt; var destBuff: PByte);
begin
  destSiz := siz - 8;
  local_id := PCardinal(sour)^;
  inc(sour, 4);
  remote_id := PCardinal(sour)^;
  inc(sour, 4);
  destBuff := sour;
end;

end.
 
