{ ****************************************************************************** }
{ * PhysicsIO interface, written by QQ 600585@qq.com                           * }
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
unit PhysicsIO;

{$INCLUDE zDefine.inc}

interface

uses
{$IFDEF FPC}
  CommunicationFramework_Server_Synapse, CommunicationFramework_Client_Synapse,
{$ELSE FPC}
{$IFDEF PhysicsIO_On_ICS}
  CommunicationFramework_Server_ICS, CommunicationFramework_Client_ICS,
{$ENDIF PhysicsIO_On_ICS}
{$IFDEF PhysicsIO_On_CrossSocket}
  CommunicationFramework_Server_CrossSocket, CommunicationFramework_Client_CrossSocket,
{$ENDIF PhysicsIO_On_CrossSocket}
{$IFDEF PhysicsIO_On_DIOCP}
  CommunicationFramework_Server_DIOCP, CommunicationFramework_Client_DIOCP,
{$ENDIF PhysicsIO_On_DIOCP}
{$IFDEF PhysicsIO_On_Indy}
  CommunicationFramework_Server_Indy, CommunicationFramework_Client_Indy,
{$ENDIF PhysicsIO_On_Indy}
{$IFDEF PhysicsIO_On_Synapse}
  CommunicationFramework_Server_Synapse, CommunicationFramework_Client_Synapse,
{$ENDIF PhysicsIO_On_Synapse}

{$ENDIF FPC}
  CoreClasses;

type
{$IFDEF FPC}
  TPhysicsServer = TCommunicationFramework_Server_Synapse;
  TPhysicsClient = TCommunicationFramework_Client_Synapse;
{$ELSE FPC}
{$IFDEF PhysicsIO_On_ICS}
  TPhysicsServer = TCommunicationFramework_Server_ICS;
  TPhysicsClient = TCommunicationFramework_Client_ICS;
{$ENDIF PhysicsIO_On_ICS}
{$IFDEF PhysicsIO_On_CrossSocket}
  TPhysicsServer = TCommunicationFramework_Server_CrossSocket;
  TPhysicsClient = TCommunicationFramework_Client_CrossSocket;
{$ENDIF PhysicsIO_On_CrossSocket}
{$IFDEF PhysicsIO_On_DIOCP}
  TPhysicsServer = TCommunicationFramework_Server_DIOCP;
  TPhysicsClient = TCommunicationFramework_Client_DIOCP;
{$ENDIF PhysicsIO_On_DIOCP}
{$IFDEF PhysicsIO_On_Indy}
  TPhysicsServer = TCommunicationFramework_Server_Indy;
  TPhysicsClient = TCommunicationFramework_Client_Indy;
{$ENDIF PhysicsIO_On_Indy}
{$IFDEF PhysicsIO_On_Synapse}
  TPhysicsServer = TCommunicationFramework_Server_Synapse;
  TPhysicsClient = TCommunicationFramework_Client_Synapse;
{$ENDIF PhysicsIO_On_Synapse}
{$ENDIF FPC}

implementation

end.
