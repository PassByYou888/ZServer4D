{ ****************************************************************************** }
{ * x Nat tunnel Physics interface, written by QQ 600585@qq.com                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit xNATPhysics;

{$INCLUDE zDefine.inc}

interface

uses
{$IFDEF FPC}
  CommunicationFramework_Server_Synapse, CommunicationFramework_Client_Synapse,
{$ELSE FPC}
{$IFDEF NatTunnel_On_ICS}
  CommunicationFramework_Server_ICS, CommunicationFramework_Client_ICS,
{$ENDIF NatTunnel_On_ICS}
{$IFDEF NatTunnel_On_CrossSocket}
  CommunicationFramework_Server_CrossSocket, CommunicationFramework_Client_CrossSocket,
{$ENDIF NatTunnel_On_CrossSocket}
{$IFDEF NatTunnel_On_DIOCP}
  CommunicationFramework_Server_DIOCP, CommunicationFramework_Client_DIOCP,
{$ENDIF NatTunnel_On_DIOCP}
{$IFDEF NatTunnel_On_Indy}
  CommunicationFramework_Server_Indy, CommunicationFramework_Client_Indy,
{$ENDIF NatTunnel_On_Indy}

{$ENDIF FPC}
  CoreClasses;

type
{$IFDEF FPC}
  TXPhysicsServer = TCommunicationFramework_Server_Synapse;
  TXPhysicsClient = TCommunicationFramework_Client_Synapse;
{$ELSE FPC}
{$IFDEF NatTunnel_On_ICS}
  TXPhysicsServer = TCommunicationFramework_Server_ICS;
  TXPhysicsClient = TCommunicationFramework_Client_ICS;
{$ENDIF NatTunnel_On_ICS}
{$IFDEF NatTunnel_On_CrossSocket}
  TXPhysicsServer = TCommunicationFramework_Server_CrossSocket;
  TXPhysicsClient = TCommunicationFramework_Client_CrossSocket;
{$ENDIF NatTunnel_On_CrossSocket}
{$IFDEF NatTunnel_On_DIOCP}
  TXPhysicsServer = TCommunicationFramework_Server_DIOCP;
  TXPhysicsClient = TCommunicationFramework_Client_DIOCP;
{$ENDIF NatTunnel_On_DIOCP}
{$IFDEF NatTunnel_On_Indy}
  TXPhysicsServer = TCommunicationFramework_Server_Indy;
  TXPhysicsClient = TCommunicationFramework_Client_Indy;
{$ENDIF NatTunnel_On_Indy}
{$ENDIF FPC}

implementation

end.
