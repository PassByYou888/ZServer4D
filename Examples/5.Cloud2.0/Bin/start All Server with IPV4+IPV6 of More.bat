start /D Win32\ManagerServer Win32\ManagerServer\ManagerServer.exe -ipv4+ipv6 -delayService:0

ping 1.1.1.1 -n 1 -w 1000

start /D Win32\DataStore Win32\DataStore\DataStoreServer.exe -ipv4+ipv6 -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:1

ping 1.1.1.1 -n 1 -w 1000

start /D Win32\FileStore Win32\FileStore\FileStoreServer.exe -ipv4+ipv6 -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:2


ping 1.1.1.1 -n 1 -w 1000

start /D Win32\Logic Win32\Logic\LogicServer.exe -r:7311 -s:7312 -PayR:7411 -PayS:7412 -ipv4+ipv6 -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:3
rem start /D Win32\Logic Win32\Logic\LogicServer.exe -r:7321 -s:7322 -PayR:7421 -PayS:7422 -ipv4+ipv6 -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:5
rem start /D Win32\Logic Win32\Logic\LogicServer.exe -r:7331 -s:7332 -PayR:7431 -PayS:7432 -ipv4+ipv6 -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:7
rem start /D Win32\Logic Win32\Logic\LogicServer.exe -r:7341 -s:7342 -PayR:7441 -PayS:7442 -ipv4+ipv6 -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:9

ping 1.1.1.1 -n 1 -w 1000

start /D Win32\IOSPaymentQuery Win32\IOSPaymentQuery\IOSPaymentQueryServer.exe -ipv4+ipv6 -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:4

ping 1.1.1.1 -n 1 -w 1000

start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5111 -s:5112 -ipv4+ipv6 -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:4
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5113 -s:5114 -ipv4+ipv6 -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:5
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5115 -s:5116 -ipv4+ipv6 -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:6
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5117 -s:5118 -ipv4+ipv6 -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:7
