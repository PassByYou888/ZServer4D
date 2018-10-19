start /D Win32\ManagerServer Win32\ManagerServer\ManagerServer.exe -ipv4 -delayService:1

ping 1.1.1.1 -n 1 -w 1000

start /D Win32\DataStore Win32\DataStore\DataStoreServer.exe -ipv4 -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:1

ping 1.1.1.1 -n 1 -w 1000

start /D Win32\FileStore Win32\FileStore\FileStoreServer.exe -ipv4 -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:2


ping 1.1.1.1 -n 1 -w 1000

start /D Win32\Logic Win32\Logic\LogicServer.exe -ipv4 -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:3

ping 1.1.1.1 -n 1 -w 1000

start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5111 -s:5112 -ipv4 -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:4

ping 1.1.1.1 -n 1 -w 1000

start /D Win32\IOSPaymentQuery Win32\IOSPaymentQuery\IOSPaymentQueryServer.exe -ipv4 -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:5

