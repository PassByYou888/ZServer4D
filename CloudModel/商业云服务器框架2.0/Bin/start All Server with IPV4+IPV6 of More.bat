start /D Win32\ManagerServer Win32\ManagerServer\ManagerServer.exe -ipv4+ipv6 -NoStatus -delayService:0

ping 1.1.1.1 -n 1 -w 1000

start /D Win32\DataStore Win32\DataStore\DataStoreServer.exe -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:1

ping 1.1.1.1 -n 1 -w 1000

start /D Win32\FileStore Win32\FileStore\FileStoreServer.exe -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:2


ping 1.1.1.1 -n 1 -w 1000

start /D Win32\Logic Win32\Logic\LogicServer.exe -r:7311 -s:7312 -PayR:7411 -PayS:7412 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:3
rem start /D Win32\Logic Win32\Logic\LogicServer.exe -r:7321 -s:7322 -PayR:7421 -PayS:7422 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:5
rem start /D Win32\Logic Win32\Logic\LogicServer.exe -r:7331 -s:7332 -PayR:7431 -PayS:7432 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:7
rem start /D Win32\Logic Win32\Logic\LogicServer.exe -r:7341 -s:7342 -PayR:7441 -PayS:7442 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:9

ping 1.1.1.1 -n 1 -w 1000

start /D Win32\IOSPaymentQuery Win32\IOSPaymentQuery\IOSPaymentQueryServer.exe -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:4

ping 1.1.1.1 -n 1 -w 1000

start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5111 -s:5112 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:4
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5113 -s:5114 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:7
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5115 -s:5116 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:10
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5117 -s:5118 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:13

start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5121 -s:5122 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:16
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5123 -s:5124 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:19
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5125 -s:5126 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:21
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5127 -s:5128 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:24

start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5131 -s:5132 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:27
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5133 -s:5134 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:30
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5135 -s:5136 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:33
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5137 -s:5138 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:36

start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5211 -s:5212 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:39
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5213 -s:5214 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:41
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5215 -s:5216 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:44
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5217 -s:5218 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:47

start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5221 -s:5222 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:50
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5223 -s:5224 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:53
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5225 -s:5226 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:56
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5227 -s:5228 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:59

start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5231 -s:5232 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:62
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5233 -s:5234 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:65
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5235 -s:5236 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:68
start /D Win32\FogCompute Win32\FogCompute\FOGComputeServer.exe -r:5237 -s:5238 -ipv4+ipv6 -NoStatus -delayService:1 -Manager:127.0.0.1 -RegAddr:127.0.0.1 -DelayReg:71


