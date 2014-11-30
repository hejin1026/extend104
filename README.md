extend104
=========

extend104 client libary

rabbitmq 初始配置

./rabbitmqctl add_user extend104 public
./rabbitmqctl add_vhost /extend104
./rabbitmqctl set_user_tags extend104 administrator
./rabbitmqctl set_permissions -p /extend104 extend104 ".*" ".*" ".*"
./rabbitmqctl list_exchanges -p /extend104

项目作用及介绍
rabbitmq:amqp消息服务器
ertdb:实时数据库
master:主采集
node:子采集
agent:采集服务器性能
lib:依赖库

如何配置？
node:
etc/node.args
######配置启动节点数
-num 3  
######配置区域信息
-cityid common

/etc/node.config
配置amqp的主机ip，port，用户名，密码，ertdb的主机ip和port，重连间隔


命令
rabbitmq：
1。启动
./sbin/rabbitmq-server -detached
2.停止
./sbin/rabbimqctl stop

ertdb:
1.启动
./bin/ertdb start
2.停止
./bin/ertdb stop
3.查找某个key的配置
./bin/ertdb lookup key(1:11:2)

master:
1.启动
./bin/master start
2.停止
./bin/master stop
3.下发采集任务
./bin/master run
4.下发实时数据库配置
./bin/master ertdb_config 
5.下发子采集配置
./bin/master node_config 
6.查看主采集状态
./bin/master status



node:
1.启动
./bin/start
2.停止
./bin/stop


agent:
1.启动
./start
2.停止
./stop

启动下发顺序
先启动master，然后node，agent，再下发任务，再同步命令

部署
rabbitmq，ertdb部署在主服务器上（。101）
master部署在主服务器上（.101）
node,agent可多台机器单个部署（。102.103）
mysql，web部署在（。103）

===========================
完整启动命令,从停止到启动

1.启动rabbitmq
cd /opt/rabbit_server_3...  
./sbin/rabbitmq-server -detached

2.启动实时数据库
cd /opt/ertdb
./bin/ertdb start
./bin/ertdb cluster master@master101

3.启动主采集
cd /opt/extend104/master
./bin/master start

4.启动子采集
cd /opt/extend104/node
./bin/start

5.启动监控站
cd /opt/extend104/pavilion
./bin/pavilion start


6.同步
cd /opt/extend104/master
a) ./bin/master ertdb_config 下发实时数据库配置
b) ./bin/master run  下发通道
c) ./bin/master node_config 下发子采集配置


重启采集
1.停止子采集
cd /opt/extend104/node
./bin/stop
(删除日志，若需要请备份)
rm log/* -f

2.停止主采集
cd /opt/extend104/master
./bin/master stop
(删除日志，若需要请备份)
rm log/* -f

3.启动朱采集
./bin/master start

4.启动子采集
cd /opt/extend104/node
./bin/start

5.同步
同上




 







