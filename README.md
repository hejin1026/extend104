extend104
=========

extend104 client libary

 rabbitmq config

./rabbitmqctl add_user extend104 public
./rabbitmqctl add_vhost /extend104
./rabbitmqctl set_user_tags extend104 administrator
./rabbitmqctl set_permissions -p /extend104 extend104 ".*" ".*" ".*"
./rabbitmqctl list_exchanges -p /extend104

项目作用及介绍
master:主采集
node:子采集
agent:采集服务器性能
lib:依赖库

如何配置？
node:
etc/node.args
%配置启动节点数
-num 3  


如何启动？
master:
./bin/master start

node:
./bin/start

agent:
./start

下发任务
master:
./bin/master run

下发同步命令
master:
./bin/master sync


如何停止？
master:
./bin/master stop

node:
./bin/stop

agent
./stop

启动下发顺序
先启动master，然后node，agent，再下发任务，再同步命令

部署
master部署在主服务器上
node,agent可多台机器单个部署

