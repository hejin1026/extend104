extend104
=========

extend104 client libary


= rabbitmq config

./rabbitmqctl add_user extend104 public
./rabbitmqctl add_vhost /extend104
./rabbitmqctl set_permissions -p /extend104 extend104 ".*" ".*" ".*"
./rabbitmqctl list_exchanges -p /extend104