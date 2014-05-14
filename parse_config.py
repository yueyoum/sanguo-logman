# -*- coding: utf-8 -*-

__author__ = 'Wang Chao'
__date__ = '5/9/14'

import xml.etree.ElementTree as et
tree = et.ElementTree(file="config.xml")

log_port = tree.find("log/port").text
log_flush_time = tree.find("log/flush_time").text

mysql_host = tree.find("mysql/host").text
mysql_port = tree.find("mysql/port").text
mysql_db = tree.find("mysql/db").text
mysql_user = tree.find("mysql/user").text
mysql_password = tree.find("mysql/password").text

config_template = """[
{sasl, [{sasl_error_logger, {file, "run.log"}}]},
{logman, [{port, %s},
          {logs_flush_time, %s},
          {mysql_host, "%s"},
          {mysql_port, %s},
          {mysql_db, "%s"},
          {mysql_user, "%s"},
          {mysql_password, "%s"}]}
].
"""

config = config_template % (
    log_port,
    log_flush_time,
    mysql_host,
    mysql_port,
    mysql_db,
    mysql_user,
    mysql_password
)

with open("logman_collect/config.config", 'w') as f:
    f.write(config)

