1.  cp config.template.xml config.xml
2.  vim config.xml
3.  python parse_config.py
4.  cd logman_show
5.  virtualenv env
6.  source env/bin/activate
7.  pip install -r requirements.txt
8.  python manage.py syncdb
9.  python manage.py collectstatic
9.  config nginx, uwsgi. and run
11. deactivate
10. cd ../logman_collect
11. ./rebar get-deps
12. ./rebar compile
13. ./start
