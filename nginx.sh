#!/bin/sh

/usr/bin/cgi-fcgi -start -connect :9000 /app/term-graph
/usr/sbin/nginx -g "daemon off;"

