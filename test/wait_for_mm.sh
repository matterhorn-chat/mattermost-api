#!/bin/sh

ECHO=/bin/echo
$ECHO "Waiting for mattermost server to become available"
while ! curl https://localhost:8065 1> /dev/null 2> /dev/null
do
  $ECHO -n "."
  sleep 1
done
