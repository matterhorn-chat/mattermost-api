#!/bin/bash

set -e

HERE=$(cd `dirname $0`; pwd)
TEST_PROGRAM=test-mm-api
ROOT=$HERE/..
VERSION=4.2
CONTAINER=mattermost-preview

LOGFILE=$(mktemp)

function cleanup_log {
    status=$?
    if [ $? != 0 ]
    then
        echo "Log:"
        cat $LOGFILE
    fi

    rm $LOGFILE
}

function logged {
    $* 2>>$LOGFILE >>$LOGFILE
}

trap cleanup_log EXIT

function error {
    echo Error: $* >&2
    echo Error: $* >>$LOGFILE
    exit 1
}

function notice {
    echo $*
    echo $* >>$LOGFILE
}

function docker_installed {
    which docker 2>/dev/null >/dev/null
}

function container_present {
    docker ps --all | grep $CONTAINER >/dev/null
}

function cleanup_last_container {
    # This check ensures that we only attempt to stop and remove the
    # container if it already exists. Any failure in these commands is
    # thus not indicative of a missing image (say, on first run of this
    # script on a fresh system).
    if container_present
    then
        logged docker stop $CONTAINER
        logged docker rm   $CONTAINER
    fi
}

if [ -d "$HERE/../dist-newstyle" ]
then
    PACKAGE_VERSION=$(awk '$1 == "version:" { print $2 }' < $HERE/../mattermost-api.cabal)
    TEST_RUNNER=$HERE/../dist-newstyle/build/mattermost-api-$PACKAGE_VERSION/build/test-mm-api/test-mm-api
elif [ -d "$HERE/../dist" ]
then
    TEST_RUNNER=$(find $HERE/../dist -type f -name test-mm-api)
else
    error "Cannot run test suite; neither .cabal-sandbox nor dist-newstyle exists!"
    exit 1
fi

if [ ! -e "$TEST_RUNNER" ]
then
    error "Error: program missing: $TEST_PROGRAM"
fi

if ! docker_installed
then
    error "'docker' not found in PATH, exiting"
fi

notice "Log file location: $LOGFILE"
notice "Cleaning up preexisting Mattermost container"
cleanup_last_container

# If this command fails we're in trouble.
notice "Running a new Mattermost container"
logged docker run  --name mattermost -d --publish 8065:8065 mattermost/$CONTAINER:$VERSION
notice "docker run returned: $?"

# It takes a while for the MM server to start accepting logins
$HERE/wait_for_mm.sh

# Finally we are ready to run the test suite
notice "Running the test suite"
$TEST_RUNNER
