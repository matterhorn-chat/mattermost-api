#!/bin/sh

set -e

HERE=$(cd `dirname $0`; pwd)
TEST_PROGRAM=test-mm-api
ROOT=$HERE/..
CONTAINER=mattermost-preview

function docker_installed {
    which docker 2>/dev/null >/dev/null
}

function container_present {
    docker ps | grep $CONTAINER >/dev/null
}

function cleanup_last_container {
    # These first two docker commands are allowed to fail. For instance,
    # on a first run of this script. This check ensures that if they
    # do fail, we abort the script because the container exists AND
    # stopping it failed somehow.
    if container_present
    then
        docker stop $CONTAINER
        docker rm   $CONTAINER
    fi
}

# Note: [JED] You may need to change TEST_RUNNER depending on your environment.
# As of writing this script `cabal new-build` doesn't have a `new-run` so I use
# the following find command to locate the test-mm-api executable. YMMV.
TEST_RUNNER=$(find $ROOT -name $TEST_PROGRAM -type f)

if [ -z "$TEST_RUNNER" ]
then
    echo "Error: cannot find $TEST_PROGRAM in $ROOT, exiting"
    exit 1
fi

if ! docker_installed
then
    echo "Error: 'docker' not found in PATH, exiting"
    exit 1
fi

cleanup_last_container

# If this command fails we're in trouble.
docker run  --name $CONTAINER -d --publish 8065:8065 mattermost/$CONTAINER

# It takes a while for the MM server to start accepting logins
$HERE/wait_for_mm.sh
echo

# Finally we are ready to run the test suite
$TEST_RUNNER
