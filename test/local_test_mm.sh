#!/bin/sh

set -e

# Note: [JED] You may need to change TEST_RUNNER depending on your environment.
# As of writing this script `cabal new-build` doesn't have a `new-run` so I use
# the following find command to locate the test-mm-api executable. YMMV.
TEST_RUNNER=$(find . -name test-mm-api -type f)

die() {
  echo $1
  exit 1
}

if ! which docker 2>/dev/null >/dev/null
then
    echo "Error: 'docker' not found in PATH, exiting"
    exit 1
fi

# These first two docker commands are allowed to fail. For instance, on a first
# run of this script.
docker stop mattermost-preview
docker rm   mattermost-preview

# If this command fails we're in trouble.
docker run  --name mattermost-preview -d --publish 8065:8065 \
       mattermost/mattermost-preview                         \
       || die "Failed to start mattermost"

# It takes a while for the MM server to start accepting logins
./test/wait_for_mm.sh
echo
# Finally we are ready to run the test suite
$TEST_RUNNER
