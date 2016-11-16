[![Build
Status](https://travis-ci.org/matterhorn-chat/mattermost-api.svg?branch=master)](https://travis-ci.org/matterhorn-chat/mattermost-api)
# mattermost-api
Client side API for communicating with a MatterMost server, in Haskell.

# Testing

We use the MaterMost docker image for detecting changes in the API. See
`.travis.yml` or the [MatterMost
docs](https://docs.mattermost.com/install/docker-local-machine.html#one-line-docker-install)
for the details.

If you are testing your changes locally during development, you will want to run
the script `./test/local_test_mm.sh`.

**Note: The `local_test_mm.sh` script will stop and remove a docker container
named `mattermost-preview`.**

**Note: The tests can only be run once against a given MatterMost instance. This
is because the scripts currently assume they can create an initial admin user.**

**Note: The scripts assume the instance is reachable on `localhost:8065` over plain
HTTP.**

For use in production we use TLS, but for testing purposes we avoid the
certificate setup.
