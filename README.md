[![Hackage](https://img.shields.io/hackage/v/mattermost-api.svg)](https://hackage.haskell.org/package/mattermost-api) [![Build
Status](https://travis-ci.org/matterhorn-chat/mattermost-api.svg?branch=master)](https://travis-ci.org/matterhorn-chat/mattermost-api)
[![Build status](https://github.com/matterhorn-chat/mattermost-api/actions/workflows/ci.yml/badge.svg)](https://github.com/matterhorn-chat/mattermost-api/actions/workflows/ci.yml)
# mattermost-api

Client-side API for communicating with a Mattermost server, written in
Haskell.

# Testing

We use the MatterMost docker image for detecting changes in the API. See
`.travis.yml` or the [Mattermost
docs](https://docs.mattermost.com/install/docker-local-machine.html#one-line-docker-install)
for the details.

If you are testing your changes locally during development, you will
want to run the script `./test/local_test_mm.sh`.

**Note: The `local_test_mm.sh` script will stop and remove a docker
container named `mattermost-preview`.**

**Note: The tests can only be run once against a given Mattermost
instance. This is because the scripts currently assume they can create
an initial admin user.**

**Note: The scripts assume the instance is reachable on `localhost:8065`
over plain HTTP.**

For use in production we use TLS, but for testing purposes we avoid the
certificate setup.

# Our Versioning Scheme

This library uses the same versioning scheme as `matterhorn`, see [Our
Versioning
Scheme](https://github.com/matterhorn-chat/matterhorn/blob/master/README.md#our-versioning-scheme).
The short version is that in `ABBCC.X.Y`, the `ABBCC` corresponds to Mattermost
server version `A.BB.CC` and the `X.Y` portion of the version string corresponds
to the version of `mattermost-api` package releases.
