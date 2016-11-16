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
the following commands:
```sh
docker run --name mattermost-preview -d --publish 8065:8065 mattermost/mattermost-preview
./test/wait_for_mm.sh
# Run your tests, such as the test-mm-api executable
```
The docker image takes a while to stabilize after being started so that's why we
provide the `wait_for_mm.sh` script.

**Note: The tests can only be run once against a given MatterMost instance. This
is because the scripts currently assume they can create an initial admin user.**

To tear down your docker instance use the following:
```sh
docker stop mattermost/mattermost-preview
docker rm   mattermost/mattermost-preview
```

Now you may create a new instance using the `docker run` command above.

The scripts assume the instance is reachable on `localhost:8065` over plain
HTTP.  For use in production we use TLS, but for testing purposes we avoid the
certificate setup.
