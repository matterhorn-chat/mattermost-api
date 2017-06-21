
Mattermost API Release Process
==============================

This is the release procedure for making a release of `mattermost-api`.
Before beginning, the person making the release should have access to
the platforms on which binary distributions will be built.

1. Set the `mattermost-api` package version in the cabal file. The
   version string must be of the form `XYYZZ.B.C` where XYYZZ
   corresponds to the Mattermost server version supported by the
   release. For example, if the release supports Mattermost server
   version 1.2.3, the XYYZZ portion of the `matterhorn` version should
   be `10203`. The `B.C` portion of the version corresponds to our own
   version namespace for the package and should follow the guidelines
   for B and C components from https://pvp.haskell.org.  If the server
   version changes, `B.C` SHOULD be `0.0`. Otherwise the first
   component should increment if the package undergoes major code
   changes or functionality changes. The second component alone should
   change only if the package undergoes security fixes or other bug
   fixes.

2. Generate a `Changelog.md` entry list from the git log since the last
   release tag. In the changelog, include

   * changes to supported server versions
   * bugs fixed
   * contributions received (and thank contributors)
   * server feature support changes

   The changes listed in the changelog should inform the choice of
   version number.

3. Commit the changelog changes and `matterhorn-api.cabal` version change.

4. Check for a passing Travis CI build.

5. Upload to Hackage:

   * Generate a `cabal sdist`
   * Unpack the `sdist` archive
   * Perform a complete build on the unpacked archive
   * If any issues arise, repair and go to (2)
   * Otherwise, `cabal upload` the package

6. Tag the release commit using the package version as the tag string.

7. `git push --tags`

