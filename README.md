# cookci
A tiny CI tool that provides just the bare minimum needed.

## Install
Currently you'll have to build cookci from source. `cabal` and `ghc` is required.

```bash
$ git clone https://github.com/factisresearch/cookci.git
$ cd cookci && cabal sandbox init
$ cabal install -j8 --only-dependencies
$ cabal configure
$ cabal install
```

## Howto
You'll need a web service providing repository-states that you'd like to check. This web service needs the following api endpoints:

### poll api
This endpoint provides a list of all repository-states that aren't built yet. The response to this endpoint should look like this:

```javascript
[{'id':'someId', 'download':'http://somfile.tar.gz'}]
```

Every repository-state should have a unique ID and an url where cookci can download it. Every .tar.gz file needs a `build.sh` at its directory root - this will be called to perform the build.

### callback hook
This endpoint get called when a build is complete. cookci will make a POST request to this url with the following format:

```javascript
{'id':'someId','success':true, 'log':'foobarbaz'}
```

## Help
If you need more help, call `cookci --help`. You can for example configure where the web service is located, how many workers you'll need and a poll interval.