* The hyena handler is not complete, i.e. the request body is not preserved. Patch needed to convert an enumerable to lazy bytestring :)
* there is a bug in the handler, which prevents hack from serving large binary files, e.g. `Hack.Contrib.Middleware.Static` won't work properly
