### Note

The hyena handler is not complete, i.e. the request body is not preserved. Patch needed to convert an enumerable to lazy bytestring :)

### Config

to use a different port, start with

    ./main -p 3456

It's the same as the hyena config.

Also use runWithConfig to put your custom port into Env, otherwise, your app won't get this port information.

    runWithConfig (def {port = 3456}) your_app

### In practice

You must set content-length in response explicitly, e.g. use the conetnt_length middleware

    import Hack.Handler.Hyena
    import Hack.Contrib.Middleware.ContentLength
    import Hack.Frontend.Happstack
    import Network.Gitit

    main = do
      conf <- getDefaultConfig
      createStaticIfMissing conf
      createTemplateIfMissing conf
      createRepoIfMissing conf
      initializeGititState conf
      run . content_length $ serverPartToApp (wiki conf)
    