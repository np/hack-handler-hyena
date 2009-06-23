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

    import Hack
    import Hack.Contrib.Middleware.ContentLength
    import Hack.Contrib.Middleware.ContentType
    import Hack.Contrib.Utils
    import Hack.Frontend.Happstack
    import Hack.Handler.Hyena
    import Network.Gitit


    default_content_type :: String
    default_content_type = "text/plain; charset=UTF-8"

    stack :: [Middleware]
    stack = 
      [  dummy_middleware
      -- completeness
      ,  content_length
      ,  content_type default_content_type
      ]

    main = do
      conf <- getDefaultConfig
      createStaticIfMissing conf
      createTemplateIfMissing conf
      createRepoIfMissing conf
      initializeGititState (userFile conf) (pluginModules conf)
      run $ use stack $ serverPartToApp (wikiHandler conf)
    