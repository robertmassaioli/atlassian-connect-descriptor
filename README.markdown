# Atlassian Connect Descriptor

When writing an Atlassian Connect add-on in Haskell you will want to create an Atlassian Connect Descriptor. You can
use this library to help you generate an Atlassian Connect Descriptor in a type safe manner and easily convert it to the
required JSON using the Aeson library.

You can [find this library on Hackage][4]; the documentation is quite extensive.

This library is used in production by the [My Reminders][1] Atlassian Connect add-on; as you can view in our [source][2]
and [live][3].

 [1]: https://my-reminders.useast.atlassian.io/
 [2]: https://bitbucket.org/atlassianlabs/my-reminders/src/9bb5e43a78ed3f8565fbbf64f24218e27f87c7af/src/AtlassianConnect.hs?at=master
 [3]: https://my-reminders.useast.atlassian.io/atlassian-connect.json
 [4]: http://hackage.haskell.org/package/atlassian-connect-descriptor