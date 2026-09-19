# Site configuration

`environments.json` is the one place this site's configuration lives: which ORE
Studio environments exist, which one this site serves, the shared certificates,
and whether the developer surface is offered.

## Why configuration rather than a screen

Where the application points is an operator's decision, not a user's. Declaring
it here means:

- Nobody signing in is asked about namespaces, ports or certificates.
- The browser is never told the host, so it cannot ask the server to connect
  anywhere else.
- Pointing a deployment somewhere else is one line and a restart, which is what
  a deployment should be.

## Selecting an environment

Pass it at start, either way round:

```sh
npm run dev:bff -- --env bright_hopper
ORES_WEB_ENV=bright_hopper npm run dev:bff
```

Omitting it uses the file's `active` field, or the first environment when
that is empty.

## Pointing the site at a different environment

`ORES_WEB_SITE_CONFIG` can point at a different file, so a deployment can take its
configuration from configuration management rather than from the repository.

## The developer surface

`developerTools` is deployment configuration, not a browser preference. Set it
in the environment file or override it with `ORES_WEB_DEVELOPER_TOOLS=0`, because
a flag a browser can switch on is not a control.

`developerAccounts` is the list of ACME test accounts offered for signing in as.
They are ordinary accounts with a shared, well-known password; the list is a
convenience for filling in the form.
