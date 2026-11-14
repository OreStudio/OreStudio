"""What every HTTP route is, as data.

The design's four-way rule (doc/plans/2026-10-28-single-http-server-design.org,
sections 11.1 and 11.2) decides each row of the route inventory. This module
holds the decisions; build/scripts/route_inventory.py applies them.

Precedence:

1. An entry in EXCEPTIONS wins. It is keyed by the exact (server, method, path)
   of one route, with the method written as a request line writes it ("GET"),
   for a route no rule decides on its own.
2. Otherwise the first rule in RULES order whose match and server accept the
   route wins. A rule's match is tested against the string "METHOD /path", as
   the inventory prints it, and is one of:

       ("exact", "METHOD /path")
       ("prefix", "METHOD /prefix")
       ("regex", r"...")

   A rule whose `server` is None accepts either surface; otherwise it accepts
   only the surface it names.
3. Otherwise the route is unclassified, and the gate fails.

The order carries meaning: a narrow judgement stands before the family rule it
would otherwise be swallowed by.

The line between a resource and a composed view is the number of collections the
answer weaves. One collection with its keys resolved is a resource: a book with its
portfolio's name, a right with its holder's account, a run with its definition's
name, a page of defects with the party that made each. Two or more independent
collections woven into one answer are a view: a blotter with its rail, a grid summary
with its hosts and roster, a catalogue with the rules that make it meaningful.

AREAS is the published namespace of design section 11.2 and the areas this
table adds beside it. A route that is not an API publishes no area, and states
NO_AREA instead.
"""

from dataclasses import dataclass

RESOURCE = "resource"
COMPOSED = "composed view"
TRANSPORT = "transport capability"
NOT_API = "not an API"

KINDS = (RESOURCE, COMPOSED, TRANSPORT, NOT_API)

NO_AREA = "(none)"

AREAS = (
    "access",
    "analytics",
    "agile",
    "assets",
    "auth",
    "changes",
    "grid",
    "market-data",
    "operations",
    "qa",
    "reference-data",
    "reporting",
    "simulation",
    "tenancy",
    "trading",
    "workflow",
)

BFF = "bff"
CPP = "ores.http"


@dataclass(frozen=True)
class Rule:
    name: str
    match: tuple
    server: str | None
    kind: str
    area: str
    note: str


@dataclass(frozen=True)
class Exemption:
    """A decision for one route, which no rule on its own can make."""

    name: str
    kind: str
    area: str
    note: str


RULES = (
    # The IAM composed views ores.http answers, ahead of the rules for the screen
    # routes they share a part with; each keeps the BFF's view name, so its
    # obligations are stated once.
    Rule("the adapters screen's route",
         ("exact", "GET /api/marketdata/adapters"),
         CPP, RESOURCE, "market-data",
         "Every adapter the market data service tracks, with what it serves, what it was asked to"
         " serve, and the kind of source its feed reads, joined from the feeds and the sessions."),
    Rule("the public rates' route",
         ("regex", r"^(GET /api/marketdata/public-rates/fixings"
                   r"|POST /api/marketdata/public-rates(/\{code\})?/(accept|withdraw)-terms)$"),
         CPP, RESOURCE, "market-data",
         "The fixings the store holds for up to a dozen series, oldest first, and the three"
         " answers a person gives to a publisher's terms: accept one at the version read,"
         " withdraw one, and accept many in one request."),
    Rule("the ORE URI codec route",
         ("regex", r"^(GET|POST) /api/oresmd"),
         CPP, TRANSPORT, NO_AREA,
         "The codec's own table, the read of a URI into its parts, a batch of them, and the"
         " write of parts into a URI, at the addresses and in the shapes the BFF answered,"
         " storing nothing."),
    Rule("a series' observations route",
         ("exact", "GET /api/series/{id}/observations"),
         CPP, RESOURCE, "market-data",
         "One series' observations inside a window, oldest first, the count read as the cap on"
         " the answer, at the address and in the shape the BFF answered."),
    Rule("the curves' route",
         ("regex", r"^POST /api/curves/(series|series-by-id|series-list|slice|evolution"
                   r"|snapshot|manual-point(/clear)?|compute|fx-forward-ladder"
                   r"|published(/withdraw|-sets)?)$"),
         CPP, RESOURCE, "market-data",
         "The curves at the address and in the shape the BFF answered: the series an identity"
         " names, the series a party sees, the URIs of the series a blueprint names, the object"
         " snapshotted and sliced and evolved, the points a recipe computes without storing"
         " them, the manual points a person keys and clears, the published sets a party holds,"
         " the FX forward ladder of one pair, and the withdrawal that takes a set out of use."),
    Rule("the market data subscriptions' route",
         ("regex", r"^(GET|POST|DELETE) /api/marketdata/(subscriptions|routes)"
                   r"(/\{(provider|publisher)\})?$"),
         CPP, RESOURCE, "market-data",
         "The subscriptions and routes screens at the address and in the shape the BFF answered:"
         " every subscription and route the caller's party holds, the two creates that name a"
         " provider or a feed, and the removals that name the version they read."),
    Rule("the blotter",
         ("exact", "GET /api/blotter"),
         CPP, COMPOSED, "trading",
         "One page of trades with the rail that groups them, answered by ores.trading from the"
         " summaries, the structures, the members, the books, the portfolios, the"
         " counterparties and the catalogues, every row read whole before the page is cut."),
    Rule("the book tree",
         ("exact", "GET /api/books/tree"),
         CPP, COMPOSED, "trading",
         "Every portfolio and every book in one answer, two collection reads woven into the"
         " tree a person navigates."),
    Rule("the image map",
         ("exact", "GET /api/image-map"),
         CPP, COMPOSED, "assets",
         "Which image every flagged record uses, answered by ores.http from four reference"
         " data lists and one image search, at the address and in the shape the BFF"
         " answered."),
    Rule("the tenor catalogue",
         ("exact", "GET /api/tenors/catalogue"),
         CPP, COMPOSED, "reference-data",
         "The tenors in their own order with their conventions and schedules, answered by"
         " ores.http from three whole list reads at the address and in the shape the BFF"
         " answered."),
    Rule("the deployment overview",
         ("exact", "GET /api/overview"),
         CPP, COMPOSED, "tenancy",
         "The system administrator's home answered by ores.http at the address and in the"
         " shape the BFF answered: the roster counted by status, the tenants that need the"
         " administrator, the first page and the newest setups."),
    Rule("the bootstrap status",
         ("exact", "GET /api/bootstrap"),
         CPP, COMPOSED, "tenancy",
         "Whether the deployment still needs its first administrator, answered by ores.http"
         " at the address and in the shape the BFF answered, with the two first-run settings"
         " a failed read leaves false."),
    Rule("the principal's ways to hold",
         ("exact", "GET /api/me/ways-to-hold"),
         CPP, COMPOSED, "access",
         "The roles the caller could ask for that would give a permission, answered by"
         " ores.http at the address and in the shape the BFF answered."),
    Rule("the cookie credential",
         ("regex", r"^(GET|POST|DELETE) /api/session(/party|/switch-party)?$"),
         BFF, TRANSPORT, "auth",
         "The browser's cookie credential: minted by the sign-in, read back, ended, and"
         " re-scoped to another party, and the session it wraps is the /api/v1/auth/sessions"
         " resource."),
    Rule("the change stream",
         ("regex", r"^(GET /api/events|POST /api/events/watch)$"),
         BFF, TRANSPORT, "changes",
         "The change stream a client subscribes to, and the watch set that says which"
         " subjects it wants; delivery of events, not a domain object."),
    Rule("the browser's change stream route",
         ("regex", r"^(GET /api/events|POST /api/events/watch)$"),
         CPP, TRANSPORT, "changes",
         "The change stream in the shape ores.web reads, carrying only the collections the"
         " session watches, and the route that replaces that watch set."),
    Rule("the change stream route",
         ("exact", "GET /api/v1/changes"),
         CPP, TRANSPORT, "changes",
         "The change stream itself: a response whose body is written from the hub as"
         " changes happen, with a comment line keeping a quiet stream alive."),
    Rule("the liveness endpoint",
         ("exact", "GET /api/health"),
         BFF, TRANSPORT, NO_AREA,
         "The liveness answer the deployment's probes read, beside the metrics endpoints the"
         " design puts in the server."),
    Rule("the client's journal",
         ("exact", "POST /api/client-log"),
         BFF, TRANSPORT, "operations",
         "The browser's own trail written into the services' journal under the session's id;"
         " it carries log lines rather than a domain object."),
    Rule("the served client's configuration",
         ("prefix", "GET /api/site"),
         BFF, TRANSPORT, NO_AREA,
         "What the served client needs to render itself, the environment and on the"
         " developer surface the host, port and namespace, which the design delivers with"
         " the bundle rather than as a resource."),
    Rule("the liveness endpoint route",
         ("exact", "GET /api/health"),
         CPP, TRANSPORT, NO_AREA,
         "The liveness answer the BFF gave the deployment's probes, kept so a probe needs"
         " no change when the BFF goes."),
    Rule("the client's journal route",
         ("exact", "POST /api/client-log"),
         CPP, TRANSPORT, "operations",
         "The browser's trail written into the server's journal under the session's id,"
         " as the BFF wrote it."),
    Rule("the served client's configuration route",
         ("prefix", "GET /api/site"),
         CPP, TRANSPORT, NO_AREA,
         "What the served client needs to render itself, read from the site configuration"
         " the BFF read, and the deployment's plumbing where the developer surface is offered."),
    Rule("the ORE URI codec",
         ("regex", r"^(GET|POST) /api/oresmd"),
         BFF, TRANSPORT, NO_AREA,
         "A codec for ORE URIs: the grammar lives in the server, and the route turns a URI"
         " into its parts or the parts into a URI, storing nothing."),
    Rule("the bundle index",
         ("exact", "GET /"),
         CPP, TRANSPORT, NO_AREA,
         "The built browser bundle's index, served so a browser can start the application."),
    Rule("the bundle and its shell",
         ("exact", "GET /{path}"),
         CPP, TRANSPORT, NO_AREA,
         "A file of the built bundle, or the application shell for a screen path, and an"
         " /api path is answered 404 so the shell never stands in for JSON."),
    Rule("the bucket listing",
         ("exact", "GET {bucket_pattern}"),
         CPP, TRANSPORT, NO_AREA,
         "A page of the keys one bucket of the object store holds."),
    Rule("the storage object",
         ("regex", r"^(GET|HEAD|PUT|DELETE) \{object_pattern\}$"),
         CPP, TRANSPORT, NO_AREA,
         "One object of the object store, uploaded, downloaded, checked or removed by"
         " bucket and key."),
    Rule("the API's own identity",
         ("exact", "GET /api/v1/info"),
         CPP, TRANSPORT, NO_AREA,
         "The name, version and running state the API states about itself, the readiness"
         " answer beside the metrics."),

    Rule("the refdata resource catalogue",
         ("exact", "GET /api/refdata"),
         BFF, NOT_API, NO_AREA,
         "A catalogue of the resources a screen may read, each with its key fields,"
         " sortable fields, search flag, version flag and the write and delete permission"
         " names, which is a table description rather than a resource."),
    Rule("the refdata generic router",
         ("regex", r"^(GET|PUT|DELETE) /api/refdata/:"),
         BFF, NOT_API, NO_AREA,
         "The resource is a string in the URL, so the payload's shape changes with it and"
         " no client can be generated from it; the generated collection per resource"
         " already carries the same rows under a named entity."),
    Rule("the refdata resource catalogue route",
         ("exact", "GET /api/refdata"),
         CPP, NOT_API, NO_AREA,
         "The catalogue the BFF answered, built from each record unit's own entry, so the"
         " screens keep reading the resources they may name when the BFF goes."),
    Rule("the refdata record routes",
         ("regex", r"^(GET|PUT|DELETE) /api/refdata/[a-z-]+(/key/\{key\}|/by/\{parent\})?$"),
         CPP, NOT_API, NO_AREA,
         "The generic router's addresses, one generated unit per resource in the BFF's"
         " shapes, kept so the screens work when the BFF goes; the published collection"
         " per resource is the API."),
    Rule("the synthetic resource catalogue route",
         ("exact", "GET /api/synthetic"),
         CPP, NOT_API, NO_AREA,
         "The catalogue the BFF answered, built from each record unit's own entry for the"
         " session, so the screens keep reading the resources they may name when the BFF goes."),
    # The synthetic composed views, ahead of the record routes whose addresses
    # they share; each keeps the name the BFF's rule gives it, so the view's
    # obligations are stated once.
    Rule("the synthetic folder tree",
         ("exact", "GET /api/synthetic/feed-folders"),
         CPP, COMPOSED, "simulation",
         "The folders and the feeds under each at any depth, answered by ores.http from one"
         " folder call at the address and in the shape the BFF answered."),
    Rule("the market trace",
         ("exact", "GET /api/synthetic/market-trace"),
         CPP, COMPOSED, "simulation",
         "The scheduler's recent decisions and every running feed's law, answered by"
         " ores.http from one trace call at the address and in the shape the BFF answered."),
    Rule("the calibration of an asset class",
         ("exact", "GET /api/synthetic/calibration/{assetClass}"),
         CPP, COMPOSED, "simulation",
         "The policy a class calibrates under and how every feed of it resolves, with the"
         " coverage counts, answered by ores.http at the address and in the shape the BFF"
         " answered; nothing starts."),
    Rule("the synthetic record routes",
         ("regex", r"^(GET|PUT|DELETE) /api/synthetic/[a-z-]+(/key/\{key\}|/by/\{parent\})?$"),
         CPP, NOT_API, NO_AREA,
         "The generic router's addresses, one generated unit per resource in the BFF's"
         " shapes and two hand-written feed key reads, kept so the screens work when the BFF"
         " goes."),
    Rule("the synthetic resource catalogue",
         ("exact", "GET /api/synthetic"),
         BFF, NOT_API, NO_AREA,
         "A catalogue of the synthetic resources a screen may write, each with its key"
         " field, sortable fields, search flag, permission names and the reason it may be"
         " read only, which is a table description rather than a resource."),
    Rule("the synthetic generic router",
         ("regex", r"^(GET|PUT|DELETE) /api/synthetic/:"),
         BFF, NOT_API, NO_AREA,
         "The resource is a string in the URL, so the payload's shape changes with it and no"
         " client can be generated from it; the feeds' own routes name what they serve."),
    Rule("the classification catalogue",
         ("exact", "GET /api/classifications"),
         BFF, NOT_API, NO_AREA,
         "The lists a screen offers, each with the columns to draw and a row count read per"
         " list in twenty eight sequential calls, which describes a table rather than a"
         " resource."),
    Rule("the classification display order",
         ("exact", "PUT /api/classifications/:list/order"),
         BFF, NOT_API, NO_AREA,
         "Writes the display order of the rows that moved; order for display is a rendering"
         " concern the design keeps in the client."),
    Rule("the classification label",
         ("exact", "PUT /api/classifications/:list/rows/:code/label"),
         BFF, NOT_API, NO_AREA,
         "Attaches or removes a label code, and the handler states that a label is how a"
         " code is drawn rather than what it is."),
    Rule("the classification list rows",
         ("regex", r"^(GET|POST|PUT|DELETE) /api/classifications/:"),
         BFF, NOT_API, NO_AREA,
         "One code list is named by a string in the URL and reached through that one router,"
         " and each row arrives with a labelCode, the words a screen draws it with, and a"
         " displayOrder, the position the screen shows it in."),
    Rule("the label catalogue",
         ("exact", "GET /api/labels"),
         BFF, NOT_API, NO_AREA,
         "Every label and the labels each code domain uses: translated copy for display,"
         " which the design puts in the client."),
    Rule("the classification catalogue route",
         ("exact", "GET /api/classifications"),
         CPP, NOT_API, NO_AREA,
         "The catalogue the BFF answered, built from each list unit's own entry with its"
         " count, so the classification screen works when the BFF goes."),
    Rule("the classification list routes",
         ("regex", r"^(GET|POST|PUT|DELETE) /api/classifications/[a-z-]+"
                   r"(/rows(/\{code\}(/label)?)?|/order)?$"),
         CPP, NOT_API, NO_AREA,
         "The classification router's addresses, one generated unit per code list in the"
         " BFF's shapes, kept so the screen works when the BFF goes."),
    Rule("the label catalogue route",
         ("exact", "GET /api/labels"),
         CPP, NOT_API, NO_AREA,
         "The label catalogue the BFF answered, kept so the screens draw their codes when"
         " the BFF goes."),
    Rule("the history read route",
         ("exact", "GET /api/history"),
         CPP, NOT_API, NO_AREA,
         "The one history read every history tab calls, kept so the tabs work when the BFF"
         " goes; the owning component answers on the subject the entity type names."),
    Rule("the generic history read",
         ("exact", "GET /api/history"),
         BFF, NOT_API, NO_AREA,
         "One history route for every entity type, named by an opaque entityType string, so"
         " the payload has no per-entity schema; ores.http already serves /versions for each"
         " versioned collection."),
    Rule("a form's pick lists route",
         ("regex", r"^GET /api/(books|conventions|counterparties|tenors)/pick-lists$"),
         CPP, NOT_API, NO_AREA,
         "The option lists the BFF answered for each reference data form, keyed by the"
         " control's name, kept so the forms work when the BFF goes."),
    Rule("a form's pick lists",
         ("regex", r"^GET /api/(books|conventions|counterparties|tenors)/pick-lists$"),
         BFF, NOT_API, NO_AREA,
         "The option lists one form's controls need, keyed by the control's own name and"
         " joined by nothing; each list is a collection a client reads at its own resource."),
    Rule("an edit form's codes route",
         ("exact", "GET /api/reporting/choices"),
         CPP, NOT_API, NO_AREA,
         "The codes the report edit form offers, kept so the form works when the BFF goes;"
         " they are the form's option set and not a resource."),
    Rule("an edit form's codes",
         ("exact", "GET /api/reporting/choices"),
         BFF, NOT_API, NO_AREA,
         "The codes the report edit form offers so a save is accepted rather than refused,"
         " which is the form's option set and not a resource."),
    Rule("a template form's lookups",
         ("exact", "GET /api/analytics/sensitivity-templates/choices"),
         BFF, NOT_API, NO_AREA,
         "The two lookups a template states, in the order the screen offers them, which is"
         " display order for a form."),
    Rule("the pricing engine vocabulary",
         ("exact", "GET /api/reporting/pricing-engines/vocabulary"),
         BFF, NOT_API, NO_AREA,
         "Fifteen lookup collections read at once and keyed by the names the editor's"
         " controls use, so a client that wants one of them reads that collection instead."),

    Rule("the request story",
         ("regex", r"^GET /api/requests/:id/(story|changes)$"),
         BFF, COMPOSED, "workflow",
         "One request's whole story woven from every row any component wrote for it, the"
         " request's versions, the answers given on it, the roles it asked for and the"
         " notices, or the change lines of its kind beside the live record's values."),
    Rule("the request checks",
         ("exact", "GET /api/requests/:id/checks"),
         BFF, COMPOSED, "workflow",
         "Runs a request's proposed lines through the real write and answers what it says"
         " now, weaving the request with the record as it stands."),
    Rule("the approval inbox",
         ("exact", "GET /api/requests"),
         BFF, COMPOSED, "workflow",
         "The requests this person may decide, gathered from every kind into one queue with"
         " each kind's subject, which is the approval inbox the design names."),
    Rule("the authorisation queue",
         ("exact", "GET /api/queue/authorisations"),
         BFF, COMPOSED, "operations",
         "The authorisation requests this person may decide with the deal each one names as"
         " it looks now, the open requests and the answered tail in one answer."),
    Rule("the grid summary",
         ("exact", "GET /api/operations/grid"),
         BFF, COMPOSED, "grid",
         "The compute grid as one answer: the stored summary, the host registry with its"
         " hardware rows and the service roster, three reads so a node carries the name a"
         " person reads and the runner that serves it."),
    Rule("the grid series",
         ("exact", "GET /api/operations/grid/series"),
         BFF, COMPOSED, "grid",
         "The grid counters per bucket and one row of buckets per node on one time axis, two"
         " reads joined onto the host registry."),
    Rule("the grid usage",
         ("exact", "GET /api/operations/grid/usage"),
         BFF, COMPOSED, "grid",
         "Who uses the grid, the results grouped by tenant or by party with each group's"
         " share, the results read and the party names beside them."),
    Rule("the message bus",
         ("exact", "GET /api/operations/bus"),
         BFF, COMPOSED, "operations",
         "The NATS server samples over a window and one row per stream, a second read for"
         " each named stream, because no operation lists the streams."),
    Rule("the run's progress",
         ("regex", r"^GET /api/workflow/:"),
         BFF, COMPOSED, "workflow",
         "A run's progress as a rail renders it: the run's status, the step executing and one"
         " summary per step, three reads, so a page follows the run rather than the engine's"
         " events."),
    Rule("the run's retry",
         ("prefix", "POST /api/workflow/"),
         BFF, RESOURCE, "workflow",
         "A command on one run: retrying a run that stopped asks the engine to start it"
         " again, and answers the run rather than a collection the client reads."),
    Rule("the tenor catalogue",
         ("exact", "GET /api/tenors/catalogue"),
         BFF, COMPOSED, "reference-data",
         "The tenors with their conventions and their schedules in one answer, three"
         " collection reads, because a tenor is meaningless without the rules that resolve it."),
    Rule("the market data catalogue",
         ("exact", "GET /api/marketdata/catalogue"),
         BFF, COMPOSED, "market-data",
         "The providers and the feeds that serve them in one answer, two collection reads,"
         " so a subscription or a route can name a feed by id."),
    Rule("the public publishers",
         ("exact", "GET /api/marketdata/public-rates"),
         BFF, COMPOSED, "market-data",
         "Every public publisher with its terms, its feeds with their suppliers and asset"
         " classes, and the fixing series it fills, six collections read together."),
    Rule("the public rates coverage",
         ("exact", "GET /api/marketdata/public-rates/coverage"),
         BFF, COMPOSED, "market-data",
         "The first and last date and the count of every series a public publisher fills,"
         " computed from the public catalogue and its fixings."),
    Rule("the public rates file",
         ("exact", "GET /api/marketdata/public-rates/fixings.csv"),
         BFF, COMPOSED, "market-data",
         "One publisher's fixings as a CSV file over a date range, the catalogue read to"
         " find the publisher's series and their fixings read after."),
    Rule("the connectivity catalogue",
         ("exact", "GET /api/marketdata/connectivity"),
         BFF, COMPOSED, "market-data",
         "The publishers, suppliers, adapters, sessions, feeds, scopes and routes the"
         " Connectivity screen draws, seven collection reads in one answer."),
    Rule("an item's fixing series",
         ("exact", "GET /api/marketdata/fixing-series"),
         BFF, COMPOSED, "market-data",
         "One item named by its oresmd URI: its series, the feeds and sessions that brought"
         " its values, and the fixings written against it."),
    Rule("the report health",
         ("exact", "GET /api/reporting/report-health"),
         BFF, COMPOSED, "reporting",
         "Each report definition with the row the list would draw for it and the reads that"
         " failed, so the answer says a run could not be checked rather than that it never ran."),
    Rule("an ORE schema's contents",
         ("exact", "GET /api/reporting/ore-files/:code/schema"),
         BFF, COMPOSED, "reporting",
         "One schema of one engine version: the file catalogue names the schema and the"
         " stored versions supply its contents, so two collections are joined to answer."),
    Rule("the engine's documented groups",
         ("exact", "GET /api/reporting/engine-settings/documentation"),
         BFF, RESOURCE, "reporting",
         "One collection, the parameter groups read from the store and returned with what"
         " each group means; the documentation screen reads it and no collection is joined"
         " onto it."),
    Rule("the synthetic folder tree",
         ("exact", "GET /api/synthetic/feed-folders"),
         BFF, COMPOSED, "simulation",
         "The folders and the feeds under each at any depth, two collections woven, with the"
         " count of each kind so no screen reads the feeds."),

    Rule("a reference data collection",
         ("regex", r"^GET /api/v1/reference-data/\w"),
         CPP, RESOURCE, "reference-data",
         "A generated reference data collection, answering under its published area rather"
         " than under the component's name: the list, one row by key or by one of its"
         " lookups, its versions, and the bulk get."),
    Rule("a reference data collection writes",
         ("regex", r"^(POST|PUT|DELETE) /api/v1/reference-data/\w"),
         CPP, RESOURCE, "reference-data",
         "A generated write of a reference data collection: create, replace, remove, or one"
         " of the bulk put and delete calls."),
    Rule("a synthetic collection",
         ("regex", r"^GET /api/v1/simulation/\w"),
         CPP, RESOURCE, "simulation",
         "A generated synthetic collection, answering under the published simulation area:"
         " the list, one row by key, its versions, and the bulk get."),
    Rule("a synthetic collection writes",
         ("regex", r"^(POST|PUT|DELETE) /api/v1/simulation/\w"),
         CPP, RESOURCE, "simulation",
         "A generated write of a synthetic collection: create, replace, remove, or one of"
         " the bulk put and delete calls."),
    Rule("the ORE currency document",
         ("regex", r"^(GET|POST) /api/refdata/currencies/ore-"),
         BFF, RESOURCE, "reference-data",
         "The standard import and export pair, the document the ORE service writes, the"
         " change a file would make and the write, exchanged whole rather than row by row."),
    Rule("a calendar's business days",
         ("exact", "GET /api/refdata/calendars/:code/days"),
         BFF, RESOURCE, "reference-data",
         "The materialised days of one calendar in one year, one page of a derived collection."),
    Rule("a calendar rebuild",
         ("exact", "POST /api/refdata/calendars/:code/rebuild"),
         BFF, RESOURCE, "reference-data",
         "Builds the materialised days of one calendar to the end of a year, a write to one"
         " calendar's derived rows."),
    Rule("the curve configurations",
         ("regex", r"^(GET|PUT) /api/refdata/curve-configurations"),
         BFF, RESOURCE, "reference-data",
         "The curve configurations a party holds, with the definitions, the count of curves"
         " each section names, and a write that changes the leaves it names."),
    Rule("the counterparty collection",
         ("regex", r"^(GET|PUT) /api/counterparties"),
         BFF, RESOURCE, "reference-data",
         "The counterparty collection: a page, one record with its identifiers, the children"
         " of a parent, the composite write, and the visibility and business centre writes."),
    Rule("the convention families",
         ("regex", r"^(GET|PUT) /api/conventions/"),
         BFF, RESOURCE, "reference-data",
         "The convention families with their parts: the families, one family's rows, one"
         " document's terms, and the write of a family the journey draws."),
    Rule("the tenor rules",
         ("regex", r"^(GET|PUT|POST|DELETE) /api/tenors/(conventions|tenors|preview-resolution|resolve)"),
         BFF, RESOURCE, "reference-data",
         "A tenor's conventions and the resolution rows hanging off them: one convention's"
         " set, the write of a tenor, a convention or a resolution row, and the resolution of"
         " a drafted rule."),
    Rule("the change reasons",
         ("exact", "GET /api/change-reasons"),
         BFF, RESOURCE, "reference-data",
         "The reasons a write may carry, a deployment-specific code list the server holds"
         " rather than the client."),

    Rule("a curve's values",
         ("regex", r"^POST /api/curves/(series|series-by-id|series-list|slice|evolution|snapshot|manual-point)"),
         BFF, RESOURCE, "market-data",
         "The values of one curve series, read by id or by list, sliced, evolved and"
         " snapshotted, with the manual points a person adds or clears."),
    Rule("the computed curve",
         ("exact", "POST /api/curves/compute"),
         BFF, RESOURCE, "market-data",
         "Computes a curve's points from a bootstrap configuration and stores nothing, a"
         " derived read of the curve the configuration describes."),
    Rule("the FX forward ladder",
         ("exact", "POST /api/curves/fx-forward-ladder"),
         BFF, RESOURCE, "market-data",
         "One currency pair's forward ladder at one as-of, one service read that stores"
         " nothing and names the missing input when it refuses."),
    Rule("the published curves",
         ("prefix", "POST /api/curves/published"),
         BFF, RESOURCE, "market-data",
         "The published curve as a thing: the set in use, the sets a party holds, and the"
         " withdrawal that takes one out of use with the reason it names."),
    Rule("a series' observations",
         ("exact", "GET /api/series/:id/observations"),
         BFF, RESOURCE, "market-data",
         "One series' observations inside a window, oldest first, the count read as the cap"
         " on the answer."),
    Rule("the market data subscriptions",
         ("regex", r"^(GET|POST|DELETE) /api/marketdata/(subscriptions|routes)"),
         BFF, RESOURCE, "market-data",
         "A subscription or a route the session party holds, with the feed that serves it,"
         " read as a page and written or removed one at a time."),
    Rule("the market data adapters",
         ("exact", "GET /api/marketdata/adapters"),
         BFF, RESOURCE, "market-data",
         "Every adapter the service tracks, with what it serves, what it was asked to serve"
         " and the counts of the last demand it answered."),
    Rule("the public fixings",
         ("exact", "GET /api/marketdata/public-rates/fixings"),
         BFF, RESOURCE, "market-data",
         "The fixings of up to a dozen series in one read, oldest first, by series."),
    Rule("the draft reasons",
         ("exact", "GET /api/draft-reasons"),
         BFF, RESOURCE, "trading",
         "The reasons a trade may be captured as a draft with, a tenant-wide code list the"
         " booking form offers."),

    Rule("the blotter",
         ("exact", "GET /api/blotter"),
         BFF, COMPOSED, "trading",
         "One page of trades with the rail that groups them: books, portfolios, structures"
         " and structure members, counterparties, trade types, instrument codes and asset"
         " classes, eight collection reads before the page."),
    Rule("the book tree",
         ("exact", "GET /api/books/tree"),
         BFF, COMPOSED, "trading",
         "Every portfolio and every book in one answer, two collection reads woven into the"
         " tree a person navigates."),
    Rule("one book's page",
         ("exact", "GET /api/books/:id"),
         BFF, RESOURCE, "trading",
         "One book with the portfolio it sits in, the portfolio read by id so the page shows"
         " a name rather than a key; one collection with its key resolved is a resource and"
         " not a view."),
    Rule("a portfolio's rights",
         ("exact", "GET /api/books/portfolios/:id/rights"),
         BFF, RESOURCE, "trading",
         "The rights granted on one portfolio, with the tenant's accounts read beside them so"
         " each right names the account that holds it; one collection with its key resolved"
         " is a resource and not a view."),
    Rule("the book and portfolio writes",
         ("regex", r"^PUT /api/books/(books|portfolios)$"),
         BFF, RESOURCE, "trading",
         "One book or one portfolio written against the version the screen read."),
    Rule("the trade collection",
         ("regex", r"^(GET|POST) /api/trades"),
         BFF, RESOURCE, "trading",
         "The trade collection: a page, one trade, one version of it, the booking write, the"
         " links a person records and the form's securities and identifier schemes."),
    Rule("the trade link types",
         ("exact", "GET /api/trade-link-types"),
         BFF, RESOURCE, "trading",
         "The link types a person can record between trades, a lookup the server holds."),
    Rule("the book change proposal",
         ("prefix", "POST /api/book-changes"),
         BFF, RESOURCE, "trading",
         "A proposal of writes to a book: the preview runs them through the real service and"
         " writes nothing, and the write raises one request holding them."),
    Rule("the structure collection",
         ("regex", r"^(GET|POST|DELETE) /api/structures"),
         BFF, RESOURCE, "trading",
         "The structure collection: the tree flattened for reading, one structure with its"
         " legs, the leg candidates, and the leg writes that take a trade out of a container."),
    Rule("the ungrouped trades",
         ("exact", "GET /api/ungrouped-trades"),
         BFF, RESOURCE, "trading",
         "The trades no structure holds, a filtered read of the trade collection."),
    Rule("the structure kinds",
         ("exact", "GET /api/structure-kinds"),
         BFF, RESOURCE, "trading",
         "The kinds a structure may be, a lookup the server holds."),

    Rule("the report list's route",
         ("exact", "GET /api/reporting/reports"),
         CPP, RESOURCE, "reporting",
         "One page of the report definitions the caller's parties own, each row the"
         " definition with its latest run, its health word, its problem counts, its next"
         " fire and the party's own name, so a cell that cannot be read says why."),
    Rule("a report definition's route",
         ("regex", r"^((GET /api/reporting/reports/(by-name/\{name\}|\{segment\}|"
                   r"\{id\}/configurations))|(PUT /api/reporting/reports/\{id\}))$"),
         CPP, RESOURCE, "reporting",
         "One report definition at the address and in the shape the BFF answered: the id a name "
         "resolves to, the page an id or an older name opens with the choices a shared name "
         "offers, the configurations it binds, and the write that rebuilds the record from the "
         "fields the form shows."),
    Rule("a run's files",
         ("regex", r"^GET /api/reporting/runs/\{id\}/files(/(?P<file>\{file\})(/info)?)?$"),
         CPP, COMPOSED, "reporting",
         "One run's files: the tree the run hangs in, what each input package holds, the "
         "output files each accepted result's manifest names, and the bytes of whichever one "
         "the reader opened, so the screen reads one answer and not a walk of three services."),
    Rule("the kinds of cut",
         ("regex", r"^((GET|PUT|DELETE) /api/reporting/cut-kinds.*|GET /api/reporting/cut-kind-choices)$"),
         CPP, RESOURCE, "reporting",
         "The kinds of cut a report may select: the collection, one kind with the cuts the party "
         "holds of it, a write or removal, and the lookups a kind's form offers. A kind is one "
         "collection with three keys resolved -- its family's name, its resolution rule's name "
         "and the approval kind that signs a cut of it, with the permission that decider needs "
         "-- so one answer states words where the row holds codes."),
    Rule("a run's results",
         ("regex", r"^GET /api/reporting/runs/\{id\}/results$"),
         CPP, RESOURCE, "reporting",
         "One run's figures at the level the reader asks for: the result rows of the run, the "
         "bookings of its trades, and the counterparties, books and portfolios those ids name, "
         "rolled up to the trade, book or counterparty and stated against an earlier run when "
         "one is named."),
    Rule("a report run's route",
         ("regex", r"^GET /api/reporting/runs/\{id\}(/errors)?$"),
         CPP, RESOURCE, "reporting",
         "One report run at the address and in the shape the BFF answered: what it is, the state "
         "it reached, the batch that ran it and its output, and the problems it recorded."),
    Rule("a report's runs route",
         ("regex", r"^((GET|POST) /api/reporting/reports/\{id\}/(run|runs))$"),
         CPP, RESOURCE, "reporting",
         "One report's runs at the addresses and in the shapes the BFF answered: the start that "
         "answers the run it made, and the runs of a definition newest first with the state each "
         "reached."),
    Rule("the report definitions",
         ("regex", r"^GET /api/reporting/reports"),
         BFF, RESOURCE, "reporting",
         "The report definition collection: a page, one definition by id or by the name an"
         " older address saved, and the configurations it binds."),
    Rule("a report definition write",
         ("exact", "PUT /api/reporting/reports/:id"),
         BFF, RESOURCE, "reporting",
         "One report definition written against the version read."),
    Rule("a report run",
         ("exact", "POST /api/reporting/reports/:id/run"),
         BFF, RESOURCE, "reporting",
         "Starts a run of one definition and answers the run it created."),
    Rule("the report schedule",
         ("regex", r"^POST /api/reporting/reports/(un)?schedule$"),
         BFF, RESOURCE, "reporting",
         "Sets or clears the schedule of one or more definitions, and answers how many it"
         " changed, which it refused and why."),
    Rule("the report runs",
         ("regex", r"^GET /api/reporting/runs/:"),
         BFF, RESOURCE, "reporting",
         "One report run: its state, its results at the level the reader asked for, and its"
         " output archive as the run stored it."),
    Rule("a report configuration",
         ("exact", "GET /api/reporting/configurations/:id"),
         BFF, COMPOSED, "reporting",
         "One configuration with what its five keys point at: the type names, the party"
         " names, its analytics, their analytic names and its market data bindings, five"
         " reads so the page shows names rather than ids."),
    Rule("the engine settings",
         ("regex", r"^(GET|POST|PUT|DELETE) /api/reporting/engine-settings"),
         BFF, RESOURCE, "reporting",
         "The reporting engine configuration: the collection, one configuration, its"
         " history, and one group's write or proposal."),
    Rule("the pricing engines",
         ("regex", r"^(GET|POST|PUT|DELETE) /api/reporting/pricing-engines"),
         BFF, RESOURCE, "reporting",
         "The pricing engine configuration: the collection and its party names, one"
         " configuration with its bound reports, its history, and its ORE document as a file."),

    Rule("the kinds of cut",
         ("regex", r"^(GET|PUT|DELETE) /api/reporting/cut-kinds"),
         BFF, COMPOSED, "reporting",
         "The kinds of cut a report may select: the collection, one kind with the cuts the"
         " party holds of it, and a write or removal. A kind is one collection with three"
         " keys resolved — its family's name, its resolution rule's name and the approval"
         " kind that signs a cut of it, with the permission that decider needs — so one"
         " answer states words where the row holds codes."),
    Rule("the lookups a cut kind names",
         ("exact", "GET /api/reporting/cut-kind-choices"),
         BFF, COMPOSED, "reporting",
         "The families, the resolution rules and the inbox's approval kinds, three"
         " collections read so a form offers what a save will accept."),
    Rule("the hand take",
         ("exact", "POST /api/reporting/cuts/take"),
         BFF, COMPOSED, "reporting",
         "The body of a cut assembled part by part and taken, and the authorisation the"
         " take raises beside it: one call serves the flow's review and its press, so an"
         " assembly computed twice is not two rules."),
    Rule("the cuts",
         ("regex", r"^GET /api/reporting/cuts"),
         BFF, COMPOSED, "reporting",
         "The cuts a party holds, and one cut's record: the body's facts, the state and the"
         " label the reporting service derives, the kind the body was taken under, the gaps"
         " the assembly could not fill and the runs that priced from it."),
    Rule("a grid artefact",
         ("regex", r"^GET /api/operations/grid/jobs/(workunits/:id/input|results/:id/output)"),
         BFF, RESOURCE, "grid",
         "The input bundle of one workunit or the output of one run, the bytes the grid"
         " stored."),
    Rule("the grid batches",
         ("regex", r"^GET /api/operations/grid/jobs/(batches|workunits)"),
         BFF, RESOURCE, "grid",
         "One page of the grid's batches, workunits or runs the session may see, newest"
         " first, with the name of the run and party that made each; one collection with its"
         " keys resolved is a resource and not a view."),
    Rule("the report runs of one definition",
         ("exact", "GET /api/reporting/reports/:id/runs"),
         BFF, RESOURCE, "reporting",
         "The runs one report definition produced, newest first, each with the state it"
         " reached; one collection filtered by its parent."),
    Rule("one report run",
         ("exact", "GET /api/operations/grid/report-runs/:id"),
         BFF, RESOURCE, "grid",
         "One report run with its health, the batch that carried it and the name of its"
         " definition resolved onto it; one collection with its keys resolved is a resource"
         " and not a view."),
    Rule("a report run's errors",
         ("exact", "GET /api/operations/grid/report-runs/:id/errors"),
         BFF, RESOURCE, "grid",
         "The error events one report run raised, with their total; one collection filtered"
         " by its parent."),
    Rule("a QA step's images",
         ("regex", r"^POST /api/qa/scenarios/:[^/]+/steps/:[^/]+/images$"),
         BFF, RESOURCE, "qa",
         "An image a tester attaches to one step of a scenario, written into the scenario's"
         " own store and read back with it."),
    Rule("the grid results",
         ("regex", r"^GET /api/operations/grid/jobs/results"),
         BFF, RESOURCE, "grid",
         "One page of the runs that ended with an outcome, the answer to which of them"
         " failed, or one run by id."),
    Rule("the grid app versions",
         ("regex", r"^(GET|PUT) /api/operations/grid/app-versions"),
         BFF, RESOURCE, "grid",
         "One app version with its packages and the versions beside it, and the write that"
         " replaces its release note against the version read."),

    Rule("the services roster",
         ("exact", "GET /api/operations/services"),
         BFF, RESOURCE, "operations",
         "Every expected service instance with the age of its last report, read from the"
         " telemetry samples."),
    Rule("the telemetry logs",
         ("exact", "GET /api/operations/logs"),
         BFF, RESOURCE, "operations",
         "One page of telemetry entries a filter selects, with the count of everything it"
         " matches."),
    Rule("the system settings",
         ("exact", "GET /api/v1/system-settings"),
         CPP, RESOURCE, "operations",
         "The deployment's system settings, the variability component's own collection."),

    Rule("one request",
         ("exact", "GET /api/requests/:id"),
         BFF, RESOURCE, "workflow",
         "One request an identifier names, when the person may open it, which a queue read"
         " cannot answer because a notice is read after the request stopped waiting."),
    Rule("a request decision",
         ("exact", "POST /api/requests/:id/decision"),
         BFF, RESOURCE, "workflow",
         "Approves, refuses, holds or resumes one request against the version read."),
    Rule("the approvals mode",
         ("exact", "GET /api/approvals-mode"),
         BFF, RESOURCE, "workflow",
         "Whether auto-approval is on, its scope, and who set it, when and why, read from"
         " the one subject the inbox answers it on."),
    Rule("the principal's requests",
         ("regex", r"^(GET|POST|DELETE) /api/me/requests"),
         BFF, RESOURCE, "workflow",
         "The signed-in person's own requests, raised, read and taken back, the withdrawal"
         " carrying the version the person saw."),
    Rule("the principal's notifications",
         ("regex", r"^(GET|POST) /api/me/notifications"),
         BFF, RESOURCE, "workflow",
         "The signed-in person's notifications, their unread count, and the reads that mark"
         " or clear them."),
    Rule("the hold reasons",
         ("exact", "GET /api/hold-reasons"),
         BFF, RESOURCE, "workflow",
         "The codes a decider may park a request with, a tenant-wide lookup that carries no"
         " personal data."),

    Rule("an access screen's route",
         ("regex", r"^(GET|POST|PUT|DELETE) /api/(me|accounts|roles|permissions|reporting-tree)(/.*)?$"),
         CPP, RESOURCE, "access",
         "An access screen's read or write at the address and in the shape the BFF answered,"
         " forwarded to IAM as the caller, kept so the screens work when the BFF goes."),
    Rule("the counterparty screen's route",
         ("regex", r"^(GET|PUT) /api/counterparties/(\{id\}(/(visibility|business-centres))?|children|composite)$"),
         CPP, RESOURCE, "reference-data",
         "The counterparty onboarding screen's read or write at the address and in the shape"
         " the BFF answered: one record with its identifiers, the children of a page, the"
         " composite write, and the visibility and business centre writes."),
    Rule("the conventions screen's route",
         ("regex", r"^(GET /api/conventions/(families|\{family\})|PUT /api/conventions/\{family\})$"),
         CPP, RESOURCE, "reference-data",
         "The conventions screen's read or write at the address and in the shape the BFF"
         " answered: each family with its count, one family's page, and the write of a family"
         " whose terms the screen draws."),
    Rule("the tenors screen's route",
         ("regex", r"^(POST /api/tenors/(preview-resolution|resolve|resolve-convention)"
                   r"|GET /api/tenors/conventions/\{code\}/resolutions"
                   r"|PUT /api/tenors/(tenors|conventions)"
                   r"|(PUT|DELETE) /api/tenors/conventions/\{code\}/resolutions/\{tenor\})$"),
         CPP, RESOURCE, "reference-data",
         "The tenors screen's operation at the address and in the shape the BFF answered: the"
         " resolution of a drafted rule and of the stored rules, one convention's membership"
         " rows, and the writes of a tenor, a convention and a membership row."),
    Rule("the calendar screen's business days route",
         ("regex", r"^(GET /api/refdata/calendars/\{code\}/days|POST /api/refdata/calendars/\{code\}/rebuild)$"),
         CPP, RESOURCE, "reference-data",
         "One calendar's built days in one year, and the build of its days to the end of a"
         " year, at the address and in the shape the BFF answered."),
    Rule("the currencies' ORE document route",
         ("regex", r"^(GET /api/refdata/currencies/ore-document|POST /api/refdata/currencies/ore-import(/preview)?)$"),
         CPP, RESOURCE, "reference-data",
         "The currencies' ORE import and export: the document the ORE service writes, what a"
         " file would change, and the import, each answered with the service's own words."),
    Rule("the change reasons a screen offers",
         ("exact", "GET /api/change-reasons"),
         CPP, RESOURCE, "reference-data",
         "The reasons a screen offers for a write, read from the data quality service in the"
         " shape the BFF answered."),
    Rule("the feed screen's operation",
         ("regex", r"^(POST /api/synthetic/(feeds|folders)/\{id\}/(start|stop)"
                   r"|POST /api/synthetic/feed-selection/(start|stop)"
                   r"|GET /api/synthetic/folder-feeds)$"),
         CPP, RESOURCE, "simulation",
         "The feed screen's operation at the address and in the shape the BFF answered: one"
         " feed or one folder's feeds started and stopped, a page of the feeds a filter matches"
         " with whether each runs, and the start and stop of a selection."),
    Rule("the feed screens' live read",
         ("regex", r"^GET /api/synthetic/(feeds/\{id\}/ticks|feed-raster)$"),
         CPP, RESOURCE, "simulation",
         "One feed's newest ticks, and when each feed's ticks arrived in a window, read from"
         " the buffers the gateway fills by listening to the feeds a screen names."),
    Rule("the synthetic screens' preview",
         ("regex", r"^POST /api/synthetic/(simulate/(fx|ir)|convert/(fx|fx-simple|ir)|preview/ir-shape)$"),
         CPP, RESOURCE, "simulation",
         "A preview the synthetic screens draw from the values on the screen and store nothing"
         " of: sample paths, the conversions between engines and the simple view, and the rate"
         " at each ladder row."),
    Rule("the calibration screen's action",
         ("regex", r"^(POST /api/synthetic/calibration/(\{assetClass\}/run|variance/\{id\}/review)"
                   r"|(PUT|DELETE) /api/synthetic/calibration/(\{assetClass\}/policy|feeds/\{id\}/pin))$"),
         CPP, RESOURCE, "simulation",
         "The calibration screen's action at the address and in the shape the BFF answered: a"
         " class's calibration run and stored, a variance calibration reviewed, a class's policy"
         " stated or removed, and one feed's candidate pinned or cleared."),
    Rule("the market regime screen's route",
         ("regex", r"^((GET|POST) /api/synthetic/market-regime|PUT /api/synthetic/market-regime/settings"
                   r"|GET /api/synthetic/market-history)$"),
         CPP, RESOURCE, "simulation",
         "The market-wide arrival factor at the address and in the shape the BFF answered: the"
         " regime in force, the switch that forces it, the settings that move it, and the tick"
         " counts of the last stretch of the run."),
    Rule("the trade screens' read",
         ("regex", r"^GET /api/trades(/\{id\}(/versions/\{version\})?)?$"),
         CPP, RESOURCE, "trading",
         "The trade screens' reads at the address and in the shape the BFF answered: a page of"
         " the trade list enriched with what a person reads, one trade's page, and one trade"
         " at one version."),
    Rule("the booking screens' route",
         ("regex", r"^((GET|POST) /api/trades(/new|/\{id\}/links)?"
                   r"|GET /api/(draft-reasons|trade-link-types))$"),
         CPP, RESOURCE, "trading",
         "The booking screens at the address and in the shape the BFF answered: what the"
         " form offers, the reasons a deal is a draft, the link types a person records, a"
         " link between two trades, and the booking write itself."),
    Rule("the structure screens' route",
         ("regex", r"^((GET|POST|DELETE) /api/structures(/\{key\}(/legs(/\{tradeId\})?|/leg-candidates)?)?"
                   r"|GET /api/(structure-kinds|ungrouped-trades))$"),
         CPP, RESOURCE, "trading",
         "The structure screens at the address and in the shape the BFF answered: the kinds a"
         " grouping may choose, the trades no structure holds yet, the grouping write, the list"
         " as a tree flattened for reading, one structure's page with its tree and legs, the leg"
         " candidates, and the leg writes."),
    Rule("the book screens' route",
         ("regex", r"^((GET /api/books/(\{id\}|portfolios/\{id\}/rights)"
                   r"|(PUT) /api/books/(books|portfolios)"
                   r"|POST /api/book-changes(/preview)?))$"),
         CPP, RESOURCE, "trading",
         "The book screens at the address and in the shape the BFF answered: one book with the"
         " portfolio it sits in, the rights at a portfolio node with the accounts that hold them,"
         " the book and portfolio writes, and the book change proposal's preview and raise."),
    Rule("an image screen's route",
         ("regex", r"^(GET /api/(images|images/\{id\}|image-upload-policy)|POST /api/images)$"),
         CPP, RESOURCE, "assets",
         "An image screen's read or upload at the address and in the shape the BFF answered,"
         " forwarded to the assets service as the caller."),
    Rule("the tenant roster's route",
         ("regex", r"^(GET /api/(tenants|overview|tenants/\{code\})|DELETE /api/tenants/\{code\})$"),
         CPP, RESOURCE, "tenancy",
         "The deployment's tenants as system administration reads them, each joined with the"
         " run that set it up, at the address and in the shape the BFF answered, refused"
         " without the deployment read."),
    Rule("the browser's session route",
         ("regex", r"^((GET|POST|DELETE) /api/session|POST /api/session/(party|switch-party))$"),
         CPP, RESOURCE, "auth",
         "The browser's session at the address and in the shape the BFF answered: sign in,"
         " read, sign out, choose the party a login waits on, and move to another party."),
    Rule("the account door's route",
         ("regex", r"^(POST /api/(signup|account/password)|GET /api/password-policy)$"),
         CPP, RESOURCE, "auth",
         "A registration, the rules a password must keep, and the signed-in account's own"
         " password change, at the address and in the shape the BFF answered; a"
         " registration shares the sign-in budget."),
    Rule("a setup screen's route",
         ("regex", r"^(GET /api/(bootstrap|tenant-setup|registration-policy)|POST /api/bootstrap/(administrator|complete))$"),
         CPP, RESOURCE, "tenancy",
         "The deployment's setup state, the first administrator, the end of first-run setup,"
         " a tenant's own setup run and the registration policy, at the address and in the"
         " shape the BFF answered."),
    Rule("a read inside a tenant",
         ("regex", r"^GET /api/tenants/\{code\}/(parties|people|accounts/\{username\}/sign-ins|images/\{id\})$"),
         CPP, RESOURCE, "tenancy",
         "One tenant's data read from system administration, inside the tenant for the read"
         " alone, at the address and in the shape the BFF answered."),
    Rule("a provisioning journey's route",
         ("regex", r"^(GET /api/seed-profiles|POST /api/provision-(tenant|party))$"),
         CPP, RESOURCE, "tenancy",
         "The starting points a tenant is provisioned from and the two journeys that"
         " provision, at the address and in the shape the BFF answered, forwarded as the"
         " caller."),
    Rule("a tenancy screen's route",
         ("regex", r"^GET /api/(tenant-statuses|tenant-types|lei-entities|parties)$"),
         CPP, RESOURCE, "tenancy",
         "A tenancy screen's catalogue or page at the address and in the shape the BFF"
         " answered, forwarded as the caller."),
    Rule("the account collection",
         ("exact", "GET /api/accounts"),
         BFF, RESOURCE, "access",
         "The tenant's accounts, one page, because the number of accounts a tenant holds is"
         " not a number a screen uses."),
    Rule("one account",
         ("exact", "GET /api/accounts/:username"),
         BFF, RESOURCE, "access",
         "One account by username, answered with nothing rather than a 404 when the row has"
         " gone since the list was read."),
    Rule("an account's picture",
         ("exact", "GET /api/accounts/:username/picture"),
         BFF, RESOURCE, "access",
         "The image one account uses, read by username because most places that name an"
         " account know a username rather than an image id."),
    Rule("an account's profile",
         ("regex", r"^PUT /api/accounts/:username/profile$"),
         BFF, RESOURCE, "access",
         "One account's profile fields, written whole from a read taken inside the request."),
    Rule("an account's sign ins",
         ("exact", "GET /api/accounts/:username/sign-ins"),
         BFF, RESOURCE, "auth",
         "One account's sign-in state and its sessions, newest first."),
    Rule("an account's roles",
         ("regex", r"^(GET /api/accounts/:accountId/(access|permissions)|POST /api/accounts/:accountId/roles|DELETE /api/accounts/:accountId/roles/:roleId)$"),
         BFF, RESOURCE, "access",
         "The roles one account holds, or the permissions those roles grant, and the writes"
         " that give a role for a reason or take it away."),
    Rule("an account's lock",
         ("regex", r"^POST /api/accounts/:accountId/(lock|unlock)$"),
         BFF, RESOURCE, "access",
         "One account locked or unlocked, a lock refusing the next sign-in while leaving the"
         " sessions it already holds open."),
    Rule("an account's reporting line",
         ("exact", "PUT /api/accounts/:accountId/reporting-line"),
         BFF, RESOURCE, "access",
         "Sets or clears who one account reports to, and nothing else."),
    Rule("an account's contact record",
         ("regex", r"^(GET|PUT) /api/accounts/:accountId/contact-information$"),
         BFF, RESOURCE, "access",
         "One account's contact record as an administrator reads or writes it, the write"
         " reusing the identifier of the account's current record."),
    Rule("the role collection",
         ("regex", r"^(GET|POST|PUT|DELETE) /api/roles"),
         BFF, RESOURCE, "access",
         "The tenant's roles: the collection, one page of it for the editor, one role's"
         " rename or removal, the permission set it grants and the people who hold it."),
    Rule("the permission catalogue",
         ("exact", "GET /api/permissions"),
         BFF, RESOURCE, "access",
         "Every permission the platform defines, the catalogue a role is described against."),
    Rule("the reporting hierarchy",
         ("exact", "GET /api/reporting-tree"),
         BFF, RESOURCE, "access",
         "The tenant's reporting shape in one read, or one account's branch of it."),
    Rule("the principal's access",
         ("regex", r"^GET /api/me/(access|permissions)$"),
         BFF, RESOURCE, "access",
         "The roles the signed-in person holds with who gave each and why, or one page of"
         " what those roles let them do, the documented alias for the principal."),
    Rule("the principal's ways to hold",
         ("exact", "GET /api/me/ways-to-hold"),
         BFF, COMPOSED, "access",
         "The roles the person could ask for that would give them a permission, weaving the"
         " role catalogue and what each role grants against the grants the person already has."),
    Rule("the principal's own record",
         ("regex", r"^PUT /api/me/(profile|contact-information)$"),
         BFF, RESOURCE, "access",
         "The signed-in person's own profile or contact record, whose account the session"
         " names so the body carries no id."),
    Rule("the principal's contact record",
         ("exact", "GET /api/me/contact-information"),
         BFF, RESOURCE, "access",
         "The signed-in person's own contact record, answered with nothing when there is"
         " none yet because the first write creates it."),
    Rule("the principal's default party",
         ("exact", "POST /api/me/default-party"),
         BFF, RESOURCE, "access",
         "Sets or clears the party quick sign-in uses on the signed-in account."),

    Rule("the generated account collection",
         ("prefix", "GET /api/v1/access/accounts"),
         CPP, RESOURCE, "access",
         "The IAM account collection, generated with list, read by username, a version read"
         " and a bulk get."),
    Rule("the account collection writes",
         ("regex", r"^(POST|PUT|DELETE) /api/v1/access/accounts"),
         CPP, RESOURCE, "access",
         "A generated write of the IAM account collection: the delete, the bulk get, or a"
         " whole-record put."),
    Rule("the generated permission collection",
         ("regex", r"^(GET|POST|PUT|DELETE) /api/v1/access/permissions"),
         CPP, RESOURCE, "access",
         "The permission collection, generated with list, create, replace, remove, bulk get"
         " and put."),
    Rule("the generated role collection",
         ("regex", r"^(GET|POST|PUT|DELETE) /api/v1/access/roles/"),
         CPP, RESOURCE, "access",
         "The role collection, generated with list, create, replace, remove, bulk get and"
         " put, and a version read."),
    Rule("the role collection root",
         ("regex", r"^(GET|POST|PUT|DELETE) /api/v1/access/roles$"),
         CPP, RESOURCE, "access",
         "The role collection itself, read as a list and written one role at a time."),
    Rule("a role's permission set",
         ("exact", "POST /api/v1/iam/roles_permissions/put"),
         CPP, RESOURCE, "access",
         "Replaces the permissions one role grants with exactly the set sent."),
    Rule("an access collection",
         ("regex", r"^(GET|POST|PUT|DELETE) /api/v1/access/(?!accounts|permissions|roles|sessions)\w"),
         CPP, RESOURCE, "access",
         "A generated collection of the access area that is not one of the four the"
         " gateway served first: its rows, a row by key or by a lookup, and its writes."),
    Rule("an auth collection",
         ("regex", r"^(GET|POST|PUT|DELETE) /api/v1/auth/(?!sessions|login_info|account_credentials)\w"),
         CPP, RESOURCE, "auth",
         "A generated collection of the auth area beyond the door's own: its rows and its"
         " writes, published under the area rather than under the component's name."),
    Rule("the account operations",
         ("regex", r"^POST /api/v1/iam/ops/(update_account|lock_account|unlock_account)$"),
         CPP, RESOURCE, "access",
         "An RPC-shaped write to one account: its fields, or the lock that refuses the next"
         " sign-in and leaves the open sessions alone."),
    Rule("the role assignment operations",
         ("regex", r"^POST /api/v1/iam/ops/(assign_role|revoke_role|get_account_roles|get_role_permissions)$"),
         CPP, RESOURCE, "access",
         "The roles one account holds and the permissions one role grants, read or written"
         " through an RPC-shaped verb."),
    Rule("a tenancy collection",
         ("regex", r"^GET /api/v1/tenancy/\w"),
         CPP, RESOURCE, "tenancy",
         "A generated tenancy collection: the list, one row by key, its versions and the"
         " bulk get, published under the area a tenant's own rows answer on."),
    Rule("a tenancy collection write",
         ("regex", r"^(POST|PUT|DELETE) /api/v1/tenancy/\w"),
         CPP, RESOURCE, "tenancy",
         "A generated write of a tenancy collection: create, replace, remove, or one of the"
         " bulk put and delete calls."),
    Rule("the bootstrap operations",
         ("regex", r"^POST /api/v1/iam/ops/(bootstrap_status|create_initial_admin)$"),
         CPP, RESOURCE, "tenancy",
         "The deployment's first-run state and the first administrator it creates, the pair"
         " that closes bootstrap mode."),

    Rule("the auth session",
         ("regex", r"^(GET|POST|DELETE) /api/v1/auth/sessions"),
         CPP, RESOURCE, "auth",
         "A session is a thing: signing in creates one and sets the cookie it rides in,"
         " signing out ends the caller's, and /me reads the identity it states."),
    Rule("the generated session collection",
         ("prefix", "GET /api/v1/access/sessions"),
         CPP, RESOURCE, "auth",
         "The session collection, generated with list, create, replace, remove, bulk get and"
         " put."),
    Rule("the session collection writes",
         ("regex", r"^(POST|PUT|DELETE) /api/v1/access/sessions"),
         CPP, RESOURCE, "auth",
         "A generated write of the session collection: create, replace, remove, or one of"
         " the bulk get, put and delete calls."),
    Rule("the account credentials",
         ("prefix", "GET /api/v1/auth/account_credentials"),
         CPP, RESOURCE, "auth",
         "The account credential collection and its by-account read, the rows that hold a"
         " password's hash and salt."),
    Rule("the account credential writes",
         ("regex", r"^(POST|PUT|DELETE) /api/v1/auth/account_credentials"),
         CPP, RESOURCE, "auth",
         "A generated write of the account credential collection, or its bulk get."),
    Rule("the login records",
         ("prefix", "GET /api/v1/auth/login_info"),
         CPP, RESOURCE, "auth",
         "The login record collection, generated with list, read and bulk get; a record is"
         " written by signing in and carries no credential column."),
    Rule("the login record writes",
         ("regex", r"^(POST|PUT|DELETE) /api/v1/auth/login_info"),
         CPP, RESOURCE, "auth",
         "A generated write of the login record collection, or its bulk get."),
    Rule("the auth events",
         ("exact", "POST /api/v1/iam/auth_events/list"),
         CPP, RESOURCE, "auth",
         "The tenant's authentication events, read through one POST that carries the filter."),
    Rule("the sign-in opens a session",
         ("exact", "POST /api/v1/iam/ops/login"),
         CPP, RESOURCE, "auth",
         "An RPC-shaped write that mints one session: the object is the session, and the"
         " verb in the path names the write."),
    Rule("the sign-in ends a session",
         ("regex", r"^POST /api/v1/iam/ops/(logout|end_session)$"),
         CPP, RESOURCE, "auth",
         "An RPC-shaped write that ends one session, the caller's or the identifier's."),
    Rule("the session reads",
         ("regex", r"^POST /api/v1/iam/ops/(get_active_sessions|get_session_statistics)$"),
         CPP, RESOURCE, "auth",
         "An RPC-shaped read of the session store: the sessions with no end time, or the"
         " per-day per-account roll-up of them."),
    Rule("the credential writes",
         ("regex", r"^POST /api/v1/iam/ops/(change_password|reset_password|update_my_email|signup)$"),
         CPP, RESOURCE, "auth",
         "An RPC-shaped write to one credential or account: the caller's password or email,"
         " an administrator's reset, or the account a stranger registers."),
    Rule("the login records read",
         ("regex", r"^(GET|POST) /api/login-info"),
         BFF, RESOURCE, "auth",
         "The tenant's login records, one page or one account's record, the reads the audit"
         " screen uses to explain a locked account."),
    Rule("the session reads and writes",
         ("regex", r"^(GET /api/sessions|GET /api/sessions/active|POST /api/sessions/:sessionId/end)$"),
         BFF, RESOURCE, "auth",
         "The tenant's sessions, one page or the open ones, and the write that ends one of"
         " them for an administrator."),
    Rule("an auth screen's route",
         ("regex", r"^(GET /api/(auth-events|session-statistics|login-info|sessions|sessions/active|login-info/\{accountId\})|POST /api/sessions/\{sessionId\}/end)$"),
         CPP, RESOURCE, "auth",
         "An audit screen's read or an administrator's end of a session, at the address and"
         " in the shape the BFF answered, forwarded to IAM as the caller."),
    Rule("the session statistics",
         ("exact", "GET /api/session-statistics"),
         BFF, RESOURCE, "auth",
         "The tenant's session statistics, one row per day and account, newest day first,"
         " the audit screen's own collection."),
    Rule("the tenant auth events",
         ("exact", "GET /api/auth-events"),
         BFF, RESOURCE, "auth",
         "The tenant's authentication events, newest first, with the account filter the read"
         " takes though the screen does not offer it."),
    Rule("the principal's sessions",
         ("exact", "GET /api/me/sessions"),
         BFF, RESOURCE, "auth",
         "The signed-in person's own open sessions, a self read the server scopes to the"
         " caller."),
    Rule("the password policy",
         ("exact", "GET /api/password-policy"),
         BFF, RESOURCE, "auth",
         "The rules a password must satisfy, read from the validator that enforces them"
         " rather than copied into a screen."),
    Rule("the password change",
         ("exact", "POST /api/account/password"),
         BFF, RESOURCE, "auth",
         "Replaces the signed-in account's own password, the account named by the session so"
         " nobody else's is reachable."),
    Rule("the registration",
         ("exact", "POST /api/signup"),
         BFF, RESOURCE, "auth",
         "Registers one account against the tenant the address resolves to and answers its"
         " state rather than a session."),
    Rule("the address lookup",
         ("regex", r"^(GET /api/geo/country|POST /api/v1/iam/ops/lookup_country)$"),
         None, RESOURCE, "auth",
         "Resolves one address to a country against the tenant's published ranges, the read"
         " the sign-in audit uses to say where a session came from."),

    Rule("inside one tenant",
         ("regex", r"^GET /api/tenants/:code/(parties|people|accounts/:username/sign-ins|images/:id)$"),
         BFF, RESOURCE, "tenancy",
         "A page of one tenant's parties, people or accounts and their sign-ins, read from"
         " inside the tenant rather than from the deployment."),
    Rule("the tenant collection",
         ("regex", r"^(GET|DELETE) /api/tenants"),
         BFF, RESOURCE, "tenancy",
         "The tenants this deployment holds: a page of the roster, one tenant with the run"
         " that set it up, and the removal that closes a tenant and keeps its data."),
    Rule("the first administrator",
         ("exact", "POST /api/bootstrap/administrator"),
         BFF, RESOURCE, "tenancy",
         "Creates the account that closes bootstrap mode, refused unless the deployment says"
         " it is still in bootstrap."),
    Rule("the setup flag",
         ("exact", "POST /api/bootstrap/complete"),
         BFF, RESOURCE, "tenancy",
         "Records that the first-run journey finished, one settings write for the tenant the"
         " session works in."),
    Rule("the tenant setup run",
         ("exact", "GET /api/tenant-setup"),
         BFF, RESOURCE, "tenancy",
         "The tenant administrator's own setup run, the session naming the tenant so the"
         " request carries nothing."),
    Rule("the account operations",
         ("regex", r"^POST /api/v1/iam/accounts/(put|delete)$"),
         CPP, RESOURCE, "access",
         "An account write answered by a domain operation rather than by the generated"
         " collection, still addressed under the component until the operation routes move."),
    Rule("the tenant lookups",
         ("regex", r"^GET /api/tenant-(statuses|types)$"),
         BFF, RESOURCE, "tenancy",
         "The tenant lifecycle statuses or tenant types, the deployment's own rows. The"
         " badge code each row carries is stored on the row and is not a rendering concern:"
         " the route returns the row, and a client decides how to paint it."),
    Rule("the seed profiles",
         ("exact", "GET /api/seed-profiles"),
         BFF, RESOURCE, "tenancy",
         "The starting points a new tenant may be provisioned from, each with the step kinds"
         " it orders and the parameters its form declares."),
    Rule("the legal entities",
         ("exact", "GET /api/lei-entities"),
         BFF, RESOURCE, "tenancy",
         "The root legal entities a tenant can be started from, matched and sized by the"
         " read because the deployment holds tens of thousands."),
    Rule("the registration policy",
         ("exact", "GET /api/registration-policy"),
         BFF, RESOURCE, "tenancy",
         "What the deployment offers somebody not in it yet: the switch, the destination and"
         " whether a registration can be used at once."),
    Rule("the party provisioning",
         ("exact", "POST /api/provision-party"),
         BFF, RESOURCE, "tenancy",
         "Adds one party to the tenant the caller works in and starts the run that brings it"
         " to life, the row first and the run after it."),
    Rule("the tenant provisioning",
         ("exact", "POST /api/provision-tenant"),
         BFF, RESOURCE, "tenancy",
         "Provisions one tenant from a starting point and answers the run that does the rest."),
    Rule("the party collection",
         ("exact", "GET /api/parties"),
         BFF, RESOURCE, "tenancy",
         "One page of the session's own tenant's parties, scoped by row-level security so"
         " the route names no tenant."),
    Rule("the principal's parties",
         ("exact", "GET /api/me/parties"),
         BFF, RESOURCE, "tenancy",
         "The parties the signed-in person works in, each named by the server because the"
         " association read carries an identifier alone."),

    Rule("the market regime in force",
         ("exact", "GET /api/synthetic/market-regime"),
         BFF, RESOURCE, "simulation",
         "The market state in force, the multiplier every feed is scaled by, and who holds"
         " the state."),
    Rule("the market regime writes",
         ("regex", r"^(POST /api/synthetic/market-regime|PUT /api/synthetic/market-regime/settings)$"),
         BFF, RESOURCE, "simulation",
         "Forces the regime or hands it back to the chain, or sets how the market moves, the"
         " reply being the state after the write."),
    Rule("the market trace",
         ("exact", "GET /api/synthetic/market-trace"),
         BFF, COMPOSED, "simulation",
         "The scheduler's recent decisions woven into one trail: every tick with the gap its"
         " law drew and the wall gap it took, every regime turn and pin, and every change of"
         " the feeds."),
    Rule("the market history",
         ("exact", "GET /api/synthetic/market-history"),
         BFF, RESOURCE, "simulation",
         "The ticks of the last hour folded by the second and by asset class, with the pace"
         " and active share of each slot, counts rather than ticks so the read costs the same"
         " at any tick rate."),
    Rule("the feed raster",
         ("exact", "GET /api/synthetic/feed-raster"),
         BFF, RESOURCE, "simulation",
         "The times of the ticks each feed published in the window, one row per feed, a"
         " stopped feed keeping its row so the picture shows that it is silent."),
    Rule("the calibration of an asset class",
         ("exact", "GET /api/synthetic/calibration/:assetClass"),
         BFF, COMPOSED, "simulation",
         "The policy a class calibrates under and how every feed of it resolves, with the"
         " coverage counts; nothing starts."),
    Rule("a calibration run of an asset class",
         ("exact", "POST /api/synthetic/calibration/:assetClass/run"),
         BFF, RESOURCE, "simulation",
         "Calibrates every feed of a class, or the one feed named, and writes each moved level"
         " and deviation as a new version of the feed config."),
    Rule("the review of a variance calibration",
         ("exact", "POST /api/synthetic/calibration/variance/:id/review"),
         BFF, RESOURCE, "simulation",
         "Approves or rejects one proposed GJR-GARCH calibration with the reviewer's reason;"
         " approving supersedes the pair's earlier approval and older proposals and points the"
         " pair's feeds at it."),
    Rule("the calibration policy of an asset class",
         ("regex", r"^(PUT|DELETE) /api/synthetic/calibration/:assetClass/policy$"),
         BFF, RESOURCE, "simulation",
         "States a class's policy and brings its provider order to the list given, or returns"
         " the class to automatic calibration; two collections written together."),
    Rule("the pinned item of a feed",
         ("regex", r"^(PUT|DELETE) /api/synthetic/calibration/feeds/:id/pin$"),
         BFF, RESOURCE, "simulation",
         "Pins one candidate on a feed as the exception to its class policy, or clears the"
         " feed's own items so it follows the policy again."),
    Rule("the terms of a public publisher",
         ("regex", r"^POST /api/marketdata/public-rates/:code/(accept|withdraw)-terms$"),
         BFF, RESOURCE, "market-data",
         "Records the acceptance of a publisher's terms as the service holds them, or"
         " withdraws it so the adapter no longer fetches the publisher."),
    Rule("the terms of several public publishers",
         ("exact", "POST /api/marketdata/public-rates/accept-terms"),
         BFF, RESOURCE, "market-data",
         "Accepts the terms of several publishers at the versions read, each as the single"
         " accept would, and answers every publisher's outcome."),
    Rule("the synthetic feeds",
         ("regex", r"^(GET|POST|DELETE) /api/synthetic/(synthetic|feeds|folder-feeds)"),
         BFF, RESOURCE, "simulation",
         "A page of feeds a filter matches, one feed's ticks, and the start and stop writes"
         " of a feed's run."),
    Rule("the synthetic folders",
         ("regex", r"^POST /api/synthetic/folders/:"),
         BFF, RESOURCE, "simulation",
         "Starts or stops every feed under one folder, its subtree included, and answers what"
         " started, what was already running and what was skipped."),
    Rule("a feed selection",
         ("regex", r"^POST /api/synthetic/feed-selection/"),
         BFF, RESOURCE, "simulation",
         "Starts or stops the feeds a person picked, or the feeds a filter matches less those"
         " left out, the service finding them."),
    Rule("the synthetic conversions",
         ("regex", r"^POST /api/synthetic/(convert|simulate|preview)/"),
         BFF, RESOURCE, "simulation",
         "A derived read of the synthetic service's own function: a mixture or engine"
         " parameter set converted between views, spot or short-rate paths sampled, or the"
         " rate at each ladder row, with nothing stored."),

    Rule("the asset collections",
         ("regex", r"^(GET|POST|PUT|DELETE) /api/v1/assets/"),
         CPP, RESOURCE, "assets",
         "The image, tag and image tag collections, generated with list, create, replace,"
         " remove, bulk get and put, a read by parent, and a version read."),
    Rule("the image catalogue",
         ("regex", r"^(GET|POST) /api/images(/:id)?$"),
         BFF, RESOURCE, "assets",
         "The tenant's image catalogue: a page of the chooser's rows, an upload that answers"
         " the identifier, and one image's bytes."),
    Rule("the image upload policy",
         ("exact", "GET /api/image-upload-policy"),
         BFF, RESOURCE, "assets",
         "The rule an uploaded image must satisfy, read from the validator that enforces it"
         " rather than copied into a picker."),
    Rule("the image map",
         ("exact", "GET /api/image-map"),
         BFF, COMPOSED, "assets",
         "Which image every flagged record uses, weaving currencies, countries, calendars and"
         " business centres into one code to image map, five reads, with a calendar and a"
         " business centre falling back to their country's flag and a placeholder for a code"
         " that has none."),

    Rule("the sensitivity templates",
         ("regex", r"^(GET|POST|PUT|DELETE) /api/analytics/sensitivity-templates"),
         BFF, RESOURCE, "analytics",
         "A sensitivity template key: a page of the tenant's keys, one key, its history, and"
         " the create, correct and remove writes."),

    Rule("the sprint list",
         ("exact", "GET /api/agile/sprints"),
         BFF, RESOURCE, "agile",
         "The sprints the file store states, the collection the board and the charts are read"
         " against."),
    Rule("the sprint board",
         ("exact", "GET /api/agile/board"),
         BFF, COMPOSED, "agile",
         "One sprint's work items and the states they sit in, woven into the columns a person"
         " reads, from the sprint file and the story, task and capture documents under it."),
    Rule("the sprint charts",
         ("exact", "GET /api/agile/charts"),
         BFF, COMPOSED, "agile",
         "One sprint's scope and the work remaining per day, computed from the sprint file"
         " and the state changes of the documents under it."),
    Rule("the backlog",
         ("exact", "GET /api/agile/backlog"),
         BFF, RESOURCE, "agile",
         "The stories no sprint holds, in the order the file store keeps them."),
    Rule("the work trees",
         ("exact", "GET /api/agile/trees"),
         BFF, RESOURCE, "agile",
         "The git work trees the agents work in, the source the board reads when it shows who"
         " holds what."),
    Rule("an agile document",
         ("exact", "GET /api/agile/docs/:id"),
         BFF, RESOURCE, "agile",
         "One sprint, story, task, capture or scenario document, by the identifier its file"
         " states."),

    Rule("the scenarios",
         ("regex", r"^GET /api/qa/scenarios"),
         BFF, RESOURCE, "qa",
         "The QA scenarios the store holds, one scenario by id, and the image kept beside one"
         " of its steps."),
    Rule("a scenario run",
         ("exact", "PUT /api/qa/scenarios/:id/run"),
         BFF, RESOURCE, "qa",
         "Runs one scenario and answers its verdict, the write that makes it a run rather"
         " than a document."),
    Rule("a QA document",
         ("exact", "GET /api/qa/docs/:id"),
         BFF, RESOURCE, "qa",
         "One document the scenario store holds, by its identifier."),
)


EXCEPTIONS = {
    (BFF, "GET", "/api/overview"): Exemption(
        "the deployment overview", COMPOSED, "tenancy",
        "The system administrator's home: the tenant roster divided into in service, on"
        " evaluation and setting up, the first page of the roster and the newest setups,"
        " every count a separate list read that asks for its total."),
    (BFF, "GET", "/api/bootstrap"): Exemption(
        "the bootstrap status", COMPOSED, "tenancy",
        "Whether the deployment still needs its first administrator, composed from the IAM"
        " account and tenant answer and a variability settings read; a failure of the second"
        " read still answers the first."),
    (BFF, "GET", "/api/timeline"): Exemption(
        "one subject's story", COMPOSED, "workflow",
        "One person's, party's or request's story as one stream, each source gated by the"
        " permission its owner puts on it and a source that refuses becoming a gap in the"
        " answer."),
}


# A composed view owes four things (design section 11.1): a name in domain language,
# a published schema, an owner, and a stated computation and cache policy. The name is
# the decision's `name`; the schema follows from K1; these are the other two. The owner
# is the component that must answer the question after the port, which is not always
# the component that answers it today: `no service` marks a view whose owner has no
# HTTP surface yet, and X1 decides whether it survives at all.
VIEWS = {
    "the deployment overview": (
        "ores.iam",
        "the tenant roster counted by status, its first page, and the newest setups, "
        "each count a separate list read",
        "ETag on the whole view, revalidated at each read, because provisioning changes it"),
    "the bootstrap status": (
        "ores.iam",
        "the IAM account and tenant answer, plus a variability settings read that may "
        "fail without failing the answer",
        "ETag on the whole view, revalidated at each read, because setup changes it"),
    "one subject's story": (
        "ores.workflow",
        "every component's rows for one subject, each source read under its owner's "
        "permission and a refused source reported as a gap",
        "ETag on the whole view, stale after 30 s"),
    "the request story": (
        "ores.workflow",
        "the request's versions, the decisions taken on it, the roles it asked for and "
        "the notices, or the change lines of its kind beside the live record",
        "ETag on the whole view, stale after 15 s"),
    "the request checks": (
        "ores.workflow",
        "each proposed line run through the real write against the record as it stands",
        "ETag on the whole view, stale after 30 s"),
    "the approval inbox": (
        "ores.workflow",
        "every kind of request this person may decide, gathered into one queue with "
        "each kind's subject",
        "ETag on the whole view, stale after 15 s"),
    "the grid summary": (
        "ores.compute",
        "the stored summary, the host registry with its hardware rows, and the service "
        "roster, three reads",
        "ETag on the whole view, stale after 10 s"),
    "the grid series": (
        "ores.compute",
        "the counters per bucket joined to one row of buckets per node and the host "
        "registry, two reads",
        "ETag on the whole view, stale after 10 s"),
    "the grid usage": (
        "ores.compute",
        "the results grouped by tenant or by party with each group's share, and the "
        "party names beside them",
        "ETag on the whole view, stale after 30 s"),
    "the message bus": (
        "ores.telemetry",
        "the NATS server samples over a window, and one read per stream because no "
        "operation lists the streams",
        "ETag on the whole view, stale after 10 s"),
    "the run's progress": (
        "ores.workflow",
        "the run's status, the step executing and one summary per step",
        "ETag on the whole view, stale after 5 s"),
    "the kinds of cut": (
        "ores.reporting",
        "the cut kinds with their family and resolution rule names, the approval kinds "
        "the kinds point at, and one cut read for the counts, four reads",
        "ETag on the whole view, stale after 60 s"),
    "the lookups a cut kind names": (
        "ores.reporting",
        "the cut families, the cut resolution rules and the inbox approval kinds, three "
        "collection reads",
        "ETag on the whole view, stale after 5 min"),
    "the hand take": (
        "ores.reporting",
        "the body assembled from the trading, market data, compute and reporting reads, "
        "answered part by part with the gaps it could not fill, and the cut the press "
        "wrote with the approval request it raised",
        "no cache: the call writes when it does not preview"),
    "the cuts": (
        "ores.reporting",
        "the cuts of the session's party with the derived state, label and run tally the "
        "reporting service answers, or one cut with its kind and the gaps the assembly "
        "could not fill",
        "ETag on the whole view, stale after 15 s"),
    "the tenor catalogue": (
        "ores.refdata",
        "the tenors with their conventions and schedules, three collection reads",
        "ETag on the whole view, revalidated at each read, because the tenors screen's"
        " writes change it"),
    "the market data catalogue": (
        "ores.marketdata",
        "the providers and the feeds that serve them, two collection reads",
        "ETag on the whole view, stale after 5 min"),
    "the public publishers": (
        "ores.marketdata",
        "the providers, feeds, scopes, sessions, suppliers and market series read together "
        "and joined onto each publisher",
        "ETag on the whole view, stale after 5 min"),
    "the public rates coverage": (
        "ores.marketdata",
        "the public catalogue and its fixings, reduced to each series' first date, last "
        "date and count",
        "ETag on the whole view, stale after 5 min"),
    "the public rates file": (
        "ores.marketdata",
        "the public catalogue to find the publisher's series, then their fixings filtered "
        "to the range and item",
        "no cache: a file is built per request"),
    "the connectivity catalogue": (
        "ores.marketdata",
        "seven collection reads in parallel, each parsed and sorted by name",
        "ETag on the whole view, stale after 5 min"),
    "an item's fixing series": (
        "ores.marketdata",
        "the market series found by URI, with the providers, feeds, scopes, sessions and "
        "suppliers read beside it, then its fixings",
        "ETag on the whole view, stale after 5 min"),
    "the authorisation queue": (
        "ores.inbox",
        "the request queue for this viewer, each open and answered request joined to the "
        "deal it names",
        "no cache: the queue answers what this person may decide now"),
    "a report configuration": (
        "ores.reporting",
        "the configuration, the names of its type and parties, its analytics and their "
        "names, and its market data bindings, five reads",
        "ETag on the whole view, stale after 60 s"),
    "a run's files": (
        "ores.reporting",
        "the run, the packages compute says it read and wrote, the entries storage says each "
        "input package holds, the files each accepted result's manifest names, and the ORE "
        "catalogue, so a read that fails leaves its part of the tree out",
        "no cache: a run's files are read when a person opens the run, and the bytes of a file "
        "are served from the object as the socket asks for them"),
    "the report health": (
        "ores.reporting",
        "each report definition with the row the list would draw and the reads that "
        "failed, so a failed read is not read as never having run",
        "ETag on the whole view, stale after 60 s"),
    "an ORE schema's contents": (
        "ores.reporting",
        "the catalogue entry that is a schema with this code, and its stored contents "
        "at the engine version asked for, two reads",
        "no cache: the schema is read from the store per request"),
    "the synthetic folder tree": (
        "ores.synthetic",
        "the folders and the feeds under each, at any depth, with the count of each "
        "kind so no screen reads the feeds",
        "ETag on the whole view, revalidated at each read, because folder writes change it"),
    "the blotter": (
        "ores.trading",
        "one page of trades with the rail that groups them: books, portfolios, "
        "structures, counterparties, trade types, instrument codes and asset classes, "
        "eight collection reads before the page",
        "ETag on the whole view, stale after 30 s"),
    "the book tree": (
        "ores.trading",
        "every portfolio and every book, two collection reads woven into the tree",
        "ETag on the whole view, stale after 60 s"),
    "the principal's ways to hold": (
        "ores.iam",
        "the role catalogue and what each role grants against the grants the person "
        "already holds",
        "ETag on the whole view, revalidated at each read, because a grant changes it"),
    "the calibration of an asset class": (
        "ores.synthetic",
        "every feed of the class resolved as a start would, with its candidates and their "
        "reasons, under the class policy or the defaults; nothing starts",
        "No cache: a resolution reads the newest fixings each time"),
    "the market trace": (
        "ores.synthetic",
        "the scheduler's recent decisions: every tick with the gap its law drew and the "
        "wall gap it took, every regime turn and pin, and every change of the feeds",
        "ETag on the whole view, stale after 10 s"),
    "the image map": (
        "ores.assets",
        "which image every flagged record uses: currencies, countries, calendars and "
        "business centres in one code to image map, five reads, with a country fallback "
        "and a placeholder",
        "ETag on the whole view, stale after 5 min"),
    "the sprint board": (
        "no service, X1 decides",
        "one sprint's work items and their states, read from the sprint file and the "
        "documents under it",
        "ETag on the whole view, stale after 60 s"),
    "the sprint charts": (
        "no service, X1 decides",
        "one sprint's scope and the work remaining per day, computed from the sprint "
        "file and the state changes under it",
        "ETag on the whole view, stale after 60 s"),
}
