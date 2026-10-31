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

The order carries meaning. A rule the inventory's own sorting suggests is not
enough: the dispatch order is this list's order, so a narrow judgement stands
before the family rule it would otherwise be swallowed by. The pick lists of a
form stand before the collections they are drawn from.

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
    """One judgement, and the routes its match covers."""

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
    # The connection, not the domain.
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

    # A rendering concern, or a router that hides the schema.
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
    Rule("the generic history read",
         ("exact", "GET /api/history"),
         BFF, NOT_API, NO_AREA,
         "One history route for every entity type, named by an opaque entityType string, so"
         " the payload has no per-entity schema; ores.http already serves /versions for each"
         " versioned collection."),
    Rule("a form's pick lists",
         ("regex", r"^GET /api/(books|conventions|counterparties|tenors)/pick-lists$"),
         BFF, NOT_API, NO_AREA,
         "The option lists one form's controls need, keyed by the control's own name and"
         " joined by nothing; each list is a collection a client reads at its own resource."),
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

    # Read models: one answer, several entities.
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
         ("regex", r"^(GET|POST) /api/workflow/:"),
         BFF, COMPOSED, "workflow",
         "A run's progress as a rail renders it: the run's status, the step executing and one"
         " summary per step, so a page follows the run rather than the engine's events."),
    Rule("the tenor catalogue",
         ("exact", "GET /api/tenors/catalogue"),
         BFF, COMPOSED, "reference-data",
         "The tenors with their conventions and their schedules in one answer, three"
         " collection reads, because a tenor is meaningless without the rules that resolve it."),
    Rule("the market data catalogue",
         ("exact", "GET /api/marketdata/catalogue"),
         BFF, COMPOSED, "market-data",
         "The providers and the sources that serve them in one answer, two collection reads,"
         " so a subscription or a route can name a source by id."),
    Rule("the official providers",
         ("exact", "GET /api/marketdata/official"),
         BFF, COMPOSED, "market-data",
         "Every official provider with its terms, its sources and the fixing series it fills,"
         " the providers read first and the sources and market series read together after."),
    Rule("the report health",
         ("exact", "GET /api/reporting/report-health"),
         BFF, COMPOSED, "reporting",
         "Each report definition with the row the list would draw for it and the reads that"
         " failed, so the answer says a run could not be checked rather than that it never ran."),
    Rule("the engine's documented groups",
         ("exact", "GET /api/reporting/engine-settings/documentation"),
         BFF, COMPOSED, "reporting",
         "The parameter groups read from the store and shaped as the documentation of what"
         " each group means, for the screen that edits them."),
    Rule("the synthetic folder tree",
         ("exact", "GET /api/synthetic/feed-folders"),
         BFF, COMPOSED, "simulation",
         "The folders and the feeds under each at any depth, two collections woven, with the"
         " count of each kind so no screen reads the feeds."),

    # Reference data.
    Rule("the currency collection",
         ("prefix", "GET /api/v1/refdata/currencies"),
         CPP, RESOURCE, "reference-data",
         "The currency collection, generated with list, create, replace, remove, bulk get and"
         " put, and a version read."),
    Rule("the currency collection writes",
         ("regex", r"^(POST|PUT|DELETE) /api/v1/refdata/currencies"),
         CPP, RESOURCE, "reference-data",
         "A generated write of the currency collection: create, replace, remove, or one of"
         " the bulk get, put and delete calls."),
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

    # Market data.
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
         "A subscription or a route the session party holds, with the source that serves it,"
         " read as a page and written or removed one at a time."),
    Rule("the market data adapters",
         ("exact", "GET /api/marketdata/adapters"),
         BFF, RESOURCE, "market-data",
         "Every adapter the service tracks, with what it serves, what it was asked to serve"
         " and the counts of the last demand it answered."),
    Rule("the official fixings",
         ("exact", "GET /api/marketdata/official/fixings"),
         BFF, RESOURCE, "market-data",
         "The fixings of up to a dozen series in one read, oldest first, by series."),
    Rule("a provider's terms",
         ("exact", "POST /api/marketdata/official/:code/accept-terms"),
         BFF, RESOURCE, "market-data",
         "Records that a party accepted one official provider's terms at the version read,"
         " one row written against a provider that may have moved on."),

    # Trading.
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
         " a name rather than a key."),
    Rule("a portfolio's rights",
         ("exact", "GET /api/books/portfolios/:id/rights"),
         BFF, RESOURCE, "trading",
         "The rights granted on one portfolio, with the tenant's accounts read beside them so"
         " each right names the account that holds it."),
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

    # Reporting.
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
    Rule("the report runs",
         ("regex", r"^GET /api/reporting/runs/:"),
         BFF, RESOURCE, "reporting",
         "One report run: its state, its results at the level the reader asked for, and its"
         " output archive as the run stored it."),
    Rule("a report configuration",
         ("exact", "GET /api/reporting/configurations/:id"),
         BFF, RESOURCE, "reporting",
         "One report configuration by id."),
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

    # The grid.
    Rule("a grid artefact",
         ("regex", r"^GET /api/operations/grid/jobs/(workunits/:id/input|results/:id/output)"),
         BFF, RESOURCE, "grid",
         "The input bundle of one workunit or the output of one run, the bytes the grid"
         " stored."),
    Rule("the grid batches",
         ("regex", r"^GET /api/operations/grid/jobs/(batches|workunits)"),
         BFF, RESOURCE, "grid",
         "One page of the grid's batches, workunits or runs the session may see, newest"
         " first, with the name of the run and party that made each."),
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

    # Operations.
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

    # Workflow.
    Rule("one request",
         ("exact", "GET /api/requests/:id"),
         BFF, RESOURCE, "workflow",
         "One request an identifier names, when the person may open it, which a queue read"
         " cannot answer because a notice is read after the request stopped waiting."),
    Rule("a request decision",
         ("exact", "POST /api/requests/:id/decision"),
         BFF, RESOURCE, "workflow",
         "Approves, refuses, holds or resumes one request against the version read."),
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

    # Access: the tenant's identity and permission records.
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
         ("prefix", "GET /api/v1/iam/accounts"),
         CPP, RESOURCE, "access",
         "The IAM account collection, generated with list, read by username, a version read"
         " and a bulk get."),
    Rule("the account collection writes",
         ("regex", r"^(POST|PUT|DELETE) /api/v1/iam/accounts"),
         CPP, RESOURCE, "access",
         "A generated write of the IAM account collection: the delete, the bulk get, or a"
         " whole-record put."),
    Rule("the generated permission collection",
         ("regex", r"^(GET|POST|PUT|DELETE) /api/v1/iam/permissions"),
         CPP, RESOURCE, "access",
         "The permission collection, generated with list, create, replace, remove, bulk get"
         " and put."),
    Rule("the generated role collection",
         ("regex", r"^(GET|POST|PUT|DELETE) /api/v1/iam/roles/"),
         CPP, RESOURCE, "access",
         "The role collection, generated with list, create, replace, remove, bulk get and"
         " put, and a version read."),
    Rule("the role collection root",
         ("regex", r"^(GET|POST|PUT|DELETE) /api/v1/iam/roles$"),
         CPP, RESOURCE, "access",
         "The role collection itself, read as a list and written one role at a time."),
    Rule("a role's permission set",
         ("exact", "POST /api/v1/iam/roles_permissions/put"),
         CPP, RESOURCE, "access",
         "Replaces the permissions one role grants with exactly the set sent."),
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
    Rule("the bootstrap operations",
         ("regex", r"^POST /api/v1/iam/ops/(bootstrap_status|create_initial_admin)$"),
         CPP, RESOURCE, "tenancy",
         "The deployment's first-run state and the first administrator it creates, the pair"
         " that closes bootstrap mode."),

    # Auth: the credential, the sign-in, the session.
    Rule("the auth session",
         ("regex", r"^(GET|POST|DELETE) /api/v1/auth/sessions"),
         CPP, RESOURCE, "auth",
         "A session is a thing: signing in creates one and sets the cookie it rides in,"
         " signing out ends the caller's, and /me reads the identity it states."),
    Rule("the generated session collection",
         ("prefix", "GET /api/v1/iam/sessions"),
         CPP, RESOURCE, "auth",
         "The session collection, generated with list, create, replace, remove, bulk get and"
         " put."),
    Rule("the session collection writes",
         ("regex", r"^(POST|PUT|DELETE) /api/v1/iam/sessions"),
         CPP, RESOURCE, "auth",
         "A generated write of the session collection: create, replace, remove, or one of"
         " the bulk get, put and delete calls."),
    Rule("the account credentials",
         ("prefix", "GET /api/v1/iam/account_credentials"),
         CPP, RESOURCE, "auth",
         "The account credential collection and its by-account read, the rows that hold a"
         " password's hash and salt."),
    Rule("the account credential writes",
         ("regex", r"^(POST|PUT|DELETE) /api/v1/iam/account_credentials"),
         CPP, RESOURCE, "auth",
         "A generated write of the account credential collection, or its bulk get."),
    Rule("the login records",
         ("prefix", "GET /api/v1/iam/login_info"),
         CPP, RESOURCE, "auth",
         "The login record collection, generated with list, read and bulk get; a record is"
         " written by signing in and carries no credential column."),
    Rule("the login record writes",
         ("regex", r"^(POST|PUT|DELETE) /api/v1/iam/login_info"),
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

    # Tenancy.
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
    Rule("the tenant lookups",
         ("regex", r"^GET /api/tenant-(statuses|types)$"),
         BFF, RESOURCE, "tenancy",
         "The tenant lifecycle statuses or tenant types, the deployment's own rows, each"
         " carrying the badge code it is painted with."),
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

    # Simulation.
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

    # Assets.
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

    # Analytics.
    Rule("the sensitivity templates",
         ("regex", r"^(GET|POST|PUT|DELETE) /api/analytics/sensitivity-templates"),
         BFF, RESOURCE, "analytics",
         "A sensitivity template key: a page of the tenant's keys, one key, its history, and"
         " the create, correct and remove writes."),

    # Agile, and the project's own documents.
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

    # Quality.
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
        "ETag on the whole view, stale after 15 s"),
    "the bootstrap status": (
        "ores.iam",
        "the IAM account and tenant answer, plus a variability settings read that may "
        "fail without failing the answer",
        "ETag on the whole view, stale after 5 s"),
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
    "the tenor catalogue": (
        "ores.refdata",
        "the tenors with their conventions and schedules, three collection reads",
        "ETag on the whole view, stale after 5 min"),
    "the market data catalogue": (
        "ores.marketdata",
        "the providers and the sources that serve them, two collection reads",
        "ETag on the whole view, stale after 5 min"),
    "the official providers": (
        "ores.marketdata",
        "the providers first, then the sources and the market series joined onto them",
        "ETag on the whole view, stale after 5 min"),
    "the report health": (
        "ores.reporting",
        "each report definition with the row the list would draw and the reads that "
        "failed, so a failed read is not read as never having run",
        "ETag on the whole view, stale after 60 s"),
    "the engine's documented groups": (
        "ores.reporting",
        "the parameter groups read from the store and shaped as the documentation of "
        "what each group means",
        "ETag on the whole view, stale after 5 min"),
    "the synthetic folder tree": (
        "ores.synthetic",
        "the folders and the feeds under each, at any depth, with the count of each "
        "kind so no screen reads the feeds",
        "ETag on the whole view, stale after 60 s"),
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
        "ETag on the whole view, stale after 5 min"),
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
