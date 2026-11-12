"""Which C++ route answers each BFF route, as data.

Coverage is counted from this table. A key is the BFF route and a value is the
ores.http route, each as a (METHOD, path) pair written as the inventory prints it.
A BFF route with no entry is an explicit gap: the inventory prints "none" beside it.

Pair a route only when the C++ route clearly does the same thing. A pair that is
wrong breaks ores.web at the switch, and a gap that is wrong costs one more look. A
route keyed differently on the two servers, such as an id on one and a code on the
other, stays a gap until a port task reconciles the key.

A pair records the mapping and does not prove it. The port tasks prove each route
against the answer the BFF gives.

The two routes of a pair can use different verbs. Many BFF GET and DELETE routes map
to a C++ POST operation on purpose, so do not change a verb to make a pair match.
"""

ACCESS = "/api/v1/access"
REFDATA = "/api/v1/reference-data"

PAIRS = {
    # access
    ("GET", "/api/accounts"): ("GET", "/api/accounts"),
    ("GET", "/api/accounts/:username"): ("GET", "/api/accounts/{username}"),
    ("POST", "/api/accounts/:accountId/lock"): ("POST", "/api/accounts/{accountId}/lock"),
    ("POST", "/api/accounts/:accountId/unlock"): ("POST", "/api/accounts/{accountId}/unlock"),
    ("POST", "/api/me/default-party"): ("POST", "/api/me/default-party"),
    ("PUT", "/api/roles/:roleId"): ("PUT", "/api/roles/{roleId}"),
    ("POST", "/api/roles/:roleId/permissions/changes"): ("POST", "/api/roles/{roleId}/permissions/changes"),
    ("PUT", "/api/me/profile"): ("PUT", "/api/me/profile"),
    ("PUT", "/api/accounts/:username/profile"): ("PUT", "/api/accounts/{username}/profile"),
    ("GET", "/api/me/contact-information"): ("GET", "/api/me/contact-information"),
    ("PUT", "/api/me/contact-information"): ("PUT", "/api/me/contact-information"),
    ("GET", "/api/accounts/:accountId/contact-information"): ("GET", "/api/accounts/{accountId}/contact-information"),
    ("PUT", "/api/accounts/:accountId/contact-information"): ("PUT", "/api/accounts/{accountId}/contact-information"),
    ("GET", "/api/accounts/:username/picture"): ("GET", "/api/accounts/{username}/picture"),
    ("PUT", "/api/accounts/:accountId/reporting-line"): ("PUT", "/api/accounts/{accountId}/reporting-line"),
    ("GET", "/api/reporting-tree"): ("GET", "/api/reporting-tree"),
    ("GET", "/api/me/ways-to-hold"): ("GET", "/api/me/ways-to-hold"),
    ("GET", "/api/me/sessions"): ("GET", "/api/me/sessions"),
    ("GET", "/api/accounts/:username/sign-ins"): ("GET", "/api/accounts/{username}/sign-ins"),
    ("GET", "/api/images/:id"): ("GET", "/api/images/{id}"),
    ("GET", "/api/image-upload-policy"): ("GET", "/api/image-upload-policy"),
    ("GET", "/api/lei-entities"): ("GET", "/api/lei-entities"),
    ("GET", "/api/me/access"): ("GET", "/api/me/access"),
    ("GET", "/api/me/permissions"): ("GET", "/api/me/permissions"),
    ("GET", "/api/accounts/:accountId/permissions"): ("GET", "/api/accounts/{accountId}/permissions"),
    ("GET", "/api/roles/page"): ("GET", "/api/roles/page"),
    ("GET", "/api/roles/:roleId/holders"): ("GET", "/api/roles/{roleId}/holders"),
    ("GET", "/api/accounts/:accountId/access"): ("GET", "/api/accounts/{accountId}/access"),
    ("POST", "/api/accounts/:accountId/roles"): ("POST", "/api/accounts/{accountId}/roles"),
    ("DELETE", "/api/accounts/:accountId/roles/:roleId"): ("DELETE", "/api/accounts/{accountId}/roles/{roleId}"),
    ("GET", "/api/permissions"): ("GET", "/api/permissions"),
    ("GET", "/api/roles"): ("GET", "/api/roles"),
    ("POST", "/api/roles"): ("POST", "/api/roles"),
    ("DELETE", "/api/roles/:name"): ("DELETE", "/api/roles/{name}"),
    ("GET", "/api/roles/:roleId/permissions"): ("GET", "/api/roles/{roleId}/permissions"),
    ("PUT", "/api/roles/:roleId/permissions"): ("PUT", "/api/roles/{roleId}/permissions"),

    # auth
    ("POST", "/api/session"): ("POST", "/api/session"),
    ("POST", "/api/session/party"): ("POST", "/api/session/party"),
    ("POST", "/api/session/switch-party"): ("POST", "/api/session/switch-party"),
    ("DELETE", "/api/session"): ("DELETE", "/api/session"),
    ("GET", "/api/session"): ("GET", "/api/session"),
    ("GET", "/api/login-info"): ("GET", "/api/login-info"),
    ("GET", "/api/login-info/:accountId"): ("GET", "/api/login-info/{accountId}"),
    ("POST", "/api/account/password"): ("POST", "/api/account/password"),
    ("GET", "/api/password-policy"): ("GET", "/api/password-policy"),
    ("POST", "/api/signup"): ("POST", "/api/signup"),
    ("GET", "/api/sessions"): ("GET", "/api/sessions"),
    ("GET", "/api/sessions/active"): ("GET", "/api/sessions/active"),
    ("POST", "/api/sessions/:sessionId/end"): ("POST", "/api/sessions/{sessionId}/end"),
    ("GET", "/api/session-statistics"): ("GET", "/api/session-statistics"),
    ("GET", "/api/auth-events"): ("GET", "/api/auth-events"),
    ("GET", "/api/geo/country"): ("GET", "/api/geo/country"),

    # assets
    ("GET", "/api/images"): ("GET", "/api/images"),
    ("POST", "/api/images"): ("POST", "/api/images"),

    # changes
    ("GET", "/api/events"): ("GET", "/api/events"),
    ("POST", "/api/events/watch"): ("POST", "/api/events/watch"),

    # tenancy
    ("GET", "/api/tenants"): ("GET", "/api/tenants"),
    ("GET", "/api/overview"): ("GET", "/api/overview"),
    ("GET", "/api/tenants/:code"): ("GET", "/api/tenants/{code}"),
    ("DELETE", "/api/tenants/:code"): ("DELETE", "/api/tenants/{code}"),
    ("GET", "/api/tenants/:code/parties"): ("GET", "/api/tenants/{code}/parties"),
    ("GET", "/api/tenants/:code/people"): ("GET", "/api/tenants/{code}/people"),
    ("GET", "/api/tenants/:code/accounts/:username/sign-ins"): ("GET", "/api/tenants/{code}/accounts/{username}/sign-ins"),
    ("GET", "/api/tenants/:code/images/:id"): ("GET", "/api/tenants/{code}/images/{id}"),
    ("GET", "/api/tenant-statuses"): ("GET", "/api/tenant-statuses"),
    ("GET", "/api/tenant-types"): ("GET", "/api/tenant-types"),
    ("GET", "/api/seed-profiles"): ("GET", "/api/seed-profiles"),
    ("POST", "/api/provision-tenant"): ("POST", "/api/provision-tenant"),
    ("POST", "/api/provision-party"): ("POST", "/api/provision-party"),
    ("GET", "/api/bootstrap"): ("GET", "/api/bootstrap"),
    ("POST", "/api/bootstrap/administrator"): ("POST", "/api/bootstrap/administrator"),
    ("POST", "/api/bootstrap/complete"): ("POST", "/api/bootstrap/complete"),
    ("GET", "/api/tenant-setup"): ("GET", "/api/tenant-setup"),
    ("GET", "/api/registration-policy"): ("GET", "/api/registration-policy"),
    ("GET", "/api/parties"): ("GET", "/api/parties"),
    ("GET", "/api/me/parties"): ("GET", "/api/me/parties"),

    # reference data
    ("GET", "/api/refdata"): ("GET", "/api/refdata"),
    ("GET", "/api/synthetic"): ("GET", "/api/synthetic"),
    ("GET", "/api/classifications"): ("GET", "/api/classifications"),
    ("GET", "/api/labels"): ("GET", "/api/labels"),
    ("GET", "/api/history"): ("GET", "/api/history"),
    ("GET", "/api/health"): ("GET", "/api/health"),
    ("POST", "/api/client-log"): ("POST", "/api/client-log"),
    ("GET", "/api/site"): ("GET", "/api/site"),
    ("GET", "/api/site/deployment"): ("GET", "/api/site/deployment"),
    ("GET", "/api/books/pick-lists"): ("GET", "/api/books/pick-lists"),
    ("GET", "/api/conventions/pick-lists"): ("GET", "/api/conventions/pick-lists"),
    ("GET", "/api/counterparties/pick-lists"): ("GET", "/api/counterparties/pick-lists"),
    ("GET", "/api/tenors/pick-lists"): ("GET", "/api/tenors/pick-lists"),
    ("GET", "/api/counterparties"): ("GET", f"{REFDATA}/counterparties"),
    ("GET", "/api/counterparties/:id"): ("GET", "/api/counterparties/{id}"),
    ("GET", "/api/counterparties/children"): ("GET", "/api/counterparties/children"),
    ("PUT", "/api/counterparties/composite"): ("PUT", "/api/counterparties/composite"),
    ("GET", "/api/counterparties/:id/visibility"): ("GET", "/api/counterparties/{id}/visibility"),
    ("PUT", "/api/counterparties/:id/visibility"): ("PUT", "/api/counterparties/{id}/visibility"),
    ("PUT", "/api/counterparties/:id/business-centres"): ("PUT", "/api/counterparties/{id}/business-centres"),
    ("GET", "/api/conventions/families"): ("GET", "/api/conventions/families"),
    ("GET", "/api/conventions/:family"): ("GET", "/api/conventions/{family}"),
    ("PUT", "/api/conventions/:family"): ("PUT", "/api/conventions/{family}"),
    ("POST", "/api/tenors/preview-resolution"): ("POST", "/api/tenors/preview-resolution"),
    ("POST", "/api/tenors/resolve"): ("POST", "/api/tenors/resolve"),
    ("POST", "/api/tenors/resolve-convention"): ("POST", "/api/tenors/resolve-convention"),
    ("GET", "/api/tenors/conventions/:code/resolutions"): ("GET", "/api/tenors/conventions/{code}/resolutions"),
    ("PUT", "/api/tenors/tenors"): ("PUT", "/api/tenors/tenors"),
    ("PUT", "/api/tenors/conventions"): ("PUT", "/api/tenors/conventions"),
    ("PUT", "/api/tenors/conventions/:code/resolutions/:tenor"): ("PUT", "/api/tenors/conventions/{code}/resolutions/{tenor}"),
    ("DELETE", "/api/tenors/conventions/:code/resolutions/:tenor"): ("DELETE", "/api/tenors/conventions/{code}/resolutions/{tenor}"),
    ("GET", "/api/refdata/calendars/:code/days"): ("GET", "/api/refdata/calendars/{code}/days"),
    ("POST", "/api/refdata/calendars/:code/rebuild"): ("POST", "/api/refdata/calendars/{code}/rebuild"),
    ("GET", "/api/refdata/currencies/ore-document"): ("GET", "/api/refdata/currencies/ore-document"),
    ("POST", "/api/refdata/currencies/ore-import/preview"): ("POST", "/api/refdata/currencies/ore-import/preview"),
    ("POST", "/api/refdata/currencies/ore-import"): ("POST", "/api/refdata/currencies/ore-import"),
    ("GET", "/api/change-reasons"): ("GET", "/api/change-reasons"),

    # simulation
    ("POST", "/api/synthetic/feeds/:id/start"): ("POST", "/api/synthetic/feeds/{id}/start"),
    ("POST", "/api/synthetic/feeds/:id/stop"): ("POST", "/api/synthetic/feeds/{id}/stop"),
    ("POST", "/api/synthetic/folders/:id/start"): ("POST", "/api/synthetic/folders/{id}/start"),
    ("POST", "/api/synthetic/folders/:id/stop"): ("POST", "/api/synthetic/folders/{id}/stop"),
    ("GET", "/api/synthetic/folder-feeds"): ("GET", "/api/synthetic/folder-feeds"),
    ("POST", "/api/synthetic/feed-selection/start"): ("POST", "/api/synthetic/feed-selection/start"),
    ("POST", "/api/synthetic/feed-selection/stop"): ("POST", "/api/synthetic/feed-selection/stop"),
    ("GET", "/api/synthetic/feeds/:id/ticks"): ("GET", "/api/synthetic/feeds/{id}/ticks"),
    ("GET", "/api/synthetic/feed-raster"): ("GET", "/api/synthetic/feed-raster"),
    ("POST", "/api/synthetic/simulate/fx"): ("POST", "/api/synthetic/simulate/fx"),
    ("POST", "/api/synthetic/simulate/ir"): ("POST", "/api/synthetic/simulate/ir"),
    ("POST", "/api/synthetic/convert/fx"): ("POST", "/api/synthetic/convert/fx"),
    ("POST", "/api/synthetic/convert/fx-simple"): ("POST", "/api/synthetic/convert/fx-simple"),
    ("POST", "/api/synthetic/convert/ir"): ("POST", "/api/synthetic/convert/ir"),
    ("POST", "/api/synthetic/preview/ir-shape"): ("POST", "/api/synthetic/preview/ir-shape"),
    ("POST", "/api/synthetic/calibration/:assetClass/run"): ("POST", "/api/synthetic/calibration/{assetClass}/run"),
    ("POST", "/api/synthetic/calibration/variance/:id/review"): ("POST", "/api/synthetic/calibration/variance/{id}/review"),
    ("PUT", "/api/synthetic/calibration/:assetClass/policy"): ("PUT", "/api/synthetic/calibration/{assetClass}/policy"),
    ("DELETE", "/api/synthetic/calibration/:assetClass/policy"): ("DELETE", "/api/synthetic/calibration/{assetClass}/policy"),
    ("PUT", "/api/synthetic/calibration/feeds/:id/pin"): ("PUT", "/api/synthetic/calibration/feeds/{id}/pin"),
    ("DELETE", "/api/synthetic/calibration/feeds/:id/pin"): ("DELETE", "/api/synthetic/calibration/feeds/{id}/pin"),
    ("GET", "/api/synthetic/market-regime"): ("GET", "/api/synthetic/market-regime"),
    ("POST", "/api/synthetic/market-regime"): ("POST", "/api/synthetic/market-regime"),
    ("PUT", "/api/synthetic/market-regime/settings"): ("PUT", "/api/synthetic/market-regime/settings"),
    ("GET", "/api/synthetic/market-history"): ("GET", "/api/synthetic/market-history"),
    ("GET", "/api/synthetic/feed-folders"): ("GET", "/api/synthetic/feed-folders"),
    ("GET", "/api/synthetic/market-trace"): ("GET", "/api/synthetic/market-trace"),
    ("GET", "/api/synthetic/calibration/:assetClass"): ("GET", "/api/synthetic/calibration/{assetClass}"),
    ("GET", "/api/tenors/catalogue"): ("GET", "/api/tenors/catalogue"),
    ("GET", "/api/image-map"): ("GET", "/api/image-map"),

    # trading
    ("GET", "/api/trades"): ("GET", "/api/trades"),
    ("GET", "/api/trades/:id"): ("GET", "/api/trades/{id}"),
    ("GET", "/api/trades/:id/versions/:version"): ("GET", "/api/trades/{id}/versions/{version}"),
    ("GET", "/api/trades/new"): ("GET", "/api/trades/new"),
    ("POST", "/api/trades"): ("POST", "/api/trades"),
    ("POST", "/api/trades/:id/links"): ("POST", "/api/trades/{id}/links"),
    ("GET", "/api/trade-link-types"): ("GET", "/api/trade-link-types"),
    ("GET", "/api/draft-reasons"): ("GET", "/api/draft-reasons"),
}

# A BFF route that takes the resource as a parameter is answered in ores.http by
# one generated route per resource, at the address the BFF would have matched.
# Each family names the registry the BFF reads its resources from, the C++
# address with {resource} in place of the resource, and which resources must
# answer it: every resource, those read by one key, those read by a parent, or
# those the registry marks writable. The inventory reads the registry and fails
# when a resource that must answer a family has no route at its address.
REFDATA_RECORDS = "projects/ores.web/packages/wire-protocol/src/records.ts:REFDATA_RECORDS"
SYNTHETIC_RECORDS = "projects/ores.web/packages/bff/src/synthetic.ts:SYNTHETIC_RECORDS"

FAMILIES = {
    ("GET", "/api/refdata/:resource"): (REFDATA_RECORDS, "GET", "/api/refdata/{resource}", "every"),
    ("GET", "/api/refdata/:resource/key/:key"): (REFDATA_RECORDS, "GET", "/api/refdata/{resource}/key/{key}", "one_key"),
    ("GET", "/api/refdata/:resource/by/:parent"): (REFDATA_RECORDS, "GET", "/api/refdata/{resource}/by/{parent}", "list_by"),
    ("PUT", "/api/refdata/:resource"): (REFDATA_RECORDS, "PUT", "/api/refdata/{resource}", "writable"),
    ("DELETE", "/api/refdata/:resource"): (REFDATA_RECORDS, "DELETE", "/api/refdata/{resource}", "writable"),
    ("GET", "/api/synthetic/:resource"): (SYNTHETIC_RECORDS, "GET", "/api/synthetic/{resource}", "every"),
    ("GET", "/api/synthetic/:resource/key/:key"): (SYNTHETIC_RECORDS, "GET", "/api/synthetic/{resource}/key/{key}", "one_key"),
    ("GET", "/api/synthetic/:resource/by/:parent"): (SYNTHETIC_RECORDS, "GET", "/api/synthetic/{resource}/by/{parent}", "list_by"),
    ("PUT", "/api/synthetic/:resource"): (SYNTHETIC_RECORDS, "PUT", "/api/synthetic/{resource}", "writable"),
    ("DELETE", "/api/synthetic/:resource"): (SYNTHETIC_RECORDS, "DELETE", "/api/synthetic/{resource}", "writable"),
}

