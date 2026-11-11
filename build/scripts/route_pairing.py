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

IAM_OPS = "/api/v1/iam/ops"
ACCESS = "/api/v1/access"
AUTH = "/api/v1/auth"
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
    ("POST", "/api/session"): ("POST", f"{AUTH}/sessions"),
    ("DELETE", "/api/session"): ("DELETE", f"{AUTH}/sessions"),
    ("GET", "/api/session"): ("GET", f"{AUTH}/sessions/me"),
    ("GET", "/api/login-info"): ("GET", "/api/login-info"),
    ("GET", "/api/login-info/:accountId"): ("GET", "/api/login-info/{accountId}"),
    ("POST", "/api/account/password"): ("POST", f"{IAM_OPS}/change_password"),
    ("POST", "/api/signup"): ("POST", f"{IAM_OPS}/signup"),
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
}
