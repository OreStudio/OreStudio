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
TENANCY = "/api/v1/tenancy"
REFDATA = "/api/v1/reference-data"

PAIRS = {
    # access
    ("GET", "/api/accounts"): ("GET", "/api/accounts"),
    ("GET", "/api/accounts/:username"): ("GET", "/api/accounts/{username}"),
    ("GET", "/api/accounts/:accountId/contact-information"):
        ("GET", f"{ACCESS}/account_contact_informations/by-account-id/{{account_id}}"),
    ("PUT", "/api/accounts/:accountId/contact-information"):
        ("PUT", f"{ACCESS}/account_contact_informations"),
    ("POST", "/api/accounts/:accountId/lock"): ("POST", "/api/accounts/{accountId}/lock"),
    ("POST", "/api/accounts/:accountId/unlock"): ("POST", "/api/accounts/{accountId}/unlock"),
    ("POST", "/api/me/default-party"): ("POST", "/api/me/default-party"),
    ("PUT", "/api/roles/:roleId"): ("PUT", "/api/roles/{roleId}"),
    ("POST", "/api/roles/:roleId/permissions/changes"): ("POST", "/api/roles/{roleId}/permissions/changes"),
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
    ("GET", "/api/login-info"): ("GET", f"{AUTH}/login_info"),
    ("GET", "/api/login-info/:accountId"): ("GET", f"{AUTH}/login_info/{{account_id}}"),
    ("POST", "/api/account/password"): ("POST", f"{IAM_OPS}/change_password"),
    ("POST", "/api/signup"): ("POST", f"{IAM_OPS}/signup"),
    ("GET", "/api/sessions"): ("GET", f"{ACCESS}/sessions"),
    ("GET", "/api/sessions/active"): ("POST", f"{IAM_OPS}/get_active_sessions"),
    ("POST", "/api/sessions/:sessionId/end"): ("POST", f"{IAM_OPS}/end_session"),
    ("GET", "/api/session-statistics"): ("POST", f"{IAM_OPS}/get_session_statistics"),
    ("GET", "/api/auth-events"): ("POST", "/api/v1/iam/auth_events/list"),
    ("GET", "/api/geo/country"): ("POST", f"{IAM_OPS}/lookup_country"),

    # assets
    ("GET", "/api/images"): ("GET", "/api/v1/assets/images"),
    ("POST", "/api/images"): ("POST", "/api/v1/assets/images"),

    # changes
    ("GET", "/api/events"): ("GET", "/api/events"),
    ("POST", "/api/events/watch"): ("POST", "/api/events/watch"),

    # tenancy
    ("GET", "/api/tenants"): ("GET", f"{TENANCY}/tenants"),
    ("GET", "/api/tenants/:code"): ("GET", f"{TENANCY}/tenants/{{code}}"),
    ("DELETE", "/api/tenants/:code"): ("DELETE", f"{TENANCY}/tenants/{{code}}"),
    ("GET", "/api/tenant-statuses"): ("GET", f"{TENANCY}/tenant_statuses"),
    ("GET", "/api/tenant-types"): ("GET", f"{TENANCY}/tenant_types"),
    ("GET", "/api/seed-profiles"): ("GET", f"{TENANCY}/seed_profiles"),
    ("POST", "/api/bootstrap/administrator"): ("POST", f"{IAM_OPS}/create_initial_admin"),
    ("GET", "/api/parties"): ("GET", f"{REFDATA}/parties"),

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
