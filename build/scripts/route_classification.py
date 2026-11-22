"""The published API areas of ORE Studio's HTTP routes.

compass-ng's model_rules codec reads AREAS and NO_AREA from here: an entity's
=#+http_area:= must name one of AREAS. The rules, exemptions, views, pairs and
families that decide each route of the inventory are tables of
compass/checks/route_inventory.org, which the route_inventory codec reads.

AREAS is the published namespace of design section 11.2
(doc/plans/2026-10-28-single-http-server-design.org) and the areas the route
rules add beside it. A route that is not an API publishes no area, and states
NO_AREA instead.
"""

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
