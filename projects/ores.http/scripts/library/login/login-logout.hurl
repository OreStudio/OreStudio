# =POST /api/v1/iam/auth/logout=
#
# GENERATED from doc/recipes/http/login/login.org — do not edit by hand.
# Regenerate with: ./compass.sh build --direct tangle_http_recipes
#
POST {{base_url}}/api/v1/iam/auth/logout
Accept: application/json
Authorization: Bearer {{token}}
HTTP 200
