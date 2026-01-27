---
name: ores-http-recipes
description: Update HTTP recipes by comparing routes to the recipe documentation.
license: Complete terms in LICENSE.txt
---


# When to use this skill

Use this skill when:

-   The user wants to update the ORE Studio HTTP recipes documentation
-   New HTTP endpoints have been added and need to be documented
-   Existing HTTP routes have changed and recipes need updating
-   API request/response formats have been modified and need verification


# How to use this skill

1.  Compare the routes in `projects/ores.http.server/src/routes` to `doc/recipes/http_recipes.org`.
2.  Identify any missing endpoints that are not documented.
3.  Add recipes for missing endpoints following the existing pattern.
4.  Use the Detailed Instructions section for the proper recipe format.


# Detailed instructions


## Recipe file location

The HTTP recipes are stored in `doc/recipes/http_recipes.org`. This file demonstrates all functionality available in the ORE Studio HTTP API using Emacs [verb mode](https://github.com/federicotdn/verb).


## Route source locations

Routes are implemented in the following files:

-   `projects/ores.http.server/src/routes/iam_routes.cpp`: Authentication, accounts, RBAC, sessions
-   `projects/ores.http.server/src/routes/risk_routes.cpp`: Currencies
-   `projects/ores.http.server/src/routes/variability_routes.cpp`: Feature flags
-   `projects/ores.http.server/src/routes/assets_routes.cpp`: Get images by ID

New entity routes should be added following the [http-entity-creator](../http-entity-creator/SKILL.md) skill.


## Recipe format

Each recipe follows this pattern using Emacs verb mode syntax:

\#+begin\_src fundamental


### Recipe Name

Description of what this recipe demonstrates.

```verb
METHOD /path
Accept: application/json
Content-Type: application/json
Authorization: Bearer {{(verb-var token)}}

{
    "field": "value"
}
```

\#+end\_src

Key elements:

-   Use `verb` as the source block language
-   Add `:wrap src json` to format JSON responses
-   Include `Accept: application/json` header for all requests
-   Include `Content-Type: application/json` for requests with bodies
-   Use `Authorization: Bearer {{(verb-var token)}}` for authenticated endpoints


## Recipe sections

Recipes are organized into sections:

-   **Authentication**: Bootstrap status, bootstrap, login, signup, logout
-   **Accounts**: List, create, update, delete, history, login info, lock, unlock, reset password
-   **Current User**: Change password, update email
-   **RBAC**: List roles, get role, list permissions, account roles/permissions, assign/revoke roles
-   **Sessions**: List sessions, statistics, active sessions
-   **Currencies**: List, save, delete, history
-   **Feature Flags**: List feature flags
-   **Assets**: Get images by ID


## Verifying routes

When checking routes, compare the endpoint definitions in the route files with the recipes. Common issues include:

-   Missing endpoints not documented
-   Changed URL paths (e.g., `/accounts` vs `/users`)
-   Modified HTTP methods (GET vs POST)
-   Updated request body fields
-   Changed response formats
-   New query parameters (offset, limit for pagination)


## Adding a new recipe

1.  Identify which section the endpoint belongs to.
2.  Check the route file for the exact URL path and HTTP method.
3.  Create a verb src block with the HTTP request.
4.  Include appropriate headers (Accept, Content-Type, Authorization).
5.  For endpoints with request bodies, include a JSON example.
6.  Add a descriptive paragraph explaining what the recipe demonstrates.
7.  Run the request to capture a sample response in the `#+RESULTS:` block.


## Recipe naming conventions

-   Use title case for recipe names: `List Accounts`, `Create Currency`
-   Be descriptive: `Get Account History` not just `History`
-   Group related endpoints: `Lock Accounts`, `Unlock Accounts`


## Template variables

The recipes use verb-mode variables for dynamic values. Set these using `M-x verb-set-var` before running authenticated requests:

| Variable    | Description              | Example                              |
|----------- |------------------------ |------------------------------------ |
| token       | JWT authentication token | eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9 |
| account\_id | UUID of an account       | 12345678-1234-1234-1234-123456789abc |
| role\_id    | UUID of a role           | 87654321-4321-4321-4321-cba987654321 |
| image\_id   | UUID of an image         | abcdef12-3456-7890-abcd-ef1234567890 |


## Running recipes

To use the HTTP recipes:

1.  Ensure `verb` package is installed in Emacs
2.  Start the HTTP server: `ores.http.server`
3.  Position cursor on a request and run `C-c C-r C-r` (`verb-send-request-on-point`)

All requests inherit from the base URL template defined at the top of the requests section: `template http://localhost:8081/api/v1`
