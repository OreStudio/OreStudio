---
name: http-entity-creator
description: Create HTTP REST API endpoints for domain entities in ores.http.server including CRUD operations and history.
license: Complete terms in LICENSE.txt
---


# When to use this skill

When you need to add HTTP REST API endpoints for a new entity in `ores.http.server`. This skill guides you through creating the routes class, registering it with the application, and implementing standard CRUD operations following established patterns.

Prerequisites:

-   The domain type must already exist (see [domain-type-creator](../domain-type-creator/SKILL.md) skill)
-   The messaging protocol for CRUD operations must exist (see [binary-protocol-developer](../binary-protocol-developer/SKILL.md) skill)
-   The service layer must be implemented with repository support
-   JSON serialization via rfl must be available for request/response types


# How to use this skill

1.  Gather entity requirements (name, domain, operations needed, authorization).
2.  Follow the detailed instructions to create routes in phases.
3.  Each phase ends with a PR checkpoint - raise PR, wait for review, merge.
4.  Create a fresh branch from main for the next phase (see [feature-branch-manager](../feature-branch-manager/SKILL.md)).
5.  Build and test after each step.


# PR Strategy

This skill is structured into **three phases**, each resulting in a separate PR.

| Phase | Steps     | PR Title Template                             |
|----- |--------- |--------------------------------------------- |
| 1     | Steps 1-3 | `[http] Add <entity> list and save endpoints` |
| 2     | Steps 4-5 | `[http] Add <entity> delete and history`      |
| 3     | Step 6    | `[doc] Add <entity> HTTP endpoint recipes`    |

After each PR is merged, use [feature-branch-manager](../feature-branch-manager/SKILL.md) to transition to the next phase.


# Detailed instructions

The following sections describe the step-by-step process for creating HTTP endpoints for an entity.


## Gather Requirements

Before starting, gather the following information:

-   **Entity name**: The name of the entity (e.g., `currency`, `country`, `account`).
-   **Domain location**: Which domain the entity belongs to (e.g., `ores.refdata`, `ores.iam`).
-   **Routes class**: The routes file to add to (e.g., `risk_routes`, `iam_routes`) or create new.
-   **API prefix**: The URL prefix (typically `/api/v1/<entities>`).
-   **Operations needed**:
    -   [ ] List/get all entities (GET with pagination)
    -   [ ] Save/create entity (POST)
    -   [ ] Delete entities (DELETE batch)
    -   [ ] Get entity history (GET by ID)
    -   [ ] Get single entity (GET by ID)
-   **Authorization**:
    -   Which operations require authentication (`.auth_required()`)
    -   Which operations require specific roles (`.roles({"Admin"})`)
-   **OpenAPI metadata**: Tags, summaries, and descriptions for documentation.


# Phase 1: List and Save Endpoints

This phase creates the routes class with basic retrieval and save operations. After completing Steps 1-3, raise a PR.

**Suggested PR title:** `[http] Add <entity> list and save endpoints`


## Step 1: Create or Update Header File

Add handler declarations to the routes class header.


### File location

For new routes class: `projects/ores.http.server/include/ores.http.server/routes/<domain>_routes.hpp`

For existing routes class, add to the appropriate header.


### Header structure (new routes class)

```cpp
/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * licensing text...
 */
#ifndef ORES_HTTP_SERVER_ROUTES_<DOMAIN>_ROUTES_HPP
#define ORES_HTTP_SERVER_ROUTES_<DOMAIN>_ROUTES_HPP

#include <memory>
#include <boost/asio/awaitable.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.http/domain/http_request.hpp"
#include "ores.http/domain/http_response.hpp"

namespace ores::http::net {

class router;

}

namespace ores::http::openapi {

class endpoint_registry;

}

namespace ores::comms::service {

class auth_session_service;

}

namespace ores::http::server::routes {

/**
 * @brief HTTP routes for <domain> entities.
 */
class <domain>_routes final {
public:
    <domain>_routes(database::context ctx,
        std::shared_ptr<comms::service::auth_session_service> sessions);

    void register_routes(std::shared_ptr<http::net::router> router,
        std::shared_ptr<http::openapi::endpoint_registry> registry);

private:
    inline static std::string_view logger_name =
        "ores.http.server.routes.<domain>_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    // <Entity> handlers
    boost::asio::awaitable<http::domain::http_response>
    handle_get_<entities>(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_save_<entity>(const http::domain::http_request& req);

    database::context ctx_;
    std::shared_ptr<comms::service::auth_session_service> sessions_;
};

}

#endif
```


### Header additions (existing routes class)

Add to the private section:

```cpp
// <Entity> handlers
boost::asio::awaitable<http::domain::http_response>
handle_get_<entities>(const http::domain::http_request& req);

boost::asio::awaitable<http::domain::http_response>
handle_save_<entity>(const http::domain::http_request& req);
```


### Key patterns

-   Class is `final` and non-copyable
-   Logger uses inline static `logger_name` with fully qualified path
-   All handlers return `boost::asio::awaitable<http::domain::http_response>`
-   Handlers take `const http::domain::http_request&`
-   Constructor takes `database::context` and shared services


### Commit message

```
[http] Add <entity> handler declarations to <domain>_routes

Declare handle_get_<entities> and handle_save_<entity> methods.
```


## Step 2: Create or Update Implementation File

Implement the route registration and handler functions.


### File location

`projects/ores.http.server/src/routes/<domain>_routes.cpp`


### Implementation structure (new routes class)

```cpp
/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * licensing text...
 */
#include "ores.http.server/routes/<domain>_routes.hpp"

#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.http/net/router.hpp"
#include "ores.http/openapi/endpoint_registry.hpp"
#include "ores.<domain>/messaging/<entity>_protocol.hpp"
#include "ores.<domain>/service/<entity>_service.hpp"

namespace ores::http::server::routes {

using namespace logging;
using http::domain::http_request;
using http::domain::http_response;
namespace asio = boost::asio;

<domain>_routes::<domain>_routes(database::context ctx,
    std::shared_ptr<comms::service::auth_session_service> sessions)
    : ctx_(std::move(ctx)), sessions_(std::move(sessions)) {}

void <domain>_routes::
register_routes(std::shared_ptr<http::net::router> router,
    std::shared_ptr<http::openapi::endpoint_registry> registry) {
    using namespace <domain>::messaging;

    // GET /api/v1/<entities> - List all <entities>
    {
        auto route_builder = router->get("/api/v1/<entities>")
            .summary("Get <entities>")
            .description("Retrieve <entities> with pagination")
            .tags({"<entities>"})
            .auth_required()
            .query_param("offset", "integer", "", false, "Pagination offset", "0")
            .query_param("limit", "integer", "", false, "Maximum results", "100")
            .response<get_<entities>_response>()
            .handler([this](const http_request& req) {
                return handle_get_<entities>(req);
            });
        const auto route = route_builder.build();
        router->add_route(route);
        registry->register_route(route);
    }

    // POST /api/v1/<entities> - Save <entity>
    {
        auto route_builder = router->post("/api/v1/<entities>")
            .summary("Save <entity>")
            .description("Create or update a <entity>")
            .tags({"<entities>"})
            .auth_required()
            .roles({"Admin"})
            .body<save_<entity>_request>()
            .response<save_<entity>_response>()
            .handler([this](const http_request& req) {
                return handle_save_<entity>(req);
            });
        const auto route = route_builder.build();
        router->add_route(route);
        registry->register_route(route);
    }
}

// Handler implementations follow...

}
```


### Handler patterns

-   Pattern A: List with pagination

    ```cpp
    asio::awaitable<http_response> <domain>_routes::
    handle_get_<entities>(const http_request& req) {
        BOOST_LOG_SEV(lg(), debug) << "Handling get <entities> request";
    
        try {
            // Parse pagination parameters
            std::uint32_t offset = 0;
            std::uint32_t limit = 100;
            auto offset_str = req.get_query_param("offset");
            auto limit_str = req.get_query_param("limit");
            if (!offset_str.empty()) offset = std::stoul(offset_str);
            if (!limit_str.empty()) limit = std::stoul(limit_str);
    
            // Call service layer
            <domain>::service::<entity>_service service(ctx_);
            auto <entities> = service.list_<entities>(offset, limit);
            auto total = service.count_<entities>();
    
            // Build response
            <domain>::messaging::get_<entities>_response resp;
            resp.<entities> = <entities>;
            resp.total_available_count = total;
    
            BOOST_LOG_SEV(lg(), info) << "Retrieved " << <entities>.size()
                                      << " <entities>";
            co_return http_response::json(rfl::json::write(resp));
        } catch (const std::invalid_argument& e) {
            BOOST_LOG_SEV(lg(), warn) << "Invalid pagination parameter: " << e.what();
            co_return http_response::bad_request("Invalid pagination parameter");
        } catch (const std::out_of_range& e) {
            BOOST_LOG_SEV(lg(), warn) << "Pagination parameter out of range: " << e.what();
            co_return http_response::bad_request("Pagination parameter out of range");
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Get <entities> error: " << e.what();
            co_return http_response::internal_error(e.what());
        }
    }
    ```

-   Pattern B: Save (create/update)

    ```cpp
    asio::awaitable<http_response> <domain>_routes::
    handle_save_<entity>(const http_request& req) {
        BOOST_LOG_SEV(lg(), debug) << "Handling save <entity> request";
    
        try {
            // Parse request body
            auto body = req.body();
            auto request = rfl::json::read<<domain>::messaging::save_<entity>_request>(body);
            if (!request) {
                BOOST_LOG_SEV(lg(), warn) << "Invalid request body";
                co_return http_response::bad_request("Invalid request body");
            }
    
            // Call service layer
            <domain>::service::<entity>_service service(ctx_);
            service.save_<entity>(request->value().<entity>);
    
            // Build response
            <domain>::messaging::save_<entity>_response resp;
            resp.success = true;
            resp.message = "<Entity> saved successfully";
    
            BOOST_LOG_SEV(lg(), info) << "Saved <entity>";
            co_return http_response::json(rfl::json::write(resp));
        } catch (const rfl::json::JsonException& e) {
            BOOST_LOG_SEV(lg(), warn) << "Invalid JSON in request body: " << e.what();
            co_return http_response::bad_request("Invalid JSON in request body");
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Save <entity> error: " << e.what();
            co_return http_response::internal_error(e.what());
        }
    }
    ```

-   Pattern C: Get single by ID (path parameter)

    ```cpp
    asio::awaitable<http_response> <domain>_routes::
    handle_get_<entity>(const http_request& req) {
        BOOST_LOG_SEV(lg(), debug) << "Handling get <entity> request";
    
        try {
            // Extract path parameter
            auto <entity>_id = req.get_path_param("<entity>_id");
            if (<entity>_id.empty()) {
                co_return http_response::bad_request("Missing <entity> ID");
            }
    
            // Call service layer
            <domain>::service::<entity>_service service(ctx_);
            auto <entity> = service.get_<entity>(<entity>_id);
    
            if (!<entity>) {
                co_return http_response::not_found("<Entity> not found");
            }
    
            // Build response
            <domain>::messaging::get_<entity>_response resp;
            resp.<entity> = *<entity>;
    
            co_return http_response::json(rfl::json::write(resp));
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Get <entity> error: " << e.what();
            co_return http_response::internal_error(e.what());
        }
    }
    ```


### Commit message

```
[http] Implement <entity> list and save endpoints

Add handle_get_<entities> for listing with pagination and
handle_save_<entity> for creating/updating <entities>.
```


## Step 3: Register in Application

Register the routes class in the application setup.


### File to modify

`projects/ores.http.server/src/app/application.cpp`


### Changes required

1.  Add include at top of file:

```cpp
#include "ores.http.server/routes/<domain>_routes.hpp"
```

1.  Add registration in the appropriate setup method:

```cpp
// Register <Domain> routes
routes::<domain>_routes <domain>(ctx, sessions);
<domain>.register_routes(router, registry);
```


### Commit message

```
[http] Register <domain>_routes in application

Add <entity> endpoints to HTTP server.
```


## Phase 1 Checkpoint: Raise PR

At this point:

1.  Build and verify: `cmake --build --preset linux-clang-debug`
2.  Test endpoints using curl or HTTP client.
3.  Verify OpenAPI documentation is generated.
4.  Commit all changes.
5.  Push branch and raise PR.

**PR Title:** `[http] Add <entity> list and save endpoints`

**PR Description:**

```
## Summary

- Add <entity> endpoints to <domain>_routes
- GET /api/v1/<entities> - List with pagination
- POST /api/v1/<entities> - Create/update <entity>
- OpenAPI documentation generated automatically
```

Wait for review feedback and merge before continuing to Phase 2.


# Phase 2: Delete and History Endpoints

After Phase 1 PR is merged, use [feature-branch-manager](../feature-branch-manager/SKILL.md) to transition to Phase 2.

**Suggested PR title:** `[http] Add <entity> delete and history endpoints`


## Step 4: Add Delete Endpoint

Add batch delete functionality.


### Header additions

Add to the routes class header:

```cpp
boost::asio::awaitable<http::domain::http_response>
handle_delete_<entities>(const http::domain::http_request& req);
```


### Route registration

Add to `register_routes()`:

```cpp
// DELETE /api/v1/<entities> - Delete <entities>
{
    auto route_builder = router->delete_("/api/v1/<entities>")
        .summary("Delete <entities>")
        .description("Delete one or more <entities>")
        .tags({"<entities>"})
        .auth_required()
        .roles({"Admin"})
        .body<delete_<entity>_request>()
        .response<delete_<entity>_response>()
        .handler([this](const http_request& req) {
            return handle_delete_<entities>(req);
        });
    const auto route = route_builder.build();
    router->add_route(route);
    registry->register_route(route);
}
```


### Handler implementation

```cpp
asio::awaitable<http_response> <domain>_routes::
handle_delete_<entities>(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling delete <entities> request";

    try {
        auto body = req.body();
        auto request = rfl::json::read<<domain>::messaging::delete_<entity>_request>(body);
        if (!request) {
            co_return http_response::bad_request("Invalid request body");
        }

        <domain>::service::<entity>_service service(ctx_);
        <domain>::messaging::delete_<entity>_response resp;

        for (const auto& id : request->value().<entity>_ids) {
            <domain>::messaging::delete_<entity>_result result;
            result.<entity>_id = id;
            try {
                service.delete_<entity>(id);
                result.success = true;
                result.message = "Deleted successfully";
            } catch (const std::exception& e) {
                result.success = false;
                result.message = e.what();
            }
            resp.results.push_back(result);
        }

        co_return http_response::json(rfl::json::write(resp));
    } catch (const rfl::json::JsonException& e) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid JSON in request body: " << e.what();
        co_return http_response::bad_request("Invalid JSON in request body");
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Delete <entities> error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}
```


### Commit message

```
[http] Add <entity> delete endpoint

Implement batch delete with per-item results.
```


## Step 5: Add History Endpoint

Add version history retrieval.


### Header additions

```cpp
boost::asio::awaitable<http::domain::http_response>
handle_get_<entity>_history(const http::domain::http_request& req);
```


### Route registration

```cpp
// GET /api/v1/<entities>/{<entity>_id}/history - Get <entity> history
{
    auto route_builder = router->get("/api/v1/<entities>/{<entity>_id}/history")
        .summary("Get <entity> history")
        .description("Retrieve version history for a <entity>")
        .tags({"<entities>"})
        .auth_required()
        .response<get_<entity>_history_response>()
        .handler([this](const http_request& req) {
            return handle_get_<entity>_history(req);
        });
    const auto route = route_builder.build();
    router->add_route(route);
    registry->register_route(route);
}
```


### Handler implementation

```cpp
asio::awaitable<http_response> <domain>_routes::
handle_get_<entity>_history(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling get <entity> history request";

    try {
        auto <entity>_id = req.get_path_param("<entity>_id");
        if (<entity>_id.empty()) {
            co_return http_response::bad_request("Missing <entity> ID");
        }

        <domain>::service::<entity>_service service(ctx_);
        auto history = service.get_<entity>_history(<entity>_id);

        <domain>::messaging::get_<entity>_history_response resp;
        resp.history = history;

        BOOST_LOG_SEV(lg(), info) << "Retrieved " << history.size()
                                  << " history entries for " << <entity>_id;
        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Get <entity> history error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}
```


### Commit message

```
[http] Add <entity> history endpoint

Implement version history retrieval by <entity> ID.
```


## Phase 2 Checkpoint: Raise PR

At this point:

1.  Build and verify: `cmake --build --preset linux-clang-debug`
2.  Test delete and history endpoints.
3.  Commit all changes.
4.  Push branch and raise PR.

**PR Title:** `[http] Add <entity> delete and history endpoints`

**PR Description:**

```
## Summary

- DELETE /api/v1/<entities> - Batch delete with per-item results
- GET /api/v1/<entities>/{<entity>_id}/history - Version history retrieval
```

Wait for review feedback and merge before continuing to Phase 3.


# Phase 3: Recipe Documentation

After Phase 2 PR is merged, use [feature-branch-manager](../feature-branch-manager/SKILL.md) to transition to Phase 3.

**Suggested PR title:** `[doc] Add <entity> HTTP endpoint recipes`


## Step 6: Add HTTP Recipes

Document all endpoints in the HTTP recipes file.


### File to modify

`doc/recipes/http_recipes.org`


### Recipe structure

Add a section for the entity:

\#+begin\_src fundamental


## <Entities>

HTTP endpoints for <entity> management.


### List <Entities>

Retrieve <entities> with pagination.

```sh
curl -s -X GET "${base_url}/api/v1/<entities>?offset=0&limit=10" \
  -H "Authorization: Bearer ${jwt_token}" \
  -H "Content-Type: application/json" | jq .
```


### Save <Entity>

Create or update a <entity>.

```sh
curl -s -X POST "${base_url}/api/v1/<entities>" \
  -H "Authorization: Bearer ${jwt_token}" \
  -H "Content-Type: application/json" \
  -d '{
    "<entity>": {
      "field1": "value1",
      "field2": "value2"
    }
  }' | jq .
```


### Delete <Entities>

Delete one or more <entities>.

```sh
curl -s -X DELETE "${base_url}/api/v1/<entities>" \
  -H "Authorization: Bearer ${jwt_token}" \
  -H "Content-Type: application/json" \
  -d '{
    "<entity>_ids": ["id1", "id2"]
  }' | jq .
```


### <Entity> History

Get version history for a <entity>.

```sh
curl -s -X GET "${base_url}/api/v1/<entities>/<entity>_id/history" \
  -H "Authorization: Bearer ${jwt_token}" \
  -H "Content-Type: application/json" | jq .
```

\#+end\_src


### Commit message

```
[doc] Add <entity> HTTP endpoint recipes

Document list, save, delete, and history endpoints with curl examples.
```


## Phase 3 Checkpoint: Raise PR

At this point:

1.  Verify recipes are correctly formatted.
2.  Test curl commands work against running server.
3.  Commit all changes.
4.  Push branch and raise PR.

**PR Title:** `[doc] Add <entity> HTTP endpoint recipes`

**PR Description:**

```
## Summary

- Add HTTP recipes for <entity> endpoints
- Document list, save, delete, and history operations
- Include curl examples with JWT authentication
```


# Key conventions reference


## Route builder methods

| Method                   | Purpose                            |
|------------------------ |---------------------------------- |
| `.summary(string)`       | OpenAPI operation summary          |
| `.description(string)`   | OpenAPI operation description      |
| `.tags(vector<string>)`  | OpenAPI tags for grouping          |
| `.auth_required()`       | Require JWT authentication         |
| `.roles(vector<string>)` | Require specific roles             |
| `.query_param(...)`      | Define query parameter             |
| `.body<T>()`             | Request body schema (auto via rfl) |
| `.response<T>()`         | Response schema (auto via rfl)     |
| `.handler(lambda)`       | The handler function               |


## HTTP response methods

| Method                               | Status | Use Case                 |
|------------------------------------ |------ |------------------------ |
| `http_response::json(string)`        | 200    | Success with JSON body   |
| `http_response::bad_request(msg)`    | 400    | Invalid input            |
| `http_response::unauthorized(msg)`   | 401    | Missing/invalid auth     |
| `http_response::forbidden(msg)`      | 403    | Insufficient permissions |
| `http_response::not_found(msg)`      | 404    | Resource not found       |
| `http_response::internal_error(msg)` | 500    | Server error             |


## Logging levels

| Level | Use Case                              |
|----- |------------------------------------- |
| debug | Request received, parameter values    |
| info  | Successful completion with counts     |
| warn  | Handled errors (bad input, not found) |
| error | Unexpected exceptions                 |


## URL patterns

| Pattern                                    | Use Case                          |
|------------------------------------------ |--------------------------------- |
| `/api/v1/<entities>`                       | List (GET), Create (POST), Delete |
| `/api/v1/<entities>/{<entity>_id}`         | Get single, Update, Delete single |
| `/api/v1/<entities>/{<entity>_id}/history` | Version history                   |


## Related skills

-   [domain-type-creator](../domain-type-creator/SKILL.md) - For creating the underlying domain type
-   [binary-protocol-developer](../binary-protocol-developer/SKILL.md) - For creating messaging protocol
-   [feature-branch-manager](../feature-branch-manager/SKILL.md) - For transitioning between phases
-   [shell-entity-creator](../shell-entity-creator/SKILL.md) - Similar skill for shell commands
