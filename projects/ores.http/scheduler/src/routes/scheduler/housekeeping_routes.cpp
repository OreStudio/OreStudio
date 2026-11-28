/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.http/routes/scheduler/housekeeping_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.logging/logger.hpp"
#include "ores.scheduler.api/messaging/retention_policy_protocol.hpp"
#include "ores.scheduler.api/messaging/scheduling_operations_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/codec/text_form.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <chrono>
#include <cstdint>
#include <string>
#include <vector>

namespace ores::http::routes::scheduler {

namespace {

namespace forwarding = ores::http::forwarding;
namespace scheduling = ores::scheduler::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::logging::severity_level;
using ores::nats::service::nats_client;

auto& housekeeping_lg() {
    static auto instance = ores::logging::make_logger("ores.http.scheduler.housekeeping_routes");
    return instance;
}

/**
 * @brief The list the screen reads: every store, and the instant the read ran.
 *
 * The moment is the gateway's clock, so a screen that says how long ago a store
 * was measured is stating the deployment's age rather than the browser's.
 */
struct store_bounds_body {
    std::string read_at;
    std::vector<scheduling::store_bound> rows;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<store_bounds_body>) {
    return ores::utility::codec::field_list{
        ores::utility::codec::field{"read_at", &store_bounds_body::read_at},
        ores::utility::codec::field{"rows", &store_bounds_body::rows}};
}
static_assert(ores::utility::codec::names_every_member<store_bounds_body>());

/**
 * @brief One store's series, and the window the read covered.
 *
 * The window is what the plot draws its axis from, so it travels with the
 * points: a screen that computed the horizon from its own clock would state the
 * browser's time and not the installation's.
 */
struct store_samples_body {
    std::string store_code;
    std::vector<scheduling::store_sample> samples;
    std::string from_at;
    std::string to_at;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<store_samples_body>) {
    return ores::utility::codec::field_list{
        ores::utility::codec::field{"store_code", &store_samples_body::store_code},
        ores::utility::codec::field{"samples", &store_samples_body::samples},
        ores::utility::codec::field{"from_at", &store_samples_body::from_at},
        ores::utility::codec::field{"to_at", &store_samples_body::to_at}};
}
static_assert(ores::utility::codec::names_every_member<store_samples_body>());

/** What a person decided about a bound, beside the reason for it. */
struct store_bound_write {
    bool enabled = false;
    int retention_days = 0;
    int cadence_hours = 0;
    std::string reason_code;
    std::string commentary;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<store_bound_write>) {
    return ores::utility::codec::field_list{
        ores::utility::codec::field{"enabled", &store_bound_write::enabled},
        ores::utility::codec::field{"retention_days", &store_bound_write::retention_days},
        ores::utility::codec::field{"cadence_hours", &store_bound_write::cadence_hours},
        ores::utility::codec::field{"reason_code", &store_bound_write::reason_code},
        ores::utility::codec::field{"commentary", &store_bound_write::commentary}};
}
static_assert(ores::utility::codec::names_every_member<store_bound_write>());

struct store_bound_answer {
    bool success = true;
    std::string message;
    std::string outcome = "ok";
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<store_bound_answer>) {
    return ores::utility::codec::field_list{
        ores::utility::codec::field{"success", &store_bound_answer::success},
        ores::utility::codec::field{"message", &store_bound_answer::message},
        ores::utility::codec::field{"outcome", &store_bound_answer::outcome}};
}
static_assert(ores::utility::codec::names_every_member<store_bound_answer>());

std::string now_on_the_wire() {
    return ores::utility::codec::text_form<std::chrono::system_clock::time_point>::to_text(
        std::chrono::system_clock::now());
}

std::string store_code_of(const http_request& req) {
    return req.get_path_param("code");
}

/**
 * @brief The store the address names, from the catalogue the read answers.
 *
 * A write names a store, and the catalogue states its target, its action and its
 * words, so the write is composed from the catalogue and not from the request: a
 * form that sent those back could retarget a bound.
 */
const scheduling::store_bound* find_store(const std::vector<scheduling::store_bound>& stores,
                                          const std::string& code) {
    for (const auto& store : stores) {
        if (store.code == code)
            return &store;
    }
    return nullptr;
}


}

void housekeeping_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    ORES_LOG_SEV(housekeeping_lg(), severity_level::debug) << "Registering the housekeeping routes";

    auto stores =
        router->get("/api/operations/housekeeping/stores")
            .summary("Every store the installation keeps, and whether it is bounded")
            .description("Forwards to scheduler.v1.store_bounds.list: each store with "
                         "the bound the catalogue states, the last run and its size.")
            .tags({"housekeeping"})
            .operation_catalogue("scheduler", "store_bounds", "store_bounds_read")
            .auth_required()
            .handler([&session](const http_request& req) { return handle_stores(req, session); });
    const auto stores_route = stores.build();
    router->add_route(stores_route);
    registry->register_route(stores_route);

    auto samples =
        router->get("/api/operations/housekeeping/stores/{code}/samples")
            .summary("One store's measurements, oldest first")
            .description("Forwards to scheduler.v1.store_samples.list, which answers "
                         "the points the screen plots.")
            .tags({"housekeeping"})
            .operation_catalogue("scheduler", "store_samples", "store_samples_read")
            .auth_required()
            .query_param(
                "range", "string", "24h", false, "How far back the series reaches: 1h, 24h or 7d")
            .handler([&session](const http_request& req) { return handle_samples(req, session); });
    const auto samples_route = samples.build();
    router->add_route(samples_route);
    registry->register_route(samples_route);

    auto policy =
        router->put("/api/operations/housekeeping/stores/{code}/policy")
            .summary("Change a store's bound: whether it applies, its period and cadence")
            .description("Composes the policy from the catalogue and forwards the write "
                         "to scheduler.v1.retention_policies.put.")
            .tags({"housekeeping"})
            .operation_catalogue("scheduler", "retention_policies", "retention_policies_put")
            .auth_required()
            .body<store_bound_write>()
            .handler([&session](const http_request& req) { return handle_policy(req, session); });
    const auto policy_route = policy.build();
    router->add_route(policy_route);
    registry->register_route(policy_route);
}

boost::asio::awaitable<http_response> housekeeping_routes::handle_stores(const http_request& req,
                                                                         nats_client& session) {
    try {
        const auto reply =
            forwarding::forward(req, session, scheduling::list_store_bounds_request{});
        if (!reply.success) {
            co_return http_response::error(http_status::bad_gateway, reply.message);
        }
        co_return http_response::json(ores::utility::codec::object::write_json(
            store_bounds_body{now_on_the_wire(), reply.stores}));
    } catch (const std::exception& e) {
        co_return http_response::error(http_status::bad_request, e.what());
    }
}

boost::asio::awaitable<http_response> housekeeping_routes::handle_samples(const http_request& req,
                                                                          nats_client& session) {
    try {
        scheduling::list_store_samples_request msg;
        msg.store_code = store_code_of(req);
        const auto range = req.get_query_param("range");
        if (!range.empty())
            msg.range = range;

        const auto reply = forwarding::forward(req, session, msg);
        if (!reply.success) {
            co_return http_response::error(http_status::bad_gateway, reply.message);
        }
        co_return http_response::json(ores::utility::codec::object::write_json(
            store_samples_body{msg.store_code, reply.samples, reply.from_at, reply.to_at}));
    } catch (const std::exception& e) {
        co_return http_response::error(http_status::bad_request, e.what());
    }
}

boost::asio::awaitable<http_response> housekeeping_routes::handle_policy(const http_request& req,
                                                                         nats_client& session) {
    try {
        const auto body = ores::utility::codec::object::read_json<store_bound_write>(req.body);

        const auto bounds =
            forwarding::forward(req, session, scheduling::list_store_bounds_request{});
        if (!bounds.success) {
            co_return http_response::error(http_status::bad_gateway, bounds.message);
        }
        const auto* store = find_store(bounds.stores, store_code_of(req));
        if (store == nullptr) {
            co_return http_response::not_found("This installation keeps no store called " +
                                               store_code_of(req) + ".");
        }
        if (!store->bound.action.has_value() || store->bound.action->empty()) {
            co_return http_response::bad_request("The catalogue states no action that bounds " +
                                                 store->code + ".");
        }

        scheduling::put_retention_policy_request msg;
        auto& write = msg.change.write;
        write.code = store->bound.policy_code.value_or(store->code);
        write.target_kind = store->target_kind;
        write.target = store->target;
        write.store_code = store->code;
        write.action = *store->bound.action;
        write.retention_days = body.retention_days;
        write.cadence_hours = body.cadence_hours;
        write.enabled = body.enabled;
        write.description = store->bound.policy_description.value_or("");

        if (store->bound.policy_version > 0) {
            msg.change.precondition.kind =
                ores::utility::domain::precondition_kind::must_match_version;
            msg.change.precondition.version =
                static_cast<std::uint32_t>(store->bound.policy_version);
        }
        msg.intent.reason_code = body.reason_code;
        msg.intent.commentary = body.commentary;

        const auto reply = forwarding::forward(req, session, msg);
        if (reply.result.outcome != ores::utility::domain::outcome::ok) {
            co_return http_response::bad_request(reply.result.message);
        }
        co_return http_response::json(
            ores::utility::codec::object::write_json(store_bound_answer{}));
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

}
