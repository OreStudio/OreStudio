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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.http/routes/reporting/run_routes.hpp"
#include "ores.compute.api/messaging/batch_protocol.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/reporting/report_health.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/messaging/report_definition_protocol.hpp"
#include "ores.reporting.api/messaging/report_error_event_protocol.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/log/sources/record_ostream.hpp>
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::reporting {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace iam_messaging = ores::iam::messaging;
namespace reporting_domain = ores::reporting::domain;
namespace reporting_messaging = ores::reporting::messaging;
namespace compute_messaging = ores::compute::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/// The trail a read leaves when its answer is deliberately absent.
auto& run_lg() {
    static auto instance = ores::logging::make_logger("ores.http.reporting.run_routes");
    return instance;
}

/// The unit's helpers, named apart from the other units that build into one
/// translation unit with it.
namespace runs {

/// The most runs of one report a page asks for.
constexpr std::uint32_t runs_of_report_limit = 200;

using run_view = run_routes::run_view;

/// The time from a run's start to its completion, or nothing until both are
/// known and the completion does not precede the start.
std::optional<std::int64_t>
elapsed_ms(const std::optional<std::chrono::system_clock::time_point>& started,
           const std::optional<std::chrono::system_clock::time_point>& completed) {
    if (!started || !completed)
        return std::nullopt;
    const auto elapsed =
        std::chrono::duration_cast<std::chrono::milliseconds>(*completed - *started);
    if (elapsed.count() < 0)
        return std::nullopt;
    return elapsed.count();
}

/// One run as the list states it.
std::string run_json(const run_view& run) {
    return "{\"id\":" + json::write(rr::as_text(run.id)) + ",\"name\":" + json::write(run.name) +
           ",\"state_name\":" + json::write(run.state_name) +
           ",\"health\":" + json::write(run.health) +
           ",\"started_at\":" + json::write(run.started_at) +
           ",\"completed_at\":" + json::write(run.completed_at) +
           ",\"duration_ms\":" + json::write(elapsed_ms(run.started_at, run.completed_at)) +
           ",\"priced_at\":" + json::write(run.priced_at) + "}";
}

/// The runs of one definition, newest first, with the state each reached.
///
/// The health is read beside the runs, and a read of it that fails leaves every
/// state out without failing the list: a run whose state cannot be read is still
/// a run the person made.
std::vector<run_view> runs_of_definition(const http_request& req,
                                         nats_client& session,
                                         const boost::uuids::uuid& definition_id) {
    reporting_messaging::list_report_instances_request msg;
    msg.limit = runs_of_report_limit;
    msg.filter.emplace();
    msg.filter->definition_id = definition_id;
    const auto reply = forwarding::forward(req, session, msg);
    rr::require_done(reply.result);

    std::vector<run_view> runs;
    for (const auto& instance : reply.instances)
        runs.push_back({instance.id,
                        instance.name,
                        std::string(),
                        std::nullopt,
                        instance.started_at,
                        instance.completed_at,
                        instance.priced_at});
    std::ranges::stable_sort(runs, [](const auto& left, const auto& right) {
        if (!left.started_at || !right.started_at)
            return left.started_at.has_value() && !right.started_at.has_value();
        return *left.started_at > *right.started_at;
    });

    std::vector<boost::uuids::uuid> ids;
    for (const auto& run : runs)
        ids.push_back(run.id);
    if (ids.empty())
        return runs;

    try {
        reporting_messaging::get_report_instance_health_request stated;
        stated.instance_ids = ids;
        const auto health = forwarding::forward(req, session, stated);
        if (health.result.outcome == ores::utility::domain::outcome::ok) {
            for (auto& run : runs)
                for (const auto& entry : health.health)
                    if (entry.report_instance_id == run.id) {
                        run.state_name = entry.state_name;
                        run.health = report_health::word_of(entry.health);
                    }
        }
    } catch (const std::exception&) {
        // The runs are the person's own, and their state is a decoration of them.
    }
    return runs;
}


/// One run's page: what it is, the state it reached, its batch and its output.
struct run_detail final {
    reporting_domain::report_instance instance;
    std::string state_name;
    std::optional<std::string> health;
    std::optional<std::string> definition_name;
    std::optional<std::string> party_name;
    std::optional<std::string> batch_id;
};

/// One run, by the id that names it, or nothing when no run carries it.
std::optional<reporting_domain::report_instance>
run_by_id(const http_request& req, nats_client& session, const boost::uuids::uuid& id) {
    reporting_messaging::list_report_instances_request msg;
    msg.limit = 1;
    msg.filter.emplace();
    msg.filter->id_one_of = std::vector{id};
    const auto reply = forwarding::forward(req, session, msg);
    rr::require_done(reply.result);
    if (reply.instances.empty())
        return std::nullopt;
    return reply.instances.front();
}

/// The batch that names the run in its external reference, or nothing when the
/// session sees none.
std::optional<std::string>
batch_of(const http_request& req, nats_client& session, const std::string& id) {
    try {
        compute_messaging::list_batches_request msg;
        msg.limit = 5;
        msg.filter.emplace();
        msg.filter->search = id;
        const auto reply = forwarding::forward(req, session, msg);
        rr::require_done(reply.result);
        for (const auto& batch : reply.batches)
            if (batch.external_ref == id)
                return rr::as_text(batch.id);
    } catch (const rr::refusal& r) {
        BOOST_LOG_SEV(run_lg(), ores::logging::warn)
            << "The batch of the run was refused: " << r.message;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(run_lg(), ores::logging::warn)
            << "The batch of the run was not read: " << e.what();
    }
    return std::nullopt;
}

/// The name of the definition a run belongs to, or nothing when it cannot be read.
std::optional<std::string>
definition_name_of(const http_request& req, nats_client& session, const boost::uuids::uuid& id) {
    try {
        reporting_messaging::list_report_definitions_request msg;
        msg.limit = 1;
        msg.filter.emplace();
        msg.filter->id_one_of = std::vector{id};
        const auto reply = forwarding::forward(req, session, msg);
        rr::require_done(reply.result);
        if (!reply.definitions.empty())
            return reply.definitions.front().name;
    } catch (const rr::refusal& r) {
        BOOST_LOG_SEV(run_lg(), ores::logging::warn)
            << "The name of the report's definition was refused: " << r.message;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(run_lg(), ores::logging::warn)
            << "The name of the report's definition was not read: " << e.what();
    }
    return std::nullopt;
}

/// The name of a party the caller's own account works in, or nothing.
std::optional<std::string>
party_name_of(const http_request& req, nats_client& session, const boost::uuids::uuid& id) {
    try {
        const auto reply =
            forwarding::forward(req, session, iam_messaging::get_my_parties_request{});
        rr::require_done(reply.result);
        for (const auto& party : reply.parties)
            if (party.party_id == rr::as_text(id))
                return party.name;
    } catch (const rr::refusal& r) {
        BOOST_LOG_SEV(run_lg(), ores::logging::warn)
            << "The name of the run's party was refused: " << r.message;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(run_lg(), ores::logging::warn)
            << "The name of the run's party was not read: " << e.what();
    }
    return std::nullopt;
}

/// The state and health word of one run, or nothing when the read refuses.
std::optional<reporting_messaging::report_instance_health>
health_of_run(const http_request& req, nats_client& session, const boost::uuids::uuid& id) {
    try {
        reporting_messaging::get_report_instance_health_request stated;
        stated.instance_ids = std::vector{id};
        const auto reply = forwarding::forward(req, session, stated);
        rr::require_done(reply.result);
        for (const auto& entry : reply.health)
            if (entry.report_instance_id == id)
                return entry;
    } catch (const rr::refusal& r) {
        BOOST_LOG_SEV(run_lg(), ores::logging::warn)
            << "The state of the run was refused: " << r.message;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(run_lg(), ores::logging::warn)
            << "The state of the run was not read: " << e.what();
    }
    return std::nullopt;
}

/// One run's page as the screen reads it.
std::string run_detail_json(const run_detail& detail) {
    const auto& run = detail.instance;
    return "{\"id\":" + json::write(rr::as_text(run.id)) + ",\"name\":" + json::write(run.name) +
           ",\"description\":" + json::write(run.description) +
           ",\"definition_id\":" + json::write(rr::as_text(run.definition_id)) +
           ",\"definition_name\":" + json::write(detail.definition_name) +
           ",\"party_id\":" + json::write(rr::as_text(run.party_id)) +
           ",\"party_name\":" + json::write(detail.party_name) +
           ",\"kind\":" + json::write(run.kind) +
           ",\"state_name\":" + json::write(detail.state_name) +
           ",\"health\":" + json::write(detail.health) +
           ",\"output_message\":" + json::write(run.output_message) +
           ",\"output_storage_key\":" + json::write(run.output_storage_key) +
           ",\"started_at\":" + json::write(run.started_at) +
           ",\"completed_at\":" + json::write(run.completed_at) +
           ",\"duration_ms\":" + json::write(elapsed_ms(run.started_at, run.completed_at)) +
           ",\"priced_at\":" + json::write(run.priced_at) +
           ",\"price_note\":" + json::write(run.price_note) +
           ",\"batch_id\":" + json::write(detail.batch_id) +
           ",\"modified_by\":" + json::write(run.modified_by) +
           ",\"recorded_at\":" + json::write(run.recorded_at) + "}";
}

/// The scope a curve's own failure states.
constexpr std::string_view curve_scope = "Curve";

/// The one thing a problem names, as a person reads it: the curve a curve
/// failure is about, otherwise the record the engine named.
std::string about_of(const reporting_domain::report_error_event& event) {
    if (event.scope_code == curve_scope && event.cause && !event.cause->empty())
        return *event.cause;
    if (event.subject_id && !event.subject_id->empty())
        return *event.subject_id;
    return event.cause.value_or(std::string());
}

/// One problem a run recorded, as the table draws it.
std::string run_error_json(const reporting_domain::report_error_event& event) {
    return "{\"id\":" + json::write(rr::as_text(event.id)) +
           ",\"severity\":" + json::write(event.severity) +
           ",\"scope\":" + json::write(event.scope_code) +
           ",\"about\":" + json::write(about_of(event)) +
           ",\"message\":" + json::write(event.message) +
           ",\"subject\":" + json::write(event.subject) +
           ",\"subject_id\":" + json::write(event.subject_id) +
           ",\"cause\":" + json::write(event.cause) + ",\"time\":" + json::write(event.event_time) +
           "}";
}

}

}

std::string run_routes::runs_json(std::vector<run_view> runs) {
    std::ranges::stable_sort(runs, [](const auto& left, const auto& right) {
        if (!left.started_at || !right.started_at)
            return left.started_at.has_value() && !right.started_at.has_value();
        return *left.started_at > *right.started_at;
    });
    std::vector<std::string> written;
    for (const auto& run : runs)
        written.push_back(runs::run_json(run));
    return rr::array_of(written);
}

void run_routes::register_routes(std::shared_ptr<ores::http::net::router> router,
                                 std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                 nats_client& session) {
    using namespace runs;
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"reporting"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->post("/api/reporting/reports/{id}/run")
            .summary("Start a run of one report definition and answer the run it made")
            .description("The run is the service's production run of the definition, for the "
                         "caller's own tenant, and the answer names it so the page can follow it.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::trigger_report_instance_request>([&] {
                    const auto raw = req.get_path_param("id");
                    if (!rr::is_uuid(raw))
                        rr::invalid("The address names an id that is not a uuid.");
                    const auto& claims = req.authenticated_user;
                    if (!claims || !claims->tenant_id || claims->tenant_id->empty())
                        rr::invalid("The caller's own token names no tenant.");
                    reporting_messaging::trigger_report_instance_request msg;
                    msg.report_definition_id = boost::uuids::string_generator()(raw);
                    msg.tenant_id = boost::uuids::string_generator()(*claims->tenant_id);
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_done(reply.result);
                    return http_response::json(
                        "{\"runId\":" + json::write(reply.report_instance_id) + "}");
                });
            }));

    add(router->get("/api/reporting/reports/{id}/runs")
            .summary("The runs of one report, newest first, with the state each reached")
            .description("A run whose state cannot be read is still a run the person made, so "
                         "the state is left out rather than the page.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_report_instances_request>([&] {
                    const auto raw = req.get_path_param("id");
                    if (!rr::is_uuid(raw))
                        rr::invalid("The address names an id that is not a uuid.");
                    rr::refuse_unknown_params(req, {});
                    return http_response::json(runs_json(
                        runs_of_definition(req, session, boost::uuids::string_generator()(raw))));
                });
            }));

    add(router->get("/api/reporting/runs/{id}")
            .summary("One report run: what it is, the state it reached, its batch and its output")
            .description("The definition that raised it, the batch that ran it and the state it "
                         "reached are read beside the run, and each one that cannot be read is "
                         "left out rather than failing the page.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_report_instances_request>([&] {
                    const auto raw = req.get_path_param("id");
                    if (!rr::is_uuid(raw))
                        rr::invalid("The address names an id that is not a uuid.");
                    rr::refuse_unknown_params(req, {});
                    const auto id = boost::uuids::string_generator()(raw);
                    const auto found = run_by_id(req, session, id);
                    if (!found)
                        rr::refuse(
                            http_status::not_found, "not-found", "No report run has this id.");

                    run_detail detail;
                    detail.instance = *found;
                    detail.definition_name =
                        definition_name_of(req, session, detail.instance.definition_id);
                    detail.party_name = party_name_of(req, session, detail.instance.party_id);
                    detail.batch_id = batch_of(req, session, rr::as_text(id));
                    if (const auto stated = health_of_run(req, session, id)) {
                        detail.state_name = stated->state_name;
                        detail.health = report_health::word_of(stated->health);
                    }
                    return http_response::json(run_detail_json(detail));
                });
            }));

    add(router->get("/api/reporting/runs/{id}/errors")
            .summary("The problems one run recorded, one row each")
            .description("A row names the one thing a problem is about: the curve a curve failure "
                         "names, and otherwise the record the engine named.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<
                    reporting_messaging::list_by_report_instance_id_report_error_events_request>(
                    [&] {
                        const auto raw = req.get_path_param("id");
                        if (!rr::is_uuid(raw))
                            rr::invalid("The address names an id that is not a uuid.");
                        rr::refuse_unknown_params(req, {});
                        reporting_messaging::list_by_report_instance_id_report_error_events_request
                            msg;
                        msg.report_instance_id = boost::uuids::string_generator()(raw);
                        msg.limit = 500;
                        const auto reply = forwarding::forward(req, session, msg);
                        rr::require_done(reply.result);
                        std::vector<std::string> written;
                        for (const auto& event : reply.errors)
                            written.push_back(run_error_json(event));
                        return http_response::json("{\"total\":" + std::to_string(reply.total) +
                                                   ",\"errors\":" + rr::array_of(written) + "}");
                    });
            }));
}

}
