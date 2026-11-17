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
#include "ores.http/routes/compute/job_routes.hpp"
#include "ores.compute.api/messaging/batch_protocol.hpp"
#include "ores.compute.api/messaging/result_protocol.hpp"
#include "ores.compute.api/messaging/workunit_protocol.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/compute/grid_views.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <cstdint>
#include <iterator>
#include <map>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::compute {

namespace {

namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace grid = ores::http::routes::compute::grid;
namespace compute_domain = ores::compute::domain;
namespace compute_messaging = ores::compute::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/// The sentence the batches list states when its query is not one.
constexpr std::string_view batch_refused =
    "The batch list takes a page, a status of up to 32 characters, a party id and up to 256 "
    "characters of the reference.";

/// The sentence the runs read states when its query is not one.
constexpr std::string_view runs_refused =
    "The runs read takes an outcome code of 0, 1, 3 or 4, an offset of 0 or more and a limit of "
    "1 to 200.";

/// The outcome codes the runs read names: none yet, Success, ClientError, NoReply.
constexpr int outcome_codes[] = {0, 1, 3, 4};

/// What narrows the batch list: a page, a status, a party and part of the reference.
struct batch_query final {
    grid::page asked;
    std::string status;
    std::string party;
    std::string find;
};

batch_query batch_query_of(const http_request& req) {
    rr::refuse_unknown_params(req, {"offset", "limit", "status", "party", "find"});
    batch_query out;
    out.asked = grid::page_of(req, batch_refused);
    out.status = rr::trimmed(req.get_query_param("status"));
    out.party = req.get_query_param("party");
    out.find = rr::trimmed(req.get_query_param("find"));
    const auto status_stated = !req.get_query_param("status").empty();
    if ((status_stated && out.status.empty()) || rr::characters_of(out.status) > 32 ||
        (!out.party.empty() && !rr::is_uuid(out.party)) || rr::characters_of(out.find) > 256)
        rr::invalid(std::string(batch_refused));
    return out;
}

/// The outcome code the runs read names, or a refusal when it names none.
int outcome_of(const http_request& req) {
    const auto raw = req.get_query_param("outcome");
    int code = -1;
    if (raw.size() == 1 && raw.front() >= '0' && raw.front() <= '9')
        code = raw.front() - '0';
    if (std::ranges::find(outcome_codes, code) == std::end(outcome_codes))
        rr::invalid(std::string(runs_refused));
    return code;
}

/// The identifier a route's address names.
boost::uuids::uuid id_of(const http_request& req) {
    const auto raw = req.get_path_param("id");
    if (!rr::is_uuid(raw))
        rr::invalid(std::string(grid::id_refused));
    return boost::uuids::string_generator()(raw);
}

/// The references a page of batches carries, which a report run may name.
std::vector<std::string> references_of(const std::vector<compute_domain::batch>& batches) {
    std::vector<std::string> references;
    references.reserve(batches.size());
    for (const auto& batch : batches)
        references.push_back(batch.external_ref);
    return references;
}

/**
 * @brief The rows of a page of batches, each with the run and the party that made it.
 *
 * The runs are one read for the whole page, and the parties one read for it, and
 * a read of either that fails leaves its names out rather than failing the page.
 */
std::vector<std::string> batch_rows(const http_request& req,
                                    nats_client& session,
                                    const std::vector<compute_domain::batch>& batches) {
    const auto runs = grid::report_run_names(req, session, references_of(batches));
    std::map<std::string, std::string> parties;
    const auto any_party =
        std::ranges::any_of(batches, [](const auto& batch) { return batch.party_id.has_value(); });
    if (any_party)
        parties = grid::party_names(req, session);

    std::vector<std::string> rows;
    rows.reserve(batches.size());
    for (const auto& batch : batches)
        rows.push_back(grid::batch_json(batch, runs, parties));
    return rows;
}

}

void job_routes::register_routes(std::shared_ptr<ores::http::net::router> router,
                                 std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                 nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"grid"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/operations/grid/jobs/batches")
            .summary("One page of the grid's batches, newest first")
            .description("Each row names the report run and the party that made the batch where "
                         "the caller may read them, and falls back on the reference otherwise.")
            .query_param("offset", "integer", "int32", false, "How many batches to skip")
            .query_param("limit", "integer", "int32", false, "How many batches to return")
            .query_param("status", "string", "", false, "Only batches in this status")
            .query_param("party", "string", "uuid", false, "Only batches of this party")
            .query_param("find", "string", "", false, "A text the reference holds")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<compute_messaging::list_batches_request>([&] {
                    const auto asked = batch_query_of(req);
                    compute_messaging::list_batches_request msg;
                    msg.offset = asked.asked.offset;
                    msg.limit = asked.asked.limit;
                    msg.filter.emplace();
                    if (!asked.status.empty())
                        msg.filter->status = asked.status;
                    if (!asked.party.empty())
                        msg.filter->party_id = boost::uuids::string_generator()(asked.party);
                    if (!asked.find.empty())
                        msg.filter->search = asked.find;
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);
                    return http_response::json(grid::page_json(
                        batch_rows(req, session, reply.batches), reply.total, asked.asked));
                });
            }));

    add(router->get("/api/operations/grid/jobs/batches/{id}")
            .summary("One batch, with the run and the party that made it")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<compute_messaging::list_batches_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto id = id_of(req);
                    compute_messaging::list_batches_request msg;
                    msg.limit = 1;
                    msg.filter.emplace();
                    msg.filter->id_one_of = std::vector{id};
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);
                    if (reply.batches.empty())
                        rr::refuse(http_status::not_found, "not-found", "No batch has this id.");
                    const auto rows = batch_rows(req, session, reply.batches);
                    return http_response::json(rows.front());
                });
            }));

    add(router->get("/api/operations/grid/jobs/batches/{id}/workunits")
            .summary("One page of the work units of one batch")
            .query_param("offset", "integer", "int32", false, "How many work units to skip")
            .query_param("limit", "integer", "int32", false, "How many work units to return")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<compute_messaging::list_by_batch_id_workunits_request>([&] {
                    rr::refuse_unknown_params(req, {"offset", "limit"});
                    const auto id = id_of(req);
                    const auto asked = grid::page_of(req, grid::page_refused);
                    compute_messaging::list_by_batch_id_workunits_request msg;
                    msg.batch_id = id;
                    msg.offset = asked.offset;
                    msg.limit = asked.limit;
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);

                    std::vector<std::string> rows;
                    rows.reserve(reply.workunits.size());
                    for (const auto& workunit : reply.workunits)
                        rows.push_back(grid::workunit_json(workunit));
                    return http_response::json(grid::page_json(rows, reply.total, asked));
                });
            }));

    add(router->get("/api/operations/grid/jobs/workunits/{id}")
            .summary("One work unit: the batch it belongs to and the build it runs")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<compute_messaging::get_workunit_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    compute_messaging::get_workunit_request msg;
                    msg.key.id = id_of(req);
                    const auto reply = forwarding::forward(req, session, msg);
                    if (reply.result.outcome == ores::utility::domain::outcome::missing)
                        rr::refuse(
                            http_status::not_found, "not-found", "No work unit has this id.");
                    rr::require_ok(reply.result);
                    if (!reply.workunit)
                        rr::refuse(
                            http_status::not_found, "not-found", "No work unit has this id.");
                    return http_response::json(grid::workunit_json(*reply.workunit));
                });
            }));

    add(router->get("/api/operations/grid/jobs/workunits/{id}/results")
            .summary("One page of the runs of one work unit")
            .description("A run in flight has no node and no output yet, and states null for "
                         "both.")
            .query_param("offset", "integer", "int32", false, "How many runs to skip")
            .query_param("limit", "integer", "int32", false, "How many runs to return")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<compute_messaging::list_by_workunit_id_results_request>([&] {
                    rr::refuse_unknown_params(req, {"offset", "limit"});
                    const auto id = id_of(req);
                    const auto asked = grid::page_of(req, grid::page_refused);
                    compute_messaging::list_by_workunit_id_results_request msg;
                    msg.workunit_id = id;
                    msg.offset = asked.offset;
                    msg.limit = asked.limit;
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);

                    const auto host_visible = grid::may_read_host(req);
                    std::vector<std::string> rows;
                    rows.reserve(reply.results.size());
                    for (const auto& result : reply.results)
                        rows.push_back(grid::result_json(result, host_visible));
                    return http_response::json(grid::page_json(rows, reply.total, asked));
                });
            }));

    add(router->get("/api/operations/grid/jobs/results")
            .summary("One page of the runs that ended with an outcome, newest first")
            .description("The answer to which of the runs failed: 1 is Success, 3 is "
                         "ClientError, 4 is NoReply, and 0 is a run with no outcome yet.")
            .query_param("outcome", "integer", "int32", true, "The outcome code to read")
            .query_param("offset", "integer", "int32", false, "How many runs to skip")
            .query_param("limit", "integer", "int32", false, "How many runs to return")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<compute_messaging::list_results_request>([&] {
                    rr::refuse_unknown_params(req, {"outcome", "offset", "limit"});
                    const auto code = outcome_of(req);
                    const auto asked = grid::page_of(req, runs_refused);
                    compute_messaging::list_results_request msg;
                    msg.offset = asked.offset;
                    msg.limit = asked.limit;
                    msg.order.field = "recorded_at";
                    msg.order.descending = true;
                    msg.filter.emplace();
                    msg.filter->outcome = code;
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);

                    const auto host_visible = grid::may_read_host(req);
                    std::vector<std::string> rows;
                    rows.reserve(reply.results.size());
                    for (const auto& result : reply.results)
                        rows.push_back(grid::result_json(result, host_visible));
                    return http_response::json(grid::page_json(rows, reply.total, asked));
                });
            }));

    add(router->get("/api/operations/grid/jobs/results/{id}")
            .summary("One run: its state, its outcome, its node and its output")
            .description("The node is stated only where the caller may read it, and host_hidden "
                         "says the caller may not.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<compute_messaging::get_result_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    compute_messaging::get_result_request msg;
                    msg.key.id = id_of(req);
                    const auto reply = forwarding::forward(req, session, msg);
                    if (reply.result.outcome == ores::utility::domain::outcome::missing)
                        rr::refuse(http_status::not_found, "not-found", "No result has this id.");
                    rr::require_ok(reply.result);
                    if (!reply.result_value)
                        rr::refuse(http_status::not_found, "not-found", "No result has this id.");
                    return http_response::json(
                        grid::result_json(*reply.result_value, grid::may_read_host(req)));
                });
            }));
}

}
