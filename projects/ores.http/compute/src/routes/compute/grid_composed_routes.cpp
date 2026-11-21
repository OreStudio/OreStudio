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
#include "ores.http/routes/compute/grid_composed_routes.hpp"
#include "ores.compute.api/messaging/host_protocol.hpp"
#include "ores.compute.api/messaging/telemetry_protocol.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.iam.api/messaging/tenant_protocol.hpp"
#include "ores.iam.api/messaging/tenant_session_protocol.hpp"
#include "ores.logging/logger.hpp"
#include "ores.telemetry.core/messaging/service_samples_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cctype>
#include <chrono>
#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::http::routes::compute {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace compute_messaging = ores::compute::messaging;
namespace iam_messaging = ores::iam::messaging;
namespace telemetry_messaging = ores::telemetry::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;

/// The sentences a read states when its query is not the one it takes.
constexpr std::string_view summary_refused = "The grid read takes no query fields.";
constexpr std::string_view series_refused = "The grid series read takes a range of 1h, 24h or 7d.";
constexpr std::string_view usage_refused = "The grid usage read takes no query fields.";

/// What each view states it reads, in the sentence its refusal completes.
constexpr std::string_view summary_what = "The grid of a deployment";
constexpr std::string_view series_what = "The grid series of a deployment";
constexpr std::string_view usage_what = "The grid usage of a deployment";

/// How long each view stays fresh.
constexpr std::chrono::seconds summary_stale_after{10};
constexpr std::chrono::seconds series_stale_after{10};
constexpr std::chrono::seconds usage_stale_after{30};

/// One range the series screen offers, with the window and the bucket it reads at.
struct series_range final {
    std::string_view name;
    std::uint32_t seconds;
    std::uint32_t bucket_seconds;
};

/**
 * The ranges the screen offers.
 *
 * The bucket width is chosen here rather than by the browser, so a range comes
 * back as a few hundred buckets and never as raw rows: an hour at a minute, a
 * day at five minutes and a week at half an hour.
 */
constexpr series_range series_ranges[] = {
    {"1h", 60 * 60, 60}, {"24h", 24 * 60 * 60, 5 * 60}, {"7d", 7 * 24 * 60 * 60, 30 * 60}};

/// The one service whose instances run on the grid's nodes.
constexpr std::string_view compute_runner = "ores.compute.wrapper";

/// The tenant type the usage view leaves out: the automation tenants run the tests.
constexpr std::string_view automation_tenant = "automation";

/// The most tenants one usage read enters.
constexpr std::uint32_t tenants_read_max = 1000;

auto& lg() {
    static auto instance = ores::logging::make_logger("ores.http.compute.grid_composed_routes");
    return instance;
}

/// The text as a key, so two spellings of one id find each other.
std::string key_of(std::string_view text) {
    std::string out(text);
    std::ranges::transform(
        out, out.begin(), [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return out;
}

/**
 * Whether the caller acts on the installation rather than on one tenant.
 *
 * A session in the system tenant is the installation's own; every other session
 * reads its tenant's corner. This is the one thing the gateway can tell without
 * asking another service, and it is what the BFF's session mode states.
 */
bool reads_the_installation(const http_request& req) {
    const auto& claims = req.authenticated_user;
    return claims && claims->tenant_id && *claims->tenant_id == rr::system_tenant_id;
}

/// The tenant the caller's session stands in, as the wire names it.
std::string own_tenant(const http_request& req) {
    const auto& claims = req.authenticated_user;
    if (!claims || !claims->tenant_id)
        return {};
    return key_of(*claims->tenant_id);
}

/// The instant as the wire states it, or nothing when it was never stamped.
std::optional<std::chrono::system_clock::time_point>
stated_instant(const std::chrono::system_clock::time_point& instant) {
    if (instant == std::chrono::system_clock::time_point{})
        return std::nullopt;
    return instant;
}

/// The buckets of one whole-number column.
std::string column_json(const std::vector<int>& column) {
    std::vector<std::string> cells;
    cells.reserve(column.size());
    for (const auto value : column)
        cells.push_back(std::to_string(value));
    return rr::array_of(cells);
}

/// The buckets of one 64-bit column.
std::string wide_column_json(const std::vector<std::int64_t>& column) {
    std::vector<std::string> cells;
    cells.reserve(column.size());
    for (const auto value : column)
        cells.push_back(std::to_string(value));
    return rr::array_of(cells);
}

/// The columns of a grid or node series, named as the screen reads them.
std::string grid_columns_json(const compute_messaging::get_grid_series_response& series) {
    return "\"samples\":" + column_json(series.samples) +
           ",\"total_hosts\":" + column_json(series.total_hosts) +
           ",\"online_hosts\":" + column_json(series.online_hosts) +
           ",\"idle_hosts\":" + column_json(series.idle_hosts) +
           ",\"results_inactive\":" + column_json(series.results_inactive) +
           ",\"results_unsent\":" + column_json(series.results_unsent) +
           ",\"results_in_progress\":" + column_json(series.results_in_progress) +
           ",\"results_done\":" + column_json(series.results_done) +
           ",\"total_workunits\":" + column_json(series.total_workunits) +
           ",\"total_batches\":" + column_json(series.total_batches) +
           ",\"active_batches\":" + column_json(series.active_batches) +
           ",\"outcomes_success\":" + column_json(series.outcomes_success) +
           ",\"outcomes_client_error\":" + column_json(series.outcomes_client_error) +
           ",\"outcomes_no_reply\":" + column_json(series.outcomes_no_reply);
}

/// The columns of the nodes' series, each row-major over nodes and buckets.
std::string node_columns_json(const compute_messaging::get_node_series_response& series) {
    return "\"samples\":" + column_json(series.samples) +
           ",\"tasks_completed\":" + column_json(series.tasks_completed) +
           ",\"tasks_failed\":" + column_json(series.tasks_failed) +
           ",\"tasks_since_last\":" + column_json(series.tasks_since_last) +
           ",\"avg_task_duration_ms\":" + wide_column_json(series.avg_task_duration_ms) +
           ",\"max_task_duration_ms\":" + wide_column_json(series.max_task_duration_ms) +
           ",\"input_bytes_fetched\":" + wide_column_json(series.input_bytes_fetched) +
           ",\"output_bytes_uploaded\":" + wide_column_json(series.output_bytes_uploaded);
}

/// The roster's runner on one machine, or nothing when no runner reported there.
const telemetry_messaging::service_roster_slot*
runner_on(const std::map<std::string, const telemetry_messaging::service_roster_slot*>& runners,
          const std::string& host_id) {
    const auto found = runners.find(key_of(host_id));
    return found == runners.end() ? nullptr : found->second;
}

/// One node's row: its measurements, the name of its machine and its runner.
std::string
node_json(const compute_messaging::node_stats_summary& node,
          const std::map<std::string, const ores::compute::domain::host*>& hosts,
          const std::map<std::string, const telemetry_messaging::service_roster_slot*>& runners) {
    const auto host = hosts.find(key_of(node.host_id));
    const auto* machine = host == hosts.end() ? nullptr : host->second;
    std::optional<std::string> host_name;
    if (machine != nullptr) {
        const auto& named =
            machine->display_name.empty() ? machine->external_id : machine->display_name;
        if (!named.empty())
            host_name = named;
    }
    const auto* runner = runner_on(runners, node.host_id);
    std::optional<std::string> instance_id;
    std::optional<std::string> version;
    if (runner != nullptr) {
        instance_id = runner->instance_id;
        version = runner->version;
    }

    return "{\"host_id\":" + json::write(node.host_id) +
           ",\"machine\":" + json::write(node.machine) +
           ",\"tasks_completed\":" + std::to_string(node.tasks_completed) +
           ",\"tasks_failed\":" + std::to_string(node.tasks_failed) +
           ",\"tasks_since_last\":" + std::to_string(node.tasks_since_last) +
           ",\"avg_task_duration_ms\":" + std::to_string(node.avg_task_duration_ms) +
           ",\"max_task_duration_ms\":" + std::to_string(node.max_task_duration_ms) +
           ",\"input_bytes_fetched\":" + std::to_string(node.input_bytes_fetched) +
           ",\"output_bytes_uploaded\":" + std::to_string(node.output_bytes_uploaded) +
           ",\"seconds_since_hb\":" + std::to_string(node.seconds_since_hb) +
           ",\"host\":" + json::write(host_name) +
           ",\"cpu_count\":" + std::to_string(machine == nullptr ? 0 : machine->cpu_count) +
           ",\"ram_mb\":" + std::to_string(machine == nullptr ? 0 : machine->ram_mb) +
           ",\"instance_id\":" + json::write(instance_id) + ",\"state\":" +
           json::write(std::string(
               runner == nullptr ? "missing" : ores::telemetry::domain::to_string(runner->state))) +
           ",\"version\":" + json::write(version) + "}";
}

/// One tenant's or party's use of the grid, before the view knows every share.
struct usage_row final {
    std::string key;
    std::optional<std::string> name;
    std::int64_t batches = 0;
    std::int64_t workunits = 0;
    std::int64_t results = 0;
    std::int64_t results_unsent = 0;
    std::int64_t results_in_progress = 0;
    std::int64_t results_done = 0;
    std::int64_t outcomes_success = 0;
    std::int64_t outcomes_failed = 0;
    std::int64_t oldest_unsent_seconds = 0;
    bool unavailable = false;
};

/// One group of the answer, with the share of the results the view holds.
std::string usage_row_json(const usage_row& row, std::int64_t total) {
    const auto share =
        total == 0 ? 0.0 : static_cast<double>(row.results) / static_cast<double>(total);
    return "{\"key\":" + json::write(row.key) + ",\"name\":" + json::write(row.name) +
           ",\"batches\":" + std::to_string(row.batches) +
           ",\"workunits\":" + std::to_string(row.workunits) +
           ",\"results\":" + std::to_string(row.results) +
           ",\"results_unsent\":" + std::to_string(row.results_unsent) +
           ",\"results_in_progress\":" + std::to_string(row.results_in_progress) +
           ",\"results_done\":" + std::to_string(row.results_done) +
           ",\"outcomes_success\":" + std::to_string(row.outcomes_success) +
           ",\"outcomes_failed\":" + std::to_string(row.outcomes_failed) +
           ",\"oldest_unsent_seconds\":" + std::to_string(row.oldest_unsent_seconds) +
           ",\"share\":" + json::write(share) + ",\"unavailable\":" + json::write(row.unavailable) +
           "}";
}

/// The answer the usage view states, its groups carrying their share.
std::string usage_json(std::string_view group_kind, const std::vector<usage_row>& rows) {
    std::int64_t total = 0;
    for (const auto& row : rows)
        total += row.results;
    std::vector<std::string> groups;
    groups.reserve(rows.size());
    for (const auto& row : rows)
        groups.push_back(usage_row_json(row, total));
    return "{\"read_at\":" + json::write(std::chrono::system_clock::now()) +
           ",\"group_kind\":" + json::write(std::string(group_kind)) +
           ",\"groups\":" + rr::array_of(groups) + "}";
}

/// The window and the bucket one series read states.
struct series_query final {
    std::string_view range;
    std::chrono::system_clock::time_point start;
    std::chrono::system_clock::time_point end;
    std::uint32_t bucket_seconds = 0;
};

/**
 * The window and the bucket one series read states, from the deployment's clock.
 *
 * The browser names a range and not two instants, because the deployment's clock
 * is the one the samples were stamped against.
 */
series_query series_query_of(const http_request& req) {
    rr::refuse_unknown_params(req, {"range"});
    const auto asked = req.get_query_param("range");
    const auto* offered = std::ranges::find_if(
        series_ranges, [&](const series_range& range) { return range.name == asked; });
    if (offered == std::end(series_ranges))
        rr::invalid(std::string(series_refused));
    const auto now = std::chrono::system_clock::now();
    const auto seconds = std::chrono::seconds(offered->seconds);
    return {offered->name, now - seconds, now, offered->bucket_seconds};
}

} // namespace

http_response grid_composed_routes::summary_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req, std::move(body), ores::http::net::view_cache_policy::fresh_for(summary_stale_after));
}

http_response grid_composed_routes::series_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req, std::move(body), ores::http::net::view_cache_policy::fresh_for(series_stale_after));
}

http_response grid_composed_routes::usage_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req, std::move(body), ores::http::net::view_cache_policy::fresh_for(usage_stale_after));
}

void grid_composed_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"grid"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/operations/grid")
            .summary("The grid's summary, and every node with its machine and its runner")
            .description("A composed view owned by ores.compute: the newest stored sample, the "
                         "host registry that names each node's machine and holds its hardware, "
                         "and the service roster that says which runner serves it. Three reads, "
                         "and the gateway computes nothing but the ages and the joins. The reach "
                         "of the read is the deployment's, because the grid is the whole "
                         "installation's work rather than one tenant's. The answer carries an "
                         "ETag and stays fresh for 10 seconds.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<compute_messaging::get_grid_stats_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    forwarding::require_deployment_read(req, session, summary_what);

                    const auto stats = forwarding::forward(
                        req, session, compute_messaging::get_grid_stats_request{});
                    if (!stats.success)
                        rr::refuse(http_status::bad_gateway,
                                   "upstream-unavailable",
                                   stats.message.empty() ?
                                       "The compute service refused the grid read." :
                                       stats.message);

                    compute_messaging::list_hosts_request hosts_read;
                    hosts_read.limit = static_cast<std::uint32_t>(1000);
                    const auto hosts_reply = forwarding::forward(req, session, hosts_read);
                    rr::require_ok(hosts_reply.result);

                    const auto roster = forwarding::forward(
                        req, session, telemetry_messaging::get_service_roster_request{});
                    if (!roster.success)
                        rr::refuse(http_status::bad_gateway,
                                   "upstream-unavailable",
                                   roster.message.empty() ?
                                       "The telemetry service refused the roster read." :
                                       roster.message);

                    std::map<std::string, const ores::compute::domain::host*> host_of;
                    for (const auto& host : hosts_reply.hosts)
                        host_of[key_of(boost::uuids::to_string(host.id))] = &host;
                    std::map<std::string, const telemetry_messaging::service_roster_slot*>
                        runner_of;
                    for (const auto& slot : roster.slots) {
                        if (slot.service_name != compute_runner || !slot.host_id ||
                            slot.host_id->empty())
                            continue;
                        runner_of[key_of(*slot.host_id)] = &slot;
                    }

                    std::vector<std::string> nodes;
                    nodes.reserve(stats.node_summaries.size());
                    for (const auto& node : stats.node_summaries)
                        nodes.push_back(node_json(node, host_of, runner_of));

                    const auto sampled_at = stated_instant(stats.sampled_at);
                    return summary_answer(
                        req,
                        "{\"sampled_at\":" + json::write(sampled_at) +
                            ",\"total_hosts\":" + std::to_string(stats.total_hosts) +
                            ",\"online_hosts\":" + std::to_string(stats.online_hosts) +
                            ",\"idle_hosts\":" + std::to_string(stats.idle_hosts) +
                            ",\"total_workunits\":" + std::to_string(stats.total_workunits) +
                            ",\"total_batches\":" + std::to_string(stats.total_batches) +
                            ",\"active_batches\":" + std::to_string(stats.active_batches) +
                            ",\"outcomes_success\":" + std::to_string(stats.outcomes_success) +
                            ",\"outcomes_client_error\":" +
                            std::to_string(stats.outcomes_client_error) +
                            ",\"outcomes_no_reply\":" + std::to_string(stats.outcomes_no_reply) +
                            ",\"nodes\":" + rr::array_of(nodes) + "}");
                });
            }));

    add(router->get("/api/operations/grid/series")
            .summary("The grid's counters and every node's, on one time axis")
            .description("A composed view owned by ores.compute: the counters per bucket, and "
                         "one row of buckets per node with the host registry joined on so a node "
                         "carries the name a person reads. The window and the bucket come from "
                         "the range and the deployment's clock. An operator of the installation "
                         "reads the nodes as well; a tenant's session reads the counters its rows "
                         "make up and is shown no machines. A bucket with no sample keeps its "
                         "place and its zeros, so a reader checks the samples before it draws a "
                         "value. The answer carries an ETag and stays fresh for 10 seconds.")
            .query_param("range", "string", "", true, "The window: 1h, 24h or 7d")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<compute_messaging::get_grid_series_request>([&] {
                    const auto asked = series_query_of(req);
                    const auto operator_of_the_installation = reads_the_installation(req);
                    if (operator_of_the_installation)
                        forwarding::require_deployment_read(req, session, series_what);

                    compute_messaging::get_grid_series_request grid_read;
                    grid_read.start_time = asked.start;
                    grid_read.end_time = asked.end;
                    grid_read.bucket_seconds = static_cast<int>(asked.bucket_seconds);
                    const auto grid = forwarding::forward(req, session, grid_read);
                    if (!grid.success)
                        rr::refuse(http_status::bad_gateway,
                                   "upstream-unavailable",
                                   grid.message.empty() ?
                                       "The compute service refused the series read." :
                                       grid.message);

                    std::string nodes("{\"list\":[]}");
                    if (operator_of_the_installation) {
                        compute_messaging::get_node_series_request nodes_read;
                        nodes_read.start_time = asked.start;
                        nodes_read.end_time = asked.end;
                        nodes_read.bucket_seconds = static_cast<int>(asked.bucket_seconds);
                        const auto series = forwarding::forward(req, session, nodes_read);
                        if (!series.success)
                            rr::refuse(http_status::bad_gateway,
                                       "upstream-unavailable",
                                       series.message.empty() ?
                                           "The compute service refused the nodes' series read." :
                                           series.message);

                        compute_messaging::list_hosts_request hosts_read;
                        hosts_read.limit = static_cast<std::uint32_t>(1000);
                        const auto hosts = forwarding::forward(req, session, hosts_read);
                        rr::require_ok(hosts.result);
                        std::map<std::string, std::string> name_of;
                        for (const auto& host : hosts.hosts) {
                            const auto& named =
                                host.display_name.empty() ? host.external_id : host.display_name;
                            if (!named.empty())
                                name_of[key_of(boost::uuids::to_string(host.id))] = named;
                        }

                        std::vector<std::string> listed;
                        listed.reserve(series.nodes.size());
                        for (const auto& node : series.nodes) {
                            const auto found = name_of.find(key_of(node.host_id));
                            listed.push_back(
                                "{\"host_id\":" + json::write(node.host_id) +
                                ",\"machine\":" + json::write(node.machine) + ",\"host\":" +
                                json::write(found == name_of.end() ?
                                                std::nullopt :
                                                std::optional<std::string>(found->second)) +
                                "}");
                        }
                        nodes = "{\"list\":" + rr::array_of(listed) + "," +
                                node_columns_json(series) + "}";
                    }

                    return series_answer(
                        req,
                        "{\"read_at\":" + json::write(std::chrono::system_clock::now()) +
                            ",\"range\":" + json::write(std::string(asked.range)) +
                            ",\"start_time\":" + json::write(grid.start_time) +
                            ",\"bucket_seconds\":" + std::to_string(grid.bucket_seconds) +
                            ",\"bucket_count\":" + std::to_string(grid.bucket_count) +
                            ",\"grid\":{" + grid_columns_json(grid) + "}" + ",\"nodes\":" + nodes +
                            "}");
                });
            }));

    add(router->get("/api/operations/grid/usage")
            .summary("Who uses the grid: one group for each tenant, or for each party")
            .description("A composed view owned by ores.compute. An operator of the installation "
                         "reads one group for each tenant, entered one at a time because no "
                         "policy lets the system tenant read another tenant's work; a tenant's "
                         "session reads one group for each of its parties. Each group carries its "
                         "share of the results. A tenant that cannot be entered or read is "
                         "unavailable rather than empty, so a failure never reads as a tenant "
                         "that runs nothing. The answer carries an ETag and stays fresh for 30 "
                         "seconds.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<compute_messaging::get_grid_usage_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    if (!reads_the_installation(req)) {
                        const auto usage = forwarding::forward(
                            req, session, compute_messaging::get_grid_usage_request{});
                        if (!usage.success)
                            rr::refuse(http_status::bad_gateway,
                                       "upstream-unavailable",
                                       usage.message.empty() ?
                                           "The compute service refused the usage read." :
                                           usage.message);
                        std::map<std::string, std::string> names;
                        try {
                            const auto parties = forwarding::forward(
                                req, session, iam_messaging::get_my_parties_request{});
                            rr::require_done(parties.result);
                            for (const auto& party : parties.parties)
                                names[key_of(party.party_id)] = party.name;
                        } catch (const std::exception& e) {
                            ORES_LOG_SEV(lg(), ores::logging::warn)
                                << "The names of the parties were not read: " << e.what();
                        }
                        std::vector<usage_row> rows;
                        rows.reserve(usage.keys.size());
                        for (std::size_t i = 0; i < usage.keys.size(); ++i) {
                            usage_row row;
                            row.key = usage.keys[i];
                            const auto found = names.find(key_of(row.key));
                            if (found != names.end())
                                row.name = found->second;
                            row.batches = usage.batches[i];
                            row.workunits = usage.workunits[i];
                            row.results = usage.results[i];
                            row.results_unsent = usage.results_unsent[i];
                            row.results_in_progress = usage.results_in_progress[i];
                            row.results_done = usage.results_done[i];
                            row.outcomes_success = usage.outcomes_success[i];
                            row.outcomes_failed = usage.outcomes_failed[i];
                            row.oldest_unsent_seconds = usage.oldest_unsent_seconds[i];
                            rows.push_back(std::move(row));
                        }
                        return usage_answer(req, usage_json("party", rows));
                    }

                    forwarding::require_deployment_read(req, session, usage_what);

                    iam_messaging::list_tenants_request tenants_read;
                    tenants_read.limit = tenants_read_max;
                    tenants_read.filter.emplace();
                    const auto tenants = forwarding::forward(req, session, tenants_read);
                    rr::require_done(tenants.result);

                    std::vector<usage_row> rows;
                    for (const auto& tenant : tenants.tenants) {
                        if (tenant.type == automation_tenant)
                            continue;
                        usage_row row;
                        row.key = boost::uuids::to_string(tenant.id);
                        const auto& named = tenant.name.empty() ? tenant.code : tenant.name;
                        if (!named.empty())
                            row.name = named;

                        try {
                            // The session's own tenant is read where it already
                            // stands; every other is entered, because no policy
                            // lets the system tenant read a tenant's work from
                            // outside it.
                            auto usage = compute_messaging::get_grid_usage_response{};
                            if (row.key == own_tenant(req)) {
                                usage = forwarding::forward(
                                    req, session, compute_messaging::get_grid_usage_request{});
                            } else {
                                iam_messaging::enter_tenant_request entry;
                                entry.tenant_id = row.key;
                                const auto entered = forwarding::forward_as(
                                    session, req.get_bearer_token().value_or(std::string{}), entry);
                                if (!entered.success)
                                    throw std::runtime_error(entered.message.empty() ?
                                                                 "The tenant was not entered." :
                                                                 entered.message);
                                usage = forwarding::forward_as(
                                    session,
                                    entered.token,
                                    compute_messaging::get_grid_usage_request{});
                            }
                            if (!usage.success)
                                throw std::runtime_error(usage.message.empty() ?
                                                             "The tenant's usage was not read." :
                                                             usage.message);
                            for (std::size_t i = 0; i < usage.keys.size(); ++i) {
                                row.batches += usage.batches[i];
                                row.workunits += usage.workunits[i];
                                row.results += usage.results[i];
                                row.results_unsent += usage.results_unsent[i];
                                row.results_in_progress += usage.results_in_progress[i];
                                row.results_done += usage.results_done[i];
                                row.outcomes_success += usage.outcomes_success[i];
                                row.outcomes_failed += usage.outcomes_failed[i];
                                row.oldest_unsent_seconds = std::max(
                                    row.oldest_unsent_seconds, usage.oldest_unsent_seconds[i]);
                            }
                        } catch (const std::exception& e) {
                            ORES_LOG_SEV(lg(), ores::logging::warn)
                                << "The usage of a tenant was not read: " << e.what();
                            row.unavailable = true;
                        }
                        rows.push_back(std::move(row));
                    }
                    return usage_answer(req, usage_json("tenant", rows));
                });
            }));
}

}
