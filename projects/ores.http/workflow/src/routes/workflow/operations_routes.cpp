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
#include "ores.http/routes/workflow/operations_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.telemetry.core/messaging/logs_protocol.hpp"
#include "ores.telemetry.core/messaging/nats_samples_protocol.hpp"
#include "ores.telemetry.core/messaging/service_samples_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <algorithm>
#include <charconv>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::http::routes::workflow {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace telemetry_domain = ores::telemetry::domain;
namespace telemetry_messaging = ores::telemetry::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/// The sentence the logs state when their query is not the one they take.
constexpr std::string_view logs_refused =
    "The logs read takes a range, the filters the journey names, and a page.";

/// The most entries one page of logs may ask for.
constexpr std::uint32_t logs_page_max = 500;

/// The page of logs a read answers with when the caller states none.
constexpr std::uint32_t logs_page_default = 100;

/// One range a screen offers, and the seconds it spans.
struct range_preset final {
    std::string_view name;
    std::uint32_t seconds;
};

/**
 * The ranges the logs screen offers.
 *
 * The window is start-inclusive and end-exclusive, so adjacent ranges tile: a
 * line stamped on a boundary belongs to the older range alone.
 */
constexpr range_preset logs_ranges[] = {
    {"15m", 15 * 60}, {"1h", 60 * 60}, {"6h", 6 * 60 * 60}, {"24h", 24 * 60 * 60}};

/// The severity words the read matches, as the store writes them.
constexpr std::string_view logs_levels[] = {"error", "warn", "info", "debug"};

/// The telemetry query one logs request states.
struct logs_query final {
    std::chrono::system_clock::time_point start;
    std::chrono::system_clock::time_point end;
    std::optional<std::string> level;
    std::optional<std::string> source;
    std::optional<std::string> component;
    std::optional<std::string> tag;
    std::optional<std::string> message;
    std::uint32_t offset = 0;
    std::uint32_t limit = logs_page_default;
};

/// The whole number a parameter states, or nothing when it states none.
std::optional<std::uint32_t> logs_whole_of(const std::string& raw) {
    if (raw.empty())
        return std::nullopt;
    std::uint32_t value = 0;
    const auto* first = raw.data();
    const auto* last = raw.data() + raw.size();
    const auto parsed = std::from_chars(first, last, value);
    if (parsed.ec != std::errc() || parsed.ptr != last)
        rr::invalid(std::string(logs_refused));
    return value;
}

/// Whether a word is one of the severity words the read matches.
bool logs_is_level(const std::string& word) {
    for (const auto level : logs_levels)
        if (level == word)
            return true;
    return false;
}

/// The seconds one range spans, or nothing when the word is not one the screen offers.
template <std::size_t Count>
std::optional<std::uint32_t> span_of(const range_preset (&ranges)[Count],
                                     const std::string& range) {
    for (const auto& offered : ranges)
        if (offered.name == range)
            return offered.seconds;
    return std::nullopt;
}

/// A filter the caller stated, or nothing when it stated none.
std::optional<std::string> logs_filter_of(const http_request& req, const std::string& name) {
    if (req.query_params.find(name) == req.query_params.end())
        return std::nullopt;
    return req.get_query_param(name);
}

/**
 * The window and the filters one logs read states.
 *
 * The window comes from the deployment's own clock, which is the clock the
 * samples were stamped against; a browser that computed it would state a window
 * the deployment never measured.
 */
logs_query logs_of(const http_request& req) {
    rr::refuse_unknown_params(
        req, {"range", "level", "source", "component", "tag", "message", "offset", "limit"});
    const auto range = span_of(logs_ranges, req.get_query_param("range"));
    if (!range)
        rr::invalid(std::string(logs_refused));

    logs_query out;
    const auto now = std::chrono::system_clock::now();
    out.end = now;
    out.start = now - std::chrono::seconds(*range);

    if (const auto level = logs_filter_of(req, "level")) {
        if (!logs_is_level(*level))
            rr::invalid(std::string(logs_refused));
        out.level = level;
    }
    if (const auto source = logs_filter_of(req, "source")) {
        if (*source != "server")
            rr::invalid(std::string(logs_refused));
        out.source = source;
    }
    if (const auto component = logs_filter_of(req, "component"))
        out.component = *component;
    if (const auto tag = logs_filter_of(req, "tag"))
        out.tag = *tag;
    if (const auto message = logs_filter_of(req, "message"))
        out.message = *message;

    if (const auto stated = logs_whole_of(req.get_query_param("offset")))
        out.offset = *stated;
    if (const auto stated = logs_whole_of(req.get_query_param("limit"))) {
        if (*stated < 1 || *stated > logs_page_max)
            rr::invalid(std::string(logs_refused));
        out.limit = *stated;
    }
    return out;
}

/// The sentence the bus states when its range is not one it offers.
constexpr std::string_view bus_refused = "The message bus read takes a range of 15m, 1h or 6h.";

/// How long the bus stays fresh: its samples are taken every few seconds.
constexpr std::chrono::seconds bus_stale_after{10};

/**
 * The ranges the bus screen offers.
 *
 * The window is start-inclusive and end-exclusive, so adjacent ranges tile: a
 * sample stamped on a boundary belongs to the older range alone.
 */
constexpr range_preset bus_ranges[] = {{"15m", 15 * 60}, {"1h", 60 * 60}, {"6h", 6 * 60 * 60}};

/**
 * The five JetStream streams this installation creates, by their suffix.
 *
 * The names are the deployment's own, from
 * doc/knowledge/architecture/jetstream_streams_and_consumers.org. They are
 * declared rather than read, because no operation lists the streams that
 * exist: a stream created after this list was written is not read, and the
 * screen states that gap.
 */
constexpr std::string_view bus_stream_suffixes[] = {"marketdata_ticks",
                                                    "synthetic_ticks",
                                                    "synthetic_sandbox_ticks",
                                                    "workflow",
                                                    "compute_assignments"};

/// The window one bus read states.
struct bus_window final {
    std::chrono::system_clock::time_point start;
    std::chrono::system_clock::time_point end;
};

/**
 * The window one bus read states, from the deployment's own clock.
 *
 * The browser names a range and not two instants, because the deployment's
 * clock is the one the samples were stamped against.
 */
bus_window bus_window_of(const http_request& req) {
    rr::refuse_unknown_params(req, {"range"});
    const auto span = span_of(bus_ranges, req.get_query_param("range"));
    if (!span)
        rr::invalid(std::string(bus_refused));
    const auto now = std::chrono::system_clock::now();
    return {now - std::chrono::seconds(*span), now};
}

/// One NATS server sample, in the shape the bus screen reads.
std::string bus_server_sample_json(const telemetry_messaging::nats_server_sample& sample) {
    return "{\"sampled_at\":" + json::write(sample.sampled_at) +
           ",\"in_msgs\":" + std::to_string(sample.in_msgs) +
           ",\"out_msgs\":" + std::to_string(sample.out_msgs) +
           ",\"in_bytes\":" + std::to_string(sample.in_bytes) +
           ",\"out_bytes\":" + std::to_string(sample.out_bytes) +
           ",\"connections\":" + std::to_string(sample.connections) +
           ",\"mem_bytes\":" + std::to_string(sample.mem_bytes) +
           ",\"slow_consumers\":" + std::to_string(sample.slow_consumers) + "}";
}

/// One NATS stream sample, in the shape the bus screen reads.
std::string bus_stream_sample_json(const telemetry_messaging::nats_stream_sample& sample) {
    return "{\"sampled_at\":" + json::write(sample.sampled_at) +
           ",\"stream_name\":" + json::write(sample.stream_name) +
           ",\"messages\":" + std::to_string(sample.messages) +
           ",\"bytes\":" + std::to_string(sample.bytes) +
           ",\"consumer_count\":" + std::to_string(sample.consumer_count) + "}";
}

/**
 * The seconds since one instance last reported, or nothing when it never did.
 *
 * The subtraction happens here rather than in the browser, because the
 * deployment's clock is the one the sample was stamped against.
 */
std::optional<std::int64_t>
roster_age_of(const std::optional<std::chrono::system_clock::time_point>& sampled_at,
              std::chrono::system_clock::time_point now) {
    if (!sampled_at)
        return std::nullopt;
    const auto age = std::chrono::duration_cast<std::chrono::seconds>(now - *sampled_at).count();
    return std::max<std::int64_t>(0, age);
}

/// One expected instance, in the shape the services screen reads.
std::string roster_row_json(const telemetry_messaging::service_roster_slot& slot,
                            std::chrono::system_clock::time_point now) {
    return "{\"service_name\":" + json::write(slot.service_name) +
           ",\"display_name\":" + json::write(slot.display_name) +
           ",\"description\":" + json::write(slot.description) +
           ",\"service_account\":" + json::write(slot.service_account) +
           ",\"slot\":" + std::to_string(slot.slot) +
           ",\"state\":" + json::write(std::string(telemetry_domain::to_string(slot.state))) +
           ",\"instance_id\":" + json::write(slot.instance_id) +
           ",\"host_id\":" + json::write(slot.host_id) +
           ",\"version\":" + json::write(slot.version) +
           ",\"sampled_at\":" + json::write(slot.sampled_at) +
           ",\"age_seconds\":" + json::write(roster_age_of(slot.sampled_at, now)) + "}";
}

/// One telemetry log entry, in the shape the logs screen reads.
std::string log_entry_json(const telemetry_messaging::telemetry_log_entry& entry) {
    return "{\"id\":" + json::write(entry.id) + ",\"timestamp\":" + json::write(entry.timestamp) +
           ",\"source\":" + json::write(std::string(telemetry_domain::to_string(entry.source))) +
           ",\"source_name\":" + json::write(entry.source_name) +
           ",\"session_id\":" + json::write(entry.session_id) +
           ",\"account_id\":" + json::write(entry.account_id) +
           ",\"level\":" + json::write(entry.level) +
           ",\"component\":" + json::write(entry.component) +
           ",\"message\":" + json::write(entry.message) + ",\"tag\":" + json::write(entry.tag) +
           ",\"recorded_at\":" + json::write(entry.recorded_at) + "}";
}

/// The refusal a read states when its operation did not answer.
void require_answered(bool success, const std::string& message, std::string_view fallback) {
    if (success)
        return;
    rr::refuse(http_status::bad_gateway,
               "upstream-unavailable",
               message.empty() ? std::string(fallback) : message);
}

}

std::string
operations_routes::bus_json(const telemetry_messaging::get_nats_server_samples_response& servers,
                            const std::vector<telemetry_messaging::nats_stream_sample>& streams) {
    std::vector<std::string> samples;
    samples.reserve(servers.samples.size());
    for (const auto& sample : servers.samples)
        samples.push_back(bus_server_sample_json(sample));
    std::vector<std::string> rows;
    rows.reserve(streams.size());
    for (const auto& sample : streams)
        rows.push_back(bus_stream_sample_json(sample));
    std::string sampled_at("null");
    if (!servers.samples.empty())
        sampled_at = json::write(servers.samples.front().sampled_at);
    return "{\"sampled_at\":" + sampled_at + ",\"samples\":" + rr::array_of(samples) +
           ",\"streams\":" + rr::array_of(rows) + "}";
}

http_response operations_routes::bus_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req, std::move(body), ores::http::net::view_cache_policy::fresh_for(bus_stale_after));
}

void operations_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"operations"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/operations/services")
            .summary("Every expected service instance, with the age of its last report")
            .description("One row per expected instance, ordered by service name and then slot, "
                         "so a service that never reported keeps its row and its state word. The "
                         "age is the deployment's own subtraction, because its clock is the one "
                         "the sample was stamped against. The reach of the read is the "
                         "telemetry service's judgement: it checks the permission the roster "
                         "names, so the route does not guess at who may read a deployment.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<telemetry_messaging::get_service_roster_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto reply = forwarding::forward(
                        req, session, telemetry_messaging::get_service_roster_request{});
                    require_answered(reply.success,
                                     reply.message,
                                     "The telemetry service refused the roster read.");

                    const auto now = std::chrono::system_clock::now();
                    std::vector<std::string> rows;
                    rows.reserve(reply.slots.size());
                    for (const auto& slot : reply.slots)
                        rows.push_back(roster_row_json(slot, now));
                    return http_response::json("{\"rows\":" + rr::array_of(rows) + "}");
                });
            }));

    add(router->get("/api/operations/logs")
            .summary("One page of the telemetry logs a filter selects")
            .description("The filters combine with and, which is the query's own rule, so one "
                         "call carries all of them and the read does the narrowing. The window "
                         "comes from the deployment's clock and is start-inclusive and "
                         "end-exclusive, so adjacent windows tile. A source of client is refused "
                         "rather than forwarded, because the store holds no client entry today "
                         "and a filter that can never match must not be offered.")
            .query_param("range", "string", "", true, "The window: 15m, 1h, 6h or 24h")
            .query_param("level", "string", "", false, "The severity: error, warn, info or debug")
            .query_param("source", "string", "", false, "The source, which is server")
            .query_param("component", "string", "", false, "The component that emitted the line")
            .query_param("tag", "string", "", false, "The tag the line carries")
            .query_param("message", "string", "", false, "Text the message holds")
            .query_param("offset", "integer", "int32", false, "How many entries to skip")
            .query_param("limit", "integer", "int32", false, "How many entries to return")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<telemetry_messaging::get_telemetry_logs_request>([&] {
                    const auto asked = logs_of(req);
                    telemetry_messaging::get_telemetry_logs_request msg;
                    msg.query.start_time = asked.start;
                    msg.query.end_time = asked.end;
                    msg.query.level = asked.level;
                    if (asked.source)
                        msg.query.source = telemetry_domain::telemetry_source::server;
                    msg.query.component = asked.component;
                    msg.query.tag = asked.tag;
                    msg.query.message_contains = asked.message;
                    msg.query.offset = asked.offset;
                    msg.query.limit = asked.limit;
                    const auto reply = forwarding::forward(req, session, msg);
                    require_answered(reply.success,
                                     reply.message,
                                     "The telemetry service refused the logs read.");

                    std::vector<std::string> entries;
                    entries.reserve(reply.entries.size());
                    for (const auto& entry : reply.entries)
                        entries.push_back(log_entry_json(entry));
                    return http_response::json(
                        "{\"read_at\":" + json::write(std::chrono::system_clock::now()) +
                        ",\"entries\":" + rr::array_of(entries) +
                        ",\"total\":" + std::to_string(reply.total_count) +
                        ",\"limit\":" + std::to_string(asked.limit) +
                        ",\"offset\":" + std::to_string(asked.offset) + "}");
                });
            }));

    add(router->get("/api/operations/bus")
            .summary("The message bus over a window, newest first")
            .description("A composed view owned by ores.telemetry: the NATS server samples a "
                         "window holds, whole and newest first, because every counter is a "
                         "running total and only a subtraction over the range turns one into "
                         "movement; and the newest sample of each stream. The window comes from "
                         "the deployment's clock and is start-inclusive and end-exclusive, so "
                         "adjacent windows tile. One read per stream, because no operation lists "
                         "the streams that exist: the five the installation creates are declared "
                         "here, so a stream created after this list was written is not read and "
                         "the screen states that gap. A stream with no sample in the window "
                         "contributes no row rather than a row of zeros. The telemetry service "
                         "checks the permission each read names. The answer carries an ETag and "
                         "stays fresh for 10 seconds.")
            .query_param("range", "string", "", true, "The window: 15m, 1h or 6h")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<telemetry_messaging::get_nats_server_samples_request>([&] {
                    const auto window = bus_window_of(req);

                    telemetry_messaging::get_nats_server_samples_request read;
                    read.query.start_time = window.start;
                    read.query.end_time = window.end;
                    const auto servers = forwarding::forward(req, session, read);
                    require_answered(servers.success,
                                     servers.message,
                                     "The telemetry service refused the bus read.");

                    std::vector<telemetry_messaging::nats_stream_sample> streams;
                    for (const auto suffix : bus_stream_suffixes) {
                        telemetry_messaging::get_nats_stream_samples_request stream_read;
                        stream_read.query.stream_name = session.make_stream_name(suffix);
                        stream_read.query.start_time = window.start;
                        stream_read.query.end_time = window.end;
                        const auto reply = forwarding::forward(req, session, stream_read);
                        require_answered(reply.success,
                                         reply.message,
                                         "The telemetry service refused a stream read.");
                        if (!reply.samples.empty())
                            streams.push_back(reply.samples.front());
                    }

                    return operations_routes::bus_answer(
                        req, operations_routes::bus_json(servers, streams));
                });
            }));
}

}
