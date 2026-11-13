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
#include "ores.http/routes/reporting/report_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.reporting.api/messaging/report_definition_protocol.hpp"
#include "ores.reporting.api/messaging/report_error_event_protocol.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <array>
#include <cstddef>
#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::http::routes::reporting {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace iam_messaging = ores::iam::messaging;
namespace reporting_domain = ores::reporting::domain;
namespace reporting_messaging = ores::reporting::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;

/// The unit's own helpers, named apart from the other units that build into one
/// translation unit with it.
namespace reports {

/// The page a list opens on, as the record screen standard sets it.
constexpr std::uint32_t default_page_size = 15;

/// The largest page a caller may ask for, as the generated read allows.
constexpr std::uint32_t max_page_size = 1000;

/// The longest text a search may carry, which is the limit the service states.
constexpr std::size_t search_limit = 256;

/// The longest cut kind a list may narrow by.
constexpr std::size_t cut_kind_limit = 40;

/// The longest name an order may be stated by.
constexpr std::size_t sort_limit = 100;

/// The words a row's health cell can hold. The service derives them; this only
/// names them.
constexpr std::array<std::string_view, 5> health_words{
    "failed", "completed with errors", "running", "never run", "completed clean"};

/// What a person is told when an address names a record their parties do not
/// have.
constexpr std::string_view absent = "There is no such record, or you may not see it.";

/// What the list states when the request is not a page it reads.
constexpr std::string_view malformed =
    "A page names an offset, a limit of 1 to 1000, an order and a party.";

/// The read that fills each cell the list decorates, as the answer names it.
constexpr std::string_view parties_cell = "parties";
constexpr std::string_view latest_run_cell = "latestRun";
constexpr std::string_view health_cell = "health";
constexpr std::string_view errors_cell = "errors";
constexpr std::string_view next_fire_cell = "nextFire";

/// A read that failed, and the cell it would have filled.
struct unread final {
    std::string cell;
    std::string message;
};

/// The health word a run's stated word is, or nothing when it is no word.
std::optional<std::string> health_of(const std::string& stated) {
    const auto found = std::ranges::find(health_words, stated);
    if (found == health_words.end())
        return std::nullopt;
    return std::string(*found);
}

/// Whether a text states anything at all.
bool stated(const std::string& text) {
    return !text.empty();
}

/// The party a row names, or nothing when the read that names parties failed.
std::string party_field(const std::optional<std::vector<report_routes::party_name>>& parties,
                        const boost::uuids::uuid& id,
                        std::string report_routes::party_name::* member) {
    if (!parties)
        return std::string();
    for (const auto& party : *parties)
        if (party.id == id)
            return party.*member;
    return std::string();
}

/// Whether the parties read found a party.
bool party_found(const std::optional<std::vector<report_routes::party_name>>& parties,
                 const boost::uuids::uuid& id) {
    if (!parties)
        return false;
    return std::ranges::any_of(*parties, [&](const auto& party) { return party.id == id; });
}

/// The word the row's health cell holds.
std::optional<std::string>
health_of_row(const std::optional<std::vector<report_routes::run_row>>& runs,
              const report_routes::run_row* run,
              const std::vector<report_routes::health_row>& health) {
    if (run == nullptr)
        return runs ? std::optional<std::string>("never run") : std::nullopt;
    for (const auto& entry : health)
        if (entry.run_id == run->id)
            return health_of(entry.health);
    return std::nullopt;
}

/// The state name the row's health was derived from.
std::string state_of_row(const report_routes::run_row* run,
                         const std::vector<report_routes::health_row>& health) {
    if (run == nullptr)
        return std::string();
    for (const auto& entry : health)
        if (entry.run_id == run->id)
            return entry.state_name;
    return std::string();
}

/// The problem count of one run: nothing when the counts could not be read at
/// all, and zero when the run recorded no problem.
std::optional<std::int64_t>
count_of_row(const std::optional<std::vector<report_routes::count_row>>& counts,
             const report_routes::run_row* run,
             bool errors) {
    if (!counts)
        return std::nullopt;
    if (run == nullptr)
        return std::nullopt;
    for (const auto& entry : *counts)
        if (entry.run_id == run->id)
            return errors ? entry.errors : entry.warnings;
    return 0;
}

/// The latest run of one definition, or nothing when it never ran.
const report_routes::run_row* run_of(const std::optional<std::vector<report_routes::run_row>>& runs,
                                     const boost::uuids::uuid& definition_id) {
    if (!runs)
        return nullptr;
    for (const auto& run : *runs)
        if (run.definition_id == definition_id)
            return &run;
    return nullptr;
}

/// When the scheduler says one definition fires next.
std::optional<std::string>
next_fire_of(const std::optional<std::vector<report_routes::fire_row>>& fires,
             const boost::uuids::uuid& definition_id) {
    if (!fires)
        return std::nullopt;
    for (const auto& fire : *fires)
        if (fire.definition_id == definition_id)
            return fire.next_fire_at;
    return std::nullopt;
}

/// One row, as the list draws it.
std::string row_json(const reporting_domain::report_definition& definition,
                     const std::optional<std::vector<report_routes::party_name>>& parties,
                     const std::optional<std::vector<report_routes::run_row>>& runs,
                     const std::vector<report_routes::health_row>& health,
                     const std::optional<std::vector<report_routes::count_row>>& counts,
                     const std::optional<std::vector<report_routes::fire_row>>& fires) {
    const auto* run = run_of(runs, definition.id);
    std::optional<std::string> party_code;
    if (party_found(parties, definition.party_id)) {
        const auto code =
            party_field(parties, definition.party_id, &report_routes::party_name::short_code);
        if (stated(code))
            party_code = code;
    }

    std::string latest_run = "null";
    if (run != nullptr) {
        latest_run = "{\"id\":" + json::write(rr::as_text(run->id)) +
                     ",\"startedAt\":" + json::write(run->started_at) +
                     ",\"completedAt\":" + json::write(run->completed_at) +
                     ",\"errors\":" + json::write(count_of_row(counts, run, true)) +
                     ",\"warnings\":" + json::write(count_of_row(counts, run, false)) + "}";
    }

    return "{\"id\":" + json::write(rr::as_text(definition.id)) +
           ",\"name\":" + json::write(definition.name) +
           ",\"reportType\":" + json::write(definition.report_type) +
           ",\"cutKind\":" + json::write(definition.cut_kind_code) +
           ",\"partyId\":" + json::write(rr::as_text(definition.party_id)) + ",\"partyName\":" +
           json::write(
               party_field(parties, definition.party_id, &report_routes::party_name::name)) +
           ",\"partyCode\":" + json::write(party_code) +
           ",\"scheduleExpression\":" + json::write(definition.schedule_expression) +
           ",\"scheduled\":" + json::write(definition.scheduler_job_id.has_value()) +
           ",\"health\":" + json::write(health_of_row(runs, run, health)) +
           ",\"stateName\":" + json::write(state_of_row(run, health)) +
           ",\"latestRun\":" + latest_run +
           ",\"nextFireAt\":" + json::write(next_fire_of(fires, definition.id)) + "}";
}

}

}

std::string
report_routes::rows_json(const std::vector<ores::reporting::domain::report_definition>& definitions,
                         const std::optional<std::vector<party_name>>& parties,
                         const std::optional<std::vector<run_row>>& runs,
                         const std::vector<health_row>& health,
                         const std::optional<std::vector<count_row>>& counts,
                         const std::optional<std::vector<fire_row>>& fires) {
    std::vector<std::string> rows;
    for (const auto& definition : definitions)
        rows.push_back(reports::row_json(definition, parties, runs, health, counts, fires));
    return rr::array_of(rows);
}

void report_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using namespace reports;
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"reporting"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/reporting/reports")
            .summary("One page of the reports a party runs, one row per definition")
            .description("The row is the definition; the latest run, its health, its problem "
                         "counts, its next fire and the party's own name are decorations read "
                         "beside it, so a decoration that fails costs its own cell and says "
                         "which read refused it.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_report_definitions_request>([&] {
                    rr::refuse_unknown_params(req,
                                              {"offset",
                                               "limit",
                                               "sort",
                                               "descending",
                                               "party_id",
                                               "search",
                                               "cut_kind"});

                    reporting_messaging::list_report_definitions_request msg;
                    const auto offset = req.get_query_param("offset");
                    msg.offset = offset.empty() ? 0U : rr::read<std::uint32_t>(offset, "offset");
                    const auto limit = req.get_query_param("limit");
                    msg.limit =
                        limit.empty() ? default_page_size : rr::read<std::uint32_t>(limit, "limit");
                    if (msg.limit < 1 || msg.limit > max_page_size)
                        rr::invalid(std::string(malformed));
                    const auto sort = rr::trimmed(req.get_query_param("sort"));
                    if (rr::characters_of(sort) > sort_limit)
                        rr::invalid(std::string(malformed));
                    msg.order.field = sort;
                    const auto descending = req.get_query_param("descending");
                    if (descending.empty() || descending == "false")
                        msg.order.descending = false;
                    else if (descending == "true")
                        msg.order.descending = true;
                    else
                        rr::invalid(std::string(malformed));

                    const auto party = rr::trimmed(req.get_query_param("party_id"));
                    const auto search = rr::trimmed(req.get_query_param("search"));
                    const auto cut_kind = rr::trimmed(req.get_query_param("cut_kind"));
                    if (rr::characters_of(search) > search_limit ||
                        rr::characters_of(cut_kind) > cut_kind_limit)
                        rr::invalid(std::string(malformed));
                    const bool narrows = !party.empty() || !search.empty() || !cut_kind.empty();
                    if (narrows) {
                        msg.filter.emplace();
                        if (!party.empty()) {
                            if (!rr::is_uuid(party))
                                rr::invalid(std::string(malformed));
                            msg.filter->party_id = boost::uuids::string_generator()(party);
                        }
                        if (!search.empty())
                            msg.filter->search = search;
                        if (!cut_kind.empty())
                            msg.filter->cut_kind_code = std::optional<std::string>(cut_kind);
                    }

                    const auto page = forwarding::forward(req, session, msg);
                    rr::require_done(page.result);

                    std::vector<unread> unread_cells;
                    const auto fail = [&](std::string_view cell, const std::string& why) {
                        unread_cells.push_back({std::string(cell), why});
                    };
                    std::vector<boost::uuids::uuid> ids;
                    for (const auto& definition : page.definitions)
                        ids.push_back(definition.id);

                    std::optional<std::vector<party_name>> parties;
                    std::optional<std::vector<run_row>> runs;
                    std::optional<std::vector<fire_row>> fires;
                    std::vector<health_row> health;
                    std::optional<std::vector<count_row>> counts;

                    if (!ids.empty()) {
                        try {
                            const auto reply = forwarding::forward(
                                req, session, iam_messaging::get_my_parties_request{});
                            rr::require_done(reply.result);
                            std::vector<party_name> named;
                            for (const auto& party : reply.parties)
                                named.push_back({boost::uuids::string_generator()(party.party_id),
                                                 party.name,
                                                 party.short_code});
                            parties = std::move(named);
                        } catch (const rr::refusal& r) {
                            fail(parties_cell, r.message);
                        } catch (const std::exception&) {
                            fail(parties_cell, std::string(rr::unanswered));
                        }

                        try {
                            reporting_messaging::get_latest_report_instances_by_definition_request
                                latest;
                            latest.definition_ids = ids;
                            const auto reply = forwarding::forward(req, session, latest);
                            rr::require_done(reply.result);
                            std::vector<run_row> found;
                            for (const auto& instance : reply.instances)
                                found.push_back({instance.id,
                                                 instance.definition_id,
                                                 instance.started_at,
                                                 instance.completed_at});
                            runs = std::move(found);
                        } catch (const rr::refusal& r) {
                            fail(latest_run_cell, r.message);
                        } catch (const std::exception&) {
                            fail(latest_run_cell, std::string(rr::unanswered));
                        }

                        try {
                            reporting_messaging::get_next_fire_request next;
                            for (const auto& id : ids)
                                next.definition_ids.push_back(rr::as_text(id));
                            const auto reply = forwarding::forward(req, session, next);
                            rr::require_done(reply.result);
                            std::vector<fire_row> found;
                            for (const auto& schedule : reply.schedules)
                                found.push_back({schedule.definition_id, schedule.next_fire_at});
                            fires = std::move(found);
                        } catch (const rr::refusal& r) {
                            fail(next_fire_cell, r.message);
                        } catch (const std::exception&) {
                            fail(next_fire_cell, std::string(rr::unanswered));
                        }

                        std::vector<boost::uuids::uuid> run_ids;
                        if (runs)
                            for (const auto& run : *runs)
                                run_ids.push_back(run.id);

                        if (!run_ids.empty()) {
                            try {
                                reporting_messaging::get_report_instance_health_request stated;
                                stated.instance_ids = run_ids;
                                const auto reply = forwarding::forward(req, session, stated);
                                rr::require_done(reply.result);
                                for (const auto& entry : reply.health)
                                    health.push_back(
                                        {entry.report_instance_id, entry.state_name, entry.health});
                            } catch (const rr::refusal& r) {
                                fail(health_cell, r.message);
                            } catch (const std::exception&) {
                                fail(health_cell, std::string(rr::unanswered));
                            }

                            try {
                                reporting_messaging::get_report_error_counts_request counted;
                                counted.report_instance_ids = run_ids;
                                const auto reply = forwarding::forward(req, session, counted);
                                rr::require_done(reply.result);
                                std::vector<count_row> found;
                                for (const auto& entry : reply.counts)
                                    found.push_back(
                                        {entry.report_instance_id, entry.errors, entry.warnings});
                                counts = std::move(found);
                            } catch (const rr::refusal& r) {
                                fail(errors_cell, r.message);
                            } catch (const std::exception&) {
                                fail(errors_cell, std::string(rr::unanswered));
                            }
                        }
                    }

                    std::vector<std::string> unread_json;
                    for (const auto& entry : unread_cells)
                        unread_json.push_back("{\"cell\":" + json::write(entry.cell) +
                                              ",\"message\":" + json::write(entry.message) + "}");
                    return http_response::json(
                        "{\"rows\":" +
                        rows_json(page.definitions, parties, runs, health, counts, fires) +
                        ",\"total\":" + std::to_string(page.total) +
                        ",\"unread\":" + rr::array_of(unread_json) + "}");
                });
            }));
}

}
