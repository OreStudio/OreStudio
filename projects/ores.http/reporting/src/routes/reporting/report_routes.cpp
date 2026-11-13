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
#include "ores.http/routes/reporting/report_health.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.reporting.api/messaging/configuration_protocol.hpp"
#include "ores.reporting.api/messaging/configuration_type_protocol.hpp"
#include "ores.reporting.api/messaging/report_configuration_protocol.hpp"
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

namespace {}

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

/// Whether a text states anything at all.
bool has_text(const std::string& text) {
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
            return report_health::word_of(entry.health);
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

/// The problem counts of one run: nothing when the counts could not be read at
/// all or the run has none, and a row of zeros when the run recorded no problem.
std::optional<report_routes::count_row>
counted_of_row(const std::optional<std::vector<report_routes::count_row>>& counts,
               const report_routes::run_row* run) {
    if (!counts || run == nullptr)
        return std::nullopt;
    for (const auto& entry : *counts)
        if (entry.run_id == run->id)
            return entry;
    return report_routes::count_row{run->id, 0, 0};
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

/// Everything the screens read beside a definition, and the reads that failed.
struct composed final {
    std::optional<std::vector<report_routes::party_name>> parties;
    std::optional<std::vector<report_routes::run_row>> runs;
    std::vector<report_routes::health_row> health;
    std::optional<std::vector<report_routes::count_row>> counts;
    std::optional<std::vector<report_routes::fire_row>> fires;
    std::vector<unread> unread;
};

/// One row, as the list draws it.
std::string row_json(const reporting_domain::report_definition& definition,
                     const composed& decorated) {
    const auto* run = run_of(decorated.runs, definition.id);
    std::optional<std::string> party_code;
    if (party_found(decorated.parties, definition.party_id)) {
        const auto code = party_field(
            decorated.parties, definition.party_id, &report_routes::party_name::short_code);
        if (has_text(code))
            party_code = code;
    }

    const auto counted = counted_of_row(decorated.counts, run);
    std::string latest_run = "null";
    if (run != nullptr) {
        latest_run =
            "{\"id\":" + json::write(rr::as_text(run->id)) +
            ",\"startedAt\":" + json::write(run->started_at) +
            ",\"completedAt\":" + json::write(run->completed_at) + ",\"errors\":" +
            json::write(counted ? std::optional<std::int64_t>(counted->errors) : std::nullopt) +
            ",\"warnings\":" +
            json::write(counted ? std::optional<std::int64_t>(counted->warnings) : std::nullopt) +
            "}";
    }

    return "{\"id\":" + json::write(rr::as_text(definition.id)) +
           ",\"name\":" + json::write(definition.name) +
           ",\"reportType\":" + json::write(definition.report_type) +
           ",\"cutKind\":" + json::write(definition.cut_kind_code) +
           ",\"partyId\":" + json::write(rr::as_text(definition.party_id)) + ",\"partyName\":" +
           json::write(party_field(
               decorated.parties, definition.party_id, &report_routes::party_name::name)) +
           ",\"partyCode\":" + json::write(party_code) +
           ",\"scheduleExpression\":" + json::write(definition.schedule_expression) +
           ",\"scheduled\":" + json::write(definition.scheduler_job_id.has_value()) +
           ",\"health\":" + json::write(health_of_row(decorated.runs, run, decorated.health)) +
           ",\"stateName\":" + json::write(state_of_row(run, decorated.health)) +
           ",\"latestRun\":" + latest_run +
           ",\"nextFireAt\":" + json::write(next_fire_of(decorated.fires, definition.id)) + "}";
}


using edit_change = report_routes::edit_change;

/// A write as the form states it: the version read, the fields and the reason.
struct edit_body {
    int version{};
    edit_change change;
    rr::stated_intent intent;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<edit_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"version", &edit_body::version},
                                            field{"change", &edit_body::change},
                                            field{"intent", &edit_body::intent}};
}

/// The sentence a write states when the form is not the shape it sends.
constexpr std::string_view malformed_write =
    "A write names the version it was read at, the fields it changes and its reason.";

/// A text of at most this many characters, as the form states it.
bool within(const std::string& text, std::size_t most) {
    return rr::characters_of(text) <= most;
}

/// A code the form names: 1 to this many characters once trimmed.
bool code_of(const std::string& text, std::size_t most) {
    const auto trimmed = rr::trimmed(text);
    return !trimmed.empty() && rr::characters_of(trimmed) <= most;
}

/// The edit as the form states it, refused when a field is not the shape the
/// form offers.
void check_edit(const edit_change& change) {
    const bool ok = within(change.description, 2000) && code_of(change.report_type, 100) &&
                    code_of(change.concurrency_policy, 100) &&
                    code_of(change.schedule_expression, 200) &&
                    (!change.regulatory_book_type || !*change.regulatory_book_type ||
                     code_of(**change.regulatory_book_type, 100)) &&
                    within(change.pre_processing, 2000) && within(change.prepared_input_key, 500) &&
                    within(change.post_processing, 2000) && within(change.skipped_families, 2000);
    if (!ok)
        rr::invalid(std::string(malformed_write));
}

/// The unread list as the answer states it.
std::string unread_json(const composed& decorated) {
    std::vector<std::string> written;
    for (const auto& entry : decorated.unread)
        written.push_back("{\"cell\":" + json::write(entry.cell) +
                          ",\"message\":" + json::write(entry.message) + "}");
    return rr::array_of(written);
}

/// One definition's party name and short code, or an empty cell when the
/// parties read failed or does not hold the party.
std::string definition_row_json(const reporting_domain::report_definition& definition,
                                const composed& decorated) {
    std::optional<std::string> party_code;
    if (party_found(decorated.parties, definition.party_id)) {
        const auto code = party_field(
            decorated.parties, definition.party_id, &report_routes::party_name::short_code);
        if (has_text(code))
            party_code = code;
    }
    return "{\"id\":" + json::write(rr::as_text(definition.id)) +
           ",\"name\":" + json::write(definition.name) +
           ",\"description\":" + json::write(definition.description) +
           ",\"report_type\":" + json::write(definition.report_type) +
           ",\"party_id\":" + json::write(rr::as_text(definition.party_id)) + ",\"party_name\":" +
           json::write(party_field(
               decorated.parties, definition.party_id, &report_routes::party_name::name)) +
           ",\"party_code\":" + json::write(party_code) +
           ",\"schedule_expression\":" + json::write(definition.schedule_expression) +
           ",\"scheduled\":" + json::write(definition.scheduler_job_id.has_value()) +
           ",\"concurrency_policy\":" + json::write(definition.concurrency_policy) +
           ",\"is_official\":" + json::write(definition.is_official) +
           ",\"regulatory_book_type\":" + json::write(definition.regulatory_book_type) +
           ",\"pre_processing\":" + json::write(definition.pre_processing) +
           ",\"prepared_input_key\":" + json::write(definition.prepared_input_key) +
           ",\"post_processing\":" + json::write(definition.post_processing) +
           ",\"skipped_families\":" + json::write(definition.skipped_families) +
           ",\"version\":" + std::to_string(definition.version) +
           ",\"modified_by\":" + json::write(definition.modified_by) +
           ",\"performed_by\":" + json::write(definition.performed_by) +
           ",\"change_reason_code\":" + json::write(definition.change_reason_code) +
           ",\"change_commentary\":" + json::write(definition.change_commentary) +
           ",\"recorded_at\":" + json::write(definition.recorded_at) + "}";
}

/// One choice a shared name offers, as the page reads it.
std::string choice_json(const reporting_domain::report_definition& definition,
                        const composed& decorated) {
    const auto* run = run_of(decorated.runs, definition.id);
    return "{\"id\":" + json::write(rr::as_text(definition.id)) +
           ",\"name\":" + json::write(definition.name) + ",\"partyName\":" +
           json::write(party_field(
               decorated.parties, definition.party_id, &report_routes::party_name::name)) +
           ",\"scheduleExpression\":" + json::write(definition.schedule_expression) +
           ",\"health\":" + json::write(health_of_row(decorated.runs, run, decorated.health)) +
           ",\"stateName\":" + json::write(state_of_row(run, decorated.health)) +
           ",\"hasRun\":" + json::write(run != nullptr) + "}";
}

/// Every read the rows are composed from, each one that fails recorded against
/// the cell it would have filled.
composed compose_of(const http_request& req,
                    nats_client& session,
                    const std::vector<reporting_domain::report_definition>& definitions) {
    composed decorated;
    const auto fail = [&](std::string_view cell, const std::string& why) {
        decorated.unread.push_back({std::string(cell), why});
    };
    const auto read_or_fail = [&](std::string_view cell, const auto& work) {
        try {
            work();
        } catch (const rr::refusal& r) {
            fail(cell, r.message);
        } catch (const std::exception&) {
            fail(cell, std::string(rr::unanswered));
        }
    };

    std::vector<boost::uuids::uuid> ids;
    for (const auto& definition : definitions)
        ids.push_back(definition.id);
    if (ids.empty())
        return decorated;

    read_or_fail(parties_cell, [&] {
        const auto reply =
            forwarding::forward(req, session, iam_messaging::get_my_parties_request{});
        rr::require_done(reply.result);
        std::vector<report_routes::party_name> named;
        for (const auto& party : reply.parties)
            named.push_back(
                {boost::uuids::string_generator()(party.party_id), party.name, party.short_code});
        decorated.parties = std::move(named);
    });

    read_or_fail(latest_run_cell, [&] {
        reporting_messaging::get_latest_report_instances_by_definition_request latest;
        latest.definition_ids = ids;
        const auto reply = forwarding::forward(req, session, latest);
        rr::require_done(reply.result);
        std::vector<report_routes::run_row> found;
        for (const auto& instance : reply.instances)
            found.push_back(
                {instance.id, instance.definition_id, instance.started_at, instance.completed_at});
        decorated.runs = std::move(found);
    });

    read_or_fail(next_fire_cell, [&] {
        reporting_messaging::get_next_fire_request next;
        for (const auto& id : ids)
            next.definition_ids.push_back(rr::as_text(id));
        const auto reply = forwarding::forward(req, session, next);
        rr::require_done(reply.result);
        std::vector<report_routes::fire_row> found;
        for (const auto& schedule : reply.schedules)
            found.push_back({schedule.definition_id, schedule.next_fire_at});
        decorated.fires = std::move(found);
    });

    std::vector<boost::uuids::uuid> run_ids;
    if (decorated.runs)
        for (const auto& run : *decorated.runs)
            run_ids.push_back(run.id);
    if (run_ids.empty())
        return decorated;

    read_or_fail(health_cell, [&] {
        reporting_messaging::get_report_instance_health_request stated;
        stated.instance_ids = run_ids;
        const auto reply = forwarding::forward(req, session, stated);
        rr::require_done(reply.result);
        for (const auto& entry : reply.health)
            decorated.health.push_back({entry.report_instance_id, entry.state_name, entry.health});
    });

    read_or_fail(errors_cell, [&] {
        reporting_messaging::get_report_error_counts_request counted;
        counted.report_instance_ids = run_ids;
        const auto reply = forwarding::forward(req, session, counted);
        rr::require_done(reply.result);
        std::vector<report_routes::count_row> found;
        for (const auto& entry : reply.counts)
            found.push_back({entry.report_instance_id, entry.errors, entry.warnings});
        decorated.counts = std::move(found);
    });

    return decorated;
}

/// One definition, by the id that names it.
reporting_domain::report_definition
definition_by_id(const http_request& req, nats_client& session, const boost::uuids::uuid& id) {
    reporting_messaging::list_report_definitions_request msg;
    msg.offset = 0;
    msg.limit = 1;
    msg.filter.emplace();
    msg.filter->id_one_of = std::vector{id};
    const auto reply = forwarding::forward(req, session, msg);
    rr::require_done(reply.result);
    if (reply.definitions.empty())
        rr::refuse(http_status::not_found, "not-found", std::string(absent));
    return reply.definitions.front();
}

/// The definitions the caller's parties hold that carry a name.
///
/// The read filters on no name, so the definitions the session can see are read
/// and matched here. A name is unique only within a party, so several may match.
std::vector<reporting_domain::report_definition>
definitions_named(const http_request& req, nats_client& session, const std::string& name) {
    reporting_messaging::list_report_definitions_request msg;
    const auto all = forwarding::forward_all(
        req, session, msg, &reporting_messaging::list_report_definitions_response::definitions);
    std::vector<reporting_domain::report_definition> named;
    for (const auto& definition : all)
        if (definition.name == name)
            named.push_back(definition);
    return named;
}

/// What an address segment names, when it is not an id.
struct named_address final {
    std::optional<reporting_domain::report_definition> one;
    std::vector<reporting_domain::report_definition> many;
};

/// The definition an address segment names, or the choices when the name is shared.
named_address
address_named(const http_request& req, nats_client& session, const std::string& name) {
    const auto named = definitions_named(req, session, name);
    named_address answer;
    if (named.size() == 1)
        answer.one = named.front();
    else if (named.size() > 1)
        answer.many = named;
    return answer;
}

/// The detail page: the row, the record under the names it holds, and the reads
/// that failed.
std::string detail_json(const composed& decorated,
                        const reporting_domain::report_definition& definition) {
    return "{\"report\":" + row_json(definition, decorated) +
           ",\"definition\":" + definition_row_json(definition, decorated) +
           ",\"unread\":" + unread_json(decorated) + "}";
}


/// The refusal a shared name states, with the choices beside its words.
std::string shared_name_body(const std::string& name,
                             std::vector<reporting_domain::report_definition> named,
                             const composed& decorated) {
    std::ranges::stable_sort(named, [&](const auto& left, const auto& right) {
        return party_field(decorated.parties, left.party_id, &report_routes::party_name::name) <
               party_field(decorated.parties, right.party_id, &report_routes::party_name::name);
    });
    std::vector<std::string> choices;
    for (const auto& definition : named)
        choices.push_back(choice_json(definition, decorated));
    const auto message = std::to_string(named.size()) +
                         " reports of the parties you work in are named " + name +
                         ". Choose the one you want.";
    return "{\"choices\":" + rr::array_of(choices) +
           ",\"code\":\"conflict\",\"message\":" + json::write(message) + "}";
}

/// The answer a shared name states: a conflict whose body names the choices.
http_response shared_name_answer(const std::string& name,
                                 const std::vector<reporting_domain::report_definition>& named,
                                 const composed& decorated) {
    http_response answer;
    answer.status = http_status::conflict;
    answer.body = shared_name_body(name, named, decorated);
    return answer;
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
    reports::composed decorated;
    decorated.parties = parties;
    decorated.runs = runs;
    decorated.health = health;
    decorated.counts = counts;
    decorated.fires = fires;
    std::vector<std::string> rows;
    for (const auto& definition : definitions)
        rows.push_back(reports::row_json(definition, decorated));
    return rr::array_of(rows);
}

ores::reporting::messaging::report_definition_write
report_routes::write_of(const ores::reporting::domain::report_definition& definition,
                        const edit_change& change) {
    ores::reporting::messaging::report_definition_write write;
    write.id = definition.id;
    write.name = definition.name;
    write.party_id = definition.party_id;
    write.description = change.description;
    write.report_type = rr::trimmed(change.report_type);
    write.fsm_state_id = definition.fsm_state_id;
    write.schedule_expression = rr::trimmed(change.schedule_expression);
    write.concurrency_policy = rr::trimmed(change.concurrency_policy);
    write.scheduler_job_id = definition.scheduler_job_id;
    write.run_grant_id = definition.run_grant_id;
    write.pre_processing = change.pre_processing;
    write.prepared_input_key = change.prepared_input_key;
    write.post_processing = change.post_processing;
    write.skipped_families = change.skipped_families;
    write.is_official = change.is_official;
    write.regulatory_book_type = change.regulatory_book_type ? *change.regulatory_book_type :
                                                               definition.regulatory_book_type;
    // The cut a report selects is not on the form, so the write keeps the one
    // the record holds: a write of the form's fields alone would clear it.
    write.cut_kind_code = definition.cut_kind_code;
    write.cut_id = definition.cut_id;
    return write;
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

                    const auto decorated = compose_of(req, session, page.definitions);
                    return http_response::json("{\"rows\":" +
                                               rows_json(page.definitions,
                                                         decorated.parties,
                                                         decorated.runs,
                                                         decorated.health,
                                                         decorated.counts,
                                                         decorated.fires) +
                                               ",\"total\":" + std::to_string(page.total) +
                                               ",\"unread\":" + unread_json(decorated) + "}");
                });
            }));

    add(router->get("/api/reporting/reports/by-name/{name}")
            .summary("The id of the one definition that carries a name")
            .description("The cheap lookup behind an address an older page saved. A name the "
                         "caller's parties share is refused with the choices, so a shared name is "
                         "never guessed at.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_report_definitions_request>([&] {
                    const auto name = rr::trimmed(req.get_path_param("name"));
                    if (name.empty() || rr::characters_of(name) > 200)
                        rr::invalid("A report is named by a name of 1 to 200 characters.");
                    const auto resolved = address_named(req, session, name);
                    if (resolved.one)
                        return http_response::json(
                            "{\"id\":" + json::write(rr::as_text(resolved.one->id)) + "}");
                    if (resolved.many.empty())
                        rr::refuse(http_status::not_found, "not-found", std::string(absent));
                    const auto decorated = compose_of(req, session, resolved.many);
                    return shared_name_answer(name, resolved.many, decorated);
                });
            }));

    add(router->get("/api/reporting/reports/{segment}")
            .summary("One definition's page, by the id or the name the address carries")
            .description("An id opens that record. Anything else is the name an older address "
                         "saved: a name one definition carries answers with that record, and a "
                         "shared name answers with the choices rather than a guess.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_report_definitions_request>([&] {
                    const auto segment = req.get_path_param("segment");
                    if (rr::is_uuid(segment)) {
                        const auto definition = definition_by_id(
                            req, session, boost::uuids::string_generator()(segment));
                        return http_response::json(
                            detail_json(compose_of(req, session, {definition}), definition));
                    }
                    const auto name = rr::trimmed(segment);
                    if (name.empty() || rr::characters_of(name) > 200)
                        rr::invalid("A report is named by a name of 1 to 200 characters.");
                    const auto resolved = address_named(req, session, name);
                    if (resolved.one) {
                        const auto decorated = compose_of(req, session, {*resolved.one});
                        return http_response::json(detail_json(decorated, *resolved.one));
                    }
                    if (resolved.many.empty())
                        rr::refuse(http_status::not_found, "not-found", std::string(absent));
                    const auto decorated = compose_of(req, session, resolved.many);
                    return shared_name_answer(name, resolved.many, decorated);
                });
            }));

    add(router->get("/api/reporting/reports/{id}/configurations")
            .summary("The configurations one definition binds, in the order of their kinds")
            .description("Each binding states its kind's own name and the configuration's name, "
                         "so the page lists them and links to each. An account that may not read "
                         "the kinds still sees the bindings, under the code.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_report_configurations_request>([&] {
                    const auto raw = req.get_path_param("id");
                    if (!rr::is_uuid(raw))
                        rr::invalid("A definition is named by its id.");
                    const auto id = boost::uuids::string_generator()(raw);

                    reporting_messaging::list_report_configurations_request binding_msg;
                    binding_msg.filter.emplace();
                    binding_msg.filter->report_definition_id = id;
                    const auto bindings = forwarding::forward_all(
                        req,
                        session,
                        binding_msg,
                        &reporting_messaging::list_report_configurations_response::
                            report_configurations);

                    std::vector<reporting_domain::configuration_type> kinds;
                    try {
                        kinds = forwarding::forward_all(
                            req,
                            session,
                            reporting_messaging::list_configuration_types_request{},
                            &reporting_messaging::list_configuration_types_response::types);
                    } catch (const rr::refusal& r) {
                        if (r.status != http_status::forbidden)
                            throw;
                    }

                    std::vector<boost::uuids::uuid> configuration_ids;
                    for (const auto& binding : bindings)
                        configuration_ids.push_back(binding.configuration_id);
                    std::vector<reporting_domain::configuration> configurations;
                    if (!configuration_ids.empty()) {
                        reporting_messaging::list_configurations_request msg;
                        msg.filter.emplace();
                        msg.filter->id_one_of = configuration_ids;
                        configurations = forwarding::forward_all(
                            req,
                            session,
                            msg,
                            &reporting_messaging::list_configurations_response::configurations);
                    }

                    /*
                     * A kind the caller may read states its own name. The kinds are a
                     * closed set read whole, and a binding whose kind was refused keeps
                     * the code and sorts before every kind that has one, as the BFF did.
                     */
                    const auto kind_at = [&](const std::string& code) {
                        return std::ranges::find_if(
                            kinds, [&](const auto& kind) { return kind.code == code; });
                    };
                    const auto order_of = [&](const std::string& code) -> int {
                        const auto at = kind_at(code);
                        return at == kinds.end() ? -1 : static_cast<int>(at - kinds.begin());
                    };
                    const auto type_name_of = [&](const std::string& code) {
                        const auto at = kind_at(code);
                        return at == kinds.end() ? code : at->name;
                    };
                    std::vector<reporting_domain::report_configuration> ordered = bindings;
                    std::ranges::stable_sort(ordered, [&](const auto& left, const auto& right) {
                        return order_of(left.configuration_type_code) <
                               order_of(right.configuration_type_code);
                    });

                    std::vector<std::string> written;
                    for (const auto& binding : ordered) {
                        std::string name;
                        for (const auto& configuration : configurations)
                            if (configuration.id == binding.configuration_id) {
                                name = configuration.name;
                                break;
                            }
                        written.push_back(
                            "{\"id\":" + json::write(rr::as_text(binding.id)) + ",\"typeCode\":" +
                            json::write(binding.configuration_type_code) + ",\"typeName\":" +
                            json::write(type_name_of(binding.configuration_type_code)) +
                            ",\"configurationId\":" +
                            json::write(rr::as_text(binding.configuration_id)) +
                            ",\"name\":" + json::write(name) + "}");
                    }
                    return http_response::json("{\"bindings\":" + rr::array_of(written) + "}");
                });
            }));

    add(router->put("/api/reporting/reports/{id}")
            .summary("Write one report definition against the version it was read at")
            .description("The form offers ten fields and the record holds more, so the write is "
                         "assembled from the definition just read and only the offered fields are "
                         "replaced. A save cannot blank a field the screen does not show.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::put_report_definition_request>([&] {
                    const auto raw = req.get_path_param("id");
                    if (!rr::is_uuid(raw))
                        rr::invalid("A definition is named by its id.");
                    const auto id = boost::uuids::string_generator()(raw);
                    const auto body = rr::body_of<edit_body>(
                        req,
                        {"version", "change", "intent"},
                        "A write names the version it was read at, the fields it changes and its "
                        "reason.");
                    check_edit(body.change);

                    const auto definition = definition_by_id(req, session, id);
                    reporting_messaging::put_report_definition_request msg;
                    msg.change.write = write_of(definition, body.change);
                    msg.change.precondition.kind =
                        ores::utility::domain::precondition_kind::must_match_version;
                    msg.change.precondition.version = static_cast<std::uint32_t>(body.version);
                    msg.intent =
                        rr::intent_of(body.intent).value_or(ores::utility::domain::change_intent{});

                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_done(reply.result);
                    const auto written = reply.report_definition ?
                                             *reply.report_definition :
                                             definition_by_id(req, session, id);
                    return http_response::json(
                        detail_json(compose_of(req, session, {written}), written));
                });
            }));
}

}
