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
#include "ores.compute.api/messaging/batch_protocol.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
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
namespace compute_messaging = ores::compute::messaging;

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


/// The fields the report edit form offers, and the only ones a save may change.
struct edit_change {
    std::string description;
    std::string report_type;
    std::string concurrency_policy;
    std::string schedule_expression;
    bool is_official{};
    std::optional<std::string> regulatory_book_type;
    std::string pre_processing;
    std::string prepared_input_key;
    std::string post_processing;
    std::string skipped_families;
};

/// A write as the form states it: the version read, the fields and the reason.
struct edit_body {
    int version{};
    edit_change change;
    rr::stated_intent intent;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<edit_change>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"description", &edit_change::description},
        field{"report_type", &edit_change::report_type},
        field{"concurrency_policy", &edit_change::concurrency_policy},
        field{"schedule_expression", &edit_change::schedule_expression},
        field{"is_official", &edit_change::is_official},
        field{"regulatory_book_type", &edit_change::regulatory_book_type},
        field{"pre_processing", &edit_change::pre_processing},
        field{"prepared_input_key", &edit_change::prepared_input_key},
        field{"post_processing", &edit_change::post_processing},
        field{"skipped_families", &edit_change::skipped_families}};
}

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
                    (!change.regulatory_book_type || code_of(*change.regulatory_book_type, 100)) &&
                    within(change.pre_processing, 2000) && within(change.prepared_input_key, 500) &&
                    within(change.post_processing, 2000) && within(change.skipped_families, 2000);
    if (!ok)
        rr::invalid(std::string(malformed_write));
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
        if (stated(code))
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
    return "{\"report\":" +
           row_json(definition,
                    decorated.parties,
                    decorated.runs,
                    decorated.health,
                    decorated.counts,
                    decorated.fires) +
           ",\"definition\":" + definition_row_json(definition, decorated) +
           ",\"unread\":" + unread_json(decorated) + "}";
}


/// The most runs of one report a page asks for.
constexpr std::uint32_t runs_of_report_limit = 200;

/// One run as the runs list draws it.
struct run_view final {
    boost::uuids::uuid id;
    std::string name;
    std::string state_name;
    std::optional<std::string> health;
    std::optional<std::chrono::system_clock::time_point> started_at;
    std::optional<std::chrono::system_clock::time_point> completed_at;
    std::optional<std::chrono::system_clock::time_point> priced_at;
};

/// The time from a run's start to its completion, or nothing until both are
/// known and the completion does not precede the start.
std::optional<std::int64_t> duration_ms(const run_view& run) {
    if (!run.started_at || !run.completed_at)
        return std::nullopt;
    const auto elapsed =
        std::chrono::duration_cast<std::chrono::milliseconds>(*run.completed_at - *run.started_at);
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
           ",\"duration_ms\":" + json::write(duration_ms(run)) +
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
                        run.health = health_of(entry.health);
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
    std::string definition_name;
    std::string party_name;
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
    } catch (const rr::refusal&) {
    } catch (const std::exception&) {
    }
    return std::nullopt;
}

/// The name of the definition a run belongs to, or nothing when it cannot be read.
std::string
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
    } catch (const rr::refusal&) {
    } catch (const std::exception&) {
    }
    return std::string();
}

/// The name of a party the caller's own account works in, or nothing.
std::string
party_name_of(const http_request& req, nats_client& session, const boost::uuids::uuid& id) {
    try {
        const auto reply =
            forwarding::forward(req, session, iam_messaging::get_my_parties_request{});
        rr::require_done(reply.result);
        for (const auto& party : reply.parties)
            if (party.party_id == rr::as_text(id))
                return party.name;
    } catch (const rr::refusal&) {
    } catch (const std::exception&) {
    }
    return std::string();
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
    } catch (const rr::refusal&) {
    } catch (const std::exception&) {
    }
    return std::nullopt;
}

/// One run's page as the screen reads it.
std::string run_detail_json(const run_detail& detail) {
    const auto& run = detail.instance;
    const auto duration = [&]() -> std::optional<std::int64_t> {
        if (!run.started_at || !run.completed_at)
            return std::nullopt;
        const auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(
            *run.completed_at - *run.started_at);
        if (elapsed.count() < 0)
            return std::nullopt;
        return elapsed.count();
    }();

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
           ",\"duration_ms\":" + json::write(duration) +
           ",\"priced_at\":" + json::write(run.priced_at) +
           ",\"price_note\":" + json::write(run.price_note) +
           ",\"batch_id\":" + json::write(detail.batch_id) +
           ",\"modified_by\":" + json::write(run.modified_by) +
           ",\"recorded_at\":" + json::write(run.recorded_at) + "}";
}

/// The one thing a problem names, as a person reads it: the curve a curve
/// failure is about, otherwise the record the engine named.
std::string about_of(const reporting_domain::report_error_event& event) {
    if (event.scope_code == "Curve" && event.cause && !event.cause->empty())
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

/// The refusal a shared name states, with the choices beside its words.
std::string shared_name_body(const std::string& name,
                             const std::vector<reporting_domain::report_definition>& named,
                             const composed& decorated) {
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
                        detail.health = health_of(stated->health);
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
                    std::vector<std::string> written;
                    for (const auto& run :
                         runs_of_definition(req, session, boost::uuids::string_generator()(raw)))
                        written.push_back(run_json(run));
                    return http_response::json("{\"runs\":" + rr::array_of(written) + "}");
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

                    std::vector<std::string> kind_order;
                    try {
                        reporting_messaging::list_configuration_types_request kinds;
                        const auto reply = forwarding::forward(req, session, kinds);
                        rr::require_done(reply.result);
                        for (const auto& kind : reply.types)
                            kind_order.push_back(kind.code);
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

                    const auto order_of = [&](const std::string& code) {
                        const auto at = std::ranges::find(kind_order, code);
                        return at == kind_order.end() ?
                                   kind_order.size() :
                                   static_cast<std::size_t>(at - kind_order.begin());
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
                            "{\"id\":" + json::write(rr::as_text(binding.id)) +
                            ",\"typeCode\":" + json::write(binding.configuration_type_code) +
                            ",\"typeName\":" + json::write(binding.configuration_type_code) +
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
                    auto& write = msg.change.write;
                    write.id = definition.id;
                    write.name = definition.name;
                    write.party_id = definition.party_id;
                    write.description = body.change.description;
                    write.report_type = rr::trimmed(body.change.report_type);
                    write.fsm_state_id = definition.fsm_state_id;
                    write.schedule_expression = rr::trimmed(body.change.schedule_expression);
                    write.concurrency_policy = rr::trimmed(body.change.concurrency_policy);
                    write.scheduler_job_id = definition.scheduler_job_id;
                    write.run_grant_id = definition.run_grant_id;
                    write.pre_processing = body.change.pre_processing;
                    write.prepared_input_key = body.change.prepared_input_key;
                    write.post_processing = body.change.post_processing;
                    write.skipped_families = body.change.skipped_families;
                    write.is_official = body.change.is_official;
                    write.regulatory_book_type = body.change.regulatory_book_type ?
                                                     body.change.regulatory_book_type :
                                                     definition.regulatory_book_type;
                    /*
                     * The cut a report selects is not on the form, so the write keeps the one
                     * the record holds. The BFF omitted it, which cleared it on every save.
                     */
                    write.cut_kind_code = definition.cut_kind_code;
                    write.cut_id = definition.cut_id;
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
