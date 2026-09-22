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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_repository.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.refdata.core/repository/calendar_event_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/calendar_event_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/calendar_event_entity.hpp"
#include "ores.refdata.core/repository/calendar_event_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string calendar_event_repository::sql() {
    return generate_create_table_sql<calendar_event_entity>(lg());
}

ores::utility::domain::precondition
calendar_event_repository::replace_claim(context ctx, const domain::calendar_event& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::calendar_event
calendar_event_repository::apply_claim(context ctx,
                                       const domain::calendar_event& v,
                                       const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    switch (claim.kind) {
        case precondition_kind::must_not_exist:
            // Zero states that no current row exists, which is the one meaning the
            // store gives a zero version.
            t.version = 0;
            break;
        case precondition_kind::must_match_version:
            t.version = claim.version ? static_cast<int>(*claim.version) : 0;
            break;
        case precondition_kind::any: {
            // A caller that claims nothing still has to say what it replaces, so
            // the row is read and its version stated. A row that moved on between
            // this read and the write is a conflict the trigger raises, never a
            // silent overwrite.
            const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void calendar_event_repository::write(context ctx, const domain::calendar_event& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void calendar_event_repository::write(context ctx, const std::vector<domain::calendar_event>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void calendar_event_repository::write(context ctx,
                                      const domain::calendar_event& v,
                                      const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing calendar event. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(
        ctx, calendar_event_mapper::map(t), lg(), "Writing calendar event to database.");
}

void calendar_event_repository::write(
    context ctx,
    const std::vector<domain::calendar_event>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing calendar events. Count: " << v.size();
    std::vector<domain::calendar_event> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(
        ctx, calendar_event_mapper::map(batch), lg(), "Writing calendar events to database.");
}

std::vector<domain::calendar_event> calendar_event_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_event_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<calendar_event_entity, domain::calendar_event>(
        ctx,
        query,
        [](const auto& entities) { return calendar_event_mapper::map(entities); },
        lg(),
        "Reading latest calendar events");
}

std::vector<domain::calendar_event> calendar_event_repository::read_latest(context ctx,
                                                                           const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest calendar event. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_event_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<calendar_event_entity, domain::calendar_event>(
        ctx,
        query,
        [](const auto& entities) { return calendar_event_mapper::map(entities); },
        lg(),
        "Reading latest calendar event by id.");
}


std::vector<domain::calendar_event> calendar_event_repository::read_all(context ctx,
                                                                        const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all calendar event versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_event_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<calendar_event_entity, domain::calendar_event>(
        ctx,
        query,
        [](const auto& entities) { return calendar_event_mapper::map(entities); },
        lg(),
        "Reading all calendar event versions by id.");
}

std::optional<domain::calendar_event> calendar_event_repository::read_at_version(
    context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading calendar event at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_event_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<calendar_event_entity, domain::calendar_event>(
        ctx,
        query,
        [](const auto& entities) { return calendar_event_mapper::map(entities); },
        lg(),
        "Reading calendar event at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


std::vector<domain::calendar_event> calendar_event_repository::read_latest_by_calendar_code(
    context ctx, const std::string& calendar_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest calendar events. calendar_code: " << calendar_code
                               << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_event_entity>> |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<calendar_event_entity, domain::calendar_event>(
        ctx,
        query,
        [](const auto& entities) { return calendar_event_mapper::map(entities); },
        lg(),
        "Reading latest calendar events by calendar_code.");
}

std::uint32_t calendar_event_repository::get_total_calendar_event_count_by_calendar_code(
    context ctx, const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active calendar events count. calendar_code: "
                               << calendar_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<calendar_event_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active calendar events count by calendar_code: " << count;
    return count;
}


std::vector<domain::calendar_event> calendar_event_repository::read_by_calendar_code_as_of(
    context ctx,
    const std::string& calendar_code,
    std::chrono::system_clock::time_point valid_from_bound,
    std::chrono::system_clock::time_point valid_to_bound) {
    BOOST_LOG_SEV(lg(), debug) << "Reading calendar events as of window. calendar_code: "
                               << calendar_code;

    const auto vf(
        make_timestamp(ores::platform::time::datetime::to_db_string(valid_from_bound), lg()));
    const auto vt(
        make_timestamp(ores::platform::time::datetime::to_db_string(valid_to_bound), lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_event_entity>> |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "valid_from"_c < vt.value() && "valid_to"_c > vf.value()) |
                       order_by("id"_c);

    return execute_read_query<calendar_event_entity, domain::calendar_event>(
        ctx,
        query,
        [](const auto& entities) { return calendar_event_mapper::map(entities); },
        lg(),
        "Reading calendar events as of window by calendar_code.");
}

std::vector<domain::calendar_event> calendar_event_repository::read_latest_by_diary_entry_type(
    context ctx, const std::string& diary_entry_type, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest calendar events. diary_entry_type: "
                               << diary_entry_type << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_event_entity>> |
                       where("tenant_id"_c == tid && "diary_entry_type"_c == diary_entry_type &&
                             "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<calendar_event_entity, domain::calendar_event>(
        ctx,
        query,
        [](const auto& entities) { return calendar_event_mapper::map(entities); },
        lg(),
        "Reading latest calendar events by diary_entry_type.");
}

std::uint32_t calendar_event_repository::get_total_calendar_event_count_by_diary_entry_type(
    context ctx, const std::string& diary_entry_type) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active calendar events count. diary_entry_type: " << diary_entry_type;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<calendar_event_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "diary_entry_type"_c == diary_entry_type &&
                             "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active calendar events count by diary_entry_type: "
                               << count;
    return count;
}


std::vector<domain::calendar_event> calendar_event_repository::read_by_diary_entry_type_as_of(
    context ctx,
    const std::string& diary_entry_type,
    std::chrono::system_clock::time_point valid_from_bound,
    std::chrono::system_clock::time_point valid_to_bound) {
    BOOST_LOG_SEV(lg(), debug) << "Reading calendar events as of window. diary_entry_type: "
                               << diary_entry_type;

    const auto vf(
        make_timestamp(ores::platform::time::datetime::to_db_string(valid_from_bound), lg()));
    const auto vt(
        make_timestamp(ores::platform::time::datetime::to_db_string(valid_to_bound), lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_event_entity>> |
                       where("tenant_id"_c == tid && "diary_entry_type"_c == diary_entry_type &&
                             "valid_from"_c < vt.value() && "valid_to"_c > vf.value()) |
                       order_by("id"_c);

    return execute_read_query<calendar_event_entity, domain::calendar_event>(
        ctx,
        query,
        [](const auto& entities) { return calendar_event_mapper::map(entities); },
        lg(),
        "Reading calendar events as of window by diary_entry_type.");
}

calendar_event_repository::remove_status calendar_event_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing calendar event. " << "id: " << id;
    const auto current = read_latest(ctx, id);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().version) != *version)
        return remove_status::conflicting;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<calendar_event_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value() &&
                             "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing calendar event from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void calendar_event_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::calendar_event>
calendar_event_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest calendar events with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_event_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<calendar_event_entity, domain::calendar_event>(
        ctx,
        query,
        [](const auto& entities) { return calendar_event_mapper::map(entities); },
        lg(),
        "Reading latest calendar events with pagination.");
}

std::uint32_t calendar_event_repository::get_total_calendar_event_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active calendar event count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<calendar_event_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active calendar event count: " << count;
    return count;
}

std::vector<domain::calendar_event>
calendar_event_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_event_entity>> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    auto result = execute_read_query<calendar_event_entity, domain::calendar_event>(
        ctx,
        query,
        [](const auto& entities) { return calendar_event_mapper::map(entities); },
        lg(),
        "Reading latest calendar events by ids.");
    return result;
}

void calendar_event_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<calendar_event_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing calendar events.");
}


}
