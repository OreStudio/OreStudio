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
#include "ores.refdata.core/repository/currency_calendar_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/currency_calendar_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/currency_calendar_entity.hpp"
#include "ores.refdata.core/repository/currency_calendar_mapper.hpp"
#include <cstddef>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <stdexcept>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string currency_calendar_repository::sql() {
    return generate_create_table_sql<currency_calendar_entity>(lg());
}

currency_calendar_repository::currency_calendar_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
currency_calendar_repository::replace_claim(const domain::currency_calendar& v) {
    const auto current = read_latest(v.currency_iso_code, v.calendar_code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::currency_calendar
currency_calendar_repository::apply_claim(const domain::currency_calendar& v,
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
            const auto current = read_latest(v.currency_iso_code, v.calendar_code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void currency_calendar_repository::write(const domain::currency_calendar& currency_calendar) {
    write(currency_calendar, replace_claim(currency_calendar));
}

void currency_calendar_repository::write(
    const std::vector<domain::currency_calendar>& currency_calendars) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(currency_calendars.size());
    for (const auto& item : currency_calendars)
        claims.push_back(replace_claim(item));
    write(currency_calendars, claims);
}

void currency_calendar_repository::write(const domain::currency_calendar& currency_calendar,
                                         const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency calendar to database: "
                               << currency_calendar.currency_iso_code << "/"
                               << currency_calendar.calendar_code;
    const auto t = apply_claim(currency_calendar, claim);
    execute_write_query(
        ctx_, currency_calendar_mapper::map(t), lg(), "writing currency calendar to database");
}

void currency_calendar_repository::write(
    const std::vector<domain::currency_calendar>& currency_calendars,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency calendars to database. Count: "
                               << currency_calendars.size();
    std::vector<domain::currency_calendar> batch;
    batch.reserve(currency_calendars.size());
    for (std::size_t i = 0; i < currency_calendars.size(); ++i)
        batch.push_back(apply_claim(currency_calendars[i], claims[i]));
    execute_write_query(
        ctx_, currency_calendar_mapper::map(batch), lg(), "writing currency calendars to database");
}

std::vector<domain::currency_calendar> currency_calendar_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_calendar_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("currency_iso_code"_c, "calendar_code"_c);

    return execute_read_query<currency_calendar_entity, domain::currency_calendar>(
        ctx_,
        query,
        [](const auto& entities) { return currency_calendar_mapper::map(entities); },
        lg(),
        "Reading latest currency calendars");
}

std::vector<domain::currency_calendar>
currency_calendar_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency calendars with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_calendar_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("currency_iso_code"_c, "calendar_code"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<currency_calendar_entity, domain::currency_calendar>(
        ctx_,
        query,
        [](const auto& entities) { return currency_calendar_mapper::map(entities); },
        lg(),
        "Reading latest currency calendars (paginated).");
}

std::vector<domain::currency_calendar>
currency_calendar_repository::read_latest(const std::string& currency_iso_code,
                                          const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency calendar. " << currency_iso_code << "/"
                               << calendar_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto currency_iso_code_str = currency_iso_code;
    const auto calendar_code_str = calendar_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_calendar_entity>> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
                             "calendar_code"_c == calendar_code && "valid_to"_c == max.value());

    return execute_read_query<currency_calendar_entity, domain::currency_calendar>(
        ctx_,
        query,
        [](const auto& entities) { return currency_calendar_mapper::map(entities); },
        lg(),
        "Reading latest currency calendar by key.");
}

std::uint32_t currency_calendar_repository::get_total_currency_calendar_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active currency calendars count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<currency_calendar_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active currency calendars count: " << count;
    return count;
}

std::vector<domain::currency_calendar>
currency_calendar_repository::read_latest_by_currency(const std::string& currency_iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency calendars. Currency: "
                               << currency_iso_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_calendar_entity>> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
                             "valid_to"_c == max.value()) |
                       order_by("calendar_code"_c);

    auto rows = execute_read_query<currency_calendar_entity, domain::currency_calendar>(
        ctx_,
        query,
        [](const auto& entities) { return currency_calendar_mapper::map(entities); },
        lg(),
        "Reading latest currency calendars by currency.");

    return rows;
}

std::vector<domain::currency_calendar>
currency_calendar_repository::read_latest_by_calendar(const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency calendars. Calendar: " << calendar_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_calendar_entity>> |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "valid_to"_c == max.value()) |
                       order_by("currency_iso_code"_c);

    auto rows = execute_read_query<currency_calendar_entity, domain::currency_calendar>(
        ctx_,
        query,
        [](const auto& entities) { return currency_calendar_mapper::map(entities); },
        lg(),
        "Reading latest currency calendars by calendar.");

    return rows;
}

std::vector<domain::currency_calendar> currency_calendar_repository::read_latest_by_currency(
    const std::string& currency_iso_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency calendars. Currency: "
                               << currency_iso_code << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_calendar_entity>> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
                             "valid_to"_c == max.value()) |
                       order_by("calendar_code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    auto rows = execute_read_query<currency_calendar_entity, domain::currency_calendar>(
        ctx_,
        query,
        [](const auto& entities) { return currency_calendar_mapper::map(entities); },
        lg(),
        "Reading latest currency calendars by currency (paginated).");

    return rows;
}

std::uint32_t currency_calendar_repository::get_total_currency_calendar_count_by_currency(
    const std::string& currency_iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active currency calendars count. Currency: "
                               << currency_iso_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<currency_calendar_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active currency calendars count by currency: " << count;
    return count;
}

std::uint32_t currency_calendar_repository::get_total_currency_calendar_count_by_calendar(
    const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active currency calendars count. Calendar: "
                               << calendar_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<currency_calendar_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active currency calendars count by calendar: " << count;
    return count;
}

void currency_calendar_repository::remove(const std::string& currency_iso_code,
                                          const std::string& calendar_code) {
    static_cast<void>(remove(currency_iso_code, calendar_code, std::nullopt));
}

currency_calendar_repository::remove_status
currency_calendar_repository::remove(const std::string& currency_iso_code,
                                     const std::string& calendar_code,
                                     std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency calendar from database: " << currency_iso_code
                               << "/" << calendar_code;

    const auto current = read_latest(currency_iso_code, calendar_code);
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
    const auto currency_iso_code_str = currency_iso_code;
    const auto calendar_code_str = calendar_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<currency_calendar_entity> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
                             "calendar_code"_c == calendar_code && "valid_to"_c == max.value() &&
                             "version"_c == expected);

    execute_delete_query(ctx_, query, lg(), "removing currency calendar from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(currency_iso_code, calendar_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void currency_calendar_repository::remove(const std::vector<std::string>& currency_iso_codes,
                                          const std::vector<std::string>& calendar_codes) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (currency_iso_codes.size() != calendar_codes.size())
        throw std::invalid_argument(
            "currency_calendar_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < currency_iso_codes.size(); ++i)
        static_cast<void>(remove(currency_iso_codes[i], calendar_codes[i], std::nullopt));
}

void currency_calendar_repository::remove_by_currency(const std::string& currency_iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all currency calendars from database: "
                               << currency_iso_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<currency_calendar_entity> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code);

    execute_delete_query(ctx_, query, lg(), "removing all currency calendars from database");
}


}
