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
#include "ores.refdata.core/repository/calendar_date_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/calendar_date_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/calendar_date_entity.hpp"
#include "ores.refdata.core/repository/calendar_date_mapper.hpp"
#include <cstddef>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <stdexcept>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string calendar_date_repository::sql() {
    return generate_create_table_sql<calendar_date_entity>(lg());
}

calendar_date_repository::calendar_date_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
calendar_date_repository::replace_claim(const domain::calendar_date& v) {
    const auto current = read_latest(v.calendar_code, v.date);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::calendar_date
calendar_date_repository::apply_claim(const domain::calendar_date& v,
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
            const auto current = read_latest(v.calendar_code, v.date);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void calendar_date_repository::write(const domain::calendar_date& calendar_date) {
    write(calendar_date, replace_claim(calendar_date));
}

void calendar_date_repository::write(const std::vector<domain::calendar_date>& calendar_dates) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(calendar_dates.size());
    for (const auto& item : calendar_dates)
        claims.push_back(replace_claim(item));
    write(calendar_dates, claims);
}

void calendar_date_repository::write(const domain::calendar_date& calendar_date,
                                     const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing calendar date to database: "
                               << calendar_date.calendar_code << "/" << calendar_date.date;
    const auto t = apply_claim(calendar_date, claim);
    execute_write_query(
        ctx_, calendar_date_mapper::map(t), lg(), "writing calendar date to database");
}

void calendar_date_repository::write(
    const std::vector<domain::calendar_date>& calendar_dates,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing calendar dates to database. Count: "
                               << calendar_dates.size();
    std::vector<domain::calendar_date> batch;
    batch.reserve(calendar_dates.size());
    for (std::size_t i = 0; i < calendar_dates.size(); ++i)
        batch.push_back(apply_claim(calendar_dates[i], claims[i]));
    execute_write_query(
        ctx_, calendar_date_mapper::map(batch), lg(), "writing calendar dates to database");
}

std::vector<domain::calendar_date> calendar_date_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_date_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("calendar_code"_c, "date"_c);

    return execute_read_query<calendar_date_entity, domain::calendar_date>(
        ctx_,
        query,
        [](const auto& entities) { return calendar_date_mapper::map(entities); },
        lg(),
        "Reading latest calendar dates");
}

std::vector<domain::calendar_date> calendar_date_repository::read_latest(std::uint32_t offset,
                                                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest calendar dates with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_date_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("calendar_code"_c, "date"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<calendar_date_entity, domain::calendar_date>(
        ctx_,
        query,
        [](const auto& entities) { return calendar_date_mapper::map(entities); },
        lg(),
        "Reading latest calendar dates (paginated).");
}

std::vector<domain::calendar_date>
calendar_date_repository::read_latest(const std::string& calendar_code,
                                      const std::chrono::year_month_day& date) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest calendar date. " << calendar_code << "/" << date;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto calendar_code_str = calendar_code;
    const auto date_str = ores::platform::time::datetime::to_iso8601_date(date);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_date_entity>> |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "date"_c == date_str && "valid_to"_c == max.value());

    return execute_read_query<calendar_date_entity, domain::calendar_date>(
        ctx_,
        query,
        [](const auto& entities) { return calendar_date_mapper::map(entities); },
        lg(),
        "Reading latest calendar date by key.");
}

std::uint32_t calendar_date_repository::get_total_calendar_date_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active calendar dates count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::select_from<calendar_date_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active calendar dates count: " << count;
    return count;
}

std::vector<domain::calendar_date>
calendar_date_repository::read_latest_by_calendar(const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest calendar dates. Calendar: " << calendar_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_date_entity>> |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "valid_to"_c == max.value()) |
                       order_by("date"_c);

    auto rows = execute_read_query<calendar_date_entity, domain::calendar_date>(
        ctx_,
        query,
        [](const auto& entities) { return calendar_date_mapper::map(entities); },
        lg(),
        "Reading latest calendar dates by calendar.");

    return rows;
}

std::vector<domain::calendar_date>
calendar_date_repository::read_latest_by_date(const std::chrono::year_month_day& date) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest calendar dates. Date: " << date;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto date_str = ores::platform::time::datetime::to_iso8601_date(date);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<calendar_date_entity>> |
        where("tenant_id"_c == tid && "date"_c == date_str && "valid_to"_c == max.value()) |
        order_by("calendar_code"_c);

    auto rows = execute_read_query<calendar_date_entity, domain::calendar_date>(
        ctx_,
        query,
        [](const auto& entities) { return calendar_date_mapper::map(entities); },
        lg(),
        "Reading latest calendar dates by date.");

    return rows;
}

std::vector<domain::calendar_date> calendar_date_repository::read_latest_by_calendar(
    const std::string& calendar_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest calendar dates. Calendar: " << calendar_code
                               << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_date_entity>> |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "valid_to"_c == max.value()) |
                       order_by("date"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    auto rows = execute_read_query<calendar_date_entity, domain::calendar_date>(
        ctx_,
        query,
        [](const auto& entities) { return calendar_date_mapper::map(entities); },
        lg(),
        "Reading latest calendar dates by calendar (paginated).");

    return rows;
}

std::uint32_t calendar_date_repository::get_total_calendar_date_count_by_calendar(
    const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active calendar dates count. Calendar: "
                               << calendar_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::select_from<calendar_date_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active calendar dates count by calendar: " << count;
    return count;
}

std::uint32_t calendar_date_repository::get_total_calendar_date_count_by_date(
    const std::chrono::year_month_day& date) {
    const auto date_str = ores::platform::time::datetime::to_iso8601_date(date);
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active calendar dates count. Date: " << date;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<calendar_date_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "date"_c == date_str && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active calendar dates count by date: " << count;
    return count;
}

void calendar_date_repository::remove(const std::string& calendar_code,
                                      const std::chrono::year_month_day& date) {
    static_cast<void>(remove(calendar_code, date, std::nullopt));
}

calendar_date_repository::remove_status
calendar_date_repository::remove(const std::string& calendar_code,
                                 const std::chrono::year_month_day& date,
                                 std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing calendar date from database: " << calendar_code << "/"
                               << date;

    const auto current = read_latest(calendar_code, date);
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
    const auto calendar_code_str = calendar_code;
    const auto date_str = ores::platform::time::datetime::to_iso8601_date(date);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<calendar_date_entity> |
        where("tenant_id"_c == tid && "calendar_code"_c == calendar_code && "date"_c == date_str &&
              "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx_, query, lg(), "removing calendar date from database");
    return remove_status::removed;
}

void calendar_date_repository::remove(const std::vector<std::string>& calendar_codes,
                                      const std::vector<std::chrono::year_month_day>& dates) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (calendar_codes.size() != dates.size())
        throw std::invalid_argument(
            "calendar_date_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < calendar_codes.size(); ++i)
        static_cast<void>(remove(calendar_codes[i], dates[i], std::nullopt));
}

void calendar_date_repository::remove_by_calendar(const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all calendar dates from database: " << calendar_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<calendar_date_entity> |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code);

    execute_delete_query(ctx_, query, lg(), "removing all calendar dates from database");
}


}
