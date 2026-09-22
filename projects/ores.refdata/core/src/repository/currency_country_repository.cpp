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
#include "ores.refdata.core/repository/currency_country_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/currency_country_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/currency_country_entity.hpp"
#include "ores.refdata.core/repository/currency_country_mapper.hpp"
#include <cstddef>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <stdexcept>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string currency_country_repository::sql() {
    return generate_create_table_sql<currency_country_entity>(lg());
}

currency_country_repository::currency_country_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
currency_country_repository::replace_claim(const domain::currency_country& v) {
    const auto current = read_latest(v.currency_iso_code, v.country_alpha2_code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::currency_country
currency_country_repository::apply_claim(const domain::currency_country& v,
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
            const auto current = read_latest(v.currency_iso_code, v.country_alpha2_code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void currency_country_repository::write(const domain::currency_country& currency_country) {
    write(currency_country, replace_claim(currency_country));
}

void currency_country_repository::write(
    const std::vector<domain::currency_country>& currency_countries) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(currency_countries.size());
    for (const auto& item : currency_countries)
        claims.push_back(replace_claim(item));
    write(currency_countries, claims);
}

void currency_country_repository::write(const domain::currency_country& currency_country,
                                        const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency country to database: "
                               << currency_country.currency_iso_code << "/"
                               << currency_country.country_alpha2_code;
    const auto t = apply_claim(currency_country, claim);
    execute_write_query(
        ctx_, currency_country_mapper::map(t), lg(), "writing currency country to database");
}

void currency_country_repository::write(
    const std::vector<domain::currency_country>& currency_countries,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency countries to database. Count: "
                               << currency_countries.size();
    std::vector<domain::currency_country> batch;
    batch.reserve(currency_countries.size());
    for (std::size_t i = 0; i < currency_countries.size(); ++i)
        batch.push_back(apply_claim(currency_countries[i], claims[i]));
    execute_write_query(
        ctx_, currency_country_mapper::map(batch), lg(), "writing currency countries to database");
}

std::vector<domain::currency_country> currency_country_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_country_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("currency_iso_code"_c, "country_alpha2_code"_c);

    return execute_read_query<currency_country_entity, domain::currency_country>(
        ctx_,
        query,
        [](const auto& entities) { return currency_country_mapper::map(entities); },
        lg(),
        "Reading latest currency countries");
}

std::vector<domain::currency_country>
currency_country_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency countries with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_country_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("currency_iso_code"_c, "country_alpha2_code"_c) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<currency_country_entity, domain::currency_country>(
        ctx_,
        query,
        [](const auto& entities) { return currency_country_mapper::map(entities); },
        lg(),
        "Reading latest currency countries (paginated).");
}

std::vector<domain::currency_country>
currency_country_repository::read_latest(const std::string& currency_iso_code,
                                         const std::string& country_alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency country. " << currency_iso_code << "/"
                               << country_alpha2_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<currency_country_entity>> |
        where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
              "country_alpha2_code"_c == country_alpha2_code && "valid_to"_c == max.value());

    return execute_read_query<currency_country_entity, domain::currency_country>(
        ctx_,
        query,
        [](const auto& entities) { return currency_country_mapper::map(entities); },
        lg(),
        "Reading latest currency country by key.");
}

std::uint32_t currency_country_repository::get_total_currency_country_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active currency countries count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::select_from<currency_country_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active currency countries count: " << count;
    return count;
}

std::vector<domain::currency_country>
currency_country_repository::read_latest_by_currency(const std::string& currency_iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency countries. Currency: "
                               << currency_iso_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_country_entity>> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
                             "valid_to"_c == max.value()) |
                       order_by("country_alpha2_code"_c);

    auto rows = execute_read_query<currency_country_entity, domain::currency_country>(
        ctx_,
        query,
        [](const auto& entities) { return currency_country_mapper::map(entities); },
        lg(),
        "Reading latest currency countries by currency.");

    return rows;
}

std::vector<domain::currency_country>
currency_country_repository::read_latest_by_country(const std::string& country_alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency countries. Country: "
                               << country_alpha2_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<currency_country_entity>> |
        where("tenant_id"_c == tid && "country_alpha2_code"_c == country_alpha2_code &&
              "valid_to"_c == max.value()) |
        order_by("currency_iso_code"_c);

    auto rows = execute_read_query<currency_country_entity, domain::currency_country>(
        ctx_,
        query,
        [](const auto& entities) { return currency_country_mapper::map(entities); },
        lg(),
        "Reading latest currency countries by country.");

    return rows;
}

std::vector<domain::currency_country> currency_country_repository::read_latest_by_currency(
    const std::string& currency_iso_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency countries. Currency: "
                               << currency_iso_code << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_country_entity>> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
                             "valid_to"_c == max.value()) |
                       order_by("country_alpha2_code"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    auto rows = execute_read_query<currency_country_entity, domain::currency_country>(
        ctx_,
        query,
        [](const auto& entities) { return currency_country_mapper::map(entities); },
        lg(),
        "Reading latest currency countries by currency (paginated).");

    return rows;
}

std::uint32_t currency_country_repository::get_total_currency_country_count_by_currency(
    const std::string& currency_iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active currency countries count. Currency: "
                               << currency_iso_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::select_from<currency_country_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
                             "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active currency countries count by currency: " << count;
    return count;
}

std::uint32_t currency_country_repository::get_total_currency_country_count_by_country(
    const std::string& country_alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active currency countries count. Country: "
                               << country_alpha2_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<currency_country_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "country_alpha2_code"_c == country_alpha2_code &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active currency countries count by country: " << count;
    return count;
}

void currency_country_repository::remove(const std::string& currency_iso_code,
                                         const std::string& country_alpha2_code) {
    static_cast<void>(remove(currency_iso_code, country_alpha2_code, std::nullopt));
}

currency_country_repository::remove_status
currency_country_repository::remove(const std::string& currency_iso_code,
                                    const std::string& country_alpha2_code,
                                    std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency country from database: " << currency_iso_code
                               << "/" << country_alpha2_code;

    const auto current = read_latest(currency_iso_code, country_alpha2_code);
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
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<currency_country_entity> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
                             "country_alpha2_code"_c == country_alpha2_code &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx_, query, lg(), "removing currency country from database");
    return remove_status::removed;
}

void currency_country_repository::remove(const std::vector<std::string>& currency_iso_codes,
                                         const std::vector<std::string>& country_alpha2_codes) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (currency_iso_codes.size() != country_alpha2_codes.size())
        throw std::invalid_argument(
            "currency_country_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < currency_iso_codes.size(); ++i)
        static_cast<void>(remove(currency_iso_codes[i], country_alpha2_codes[i], std::nullopt));
}

void currency_country_repository::remove_by_currency(const std::string& currency_iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all currency countries from database: "
                               << currency_iso_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<currency_country_entity> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code);

    execute_delete_query(ctx_, query, lg(), "removing all currency countries from database");
}


}
