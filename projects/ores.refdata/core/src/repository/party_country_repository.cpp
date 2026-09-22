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
#include "ores.refdata.core/repository/party_country_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/party_country_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/party_country_entity.hpp"
#include "ores.refdata.core/repository/party_country_mapper.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <cstddef>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <stdexcept>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string party_country_repository::sql() {
    return generate_create_table_sql<party_country_entity>(lg());
}

party_country_repository::party_country_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
party_country_repository::replace_claim(const domain::party_country& v) {
    const auto current = read_latest(v.party_id, v.country_alpha2_code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::party_country
party_country_repository::apply_claim(const domain::party_country& v,
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
            const auto current = read_latest(v.party_id, v.country_alpha2_code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void party_country_repository::write(const domain::party_country& party_country) {
    write(party_country, replace_claim(party_country));
}

void party_country_repository::write(const std::vector<domain::party_country>& party_countries) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(party_countries.size());
    for (const auto& item : party_countries)
        claims.push_back(replace_claim(item));
    write(party_countries, claims);
}

void party_country_repository::write(const domain::party_country& party_country,
                                     const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party country to database: " << party_country.party_id
                               << "/" << party_country.country_alpha2_code;
    const auto t = apply_claim(party_country, claim);
    execute_write_query(
        ctx_, party_country_mapper::map(t), lg(), "writing party country to database");
}

void party_country_repository::write(
    const std::vector<domain::party_country>& party_countries,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party countries to database. Count: "
                               << party_countries.size();
    std::vector<domain::party_country> batch;
    batch.reserve(party_countries.size());
    for (std::size_t i = 0; i < party_countries.size(); ++i)
        batch.push_back(apply_claim(party_countries[i], claims[i]));
    execute_write_query(
        ctx_, party_country_mapper::map(batch), lg(), "writing party countries to database");
}

std::vector<domain::party_country> party_country_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<party_country_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("party_id"_c, "country_alpha2_code"_c);

    return execute_read_query<party_country_entity, domain::party_country>(
        ctx_,
        query,
        [](const auto& entities) { return party_country_mapper::map(entities); },
        lg(),
        "Reading latest party countries");
}

std::vector<domain::party_country> party_country_repository::read_latest(std::uint32_t offset,
                                                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party countries with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<party_country_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("party_id"_c, "country_alpha2_code"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<party_country_entity, domain::party_country>(
        ctx_,
        query,
        [](const auto& entities) { return party_country_mapper::map(entities); },
        lg(),
        "Reading latest party countries (paginated).");
}

std::vector<domain::party_country>
party_country_repository::read_latest(const boost::uuids::uuid& party_id,
                                      const std::string& country_alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party country. " << party_id << "/"
                               << country_alpha2_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto country_alpha2_code_str = country_alpha2_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<party_country_entity>> |
        where("tenant_id"_c == tid && "party_id"_c == party_id_str &&
              "country_alpha2_code"_c == country_alpha2_code && "valid_to"_c == max.value());

    return execute_read_query<party_country_entity, domain::party_country>(
        ctx_,
        query,
        [](const auto& entities) { return party_country_mapper::map(entities); },
        lg(),
        "Reading latest party country by key.");
}

std::uint32_t party_country_repository::get_total_party_country_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active party countries count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::select_from<party_country_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active party countries count: " << count;
    return count;
}

std::vector<domain::party_country>
party_country_repository::read_latest_by_party(const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party countries. Party: " << party_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<party_country_entity>> |
        where("tenant_id"_c == tid && "party_id"_c == party_id_str && "valid_to"_c == max.value()) |
        order_by("country_alpha2_code"_c);

    auto rows = execute_read_query<party_country_entity, domain::party_country>(
        ctx_,
        query,
        [](const auto& entities) { return party_country_mapper::map(entities); },
        lg(),
        "Reading latest party countries by party.");

    return rows;
}

std::vector<domain::party_country>
party_country_repository::read_latest_by_country(const std::string& country_alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party countries. Country: "
                               << country_alpha2_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<party_country_entity>> |
        where("tenant_id"_c == tid && "country_alpha2_code"_c == country_alpha2_code &&
              "valid_to"_c == max.value()) |
        order_by("party_id"_c);

    auto rows = execute_read_query<party_country_entity, domain::party_country>(
        ctx_,
        query,
        [](const auto& entities) { return party_country_mapper::map(entities); },
        lg(),
        "Reading latest party countries by country.");

    return rows;
}

std::vector<domain::party_country> party_country_repository::read_latest_by_party(
    const boost::uuids::uuid& party_id, std::uint32_t offset, std::uint32_t limit) {
    const auto party_id_str = boost::uuids::to_string(party_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party countries. Party: " << party_id
                               << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<party_country_entity>> |
        where("tenant_id"_c == tid && "party_id"_c == party_id_str && "valid_to"_c == max.value()) |
        order_by("country_alpha2_code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    auto rows = execute_read_query<party_country_entity, domain::party_country>(
        ctx_,
        query,
        [](const auto& entities) { return party_country_mapper::map(entities); },
        lg(),
        "Reading latest party countries by party (paginated).");

    return rows;
}

std::uint32_t party_country_repository::get_total_party_country_count_by_party(
    const boost::uuids::uuid& party_id) {
    const auto party_id_str = boost::uuids::to_string(party_id);
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active party countries count. Party: "
                               << party_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<party_country_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "party_id"_c == party_id_str && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active party countries count by party: " << count;
    return count;
}

std::uint32_t party_country_repository::get_total_party_country_count_by_country(
    const std::string& country_alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active party countries count. Country: "
                               << country_alpha2_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<party_country_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "country_alpha2_code"_c == country_alpha2_code &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active party countries count by country: " << count;
    return count;
}

void party_country_repository::remove(const boost::uuids::uuid& party_id,
                                      const std::string& country_alpha2_code) {
    static_cast<void>(remove(party_id, country_alpha2_code, std::nullopt));
}

party_country_repository::remove_status
party_country_repository::remove(const boost::uuids::uuid& party_id,
                                 const std::string& country_alpha2_code,
                                 std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party country from database: " << party_id << "/"
                               << country_alpha2_code;

    const auto current = read_latest(party_id, country_alpha2_code);
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
    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto country_alpha2_code_str = country_alpha2_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<party_country_entity> |
                       where("tenant_id"_c == tid && "party_id"_c == party_id_str &&
                             "country_alpha2_code"_c == country_alpha2_code &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx_, query, lg(), "removing party country from database");
    return remove_status::removed;
}

void party_country_repository::remove(const std::vector<boost::uuids::uuid>& party_ids,
                                      const std::vector<std::string>& country_alpha2_codes) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (party_ids.size() != country_alpha2_codes.size())
        throw std::invalid_argument(
            "party_country_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < party_ids.size(); ++i)
        static_cast<void>(remove(party_ids[i], country_alpha2_codes[i], std::nullopt));
}

void party_country_repository::remove_by_party(const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all party countries from database: " << party_id;

    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<party_country_entity> |
                       where("tenant_id"_c == tid && "party_id"_c == party_id_str);

    execute_delete_query(ctx_, query, lg(), "removing all party countries from database");
}


}
