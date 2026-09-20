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
#include "ores.refdata.api/domain/party_country_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/party_country_entity.hpp"
#include "ores.refdata.core/repository/party_country_mapper.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/postgres.hpp>

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

void party_country_repository::write(const domain::party_country& party_country) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party country to database: " << party_country.party_id
                               << "/" << party_country.country_alpha2_code;
    execute_write_query(
        ctx_, party_country_mapper::map(party_country), lg(), "writing party country to database");
}

void party_country_repository::write(const std::vector<domain::party_country>& party_countries) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party countries to database. Count: "
                               << party_countries.size();
    execute_write_query(ctx_,
                        party_country_mapper::map(party_countries),
                        lg(),
                        "writing party countries to database");
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
    BOOST_LOG_SEV(lg(), debug) << "Removing party country from database: " << party_id << "/"
                               << country_alpha2_code;

    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<party_country_entity> |
                       where("tenant_id"_c == tid && "party_id"_c == party_id_str &&
                             "country_alpha2_code"_c == country_alpha2_code);

    execute_delete_query(ctx_, query, lg(), "removing party country from database");
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
