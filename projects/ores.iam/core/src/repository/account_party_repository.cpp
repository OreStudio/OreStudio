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
#include "ores.iam.core/repository/account_party_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/account_party_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/account_party_entity.hpp"
#include "ores.iam.core/repository/account_party_mapper.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/postgres.hpp>

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string account_party_repository::sql() {
    return generate_create_table_sql<account_party_entity>(lg());
}

account_party_repository::account_party_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void account_party_repository::write(const domain::account_party& account_party) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account party to database: " << account_party.account_id
                               << "/" << account_party.party_id;
    execute_write_query(
        ctx_, account_party_mapper::map(account_party), lg(), "writing account party to database");
}

void account_party_repository::write(const std::vector<domain::account_party>& account_parties) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account parties to database. Count: "
                               << account_parties.size();
    execute_write_query(ctx_,
                        account_party_mapper::map(account_parties),
                        lg(),
                        "writing account parties to database");
}

std::vector<domain::account_party> account_party_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_party_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("account_id"_c, "party_id"_c);

    return execute_read_query<account_party_entity, domain::account_party>(
        ctx_,
        query,
        [](const auto& entities) { return account_party_mapper::map(entities); },
        lg(),
        "Reading latest account parties");
}

std::vector<domain::account_party> account_party_repository::read_latest(std::uint32_t offset,
                                                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account parties with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_party_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("account_id"_c, "party_id"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<account_party_entity, domain::account_party>(
        ctx_,
        query,
        [](const auto& entities) { return account_party_mapper::map(entities); },
        lg(),
        "Reading latest account parties (paginated).");
}

std::uint32_t account_party_repository::get_total_account_party_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active account parties count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::select_from<account_party_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active account parties count: " << count;
    return count;
}

std::vector<domain::account_party>
account_party_repository::read_latest_by_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account parties. Account: " << account_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_party_entity>> |
                       where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("party_id"_c);

    auto rows = execute_read_query<account_party_entity, domain::account_party>(
        ctx_,
        query,
        [](const auto& entities) { return account_party_mapper::map(entities); },
        lg(),
        "Reading latest account parties by account.");

    return rows;
}

std::vector<domain::account_party>
account_party_repository::read_latest_by_party(const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account parties. Party: " << party_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<account_party_entity>> |
        where("tenant_id"_c == tid && "party_id"_c == party_id_str && "valid_to"_c == max.value()) |
        order_by("account_id"_c);

    auto rows = execute_read_query<account_party_entity, domain::account_party>(
        ctx_,
        query,
        [](const auto& entities) { return account_party_mapper::map(entities); },
        lg(),
        "Reading latest account parties by party.");

    return rows;
}

std::vector<domain::account_party> account_party_repository::read_latest_by_account(
    const boost::uuids::uuid& account_id, std::uint32_t offset, std::uint32_t limit) {
    const auto account_id_str = boost::uuids::to_string(account_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account parties. Account: " << account_id
                               << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_party_entity>> |
                       where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("party_id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    auto rows = execute_read_query<account_party_entity, domain::account_party>(
        ctx_,
        query,
        [](const auto& entities) { return account_party_mapper::map(entities); },
        lg(),
        "Reading latest account parties by account (paginated).");

    return rows;
}

std::uint32_t account_party_repository::get_total_account_party_count_by_account(
    const boost::uuids::uuid& account_id) {
    const auto account_id_str = boost::uuids::to_string(account_id);
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active account parties count. Account: "
                               << account_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::select_from<account_party_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active account parties count by account: " << count;
    return count;
}

std::uint32_t account_party_repository::get_total_account_party_count_by_party(
    const boost::uuids::uuid& party_id) {
    const auto party_id_str = boost::uuids::to_string(party_id);
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active account parties count. Party: "
                               << party_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<account_party_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "party_id"_c == party_id_str && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active account parties count by party: " << count;
    return count;
}

void account_party_repository::remove(const boost::uuids::uuid& account_id,
                                      const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing account party from database: " << account_id << "/"
                               << party_id;

    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<account_party_entity> |
                       where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "party_id"_c == party_id_str);

    execute_delete_query(ctx_, query, lg(), "removing account party from database");
}

void account_party_repository::remove_by_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all account parties from database: " << account_id;

    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<account_party_entity> |
                       where("tenant_id"_c == tid && "account_id"_c == account_id_str);

    execute_delete_query(ctx_, query, lg(), "removing all account parties from database");
}

void account_party_repository::replace_by_account(
    const boost::uuids::uuid& account_id,
    const std::vector<domain::account_party>& account_parties,
    const std::string& modified_by,
    const std::string& performed_by,
    const std::string& change_reason_code,
    const std::string& change_commentary) {

    BOOST_LOG_SEV(lg(), debug) << "Replacing account parties for account: " << account_id;
    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto tid = ctx_.tenant_id().to_string();

    // Soft-close the currently active rows for this side so rows absent
    // from the new set disappear from the active set. Rows in @p
    // account_parties are re-inserted below; the insert trigger takes care
    // of the bitemporal bookkeeping.
    execute_parameterized_command(ctx_,
                                  "UPDATE ores_iam_account_parties_tbl"
                                  "   SET valid_to = current_timestamp"
                                  " WHERE tenant_id = $1::uuid"
                                  "   AND account_id = $2::uuid"
                                  "   AND valid_to = ores_utility_infinity_timestamp_fn()",
                                  {tid, account_id_str},
                                  lg(),
                                  "Closing existing account parties for account " + account_id_str);

    for (auto account_party : account_parties) {
        account_party.tenant_id = tid;
        account_party.modified_by = modified_by;
        account_party.performed_by = performed_by;
        account_party.change_reason_code = change_reason_code;
        account_party.change_commentary = change_commentary;
        write(account_party);
    }
}
}
