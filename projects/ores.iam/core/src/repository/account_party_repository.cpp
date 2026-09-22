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
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <cstddef>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <stdexcept>

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

ores::utility::domain::precondition
account_party_repository::replace_claim(const domain::account_party& v) {
    const auto current = read_latest(v.account_id, v.party_id);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::account_party
account_party_repository::apply_claim(const domain::account_party& v,
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
            const auto current = read_latest(v.account_id, v.party_id);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void account_party_repository::write(const domain::account_party& account_party) {
    write(account_party, replace_claim(account_party));
}

void account_party_repository::write(const std::vector<domain::account_party>& account_parties) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(account_parties.size());
    for (const auto& item : account_parties)
        claims.push_back(replace_claim(item));
    write(account_parties, claims);
}

void account_party_repository::write(const domain::account_party& account_party,
                                     const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account party to database: " << account_party.account_id
                               << "/" << account_party.party_id;
    const auto t = apply_claim(account_party, claim);
    execute_write_query(
        ctx_, account_party_mapper::map(t), lg(), "writing account party to database");
}

void account_party_repository::write(
    const std::vector<domain::account_party>& account_parties,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account parties to database. Count: "
                               << account_parties.size();
    std::vector<domain::account_party> batch;
    batch.reserve(account_parties.size());
    for (std::size_t i = 0; i < account_parties.size(); ++i)
        batch.push_back(apply_claim(account_parties[i], claims[i]));
    execute_write_query(
        ctx_, account_party_mapper::map(batch), lg(), "writing account parties to database");
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

std::vector<domain::account_party>
account_party_repository::read_latest(const boost::uuids::uuid& account_id,
                                      const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account party. " << account_id << "/" << party_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_party_entity>> |
                       where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "party_id"_c == party_id_str && "valid_to"_c == max.value());

    return execute_read_query<account_party_entity, domain::account_party>(
        ctx_,
        query,
        [](const auto& entities) { return account_party_mapper::map(entities); },
        lg(),
        "Reading latest account party by key.");
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
    static_cast<void>(remove(account_id, party_id, std::nullopt));
}

account_party_repository::remove_status
account_party_repository::remove(const boost::uuids::uuid& account_id,
                                 const boost::uuids::uuid& party_id,
                                 std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing account party from database: " << account_id << "/"
                               << party_id;

    const auto current = read_latest(account_id, party_id);
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
    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<account_party_entity> |
                       where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "party_id"_c == party_id_str && "valid_to"_c == max.value() &&
                             "version"_c == expected);

    execute_delete_query(ctx_, query, lg(), "removing account party from database");
    return remove_status::removed;
}

void account_party_repository::remove(const std::vector<boost::uuids::uuid>& account_ids,
                                      const std::vector<boost::uuids::uuid>& party_ids) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (account_ids.size() != party_ids.size())
        throw std::invalid_argument(
            "account_party_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < account_ids.size(); ++i)
        static_cast<void>(remove(account_ids[i], party_ids[i], std::nullopt));
}

void account_party_repository::remove_by_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all account parties from database: " << account_id;

    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<account_party_entity> |
                       where("tenant_id"_c == tid && "account_id"_c == account_id_str);

    execute_delete_query(ctx_, query, lg(), "removing all account parties from database");
}


}
