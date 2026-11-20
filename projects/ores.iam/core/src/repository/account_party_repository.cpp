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
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/account_party.hpp"
#include "ores.iam.api/domain/account_party_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/account_party_entity.hpp"
#include "ores.iam.core/repository/account_party_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <map>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::iam::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string account_party_repository::sql() {
    return query_builder::insert_statement_sql<account_party_entity>(lg());
}

account_party_repository::account_party_repository(const context& ctx)
    : ctx_(ctx) {}

namespace {

/**
 * @brief The key of a link in the text form the store compares it in.
 */
std::pair<std::string, std::string> account_party_pair_of(const domain::account_party& v) {
    return {boost::uuids::to_string(v.account_id), boost::uuids::to_string(v.party_id)};
}

/**
 * @brief The claim a replace makes, from the rows the key names now: the
 * version the row carries, or that no row exists yet.
 */
ores::utility::domain::precondition
account_party_replace_claim_of(const std::vector<domain::account_party>& current) {
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

/**
 * @brief The object with the claim's version stamped onto it.
 *
 * The caller passes the rows the key names now whenever the claim is any.
 */
domain::account_party account_party_apply_claim(const domain::account_party& v,
                                                const ores::utility::domain::precondition& claim,
                                                const std::vector<domain::account_party>& current) {
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
        case precondition_kind::any:
            // A caller that claims nothing still has to say what it replaces, so
            // the row's current version is stated. A row that moved on between
            // the read and the write is a conflict the trigger raises, never a
            // silent overwrite.
            t.version = current.empty() ? 0 : current.front().version;
            break;
    }
    return t;
}

/**
 * @brief The current rows the key of an object names, or none.
 */
const std::vector<domain::account_party>&
account_party_rows_of(const account_party_repository::rows_by_pair& current,
                      const domain::account_party& v) {
    static const std::vector<domain::account_party> none;
    const auto found = current.find(account_party_pair_of(v));
    return found == current.end() ? none : found->second;
}

}

account_party_repository::rows_by_pair
account_party_repository::read_current(const std::vector<domain::account_party>& of) {
    rows_by_pair current;
    static constexpr auto max = db_timestamp::max();
    // Every key is a bind and the store's bind count is finite, so a larger
    // set is read one part at a time.
    constexpr std::size_t part_size = 1000;
    for (std::size_t begin = 0; begin < of.size(); begin += part_size) {
        const auto end = std::min(begin + part_size, of.size());
        std::set<std::pair<std::string, std::string>> requested;
        std::set<std::string> left_set;
        std::set<std::string> right_set;
        for (std::size_t i = begin; i < end; ++i) {
            auto pair = account_party_pair_of(of[i]);
            left_set.insert(pair.first);
            right_set.insert(pair.second);
            requested.insert(std::move(pair));
        }
        const std::vector<std::string> lefts(left_set.begin(), left_set.end());
        const std::vector<std::string> rights(right_set.begin(), right_set.end());
        const auto tid = ctx_.tenant_id().to_string();
        const auto query = query_builder::read<std::vector<account_party_entity>> |
                           query_builder::where("tenant_id"_c == tid && "account_id"_c.in(lefts) &&
                                                "party_id"_c.in(rights) && "valid_to"_c == max);
        auto rows = query_builder::execute_read_query<account_party_entity, domain::account_party>(
            ctx_,
            query,
            [](const auto& entities) { return account_party_mapper::map(entities); },
            lg(),
            "Reading current account parties by key.");
        // The query matches every left with every right, so a row outside the
        // requested pairs is dropped here.
        for (auto& row : rows) {
            auto pair = account_party_pair_of(row);
            if (requested.contains(pair))
                current[std::move(pair)].push_back(std::move(row));
        }
    }
    return current;
}

ores::utility::domain::precondition
account_party_repository::replace_claim(const domain::account_party& v) {
    return account_party_replace_claim_of(read_latest(v.account_id, v.party_id));
}

void account_party_repository::write(const domain::account_party& account_party) {
    write(account_party, replace_claim(account_party));
}

void account_party_repository::write(const std::vector<domain::account_party>& account_parties) {
    // One read for the set, so a batch states its claims from one read rather
    // than one read per row.
    const auto current = read_current(account_parties);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(account_parties.size());
    for (const auto& item : account_parties)
        claims.push_back(account_party_replace_claim_of(account_party_rows_of(current, item)));
    write(account_parties, claims);
}

void account_party_repository::write(const domain::account_party& account_party,
                                     const ores::utility::domain::precondition& claim) {
    ORES_LOG_SEV(lg(), debug) << "Writing account party to database: " << account_party.account_id
                              << "/" << account_party.party_id;
    const auto current = claim.kind == ores::utility::domain::precondition_kind::any ?
                             read_latest(account_party.account_id, account_party.party_id) :
                             std::vector<domain::account_party>{};
    const auto t = account_party_apply_claim(account_party, claim, current);
    query_builder::execute_write_query(
        ctx_, account_party_mapper::map(t), lg(), "writing account party to database");
}

void account_party_repository::write(
    const std::vector<domain::account_party>& account_parties,
    const std::vector<ores::utility::domain::precondition>& claims) {
    ORES_LOG_SEV(lg(), debug) << "Writing account parties to database. Count: "
                              << account_parties.size();
    if (claims.size() != account_parties.size())
        throw std::invalid_argument(
            "account_party_repository::write: one claim is needed for each row");
    std::vector<domain::account_party> to_read;
    for (std::size_t i = 0; i < account_parties.size(); ++i) {
        if (claims[i].kind == ores::utility::domain::precondition_kind::any)
            to_read.push_back(account_parties[i]);
    }
    const auto current = read_current(to_read);
    std::vector<domain::account_party> batch;
    batch.reserve(account_parties.size());
    for (std::size_t i = 0; i < account_parties.size(); ++i)
        batch.push_back(account_party_apply_claim(
            account_parties[i], claims[i], account_party_rows_of(current, account_parties[i])));
    query_builder::execute_write_query(
        ctx_, account_party_mapper::map(batch), lg(), "writing account parties to database");
}

std::vector<domain::account_party> account_party_repository::read_latest() {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<account_party_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("account_id"_c, "party_id"_c);

    return query_builder::execute_read_query<account_party_entity, domain::account_party>(
        ctx_,
        query,
        [](const auto& entities) { return account_party_mapper::map(entities); },
        lg(),
        "Reading latest account parties");
}

std::vector<domain::account_party> account_party_repository::read_latest(std::uint32_t offset,
                                                                         std::uint32_t limit) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest account parties with offset: " << offset
                              << " and limit: " << limit;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<account_party_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("account_id"_c, "party_id"_c) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_read_query<account_party_entity, domain::account_party>(
        ctx_,
        query,
        [](const auto& entities) { return account_party_mapper::map(entities); },
        lg(),
        "Reading latest account parties (paginated).");
}

std::vector<domain::account_party>
account_party_repository::read_latest(const boost::uuids::uuid& account_id,
                                      const boost::uuids::uuid& party_id) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest account party. " << account_id << "/" << party_id;

    static constexpr auto max = db_timestamp::max();
    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<account_party_entity>> |
        query_builder::where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "party_id"_c == party_id_str && "valid_to"_c == max);

    return query_builder::execute_read_query<account_party_entity, domain::account_party>(
        ctx_,
        query,
        [](const auto& entities) { return account_party_mapper::map(entities); },
        lg(),
        "Reading latest account party by key.");
}

std::uint32_t account_party_repository::get_total_account_party_count() {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active account parties count";
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<account_party_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<account_party_entity>(
        ctx_, query, std::nullopt, lg(), "Counting account parties");
    ORES_LOG_SEV(lg(), debug) << "Total active account parties count: " << count;
    return count;
}

std::vector<domain::account_party>
account_party_repository::read_latest_by_account(const boost::uuids::uuid& account_id) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest account parties. Account: " << account_id;

    static constexpr auto max = db_timestamp::max();
    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<account_party_entity>> |
        query_builder::where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "valid_to"_c == max) |
        query_builder::order_by("party_id"_c);

    auto rows = query_builder::execute_read_query<account_party_entity, domain::account_party>(
        ctx_,
        query,
        [](const auto& entities) { return account_party_mapper::map(entities); },
        lg(),
        "Reading latest account parties by account.");

    return rows;
}

std::vector<domain::account_party>
account_party_repository::read_latest_by_party(const boost::uuids::uuid& party_id) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest account parties. Party: " << party_id;

    static constexpr auto max = db_timestamp::max();
    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<account_party_entity>> |
                       query_builder::where("tenant_id"_c == tid && "party_id"_c == party_id_str &&
                                            "valid_to"_c == max) |
                       query_builder::order_by("account_id"_c);

    auto rows = query_builder::execute_read_query<account_party_entity, domain::account_party>(
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
    ORES_LOG_SEV(lg(), debug) << "Reading latest account parties. Account: " << account_id
                              << " offset: " << offset << " limit: " << limit;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<account_party_entity>> |
        query_builder::where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "valid_to"_c == max) |
        query_builder::order_by("party_id"_c) | query_builder::offset(offset) |
        query_builder::limit(limit);

    auto rows = query_builder::execute_read_query<account_party_entity, domain::account_party>(
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
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active account parties count. Account: "
                              << account_id;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<account_party_entity>> |
        query_builder::where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<account_party_entity>(
        ctx_, query, std::nullopt, lg(), "Counting account parties");
    ORES_LOG_SEV(lg(), debug) << "Total active account parties count by account: " << count;
    return count;
}

std::uint32_t account_party_repository::get_total_account_party_count_by_party(
    const boost::uuids::uuid& party_id) {
    const auto party_id_str = boost::uuids::to_string(party_id);
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active account parties count. Party: "
                              << party_id;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<account_party_entity>> |
                       query_builder::where("tenant_id"_c == tid && "party_id"_c == party_id_str &&
                                            "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<account_party_entity>(
        ctx_, query, std::nullopt, lg(), "Counting account parties");
    ORES_LOG_SEV(lg(), debug) << "Total active account parties count by party: " << count;
    return count;
}

std::vector<account_party_history_row>
account_party_repository::read_all_by_account(const boost::uuids::uuid& account_id) {
    ORES_LOG_SEV(lg(), debug) << "Reading every version of the account parties. Account: "
                              << account_id;

    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<account_party_entity>> |
        query_builder::where("tenant_id"_c == tid && "account_id"_c == account_id_str) |
        query_builder::order_by("party_id"_c, "valid_from"_c.desc());

    return query_builder::execute_read_query<account_party_entity, account_party_history_row>(
        ctx_,
        query,
        [](const auto& entities) {
            std::vector<account_party_history_row> rows;
            rows.reserve(entities.size());
            for (const auto& entity : entities) {
                account_party_history_row row;
                row.link = account_party_mapper::map(entity);
                row.modified_by = row.link.modified_by;
                row.recorded_at = row.link.recorded_at;
                // A version ends when a later version replaces it, which is not
                // a closing, or when the link is closed. The rows of one link run
                // newest first, so the row before this one, when it is the same
                // link, is the next newer version, and it replaced this one only
                // if it carries the next version number. A link opened again
                // after a closing starts its numbering over.
                const bool replaced = !rows.empty() &&
                                      rows.back().link.party_id == row.link.party_id &&
                                      rows.back().link.version == row.link.version + 1;
                if (!replaced) {
                    row.closed_at = validity_end(entity.valid_to);
                    if (row.closed_at)
                        row.closed_by = entity.closed_by.value_or("");
                }
                rows.push_back(std::move(row));
            }
            return rows;
        },
        lg(),
        "Reading every version of the account parties by account.");
}

void account_party_repository::remove(const boost::uuids::uuid& account_id,
                                      const boost::uuids::uuid& party_id) {
    static_cast<void>(remove(account_id, party_id, std::nullopt));
}

account_party_repository::remove_status
account_party_repository::remove(const boost::uuids::uuid& account_id,
                                 const boost::uuids::uuid& party_id,
                                 std::optional<std::uint32_t> version) {
    ORES_LOG_SEV(lg(), debug) << "Removing account party from database: " << account_id << "/"
                              << party_id;

    const auto current = read_latest(account_id, party_id);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().version) != *version)
        return remove_status::conflicting;

    static constexpr auto max = db_timestamp::max();
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().version;
    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<account_party_entity> |
        query_builder::where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "party_id"_c == party_id_str && "valid_to"_c == max &&
                             "version"_c == expected);

    query_builder::execute_delete_query(ctx_, query, lg(), "removing account party from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(account_id, party_id).empty())
        return remove_status::conflicting;
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
    ORES_LOG_SEV(lg(), debug) << "Removing all account parties from database: " << account_id;

    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<account_party_entity> |
        query_builder::where("tenant_id"_c == tid && "account_id"_c == account_id_str);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing all account parties from database");
}


}
