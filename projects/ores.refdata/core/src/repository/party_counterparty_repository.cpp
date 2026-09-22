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
#include "ores.refdata.core/repository/party_counterparty_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/party_counterparty_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/party_counterparty_entity.hpp"
#include "ores.refdata.core/repository/party_counterparty_mapper.hpp"
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

std::string party_counterparty_repository::sql() {
    return generate_create_table_sql<party_counterparty_entity>(lg());
}

party_counterparty_repository::party_counterparty_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
party_counterparty_repository::replace_claim(const domain::party_counterparty& v) {
    const auto current = read_latest(v.party_id, v.counterparty_id);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::party_counterparty
party_counterparty_repository::apply_claim(const domain::party_counterparty& v,
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
            const auto current = read_latest(v.party_id, v.counterparty_id);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void party_counterparty_repository::write(const domain::party_counterparty& party_counterparty) {
    write(party_counterparty, replace_claim(party_counterparty));
}

void party_counterparty_repository::write(
    const std::vector<domain::party_counterparty>& party_counterparties) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(party_counterparties.size());
    for (const auto& item : party_counterparties)
        claims.push_back(replace_claim(item));
    write(party_counterparties, claims);
}

void party_counterparty_repository::write(const domain::party_counterparty& party_counterparty,
                                          const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party counterparty to database: "
                               << party_counterparty.party_id << "/"
                               << party_counterparty.counterparty_id;
    const auto t = apply_claim(party_counterparty, claim);
    execute_write_query(
        ctx_, party_counterparty_mapper::map(t), lg(), "writing party counterparty to database");
}

void party_counterparty_repository::write(
    const std::vector<domain::party_counterparty>& party_counterparties,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party counterparties to database. Count: "
                               << party_counterparties.size();
    std::vector<domain::party_counterparty> batch;
    batch.reserve(party_counterparties.size());
    for (std::size_t i = 0; i < party_counterparties.size(); ++i)
        batch.push_back(apply_claim(party_counterparties[i], claims[i]));
    execute_write_query(ctx_,
                        party_counterparty_mapper::map(batch),
                        lg(),
                        "writing party counterparties to database");
}

std::vector<domain::party_counterparty> party_counterparty_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<party_counterparty_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("party_id"_c, "counterparty_id"_c);

    return execute_read_query<party_counterparty_entity, domain::party_counterparty>(
        ctx_,
        query,
        [](const auto& entities) { return party_counterparty_mapper::map(entities); },
        lg(),
        "Reading latest party counterparties");
}

std::vector<domain::party_counterparty>
party_counterparty_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party counterparties with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<party_counterparty_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("party_id"_c, "counterparty_id"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<party_counterparty_entity, domain::party_counterparty>(
        ctx_,
        query,
        [](const auto& entities) { return party_counterparty_mapper::map(entities); },
        lg(),
        "Reading latest party counterparties (paginated).");
}

std::vector<domain::party_counterparty>
party_counterparty_repository::read_latest(const boost::uuids::uuid& party_id,
                                           const boost::uuids::uuid& counterparty_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party counterparty. " << party_id << "/"
                               << counterparty_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto counterparty_id_str = boost::uuids::to_string(counterparty_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<party_counterparty_entity>> |
        where("tenant_id"_c == tid && "party_id"_c == party_id_str &&
              "counterparty_id"_c == counterparty_id_str && "valid_to"_c == max.value());

    return execute_read_query<party_counterparty_entity, domain::party_counterparty>(
        ctx_,
        query,
        [](const auto& entities) { return party_counterparty_mapper::map(entities); },
        lg(),
        "Reading latest party counterparty by key.");
}

std::uint32_t party_counterparty_repository::get_total_party_counterparty_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active party counterparties count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<party_counterparty_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active party counterparties count: " << count;
    return count;
}

std::vector<domain::party_counterparty>
party_counterparty_repository::read_latest_by_party(const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party counterparties. Party: " << party_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<party_counterparty_entity>> |
        where("tenant_id"_c == tid && "party_id"_c == party_id_str && "valid_to"_c == max.value()) |
        order_by("counterparty_id"_c);

    auto rows = execute_read_query<party_counterparty_entity, domain::party_counterparty>(
        ctx_,
        query,
        [](const auto& entities) { return party_counterparty_mapper::map(entities); },
        lg(),
        "Reading latest party counterparties by party.");

    return rows;
}

std::vector<domain::party_counterparty> party_counterparty_repository::read_latest_by_counterparty(
    const boost::uuids::uuid& counterparty_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party counterparties. Counterparty: "
                               << counterparty_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto counterparty_id_str = boost::uuids::to_string(counterparty_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<party_counterparty_entity>> |
                       where("tenant_id"_c == tid && "counterparty_id"_c == counterparty_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("party_id"_c);

    auto rows = execute_read_query<party_counterparty_entity, domain::party_counterparty>(
        ctx_,
        query,
        [](const auto& entities) { return party_counterparty_mapper::map(entities); },
        lg(),
        "Reading latest party counterparties by counterparty.");

    return rows;
}

std::vector<domain::party_counterparty> party_counterparty_repository::read_latest_by_party(
    const boost::uuids::uuid& party_id, std::uint32_t offset, std::uint32_t limit) {
    const auto party_id_str = boost::uuids::to_string(party_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party counterparties. Party: " << party_id
                               << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<party_counterparty_entity>> |
        where("tenant_id"_c == tid && "party_id"_c == party_id_str && "valid_to"_c == max.value()) |
        order_by("counterparty_id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    auto rows = execute_read_query<party_counterparty_entity, domain::party_counterparty>(
        ctx_,
        query,
        [](const auto& entities) { return party_counterparty_mapper::map(entities); },
        lg(),
        "Reading latest party counterparties by party (paginated).");

    return rows;
}

std::uint32_t party_counterparty_repository::get_total_party_counterparty_count_by_party(
    const boost::uuids::uuid& party_id) {
    const auto party_id_str = boost::uuids::to_string(party_id);
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active party counterparties count. Party: "
                               << party_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<party_counterparty_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "party_id"_c == party_id_str && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active party counterparties count by party: " << count;
    return count;
}

std::uint32_t party_counterparty_repository::get_total_party_counterparty_count_by_counterparty(
    const boost::uuids::uuid& counterparty_id) {
    const auto counterparty_id_str = boost::uuids::to_string(counterparty_id);
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active party counterparties count. Counterparty: " << counterparty_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<party_counterparty_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "counterparty_id"_c == counterparty_id_str &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active party counterparties count by counterparty: "
                               << count;
    return count;
}

void party_counterparty_repository::remove(const boost::uuids::uuid& party_id,
                                           const boost::uuids::uuid& counterparty_id) {
    static_cast<void>(remove(party_id, counterparty_id, std::nullopt));
}

party_counterparty_repository::remove_status
party_counterparty_repository::remove(const boost::uuids::uuid& party_id,
                                      const boost::uuids::uuid& counterparty_id,
                                      std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party counterparty from database: " << party_id << "/"
                               << counterparty_id;

    const auto current = read_latest(party_id, counterparty_id);
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
    const auto counterparty_id_str = boost::uuids::to_string(counterparty_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<party_counterparty_entity> |
                       where("tenant_id"_c == tid && "party_id"_c == party_id_str &&
                             "counterparty_id"_c == counterparty_id_str &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx_, query, lg(), "removing party counterparty from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(party_id, counterparty_id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void party_counterparty_repository::remove(
    const std::vector<boost::uuids::uuid>& party_ids,
    const std::vector<boost::uuids::uuid>& counterparty_ids) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (party_ids.size() != counterparty_ids.size())
        throw std::invalid_argument(
            "party_counterparty_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < party_ids.size(); ++i)
        static_cast<void>(remove(party_ids[i], counterparty_ids[i], std::nullopt));
}

void party_counterparty_repository::remove_by_party(const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all party counterparties from database: " << party_id;

    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<party_counterparty_entity> |
                       where("tenant_id"_c == tid && "party_id"_c == party_id_str);

    execute_delete_query(ctx_, query, lg(), "removing all party counterparties from database");
}


}
