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
#include "ores.refdata.core/repository/counterparty_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/counterparty_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/counterparty_entity.hpp"
#include "ores.refdata.core/repository/counterparty_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string counterparty_repository::sql() {
    return generate_create_table_sql<counterparty_entity>(lg());
}

ores::utility::domain::precondition
counterparty_repository::replace_claim(context ctx, const domain::counterparty& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::counterparty counterparty_repository::apply_claim(
    context ctx, const domain::counterparty& v, const ores::utility::domain::precondition& claim) {
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

void counterparty_repository::write(context ctx, const domain::counterparty& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void counterparty_repository::write(context ctx, const std::vector<domain::counterparty>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void counterparty_repository::write(context ctx,
                                    const domain::counterparty& v,
                                    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing counterparty. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(
        ctx, counterparty_mapper::map(t), lg(), "Writing counterparty to database.");
}

void counterparty_repository::write(
    context ctx,
    const std::vector<domain::counterparty>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing counterparties. Count: " << v.size();
    std::vector<domain::counterparty> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(
        ctx, counterparty_mapper::map(batch), lg(), "Writing counterparties to database.");
}

std::vector<domain::counterparty> counterparty_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<counterparty_entity, domain::counterparty>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_mapper::map(entities); },
        lg(),
        "Reading latest counterparties");
}

std::vector<domain::counterparty> counterparty_repository::read_latest(context ctx,
                                                                       const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparty. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<counterparty_entity, domain::counterparty>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_mapper::map(entities); },
        lg(),
        "Reading latest counterparty by id.");
}

std::vector<domain::counterparty>
counterparty_repository::read_latest_by_code(context ctx, const std::string& short_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparty by short_code: " << short_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<counterparty_entity>> |
        where("tenant_id"_c == tid && "short_code"_c == short_code && "valid_to"_c == max.value());

    return execute_read_query<counterparty_entity, domain::counterparty>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_mapper::map(entities); },
        lg(),
        "Reading latest counterparty by short_code.");
}

std::vector<domain::counterparty>
counterparty_repository::read_any_by_short_code(context ctx, const std::string& short_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading any counterparty by short_code: " << short_code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_entity>> |
                       where("tenant_id"_c == tid && "short_code"_c == short_code) |
                       order_by("valid_from"_c.desc()) | sqlgen::limit(1);

    return execute_read_query<counterparty_entity, domain::counterparty>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_mapper::map(entities); },
        lg(),
        "Reading any counterparty by short_code.");
}


std::vector<domain::counterparty> counterparty_repository::read_all(context ctx,
                                                                    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all counterparty versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<counterparty_entity, domain::counterparty>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_mapper::map(entities); },
        lg(),
        "Reading all counterparty versions by id.");
}

std::optional<domain::counterparty> counterparty_repository::read_at_version(
    context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading counterparty at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<counterparty_entity, domain::counterparty>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_mapper::map(entities); },
        lg(),
        "Reading counterparty at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


counterparty_repository::remove_status counterparty_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing counterparty. " << "id: " << id;
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
    const auto query = sqlgen::delete_from<counterparty_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value() &&
                             "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing counterparty from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void counterparty_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::counterparty>
counterparty_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparties with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<counterparty_entity, domain::counterparty>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_mapper::map(entities); },
        lg(),
        "Reading latest counterparties with pagination.");
}

std::uint32_t counterparty_repository::get_total_counterparty_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active counterparty count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<counterparty_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active counterparty count: " << count;
    return count;
}

std::vector<domain::counterparty>
counterparty_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_entity>> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    auto result = execute_read_query<counterparty_entity, domain::counterparty>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_mapper::map(entities); },
        lg(),
        "Reading latest counterparties by ids.");
    return result;
}

void counterparty_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<counterparty_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing counterparties.");
}

std::vector<ores::utility::domain::hierarchy_flat_row> counterparty_repository::get_hierarchy(
    context ctx, const boost::uuids::uuid& root_id, bool from_root) {
    BOOST_LOG_SEV(lg(), debug) << "Reading counterparty hierarchy. Root: " << root_id
                               << " from_root: " << from_root;

    const auto tenant_str = boost::uuids::to_string(ctx.tenant_id().to_uuid());
    const auto root_str = boost::uuids::to_string(root_id);
    const std::string sql = "SELECT * FROM ores_refdata_counterparties_hierarchy_fn('" +
                            tenant_str + "'::uuid, '" + root_str + "'::uuid, " +
                            (from_root ? "true" : "false") + ")";

    const auto rows =
        execute_raw_multi_column_query(ctx, sql, lg(), "Reading counterparty hierarchy");

    std::vector<ores::utility::domain::hierarchy_flat_row> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() >= 3 && row[0]) {
            ores::utility::domain::hierarchy_flat_row r;
            r.id = boost::lexical_cast<boost::uuids::uuid>(*row[0]);
            if (row[1])
                r.parent_id = boost::lexical_cast<boost::uuids::uuid>(*row[1]);
            if (row[2])
                r.name = *row[2];
            result.push_back(std::move(r));
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Read " << result.size() << " counterparty hierarchy rows.";
    return result;
}


}
