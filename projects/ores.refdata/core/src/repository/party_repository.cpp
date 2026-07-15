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
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/party_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/party_entity.hpp"
#include "ores.refdata.core/repository/party_mapper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string party_repository::sql() {
    return generate_create_table_sql<party_entity>(lg());
}

void party_repository::write(context ctx, const domain::party& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party: " << v.id;
    execute_write_query(ctx, party_mapper::map(v), lg(), "Writing party to database.");
}

void party_repository::write(context ctx, const std::vector<domain::party>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing parties. Count: " << v.size();
    execute_write_query(ctx, party_mapper::map(v), lg(), "Writing parties to database.");
}

std::vector<domain::party> party_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<party_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<party_entity, domain::party>(
        ctx,
        query,
        [](const auto& entities) { return party_mapper::map(entities); },
        lg(),
        "Reading latest parties");
}

std::vector<domain::party> party_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party. id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<party_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<party_entity, domain::party>(
        ctx,
        query,
        [](const auto& entities) { return party_mapper::map(entities); },
        lg(),
        "Reading latest party by id.");
}

std::vector<domain::party> party_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all party versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<party_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) | order_by("version"_c.desc());

    return execute_read_query<party_entity, domain::party>(
        ctx,
        query,
        [](const auto& entities) { return party_mapper::map(entities); },
        lg(),
        "Reading all party versions by id.");
}

std::optional<domain::party>
party_repository::read_at_version(context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading party at version. id: " << id << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<party_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<party_entity, domain::party>(
        ctx,
        query,
        [](const auto& entities) { return party_mapper::map(entities); },
        lg(),
        "Reading party at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void party_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<party_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing party from database.");
}

std::vector<domain::party>
party_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest parties with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<party_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<party_entity, domain::party>(
        ctx,
        query,
        [](const auto& entities) { return party_mapper::map(entities); },
        lg(),
        "Reading latest parties with pagination.");
}

std::uint32_t party_repository::get_total_party_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active party count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<party_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active party count: " << count;
    return count;
}

void party_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<party_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing parties.");
}

std::vector<ores::utility::domain::hierarchy_flat_row>
party_repository::get_hierarchy(context ctx, const boost::uuids::uuid& root_id, bool from_root) {
    BOOST_LOG_SEV(lg(), debug) << "Reading party hierarchy. Root: " << root_id
                               << " from_root: " << from_root;

    const auto tenant_str = boost::uuids::to_string(ctx.tenant_id().to_uuid());
    const auto root_str = boost::uuids::to_string(root_id);
    const std::string sql = "SELECT * FROM ores_refdata_parties_hierarchy_fn('" + tenant_str +
                            "'::uuid, '" + root_str + "'::uuid, " + (from_root ? "true" : "false") +
                            ")";

    const auto rows = execute_raw_multi_column_query(ctx, sql, lg(), "Reading party hierarchy");

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

    BOOST_LOG_SEV(lg(), debug) << "Read " << result.size() << " party hierarchy rows.";
    return result;
}


std::vector<domain::party> party_repository::read_system_party(context ctx,
                                                               const std::string& tenant_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading system party for tenant: " << tenant_id;
    const std::string sql =
        "SELECT * FROM ores_refdata_read_system_party_fn('" + tenant_id + "'::uuid)";
    const auto rows =
        execute_raw_multi_column_query(ctx, sql, lg(), "Reading system party by tenant");
    std::vector<domain::party> result;
    result.reserve(rows.size());
    static constexpr std::array required_columns = {0, 1, 2, 3, 4, 5, 6, 9, 10, 11, 12, 13, 14};
    for (const auto& row : rows) {
        if (row.size() >= 16 && std::ranges::all_of(required_columns, [&row](int i) {
                return static_cast<bool>(row[i]);
            })) {
            domain::party p;
            p.id = boost::lexical_cast<boost::uuids::uuid>(*row[0]);
            p.tenant_id = utility::uuid::tenant_id::from_string(*row[1]).value();
            p.version = std::stoi(*row[2]);
            p.full_name = *row[3];
            p.short_code = *row[4];
            p.party_category = *row[5];
            p.party_type = *row[6];
            if (row[7])
                p.parent_party_id = boost::lexical_cast<boost::uuids::uuid>(*row[7]);
            if (row[8])
                p.business_center_code = *row[8];
            p.status = *row[9];
            p.modified_by = *row[10];
            result.push_back(p);
        }
    }
    return result;
}

std::vector<boost::uuids::uuid>
party_repository::read_descendants(context ctx, const boost::uuids::uuid& root_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading party descendants. Root: " << root_id;
    const auto id_str = boost::uuids::to_string(root_id);
    const std::string sql = "WITH RECURSIVE party_tree AS ("
                            "  SELECT id FROM ores_refdata_parties_tbl"
                            "  WHERE id = '" +
                            id_str + "' AND valid_to = '" + MAX_TIMESTAMP +
                            "'"
                            "  UNION ALL"
                            "  SELECT p.id FROM ores_refdata_parties_tbl p"
                            "  JOIN party_tree pt ON p.parent_party_id = pt.id"
                            "  WHERE p.valid_to = '" +
                            MAX_TIMESTAMP +
                            "'"
                            ") SELECT id FROM party_tree";
    const auto rows = execute_raw_multi_column_query(ctx, sql, lg(), "Reading party descendants");
    std::vector<boost::uuids::uuid> result;
    result.reserve(rows.size());
    for (const auto& row : rows)
        if (!row.empty() && row[0])
            result.push_back(boost::lexical_cast<boost::uuids::uuid>(*row[0]));
    return result;
}

}
