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
#include "ores.mq/repository/queue_repository.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"

namespace ores::mq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

namespace {

[[nodiscard]] auto& lg() {
    static auto instance = make_logger("ores.mq.repository.queue_repository");
    return instance;
}

/**
 * @brief Parses a PostgreSQL timestamptz string into a system_clock::time_point.
 */
std::chrono::system_clock::time_point parse_pg_timestamp(const std::string& s) {
    if (s.empty()) return {};

    std::string base = s;
    const auto dot = base.find('.');
    if (dot != std::string::npos) {
        base = base.substr(0, dot);
    } else {
        const auto tz = base.find_first_of("+-", 11);
        if (tz != std::string::npos) {
            base = base.substr(0, tz);
        }
    }

    try {
        return ores::platform::time::datetime::parse_time_point_utc(base);
    } catch (const std::exception&) {
        return {};
    }
}

/**
 * @brief Converts a string to queue_scope_type.
 */
domain::queue_scope_type parse_scope_type(const std::string& s) {
    if (s == "tenant") return domain::queue_scope_type::tenant;
    if (s == "system") return domain::queue_scope_type::system;
    return domain::queue_scope_type::party;
}

/**
 * @brief Converts a string to queue_type.
 */
domain::queue_type parse_queue_type(const std::string& s) {
    if (s == "channel") return domain::queue_type::channel;
    return domain::queue_type::task;
}

/**
 * @brief Converts queue_scope_type to its SQL string representation.
 */
std::string to_string(domain::queue_scope_type t) {
    switch (t) {
    case domain::queue_scope_type::tenant: return "tenant";
    case domain::queue_scope_type::system: return "system";
    default:                               return "party";
    }
}

/**
 * @brief Converts queue_type to its SQL string representation.
 */
std::string to_string(domain::queue_type t) {
    switch (t) {
    case domain::queue_type::channel: return "channel";
    default:                          return "task";
    }
}

/**
 * @brief Parses a row from ores_mq_queues_tbl into a queue_definition.
 *
 * Expected column order:
 *   id(0), tenant_id(1), party_id(2), scope_type(3), queue_type(4),
 *   name(5), description(6), created_at(7), is_active(8)
 */
domain::queue_definition parse_queue_row(
    const std::vector<std::optional<std::string>>& row) {

    domain::queue_definition def;
    if (row.size() < 9) return def;

    if (row[0]) {
        try {
            def.id = boost::lexical_cast<boost::uuids::uuid>(*row[0]);
        } catch (...) {}
    }

    if (row[1]) {
        try {
            def.tenant_id = boost::lexical_cast<boost::uuids::uuid>(*row[1]);
        } catch (...) {}
    }

    if (row[2]) {
        try {
            def.party_id = boost::lexical_cast<boost::uuids::uuid>(*row[2]);
        } catch (...) {}
    }

    def.scope_type  = parse_scope_type(row[3].value_or("party"));
    def.type  = parse_queue_type(row[4].value_or("task"));
    def.name        = row[5].value_or("");
    def.description = row[6].value_or("");
    def.created_at  = parse_pg_timestamp(row[7].value_or(""));
    def.is_active   = (row[8].value_or("f") == "t");

    return def;
}

} // anonymous namespace

// ---------------------------------------------------------------------------
// queue_repository
// ---------------------------------------------------------------------------

boost::uuids::uuid queue_repository::create_queue(context ctx,
    const domain::queue_definition& def, const std::string& modified_by) {

    BOOST_LOG_SEV(lg(), info) << "Creating queue: " << def.name;

    const auto tenant_str = def.tenant_id
        ? boost::uuids::to_string(*def.tenant_id) : "";
    const auto party_str = def.party_id
        ? boost::uuids::to_string(*def.party_id) : "";

    const std::string sql =
        "SELECT ores_mq_queues_create_fn("
        "$1::uuid, NULLIF($2,'')::uuid, NULLIF($3,'')::uuid, "
        "$4, $5, $6, $7, $8)::text";

    auto rows = execute_parameterized_string_query(ctx, sql,
        {boost::uuids::to_string(def.id),
         tenant_str,
         party_str,
         to_string(def.scope_type),
         to_string(def.type),
         def.name,
         def.description,
         modified_by},
        lg(), "Creating queue definition");

    if (rows.empty())
        return def.id;

    try {
        return boost::lexical_cast<boost::uuids::uuid>(rows.front());
    } catch (...) {
        return def.id;
    }
}

std::optional<domain::queue_definition> queue_repository::find_by_id(
    context ctx, const boost::uuids::uuid& id) {

    BOOST_LOG_SEV(lg(), debug) << "Finding queue by id: " << id;

    const std::string sql =
        "SELECT id::text, tenant_id::text, party_id::text, "
        "scope_type, queue_type, name, description, "
        "created_at::text, is_active "
        "FROM ores_mq_queues_tbl "
        "WHERE id = $1::uuid AND is_active = true";

    auto rows = execute_parameterized_multi_column_query(ctx, sql,
        {boost::uuids::to_string(id)},
        lg(), "Finding queue by id");

    if (rows.empty()) return std::nullopt;
    return parse_queue_row(rows.front());
}

std::optional<domain::queue_definition> queue_repository::find_by_name(
    context ctx, const std::string& name,
    const std::optional<boost::uuids::uuid>& tenant_id,
    const std::optional<boost::uuids::uuid>& party_id) {

    BOOST_LOG_SEV(lg(), debug) << "Finding queue by name: " << name;

    const auto tenant_str = tenant_id
        ? boost::uuids::to_string(*tenant_id) : "";
    const auto party_str = party_id
        ? boost::uuids::to_string(*party_id) : "";

    const std::string sql =
        "SELECT id::text, tenant_id::text, party_id::text, "
        "scope_type, queue_type, name, description, "
        "created_at::text, is_active "
        "FROM ores_mq_queues_tbl "
        "WHERE name = $1 "
        "AND (NULLIF($2,'')::uuid IS NULL OR tenant_id = NULLIF($2,'')::uuid) "
        "AND (NULLIF($3,'')::uuid IS NULL OR party_id = NULLIF($3,'')::uuid) "
        "AND is_active = true "
        "LIMIT 1";

    auto rows = execute_parameterized_multi_column_query(ctx, sql,
        {name, tenant_str, party_str},
        lg(), "Finding queue by name");

    if (rows.empty()) return std::nullopt;
    return parse_queue_row(rows.front());
}

std::vector<domain::queue_definition> queue_repository::list_active(
    context ctx) {

    BOOST_LOG_SEV(lg(), debug) << "Listing active queues.";

    const std::string sql =
        "SELECT id::text, tenant_id::text, party_id::text, "
        "scope_type, queue_type, name, description, "
        "created_at::text, is_active "
        "FROM ores_mq_queues_tbl "
        "WHERE is_active = true "
        "ORDER BY name";

    auto rows = execute_raw_multi_column_query(ctx, sql,
        lg(), "Listing active queues");

    std::vector<domain::queue_definition> result;
    result.reserve(rows.size());
    for (const auto& row : rows)
        result.push_back(parse_queue_row(row));
    return result;
}

void queue_repository::deactivate(context ctx,
    const boost::uuids::uuid& id) {

    BOOST_LOG_SEV(lg(), info) << "Deactivating queue: " << id;

    const std::string sql =
        "UPDATE ores_mq_queues_tbl "
        "SET is_active = false "
        "WHERE id = $1::uuid";

    execute_parameterized_command(std::move(ctx), sql,
        {boost::uuids::to_string(id)},
        lg(), "Deactivating queue");
}

}
