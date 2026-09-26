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
#include "ores.synthetic.core/repository/ir_curve_generation_config_process_parameter_value_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_process_parameter_value_json_io.hpp" // IWYU pragma: keep.
#include "ores.synthetic.core/repository/ir_curve_generation_config_process_parameter_value_entity.hpp"
#include "ores.synthetic.core/repository/ir_curve_generation_config_process_parameter_value_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::synthetic::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string ir_curve_generation_config_process_parameter_value_repository::sql() {
    return generate_create_table_sql<ir_curve_generation_config_process_parameter_value_entity>(
        lg());
}

ores::utility::domain::precondition
ir_curve_generation_config_process_parameter_value_repository::replace_claim(
    context ctx, const domain::ir_curve_generation_config_process_parameter_value& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::ir_curve_generation_config_process_parameter_value
ir_curve_generation_config_process_parameter_value_repository::apply_claim(
    context ctx,
    const domain::ir_curve_generation_config_process_parameter_value& v,
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
            const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void ir_curve_generation_config_process_parameter_value_repository::write(
    context ctx, const domain::ir_curve_generation_config_process_parameter_value& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void ir_curve_generation_config_process_parameter_value_repository::write(
    context ctx, const std::vector<domain::ir_curve_generation_config_process_parameter_value>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void ir_curve_generation_config_process_parameter_value_repository::write(
    context ctx,
    const domain::ir_curve_generation_config_process_parameter_value& v,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing IR curve generation config process parameter value. "
                               << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx,
                        ir_curve_generation_config_process_parameter_value_mapper::map(t),
                        lg(),
                        "Writing IR curve generation config process parameter value to database.");
}

void ir_curve_generation_config_process_parameter_value_repository::write(
    context ctx,
    const std::vector<domain::ir_curve_generation_config_process_parameter_value>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug)
        << "Writing IR curve generation config process parameter values. Count: " << v.size();
    std::vector<domain::ir_curve_generation_config_process_parameter_value> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx,
                        ir_curve_generation_config_process_parameter_value_mapper::map(batch),
                        lg(),
                        "Writing IR curve generation config process parameter values to database.");
}

std::vector<domain::ir_curve_generation_config_process_parameter_value>
ir_curve_generation_config_process_parameter_value_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<ir_curve_generation_config_process_parameter_value_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | order_by("id"_c);

    return execute_read_query<ir_curve_generation_config_process_parameter_value_entity,
                              domain::ir_curve_generation_config_process_parameter_value>(
        ctx,
        query,
        [](const auto& entities) {
            return ir_curve_generation_config_process_parameter_value_mapper::map(entities);
        },
        lg(),
        "Reading latest IR curve generation config process parameter values");
}

std::vector<domain::ir_curve_generation_config_process_parameter_value>
ir_curve_generation_config_process_parameter_value_repository::read_latest(context ctx,
                                                                           const std::string& id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading latest IR curve generation config process parameter value. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<ir_curve_generation_config_process_parameter_value_entity>> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<ir_curve_generation_config_process_parameter_value_entity,
                              domain::ir_curve_generation_config_process_parameter_value>(
        ctx,
        query,
        [](const auto& entities) {
            return ir_curve_generation_config_process_parameter_value_mapper::map(entities);
        },
        lg(),
        "Reading latest IR curve generation config process parameter value by id.");
}


std::vector<domain::ir_curve_generation_config_process_parameter_value>
ir_curve_generation_config_process_parameter_value_repository::read_all(context ctx,
                                                                        const std::string& id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading all IR curve generation config process parameter value versions. "
        << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<ir_curve_generation_config_process_parameter_value_entity>> |
        where("tenant_id"_c == tid && "id"_c == id) |
        order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<ir_curve_generation_config_process_parameter_value_entity,
                              domain::ir_curve_generation_config_process_parameter_value>(
        ctx,
        query,
        [](const auto& entities) {
            return ir_curve_generation_config_process_parameter_value_mapper::map(entities);
        },
        lg(),
        "Reading all IR curve generation config process parameter value versions by id.");
}

std::optional<domain::ir_curve_generation_config_process_parameter_value>
ir_curve_generation_config_process_parameter_value_repository::read_at_version(
    context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading IR curve generation config process parameter value at version. " << "id: " << id
        << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<ir_curve_generation_config_process_parameter_value_entity>> |
        where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) | sqlgen::limit(1);

    const auto entities =
        execute_read_query<ir_curve_generation_config_process_parameter_value_entity,
                           domain::ir_curve_generation_config_process_parameter_value>(
            ctx,
            query,
            [](const auto& entities) {
                return ir_curve_generation_config_process_parameter_value_mapper::map(entities);
            },
            lg(),
            "Reading IR curve generation config process parameter value at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


ir_curve_generation_config_process_parameter_value_repository::remove_status
ir_curve_generation_config_process_parameter_value_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing IR curve generation config process parameter value. "
                               << "id: " << id;
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
    const auto query =
        sqlgen::delete_from<ir_curve_generation_config_process_parameter_value_entity> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value() &&
              "version"_c == expected);

    execute_delete_query(
        ctx,
        query,
        lg(),
        "Removing IR curve generation config process parameter value from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void ir_curve_generation_config_process_parameter_value_repository::remove(context ctx,
                                                                           const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::ir_curve_generation_config_process_parameter_value>
ir_curve_generation_config_process_parameter_value_repository::read_latest(context ctx,
                                                                           std::uint32_t offset,
                                                                           std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading latest IR curve generation config process parameter values with offset: "
        << offset << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<ir_curve_generation_config_process_parameter_value_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | order_by("id"_c) |
        sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<ir_curve_generation_config_process_parameter_value_entity,
                              domain::ir_curve_generation_config_process_parameter_value>(
        ctx,
        query,
        [](const auto& entities) {
            return ir_curve_generation_config_process_parameter_value_mapper::map(entities);
        },
        lg(),
        "Reading latest IR curve generation config process parameter values with pagination.");
}

std::uint32_t ir_curve_generation_config_process_parameter_value_repository::
    get_total_process_parameter_value_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active IR curve generation config process parameter value count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<ir_curve_generation_config_process_parameter_value_entity>(
            sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug)
        << "Total active IR curve generation config process parameter value count: " << count;
    return count;
}

std::vector<domain::ir_curve_generation_config_process_parameter_value>
ir_curve_generation_config_process_parameter_value_repository::read_latest(
    context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<ir_curve_generation_config_process_parameter_value_entity>> |
        where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    auto result = execute_read_query<ir_curve_generation_config_process_parameter_value_entity,
                                     domain::ir_curve_generation_config_process_parameter_value>(
        ctx,
        query,
        [](const auto& entities) {
            return ir_curve_generation_config_process_parameter_value_mapper::map(entities);
        },
        lg(),
        "Reading latest IR curve generation config process parameter values by ids.");
    return result;
}

void ir_curve_generation_config_process_parameter_value_repository::remove(
    context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<ir_curve_generation_config_process_parameter_value_entity> |
        where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(
        ctx, query, lg(), "Batch removing IR curve generation config process parameter values.");
}


}
