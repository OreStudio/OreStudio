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
#include "ores.synthetic.core/repository/market_data_generation_config_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.synthetic.api/domain/market_data_generation_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.synthetic.core/repository/market_data_generation_config_entity.hpp"
#include "ores.synthetic.core/repository/market_data_generation_config_mapper.hpp"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <rfl.hpp>
#include <rfl/json.hpp>

namespace ores::synthetic::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

using mdgc_entity = market_data_generation_config_entity;
using mdgc_domain = domain::market_data_generation_config;

std::string market_data_generation_config_repository::sql() {
    return generate_create_table_sql<mdgc_entity>(lg());
}

void market_data_generation_config_repository::write(context ctx, const mdgc_domain& config) {
    BOOST_LOG_SEV(lg(), debug) << "Writing config to database: " << config;
    execute_write_query(
        ctx, market_data_generation_config_mapper::map(config), lg(), "Writing config to database.");
}

void market_data_generation_config_repository::write(context ctx,
                                                     const std::vector<mdgc_domain>& configs) {
    BOOST_LOG_SEV(lg(), debug) << "Writing configs to database. Count: " << configs.size();
    execute_write_query(ctx,
                        market_data_generation_config_mapper::map(configs),
                        lg(),
                        "Writing configs to database.");
}

std::vector<mdgc_domain> market_data_generation_config_repository::read_latest(context ctx) {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<mdgc_entity>> |
                       where("valid_to"_c == max.value()) | order_by("valid_from"_c.desc());

    return execute_read_query<mdgc_entity, mdgc_domain>(
        ctx,
        query,
        [](const auto& entities) { return market_data_generation_config_mapper::map(entities); },
        lg(),
        "Reading latest configs");
}

std::vector<mdgc_domain> market_data_generation_config_repository::read_latest(context ctx,
                                                                              const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest configs. id: " << id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<mdgc_entity>> |
                       where("id"_c == id && "valid_to"_c == max.value()) |
                       order_by("valid_from"_c.desc());

    return execute_read_query<mdgc_entity, mdgc_domain>(
        ctx,
        query,
        [](const auto& entities) { return market_data_generation_config_mapper::map(entities); },
        lg(),
        "Reading latest configs by id.");
}

std::vector<mdgc_domain>
market_data_generation_config_repository::read_latest(context ctx,
                                                      std::uint32_t offset,
                                                      std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest configs with offset: " << offset
                               << " and limit: " << limit;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<mdgc_entity>> |
                       where("valid_to"_c == max.value()) | order_by("valid_from"_c.desc()) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<mdgc_entity, mdgc_domain>(
        ctx,
        query,
        [](const auto& entities) { return market_data_generation_config_mapper::map(entities); },
        lg(),
        "Reading latest configs with pagination.");
}

std::uint32_t market_data_generation_config_repository::get_total_config_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active config count";

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<mdgc_entity>(sqlgen::count().as<"count">()) |
                       where("valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active config count: " << count;
    return count;
}

std::vector<mdgc_domain>
market_data_generation_config_repository::read_at_timepoint(context ctx, const std::string& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading configs at timepoint: " << as_of;

    const auto ts = make_timestamp(as_of, lg());
    const auto query = sqlgen::read<std::vector<mdgc_entity>> |
                       where("valid_from"_c <= ts.value() && "valid_to"_c >= ts.value());

    return execute_read_query<mdgc_entity, mdgc_domain>(
        ctx,
        query,
        [](const auto& entities) { return market_data_generation_config_mapper::map(entities); },
        lg(),
        "Reading configs at timepoint.");
}

std::vector<mdgc_domain>
market_data_generation_config_repository::read_at_timepoint(context ctx,
                                                            const std::string& as_of,
                                                            const std::string& id) {
    const auto ts = make_timestamp(as_of, lg());
    const auto query = sqlgen::read<std::vector<mdgc_entity>> |
                       where("id"_c == id && "valid_from"_c <= ts.value() &&
                             "valid_to"_c >= ts.value());

    return execute_read_query<mdgc_entity, mdgc_domain>(
        ctx,
        query,
        [](const auto& entities) { return market_data_generation_config_mapper::map(entities); },
        lg(),
        "Reading configs at timepoint by id.");
}

std::vector<mdgc_domain> market_data_generation_config_repository::read_all(context ctx) {
    const auto query = sqlgen::read<std::vector<mdgc_entity>> | order_by("valid_from"_c.desc());

    return execute_read_query<mdgc_entity, mdgc_domain>(
        ctx,
        query,
        [](const auto& entities) { return market_data_generation_config_mapper::map(entities); },
        lg(),
        "Reading all configs.");
}

std::vector<mdgc_domain> market_data_generation_config_repository::read_all(context ctx,
                                                                           const std::string& id) {
    const auto query = sqlgen::read<std::vector<mdgc_entity>> | where("id"_c == id) |
                       order_by("valid_from"_c.desc());

    return execute_read_query<mdgc_entity, mdgc_domain>(
        ctx,
        query,
        [](const auto& entities) { return market_data_generation_config_mapper::map(entities); },
        lg(),
        "Reading all configs by id");
}

void market_data_generation_config_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing config from database: " << id;

    // Delete only the current record - the database trigger will close the
    // temporal record instead of actually deleting it (sets valid_to = current_timestamp)
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::delete_from<mdgc_entity> |
                       where("id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing config from database.");
}

void market_data_generation_config_repository::remove(context ctx,
                                                      const std::vector<std::string>& ids) {
    const auto query = sqlgen::delete_from<mdgc_entity> | where("id"_c.in(ids));
    execute_delete_query(ctx, query, lg(), "batch removing configs");
}

}
