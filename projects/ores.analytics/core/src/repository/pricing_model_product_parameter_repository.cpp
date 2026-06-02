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
#include "ores.analytics.core/repository/pricing_model_product_parameter_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.analytics.api/domain/pricing_model_product_parameter_json_io.hpp" // IWYU pragma: keep.
#include "ores.analytics.core/repository/pricing_model_product_parameter_entity.hpp"
#include "ores.analytics.core/repository/pricing_model_product_parameter_mapper.hpp"

namespace ores::analytics::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string pricing_model_product_parameter_repository::sql() {
    return generate_create_table_sql<
        pricing_model_product_parameter_entity>(lg());
}

void pricing_model_product_parameter_repository::write(
    context ctx, const domain::pricing_model_product_parameter& v) {
    BOOST_LOG_SEV(lg(), debug)
        << "Writing pricing model product parameter: " << v.id;
    execute_write_query(ctx, pricing_model_product_parameter_mapper::map(v),
        lg(), "Writing pricing model product parameter to database.");
}

void pricing_model_product_parameter_repository::write(
    context ctx,
    const std::vector<domain::pricing_model_product_parameter>& v) {
    BOOST_LOG_SEV(lg(), debug)
        << "Writing pricing model product parameters. Count: " << v.size();
    execute_write_query(ctx, pricing_model_product_parameter_mapper::map(v),
        lg(), "Writing pricing model product parameters to database.");
}

std::vector<domain::pricing_model_product_parameter>
pricing_model_product_parameter_repository::read_latest(
    context ctx, const std::string& config_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading latest pricing model product parameters. config_id: "
        << config_id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<pricing_model_product_parameter_entity>> |
        where("tenant_id"_c == tid
            && "pricing_model_config_id"_c == config_id
            && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<
        pricing_model_product_parameter_entity,
        domain::pricing_model_product_parameter>(
        ctx, query,
        [](const auto& entities) {
            return pricing_model_product_parameter_mapper::map(entities);
        },
        lg(), "Reading latest pricing model product parameters by config id.");
}

std::vector<domain::pricing_model_product_parameter>
pricing_model_product_parameter_repository::read_latest_for_product(
    context ctx, const std::string& product_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading latest pricing model product parameters. product_id: "
        << product_id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<pricing_model_product_parameter_entity>> |
        where("tenant_id"_c == tid
            && "pricing_model_product_id"_c == product_id
            && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<
        pricing_model_product_parameter_entity,
        domain::pricing_model_product_parameter>(
        ctx, query,
        [](const auto& entities) {
            return pricing_model_product_parameter_mapper::map(entities);
        },
        lg(),
        "Reading latest pricing model product parameters by product id.");
}

std::vector<domain::pricing_model_product_parameter>
pricing_model_product_parameter_repository::read_latest_by_id(
    context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading latest pricing model product parameter. id: " << id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<pricing_model_product_parameter_entity>> |
        where("tenant_id"_c == tid && "id"_c == id
            && "valid_to"_c == max.value());

    return execute_read_query<
        pricing_model_product_parameter_entity,
        domain::pricing_model_product_parameter>(
        ctx, query,
        [](const auto& entities) {
            return pricing_model_product_parameter_mapper::map(entities);
        },
        lg(), "Reading latest pricing model product parameter by id.");
}

std::vector<domain::pricing_model_product_parameter>
pricing_model_product_parameter_repository::read_all(
    context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading all pricing model product parameter versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<pricing_model_product_parameter_entity>> |
        where("tenant_id"_c == tid && "id"_c == id) |
        order_by("version"_c.desc());

    return execute_read_query<
        pricing_model_product_parameter_entity,
        domain::pricing_model_product_parameter>(
        ctx, query,
        [](const auto& entities) {
            return pricing_model_product_parameter_mapper::map(entities);
        },
        lg(), "Reading all pricing model product parameter versions by id.");
}

void pricing_model_product_parameter_repository::remove(
    context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Removing pricing model product parameter: " << id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<pricing_model_product_parameter_entity> |
        where("tenant_id"_c == tid && "id"_c == id
            && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(),
        "Removing pricing model product parameter from database.");
}

void pricing_model_product_parameter_repository::remove_for_config(
    context ctx, const std::string& config_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Removing pricing model product parameters for config: " << config_id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<pricing_model_product_parameter_entity> |
        where("tenant_id"_c == tid
            && "pricing_model_config_id"_c == config_id
            && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(),
        "Removing pricing model product parameters for config from database.");
}

void pricing_model_product_parameter_repository::remove_for_product(
    context ctx, const std::string& product_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Removing pricing model product parameters for product: "
        << product_id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<pricing_model_product_parameter_entity> |
        where("tenant_id"_c == tid
            && "pricing_model_product_id"_c == product_id
            && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(),
        "Removing pricing model product parameters for product from database.");
}

}
