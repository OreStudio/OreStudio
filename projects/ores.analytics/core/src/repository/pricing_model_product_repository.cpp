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
#include "ores.analytics.core/repository/pricing_model_product_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.analytics.api/domain/pricing_model_product_json_io.hpp" // IWYU pragma: keep.
#include "ores.analytics.core/repository/pricing_model_product_entity.hpp"
#include "ores.analytics.core/repository/pricing_model_product_mapper.hpp"

namespace ores::analytics::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string pricing_model_product_repository::sql() {
    return generate_create_table_sql<pricing_model_product_entity>(lg());
}

void pricing_model_product_repository::write(
    context ctx, const domain::pricing_model_product& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing pricing model product: " << v.id;
    execute_write_query(ctx, pricing_model_product_mapper::map(v),
        lg(), "Writing pricing model product to database.");
}

void pricing_model_product_repository::write(
    context ctx, const std::vector<domain::pricing_model_product>& v) {
    BOOST_LOG_SEV(lg(), debug)
        << "Writing pricing model products. Count: " << v.size();
    execute_write_query(ctx, pricing_model_product_mapper::map(v),
        lg(), "Writing pricing model products to database.");
}

std::vector<domain::pricing_model_product>
pricing_model_product_repository::read_latest(
    context ctx, const std::string& config_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading latest pricing model products. config_id: " << config_id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<pricing_model_product_entity>> |
        where("tenant_id"_c == tid
            && "pricing_model_config_id"_c == config_id
            && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<
        pricing_model_product_entity, domain::pricing_model_product>(
        ctx, query,
        [](const auto& entities) {
            return pricing_model_product_mapper::map(entities);
        },
        lg(), "Reading latest pricing model products by config id.");
}

std::vector<domain::pricing_model_product>
pricing_model_product_repository::read_latest_by_id(
    context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading latest pricing model product. id: " << id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<pricing_model_product_entity>> |
        where("tenant_id"_c == tid && "id"_c == id
            && "valid_to"_c == max.value());

    return execute_read_query<
        pricing_model_product_entity, domain::pricing_model_product>(
        ctx, query,
        [](const auto& entities) {
            return pricing_model_product_mapper::map(entities);
        },
        lg(), "Reading latest pricing model product by id.");
}

std::vector<domain::pricing_model_product>
pricing_model_product_repository::read_all(
    context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading all pricing model product versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<pricing_model_product_entity>> |
        where("tenant_id"_c == tid && "id"_c == id) |
        order_by("version"_c.desc());

    return execute_read_query<
        pricing_model_product_entity, domain::pricing_model_product>(
        ctx, query,
        [](const auto& entities) {
            return pricing_model_product_mapper::map(entities);
        },
        lg(), "Reading all pricing model product versions by id.");
}

void pricing_model_product_repository::remove(
    context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing pricing model product: " << id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<pricing_model_product_entity> |
        where("tenant_id"_c == tid && "id"_c == id
            && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(),
        "Removing pricing model product from database.");
}

void pricing_model_product_repository::remove_for_config(
    context ctx, const std::string& config_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Removing pricing model products for config: " << config_id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<pricing_model_product_entity> |
        where("tenant_id"_c == tid
            && "pricing_model_config_id"_c == config_id
            && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(),
        "Removing pricing model products for config from database.");
}

}
