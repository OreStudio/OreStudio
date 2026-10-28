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
#include "ores.trading.core/repository/trade_component_queries.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.api/domain/economic_digest.hpp"
#include "ores.trading.api/domain/trade_type_routing.hpp"
#include "ores.trading.core/repository/instrument_economic_shape.hpp"
#include "ores.trading.core/repository/parent_scoped_queries.hpp"
#include "ores.trading.core/repository/trade_additional_field_entity.hpp"
#include "ores.trading.core/repository/trade_additional_field_mapper.hpp"
#include "ores.trading.core/repository/trade_identifier_entity.hpp"
#include "ores.trading.core/repository/trade_identifier_mapper.hpp"
#include "ores.trading.core/repository/trade_party_role_entity.hpp"
#include "ores.trading.core/repository/trade_party_role_mapper.hpp"
#include "ores.trading.core/repository/trade_portfolio_entity.hpp"
#include "ores.trading.core/repository/trade_portfolio_mapper.hpp"

namespace ores::trading::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

namespace {

auto& trade_component_queries_lg() {
    static auto instance =
        ores::logging::make_logger("ores.trading.repository.trade_component_queries");
    return instance;
}

}

std::vector<domain::trade_additional_field>
read_additional_fields_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, trade_component_queries_lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_additional_field_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
                                            "valid_to"_c == max.value()) |
                       query_builder::order_by("trade_id"_c, "sequence_number"_c);

    return query_builder::execute_read_query<trade_additional_field_entity,
                                             domain::trade_additional_field>(
        ctx,
        query,
        [](const auto& entities) { return trade_additional_field_mapper::map(entities); },
        trade_component_queries_lg(),
        "Reading trade additional fields by trade ids.");
}

std::vector<domain::trade_identifier>
read_identifiers_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, trade_component_queries_lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_identifier_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
                                            "valid_to"_c == max.value()) |
                       query_builder::order_by("trade_id"_c, "id_type"_c);

    return query_builder::execute_read_query<trade_identifier_entity, domain::trade_identifier>(
        ctx,
        query,
        [](const auto& entities) { return trade_identifier_mapper::map(entities); },
        trade_component_queries_lg(),
        "Reading trade identifiers by trade ids.");
}

std::vector<domain::trade_portfolio>
read_portfolios_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, trade_component_queries_lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_portfolio_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
                                            "valid_to"_c == max.value()) |
                       query_builder::order_by("trade_id"_c, "sequence_number"_c);

    return query_builder::execute_read_query<trade_portfolio_entity, domain::trade_portfolio>(
        ctx,
        query,
        [](const auto& entities) { return trade_portfolio_mapper::map(entities); },
        trade_component_queries_lg(),
        "Reading trade portfolios by trade ids.");
}

std::vector<domain::trade_party_role>
read_party_roles_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, trade_component_queries_lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_party_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
                                            "valid_to"_c == max.value()) |
                       query_builder::order_by("trade_id"_c, "role"_c);

    return query_builder::execute_read_query<trade_party_role_entity, domain::trade_party_role>(
        ctx,
        query,
        [](const auto& entities) { return trade_party_role_mapper::map(entities); },
        trade_component_queries_lg(),
        "Reading trade party roles by trade ids.");
}


std::vector<std::string>
read_instrument_digests(context ctx, const std::string& trade_id, const std::string& trade_type) {
    std::vector<std::string> digests;
    const auto append = [&digests](const auto& rows) {
        for (const auto& row : rows)
            digests.push_back(domain::economic_digest(row));
    };
    /*
     * The instrument the trade's type routes to. A type the catalogue routes
     * nowhere has no instrument, and contributes nothing. The rows that make
     * up each shape are declared in the trade-type catalogue and generated.
     */
    if (const auto table = domain::instrument_table_for(trade_type))
        read_instrument_components(ctx, trade_id, *table, append);

    read_shared_components(ctx, trade_id, append);

    return digests;
}

}
