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
#include "ores.trading.core/repository/parent_scoped_queries.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.core/repository/bond_issue_call_date_entity.hpp"
#include "ores.trading.core/repository/bond_issue_call_date_mapper.hpp"
#include "ores.trading.core/repository/bond_issue_conversion_target_entity.hpp"
#include "ores.trading.core/repository/bond_issue_conversion_target_mapper.hpp"
#include "ores.trading.core/repository/trade_envelope_additional_field_entity.hpp"
#include "ores.trading.core/repository/trade_envelope_entity.hpp"
#include "ores.trading.core/repository/trade_envelope_mapper.hpp"
#include "ores.trading.core/repository/trade_envelope_additional_field_mapper.hpp"
#include "ores.trading.core/repository/trade_envelope_portfolio_id_entity.hpp"
#include "ores.trading.core/repository/trade_envelope_portfolio_id_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

namespace {

auto& lg() {
    static auto instance = ores::logging::make_logger("ores.trading.repository.parent_scoped_queries");
    return instance;
}

}

std::vector<domain::trade_envelope>
read_envelopes_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_envelope_entity>> |
                       where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
                             "valid_to"_c == max.value()) |
                       order_by("trade_id"_c);

    return execute_read_query<trade_envelope_entity, domain::trade_envelope>(
        ctx,
        query,
        [](const auto& entities) { return trade_envelope_mapper::map(entities); },
        lg(),
        "Reading trade envelopes by trade ids.");
}

std::vector<domain::trade_envelope_portfolio_id>
read_portfolio_ids_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_envelope_portfolio_id_entity>> |
                       where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
                             "valid_to"_c == max.value()) |
                       order_by("trade_id"_c, "sequence_number"_c);

    return execute_read_query<trade_envelope_portfolio_id_entity,
                              domain::trade_envelope_portfolio_id>(
        ctx,
        query,
        [](const auto& entities) { return trade_envelope_portfolio_id_mapper::map(entities); },
        lg(),
        "Reading trade envelope portfolio identifiers by trade ids.");
}

std::vector<domain::trade_envelope_additional_field>
read_additional_fields_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_envelope_additional_field_entity>> |
                       where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
                             "valid_to"_c == max.value()) |
                       order_by("trade_id"_c, "sequence_number"_c);

    return execute_read_query<trade_envelope_additional_field_entity,
                              domain::trade_envelope_additional_field>(
        ctx,
        query,
        [](const auto& entities) { return trade_envelope_additional_field_mapper::map(entities); },
        lg(),
        "Reading trade envelope additional fields by trade ids.");
}

std::vector<domain::bond_issue_call_date>
read_call_dates_by_issue_ids(context ctx, const std::vector<std::string>& issue_ids) {
    if (issue_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_issue_call_date_entity>> |
                       where("tenant_id"_c == tid && "issue_id"_c.in(issue_ids) &&
                             "valid_to"_c == max.value()) |
                       order_by("issue_id"_c, "sequence_number"_c);

    return execute_read_query<bond_issue_call_date_entity, domain::bond_issue_call_date>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_call_date_mapper::map(entities); },
        lg(),
        "Reading bond issue call dates by issue ids.");
}

std::vector<domain::bond_issue_conversion_target>
read_conversion_targets_by_issue_ids(context ctx, const std::vector<std::string>& issue_ids) {
    if (issue_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_issue_conversion_target_entity>> |
                       where("tenant_id"_c == tid && "issue_id"_c.in(issue_ids) &&
                             "valid_to"_c == max.value()) |
                       order_by("issue_id"_c, "sequence_number"_c);

    return execute_read_query<bond_issue_conversion_target_entity,
                              domain::bond_issue_conversion_target>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_conversion_target_mapper::map(entities); },
        lg(),
        "Reading bond issue conversion targets by issue ids.");
}

}
