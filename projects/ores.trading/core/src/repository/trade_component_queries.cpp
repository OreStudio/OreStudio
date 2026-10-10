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
#include "ores.trading.core/repository/balance_guaranteed_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/bond_instrument_repository.hpp"
#include "ores.trading.core/repository/callable_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/cap_floor_instrument_repository.hpp"
#include "ores.trading.core/repository/commodity_instrument_repository.hpp"
#include "ores.trading.core/repository/composite_instrument_repository.hpp"
#include "ores.trading.core/repository/credit_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_accumulator_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_asian_option_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_barrier_option_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_digital_option_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_forward_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_option_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_position_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_variance_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/fra_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_accumulator_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_asian_forward_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_barrier_option_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_digital_option_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_forward_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_vanilla_option_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_variance_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/inflation_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/knock_out_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/rate_instrument_repository.hpp"
#include "ores.trading.core/repository/scripted_instrument_repository.hpp"
#include "ores.trading.core/repository/swaption_instrument_repository.hpp"
#include "ores.trading.core/repository/vanilla_swap_instrument_repository.hpp"
#include "ores.trading.api/domain/trade_type_routing.hpp"
#include "ores.trading.api/domain/economic_digest.hpp"
#include "ores.trading.core/repository/parent_scoped_queries.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.core/repository/trade_additional_field_entity.hpp"
#include "ores.trading.core/repository/trade_additional_field_mapper.hpp"
#include "ores.trading.core/repository/trade_identifier_entity.hpp"
#include "ores.trading.core/repository/trade_identifier_mapper.hpp"
#include "ores.trading.core/repository/trade_party_role_entity.hpp"
#include "ores.trading.core/repository/trade_party_role_mapper.hpp"
#include "ores.trading.core/repository/trade_portfolio_entity.hpp"
#include "ores.trading.core/repository/trade_portfolio_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

namespace {

auto& lg() {
    static auto instance =
        ores::logging::make_logger("ores.trading.repository.trade_component_queries");
    return instance;
}

}

std::vector<domain::trade_additional_field>
read_additional_fields_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<trade_additional_field_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) && "valid_to"_c == max.value()) |
        order_by("trade_id"_c, "sequence_number"_c);

    return execute_read_query<trade_additional_field_entity, domain::trade_additional_field>(
        ctx,
        query,
        [](const auto& entities) { return trade_additional_field_mapper::map(entities); },
        lg(),
        "Reading trade additional fields by trade ids.");
}

std::vector<domain::trade_identifier>
read_identifiers_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<trade_identifier_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) && "valid_to"_c == max.value()) |
        order_by("trade_id"_c, "id_type"_c);

    return execute_read_query<trade_identifier_entity, domain::trade_identifier>(
        ctx,
        query,
        [](const auto& entities) { return trade_identifier_mapper::map(entities); },
        lg(),
        "Reading trade identifiers by trade ids.");
}

std::vector<domain::trade_portfolio>
read_portfolios_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<trade_portfolio_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) && "valid_to"_c == max.value()) |
        order_by("trade_id"_c, "sequence_number"_c);

    return execute_read_query<trade_portfolio_entity, domain::trade_portfolio>(
        ctx,
        query,
        [](const auto& entities) { return trade_portfolio_mapper::map(entities); },
        lg(),
        "Reading trade portfolios by trade ids.");
}

std::vector<domain::trade_party_role>
read_party_roles_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<trade_party_role_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) && "valid_to"_c == max.value()) |
        order_by("trade_id"_c, "role"_c);

    return execute_read_query<trade_party_role_entity, domain::trade_party_role>(
        ctx,
        query,
        [](const auto& entities) { return trade_party_role_mapper::map(entities); },
        lg(),
        "Reading trade party roles by trade ids.");
}


std::vector<std::string>
read_instrument_digests(context ctx, const std::string& trade_id, const std::string& trade_type) {
    std::vector<std::string> digests;
    const auto append = [&digests](const auto& rows) {
        for (const auto& row : rows)
            digests.push_back(domain::economic_digest(row));
    };
    const auto ids = std::vector<std::string>{trade_id};

    /*
     * The instrument the trade's type routes to. A type the catalogue routes
     * nowhere has no instrument, and contributes nothing.
     */
    if (const auto table = domain::instrument_table_for(trade_type)) {
        switch (*table) {
        case domain::instrument_table::bond_instrument:
            append(bond_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::commodity_instrument:
            append(commodity_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::composite_instrument:
            append(composite_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::credit_instrument:
            append(credit_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_accumulator_instrument:
            append(equity_accumulator_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_asian_option_instrument:
            append(equity_asian_option_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_barrier_option_instrument:
            append(equity_barrier_option_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_digital_option_instrument:
            append(equity_digital_option_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_forward_instrument:
            append(equity_forward_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_option_instrument:
            append(equity_option_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_position_instrument:
            append(equity_position_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_swap_instrument:
            append(equity_swap_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_variance_swap_instrument:
            append(equity_variance_swap_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::fx_accumulator_instrument:
            append(fx_accumulator_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::fx_asian_forward_instrument:
            append(fx_asian_forward_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::fx_barrier_option_instrument:
            append(fx_barrier_option_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::fx_digital_option_instrument:
            append(fx_digital_option_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::fx_forward_instrument:
            append(fx_forward_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::fx_vanilla_option_instrument:
            append(fx_vanilla_option_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::fx_variance_swap_instrument:
            append(fx_variance_swap_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::rate_instrument:
            /*
             * The header, then the one product fact table the trade type
             * selects; the others hold no row for this trade.
             */
            append(rate_instrument_repository{}.read_latest(ctx, trade_id));
            append(balance_guaranteed_swap_instrument_repository{}.read_latest(ctx, trade_id));
            append(callable_swap_instrument_repository{}.read_latest(ctx, trade_id));
            append(cap_floor_instrument_repository{}.read_latest(ctx, trade_id));
            append(fra_instrument_repository{}.read_latest(ctx, trade_id));
            append(inflation_swap_instrument_repository{}.read_latest(ctx, trade_id));
            append(knock_out_swap_instrument_repository{}.read_latest(ctx, trade_id));
            append(swaption_instrument_repository{}.read_latest(ctx, trade_id));
            append(vanilla_swap_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::scripted_instrument:
            append(scripted_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        }
    }

    /*
     * The legs and amounts beneath it, and the option, schedule and strike
     * blocks the bond families keep beside the instrument.
     */
    append(read_legs_by_trade_ids(ctx, ids));
    append(read_leg_amounts_by_trade_ids(ctx, ids));
    append(read_leg_rates_by_trade_ids(ctx, ids));
    append(read_leg_amortizations_by_trade_ids(ctx, ids));
    append(read_forwards_by_trade_ids(ctx, ids));
    append(read_strikes_by_trade_ids(ctx, ids));
    append(read_options_by_trade_ids(ctx, ids));
    append(read_option_exercise_fees_by_trade_ids(ctx, ids));
    append(read_option_payment_dates_by_trade_ids(ctx, ids));
    append(read_option_premiums_by_trade_ids(ctx, ids));
    append(read_schedules_by_trade_ids(ctx, ids));
    append(read_schedule_dates_by_trade_ids(ctx, ids));

    return digests;
}

}
