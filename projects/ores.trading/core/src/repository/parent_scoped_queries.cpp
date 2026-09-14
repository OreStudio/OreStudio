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
#include "ores.trading.core/repository/bond_forward_entity.hpp"
#include "ores.trading.core/repository/bond_forward_mapper.hpp"
#include "ores.trading.core/repository/bond_future_delivery_basket_entity.hpp"
#include "ores.trading.core/repository/bond_future_delivery_basket_mapper.hpp"
#include "ores.trading.core/repository/bond_issue_call_date_entity.hpp"
#include "ores.trading.core/repository/bond_issue_call_date_mapper.hpp"
#include "ores.trading.core/repository/bond_issue_conversion_target_entity.hpp"
#include "ores.trading.core/repository/bond_issue_conversion_target_mapper.hpp"
#include "ores.trading.core/repository/bond_leg_amortization_entity.hpp"
#include "ores.trading.core/repository/bond_leg_amortization_mapper.hpp"
#include "ores.trading.core/repository/bond_leg_amount_entity.hpp"
#include "ores.trading.core/repository/bond_leg_amount_mapper.hpp"
#include "ores.trading.core/repository/bond_leg_entity.hpp"
#include "ores.trading.core/repository/bond_leg_mapper.hpp"
#include "ores.trading.core/repository/bond_leg_rate_entity.hpp"
#include "ores.trading.core/repository/bond_leg_rate_mapper.hpp"
#include "ores.trading.core/repository/instrument_option_entity.hpp"
#include "ores.trading.core/repository/instrument_option_exercise_fee_entity.hpp"
#include "ores.trading.core/repository/instrument_option_exercise_fee_mapper.hpp"
#include "ores.trading.core/repository/instrument_option_mapper.hpp"
#include "ores.trading.core/repository/instrument_option_payment_date_entity.hpp"
#include "ores.trading.core/repository/instrument_option_payment_date_mapper.hpp"
#include "ores.trading.core/repository/instrument_option_premium_entity.hpp"
#include "ores.trading.core/repository/instrument_option_premium_mapper.hpp"
#include "ores.trading.core/repository/instrument_schedule_date_entity.hpp"
#include "ores.trading.core/repository/instrument_schedule_date_mapper.hpp"
#include "ores.trading.core/repository/instrument_schedule_entity.hpp"
#include "ores.trading.core/repository/instrument_schedule_mapper.hpp"
#include "ores.trading.core/repository/instrument_strike_entity.hpp"
#include "ores.trading.core/repository/instrument_strike_mapper.hpp"
#include "ores.trading.core/repository/trade_envelope_additional_field_entity.hpp"
#include "ores.trading.core/repository/trade_envelope_additional_field_mapper.hpp"
#include "ores.trading.core/repository/trade_envelope_entity.hpp"
#include "ores.trading.core/repository/trade_envelope_mapper.hpp"
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
    static auto instance =
        ores::logging::make_logger("ores.trading.repository.parent_scoped_queries");
    return instance;
}

}

std::vector<domain::trade_envelope>
read_envelopes_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<trade_envelope_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) && "valid_to"_c == max.value()) |
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
    const auto query =
        sqlgen::read<std::vector<trade_envelope_portfolio_id_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) && "valid_to"_c == max.value()) |
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
    const auto query =
        sqlgen::read<std::vector<trade_envelope_additional_field_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) && "valid_to"_c == max.value()) |
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
    const auto query =
        sqlgen::read<std::vector<bond_issue_call_date_entity>> |
        where("tenant_id"_c == tid && "issue_id"_c.in(issue_ids) && "valid_to"_c == max.value()) |
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
    const auto query =
        sqlgen::read<std::vector<bond_issue_conversion_target_entity>> |
        where("tenant_id"_c == tid && "issue_id"_c.in(issue_ids) && "valid_to"_c == max.value()) |
        order_by("issue_id"_c, "sequence_number"_c);

    return execute_read_query<bond_issue_conversion_target_entity,
                              domain::bond_issue_conversion_target>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_conversion_target_mapper::map(entities); },
        lg(),
        "Reading bond issue conversion targets by issue ids.");
}

std::vector<domain::bond_leg>
read_legs_by_instrument_ids(context ctx, const std::vector<std::string>& instrument_ids) {
    if (instrument_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_leg_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c.in(instrument_ids) &&
                             "valid_to"_c == max.value()) |
                       order_by("instrument_id"_c, "leg_role"_c, "leg_number"_c);

    return execute_read_query<bond_leg_entity, domain::bond_leg>(
        ctx,
        query,
        [](const auto& entities) { return bond_leg_mapper::map(entities); },
        lg(),
        "Reading bond legs by instrument ids.");
}

std::vector<domain::bond_leg_amount>
read_leg_amounts_by_instrument_ids(context ctx, const std::vector<std::string>& instrument_ids) {
    if (instrument_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<bond_leg_amount_entity>> |
        where("tenant_id"_c == tid && "instrument_id"_c.in(instrument_ids) &&
              "valid_to"_c == max.value()) |
        order_by(
            "instrument_id"_c, "leg_role"_c, "leg_number"_c, "amount_role"_c, "sequence_number"_c);

    return execute_read_query<bond_leg_amount_entity, domain::bond_leg_amount>(
        ctx,
        query,
        [](const auto& entities) { return bond_leg_amount_mapper::map(entities); },
        lg(),
        "Reading bond leg amounts by instrument ids.");
}

std::vector<domain::bond_leg_rate>
read_leg_rates_by_instrument_ids(context ctx, const std::vector<std::string>& instrument_ids) {
    if (instrument_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_leg_rate_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c.in(instrument_ids) &&
                             "valid_to"_c == max.value()) |
                       order_by("instrument_id"_c, "leg_role"_c, "leg_number"_c);

    return execute_read_query<bond_leg_rate_entity, domain::bond_leg_rate>(
        ctx,
        query,
        [](const auto& entities) { return bond_leg_rate_mapper::map(entities); },
        lg(),
        "Reading bond leg rates by instrument ids.");
}

std::vector<domain::bond_leg_amortization>
read_leg_amortizations_by_instrument_ids(context ctx,
                                         const std::vector<std::string>& instrument_ids) {
    if (instrument_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<bond_leg_amortization_entity>> |
        where("tenant_id"_c == tid && "instrument_id"_c.in(instrument_ids) &&
              "valid_to"_c == max.value()) |
        order_by("instrument_id"_c, "leg_role"_c, "leg_number"_c, "sequence_number"_c);

    return execute_read_query<bond_leg_amortization_entity, domain::bond_leg_amortization>(
        ctx,
        query,
        [](const auto& entities) { return bond_leg_amortization_mapper::map(entities); },
        lg(),
        "Reading bond leg amortizations by instrument ids.");
}

std::vector<domain::instrument_schedule>
read_schedules_by_instrument_ids(context ctx, const std::vector<std::string>& instrument_ids) {
    if (instrument_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_schedule_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c.in(instrument_ids) &&
                             "valid_to"_c == max.value()) |
                       order_by("instrument_id"_c,
                                "owner_role"_c,
                                "owner_number"_c,
                                "schedule_role"_c,
                                "sequence_number"_c);

    return execute_read_query<instrument_schedule_entity, domain::instrument_schedule>(
        ctx,
        query,
        [](const auto& entities) { return instrument_schedule_mapper::map(entities); },
        lg(),
        "Reading instrument schedules by instrument ids.");
}

std::vector<domain::instrument_schedule_date>
read_schedule_dates_by_instrument_ids(context ctx, const std::vector<std::string>& instrument_ids) {
    if (instrument_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_schedule_date_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c.in(instrument_ids) &&
                             "valid_to"_c == max.value()) |
                       order_by("instrument_id"_c,
                                "owner_role"_c,
                                "owner_number"_c,
                                "schedule_role"_c,
                                "schedule_sequence_number"_c,
                                "sequence_number"_c);

    return execute_read_query<instrument_schedule_date_entity, domain::instrument_schedule_date>(
        ctx,
        query,
        [](const auto& entities) { return instrument_schedule_date_mapper::map(entities); },
        lg(),
        "Reading instrument schedule dates by instrument ids.");
}

std::vector<domain::instrument_option>
read_options_by_instrument_ids(context ctx, const std::vector<std::string>& instrument_ids) {
    if (instrument_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_option_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c.in(instrument_ids) &&
                             "valid_to"_c == max.value()) |
                       order_by("instrument_id"_c);

    return execute_read_query<instrument_option_entity, domain::instrument_option>(
        ctx,
        query,
        [](const auto& entities) { return instrument_option_mapper::map(entities); },
        lg(),
        "Reading instrument options by instrument ids.");
}

std::vector<domain::instrument_option_premium>
read_option_premiums_by_instrument_ids(context ctx,
                                       const std::vector<std::string>& instrument_ids) {
    if (instrument_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_option_premium_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c.in(instrument_ids) &&
                             "valid_to"_c == max.value()) |
                       order_by("instrument_id"_c, "sequence_number"_c);

    return execute_read_query<instrument_option_premium_entity, domain::instrument_option_premium>(
        ctx,
        query,
        [](const auto& entities) { return instrument_option_premium_mapper::map(entities); },
        lg(),
        "Reading instrument option premiums by instrument ids.");
}

std::vector<domain::instrument_option_exercise_fee>
read_option_exercise_fees_by_instrument_ids(context ctx,
                                            const std::vector<std::string>& instrument_ids) {
    if (instrument_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_option_exercise_fee_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c.in(instrument_ids) &&
                             "valid_to"_c == max.value()) |
                       order_by("instrument_id"_c, "sequence_number"_c);

    return execute_read_query<instrument_option_exercise_fee_entity,
                              domain::instrument_option_exercise_fee>(
        ctx,
        query,
        [](const auto& entities) { return instrument_option_exercise_fee_mapper::map(entities); },
        lg(),
        "Reading instrument option exercise fees by instrument ids.");
}

std::vector<domain::instrument_option_payment_date>
read_option_payment_dates_by_instrument_ids(context ctx,
                                            const std::vector<std::string>& instrument_ids) {
    if (instrument_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_option_payment_date_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c.in(instrument_ids) &&
                             "valid_to"_c == max.value()) |
                       order_by("instrument_id"_c, "sequence_number"_c);

    return execute_read_query<instrument_option_payment_date_entity,
                              domain::instrument_option_payment_date>(
        ctx,
        query,
        [](const auto& entities) { return instrument_option_payment_date_mapper::map(entities); },
        lg(),
        "Reading instrument option payment dates by instrument ids.");
}

std::vector<domain::instrument_strike>
read_strikes_by_instrument_ids(context ctx, const std::vector<std::string>& instrument_ids) {
    if (instrument_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_strike_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c.in(instrument_ids) &&
                             "valid_to"_c == max.value()) |
                       order_by("instrument_id"_c);

    return execute_read_query<instrument_strike_entity, domain::instrument_strike>(
        ctx,
        query,
        [](const auto& entities) { return instrument_strike_mapper::map(entities); },
        lg(),
        "Reading instrument strikes by instrument ids.");
}

std::vector<domain::bond_forward>
read_forwards_by_instrument_ids(context ctx, const std::vector<std::string>& instrument_ids) {
    if (instrument_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_forward_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c.in(instrument_ids) &&
                             "valid_to"_c == max.value()) |
                       order_by("instrument_id"_c);

    return execute_read_query<bond_forward_entity, domain::bond_forward>(
        ctx,
        query,
        [](const auto& entities) { return bond_forward_mapper::map(entities); },
        lg(),
        "Reading bond forwards by instrument ids.");
}

std::vector<domain::bond_future_delivery_basket>
read_delivery_baskets_by_instrument_ids(context ctx,
                                        const std::vector<std::string>& instrument_ids) {
    if (instrument_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_future_delivery_basket_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c.in(instrument_ids) &&
                             "valid_to"_c == max.value()) |
                       order_by("instrument_id"_c, "sequence_number"_c);

    return execute_read_query<bond_future_delivery_basket_entity,
                              domain::bond_future_delivery_basket>(
        ctx,
        query,
        [](const auto& entities) { return bond_future_delivery_basket_mapper::map(entities); },
        lg(),
        "Reading bond future delivery baskets by instrument ids.");
}

}
