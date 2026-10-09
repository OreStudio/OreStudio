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
#include "ores.trading.core/service/trade_export_service.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/domain/trade_type_routing.hpp"
#include "ores.trading.core/repository/callable_swap_call_date_repository.hpp"
#include "ores.trading.core/repository/commodity_basket_constituent_repository.hpp"
#include "ores.trading.core/repository/composite_leg_repository.hpp"
#include "ores.trading.core/repository/equity_position_option_underlying_repository.hpp"
#include "ores.trading.core/repository/parent_scoped_queries.hpp"
#include "ores.trading.core/repository/swap_leg_amount_repository.hpp"
#include "ores.trading.core/repository/swap_leg_rate_repository.hpp"
#include "ores.trading.core/repository/swap_leg_repository.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include "ores.trading.core/service/balance_guaranteed_swap_instrument_service.hpp"
#include "ores.trading.core/service/bond_instrument_reader.hpp"
#include "ores.trading.core/service/callable_swap_instrument_service.hpp"
#include "ores.trading.core/service/cap_floor_instrument_service.hpp"
#include "ores.trading.core/service/commodity_instrument_service.hpp"
#include "ores.trading.core/service/composite_instrument_service.hpp"
#include "ores.trading.core/service/credit_instrument_service.hpp"
#include "ores.trading.core/service/equity_accumulator_instrument_service.hpp"
#include "ores.trading.core/service/equity_asian_option_instrument_service.hpp"
#include "ores.trading.core/service/equity_barrier_option_instrument_service.hpp"
#include "ores.trading.core/service/equity_digital_option_instrument_service.hpp"
#include "ores.trading.core/service/equity_forward_instrument_service.hpp"
#include "ores.trading.core/service/equity_option_instrument_service.hpp"
#include "ores.trading.core/service/equity_position_instrument_service.hpp"
#include "ores.trading.core/service/equity_swap_instrument_service.hpp"
#include "ores.trading.core/service/equity_variance_swap_instrument_service.hpp"
#include "ores.trading.core/service/fra_instrument_service.hpp"
#include "ores.trading.core/service/fx_accumulator_instrument_service.hpp"
#include "ores.trading.core/service/fx_asian_forward_instrument_service.hpp"
#include "ores.trading.core/service/fx_barrier_option_instrument_service.hpp"
#include "ores.trading.core/service/fx_digital_option_instrument_service.hpp"
#include "ores.trading.core/service/fx_forward_instrument_service.hpp"
#include "ores.trading.core/service/fx_vanilla_option_instrument_service.hpp"
#include "ores.trading.core/service/fx_variance_swap_instrument_service.hpp"
#include "ores.trading.core/service/inflation_swap_instrument_service.hpp"
#include "ores.trading.core/service/knock_out_swap_instrument_service.hpp"
#include "ores.trading.core/service/rate_instrument_service.hpp"
#include "ores.trading.core/service/scripted_instrument_service.hpp"
#include "ores.trading.core/service/swaption_instrument_service.hpp"
#include "ores.trading.core/service/trade_envelope_reader.hpp"
#include "ores.trading.core/service/vanilla_swap_instrument_service.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <iterator>
#include <unordered_map>
#include <unordered_set>

namespace ores::trading::service {

using namespace ores::logging;
using messaging::trade_export_item;

namespace {

std::string to_uuid_array(const std::vector<std::string>& ids) {
    std::string r = "{";
    for (std::size_t i = 0; i < ids.size(); ++i) {
        if (i != 0)
            r += ',';
        r += ids[i];
    }
    return r + "}";
}

/**
 * Resolves a page of trades to their instruments and envelopes: bucket the
 * trade ids by the table the trade-type catalogue routes each type to, read
 * each table in one batch, and append what it returns to the page's batch.
 * Every element carries the trade id its instrument does, so nothing here
 * pairs a row with the item it belongs to.
 */
template <typename Ctx>
void populate_instruments_for_trades(const Ctx& ctx,
                                     std::vector<trade_export_item>& items,
                                     domain::instrument_batch& batch) {
    using ores::trading::domain::instrument_table;
    using ores::trading::domain::instrument_table_for;

    // Phase 1: bucket instrument IDs by the table that holds them
    std::vector<std::string> bond_ids, credit_ids, commodity_ids, scripted_ids, composite_ids,
        rate_ids, fxfwd_ids, fxopt_ids, fxbar_ids, fxdig_ids, fxasn_ids, fxacc_ids, fxvar_ids,
        eq_opt_ids, eq_fwd_ids, eq_swp_ids, eq_var_ids, eq_bar_ids, eq_asn_ids, eq_dig_ids,
        eq_acc_ids, eq_pos_ids;

    for (const auto& item : items) {
        const auto& t = item.anchor;
        // The trade-type catalogue decides which table holds each type;
        // a type it routes nowhere has no instrument to read.
        const auto table = instrument_table_for(t.trade_type);
        if (!table)
            continue;
        // The instrument is keyed by the trade it belongs to, so the
        // trade's own id is the key the product tables are read by.
        const auto id = boost::uuids::to_string(t.id);
        switch (*table) {
            case instrument_table::bond_instrument:
                bond_ids.push_back(id);
                break;
            case instrument_table::commodity_instrument:
                commodity_ids.push_back(id);
                break;
            case instrument_table::composite_instrument:
                composite_ids.push_back(id);
                break;
            case instrument_table::credit_instrument:
                credit_ids.push_back(id);
                break;
            case instrument_table::equity_accumulator_instrument:
                eq_acc_ids.push_back(id);
                break;
            case instrument_table::equity_asian_option_instrument:
                eq_asn_ids.push_back(id);
                break;
            case instrument_table::equity_barrier_option_instrument:
                eq_bar_ids.push_back(id);
                break;
            case instrument_table::equity_digital_option_instrument:
                eq_dig_ids.push_back(id);
                break;
            case instrument_table::equity_forward_instrument:
                eq_fwd_ids.push_back(id);
                break;
            case instrument_table::equity_option_instrument:
                eq_opt_ids.push_back(id);
                break;
            case instrument_table::equity_position_instrument:
                eq_pos_ids.push_back(id);
                break;
            case instrument_table::equity_swap_instrument:
                eq_swp_ids.push_back(id);
                break;
            case instrument_table::equity_variance_swap_instrument:
                eq_var_ids.push_back(id);
                break;
            case instrument_table::fx_accumulator_instrument:
                fxacc_ids.push_back(id);
                break;
            case instrument_table::fx_asian_forward_instrument:
                fxasn_ids.push_back(id);
                break;
            case instrument_table::fx_barrier_option_instrument:
                fxbar_ids.push_back(id);
                break;
            case instrument_table::fx_digital_option_instrument:
                fxdig_ids.push_back(id);
                break;
            case instrument_table::fx_forward_instrument:
                fxfwd_ids.push_back(id);
                break;
            case instrument_table::fx_vanilla_option_instrument:
                fxopt_ids.push_back(id);
                break;
            case instrument_table::fx_variance_swap_instrument:
                fxvar_ids.push_back(id);
                break;
            case instrument_table::rate_instrument:
                rate_ids.push_back(id);
                break;
            case instrument_table::scripted_instrument:
                scripted_ids.push_back(id);
                break;
        }
    }

    // Moves every row of a read into the batch array that holds its type.
    auto take = []<typename Rows>(Rows& rows, auto& members) {
        members.insert(members.end(),
                       std::make_move_iterator(rows.begin()),
                       std::make_move_iterator(rows.end()));
    };

    // Phase 2: the children, one read per table over the whole page. A child
    // carries the trade id its instrument does, so the reads need no pairing.
    {
        const auto& all_swap = rate_ids;
        if (!all_swap.empty()) {
            repository::swap_leg_repository leg_repo;
            auto legs = leg_repo.read_by_instruments_batch(ctx, all_swap);
            take(legs, batch.swap_legs);
            repository::swap_leg_amount_repository amount_repo;
            auto amounts = amount_repo.read_by_instruments_batch(ctx, all_swap);
            take(amounts, batch.swap_leg_amounts);
            repository::swap_leg_rate_repository rate_repo;
            auto rates = rate_repo.read_by_instruments_batch(ctx, all_swap);
            take(rates, batch.swap_leg_rates);
        }
    }
    if (!composite_ids.empty()) {
        repository::composite_leg_repository comp_leg_repo;
        const std::unordered_set<std::string> wanted(composite_ids.begin(), composite_ids.end());
        for (auto& leg : comp_leg_repo.read_latest(ctx)) {
            const auto key = boost::uuids::to_string(leg.identity.trade_id);
            if (wanted.contains(key))
                batch.composite_legs.push_back(std::move(leg));
        }
    }
    if (!rate_ids.empty()) {
        repository::callable_swap_call_date_repository call_date_repo;
        auto dates = call_date_repo.read_by_instruments_batch(ctx, rate_ids);
        take(dates, batch.callable_swap_call_dates);
        // The shared children the rates family writes: the strikes, the
        // schedules and the option block, read for the same trade ids.
        auto schedules = repository::read_schedules_by_trade_ids(ctx, rate_ids);
        take(schedules, batch.instrument_schedules);
        auto schedule_dates = repository::read_schedule_dates_by_trade_ids(ctx, rate_ids);
        take(schedule_dates, batch.instrument_schedule_dates);
        auto options = repository::read_options_by_trade_ids(ctx, rate_ids);
        take(options, batch.instrument_options);
        auto premiums = repository::read_option_premiums_by_trade_ids(ctx, rate_ids);
        take(premiums, batch.instrument_option_premiums);
        auto fees = repository::read_option_exercise_fees_by_trade_ids(ctx, rate_ids);
        take(fees, batch.instrument_option_exercise_fees);
        auto payment_dates = repository::read_option_payment_dates_by_trade_ids(ctx, rate_ids);
        take(payment_dates, batch.instrument_option_payment_dates);
        auto exercise_prices = repository::read_option_exercise_prices_by_trade_ids(ctx, rate_ids);
        take(exercise_prices, batch.instrument_option_exercise_prices);
        auto strikes = repository::read_strikes_by_trade_ids(ctx, rate_ids);
        take(strikes, batch.instrument_strikes);
    }
    if (!commodity_ids.empty()) {
        repository::commodity_basket_constituent_repository constituent_repo;
        auto constituents = constituent_repo.read_by_instruments_batch(ctx, commodity_ids);
        take(constituents, batch.commodity_basket_constituents);
    }
    if (!eq_pos_ids.empty()) {
        repository::equity_position_option_underlying_repository underlying_repo;
        auto underlyings = underlying_repo.read_by_instruments_batch(ctx, eq_pos_ids);
        take(underlyings, batch.equity_position_option_underlyings);
    }

    // Phase 3: the instruments themselves.
    if (!bond_ids.empty()) {
        service::bond_instrument_reader reader(ctx);
        for (auto& [id, data] : reader.read_instruments(bond_ids))
            batch.bond_instruments.push_back(std::move(data));
    }
    if (!credit_ids.empty()) {
        service::credit_instrument_service svc(ctx);
        auto rows = svc.get_credit_instruments(credit_ids);
        take(rows, batch.credit_instruments);
    }
    if (!commodity_ids.empty()) {
        service::commodity_instrument_service svc(ctx);
        auto rows = svc.get_commodity_instruments(commodity_ids);
        take(rows, batch.commodity_instruments);
    }
    if (!scripted_ids.empty()) {
        service::scripted_instrument_service svc(ctx);
        auto rows = svc.get_scripted_instruments(scripted_ids);
        take(rows, batch.scripted_instruments);
    }
    if (!composite_ids.empty()) {
        service::composite_instrument_service svc(ctx);
        auto rows = svc.get_composite_instruments(composite_ids);
        take(rows, batch.composite_instruments);
    }

    // The rates family: the header is the routed instrument and the eight
    // product tables are its facts, read for the same trade ids.
    if (!rate_ids.empty()) {
        service::rate_instrument_service svc(ctx);
        auto rows = svc.get_rate_instruments(rate_ids);
        take(rows, batch.rate_instruments);
    }
    if (!rate_ids.empty()) {
        service::fra_instrument_service svc(ctx);
        auto rows = svc.get_fra_instruments(rate_ids);
        take(rows, batch.fra_instruments);
    }
    if (!rate_ids.empty()) {
        service::vanilla_swap_instrument_service svc(ctx);
        auto rows = svc.get_vanilla_swap_instruments(rate_ids);
        take(rows, batch.vanilla_swap_instruments);
    }
    if (!rate_ids.empty()) {
        service::cap_floor_instrument_service svc(ctx);
        auto rows = svc.get_cap_floor_instruments(rate_ids);
        take(rows, batch.cap_floor_instruments);
    }
    if (!rate_ids.empty()) {
        service::swaption_instrument_service svc(ctx);
        auto rows = svc.get_swaption_instruments(rate_ids);
        take(rows, batch.swaption_instruments);
    }
    if (!rate_ids.empty()) {
        service::balance_guaranteed_swap_instrument_service svc(ctx);
        auto rows = svc.get_balance_guaranteed_swap_instruments(rate_ids);
        take(rows, batch.balance_guaranteed_swap_instruments);
    }
    if (!rate_ids.empty()) {
        service::callable_swap_instrument_service svc(ctx);
        auto rows = svc.get_callable_swap_instruments(rate_ids);
        take(rows, batch.callable_swap_instruments);
    }
    if (!rate_ids.empty()) {
        service::knock_out_swap_instrument_service svc(ctx);
        auto rows = svc.get_knock_out_swap_instruments(rate_ids);
        take(rows, batch.knock_out_swap_instruments);
    }
    if (!rate_ids.empty()) {
        service::inflation_swap_instrument_service svc(ctx);
        auto rows = svc.get_inflation_swap_instruments(rate_ids);
        take(rows, batch.inflation_swap_instruments);
    }

    // The FX family.
    if (!fxfwd_ids.empty()) {
        service::fx_forward_instrument_service svc(ctx);
        auto rows = svc.get_fx_forward_instruments(fxfwd_ids);
        take(rows, batch.fx_forward_instruments);
    }
    if (!fxopt_ids.empty()) {
        service::fx_vanilla_option_instrument_service svc(ctx);
        auto rows = svc.get_fx_vanilla_option_instruments(fxopt_ids);
        take(rows, batch.fx_vanilla_option_instruments);
    }
    if (!fxbar_ids.empty()) {
        service::fx_barrier_option_instrument_service svc(ctx);
        auto rows = svc.get_fx_barrier_option_instruments(fxbar_ids);
        take(rows, batch.fx_barrier_option_instruments);
    }
    if (!fxdig_ids.empty()) {
        service::fx_digital_option_instrument_service svc(ctx);
        auto rows = svc.get_fx_digital_option_instruments(fxdig_ids);
        take(rows, batch.fx_digital_option_instruments);
    }
    if (!fxasn_ids.empty()) {
        service::fx_asian_forward_instrument_service svc(ctx);
        auto rows = svc.get_fx_asian_forward_instruments(fxasn_ids);
        take(rows, batch.fx_asian_forward_instruments);
    }
    if (!fxacc_ids.empty()) {
        service::fx_accumulator_instrument_service svc(ctx);
        auto rows = svc.get_fx_accumulator_instruments(fxacc_ids);
        take(rows, batch.fx_accumulator_instruments);
    }
    if (!fxvar_ids.empty()) {
        service::fx_variance_swap_instrument_service svc(ctx);
        auto rows = svc.get_fx_variance_swap_instruments(fxvar_ids);
        take(rows, batch.fx_variance_swap_instruments);
    }

    // The equity family.
    if (!eq_opt_ids.empty()) {
        service::equity_option_instrument_service svc(ctx);
        auto rows = svc.get_equity_option_instruments(eq_opt_ids);
        take(rows, batch.equity_option_instruments);
    }
    if (!eq_fwd_ids.empty()) {
        service::equity_forward_instrument_service svc(ctx);
        auto rows = svc.get_equity_forward_instruments(eq_fwd_ids);
        take(rows, batch.equity_forward_instruments);
    }
    if (!eq_swp_ids.empty()) {
        service::equity_swap_instrument_service svc(ctx);
        auto rows = svc.get_equity_swap_instruments(eq_swp_ids);
        take(rows, batch.equity_swap_instruments);
    }
    if (!eq_var_ids.empty()) {
        service::equity_variance_swap_instrument_service svc(ctx);
        auto rows = svc.get_equity_variance_swap_instruments(eq_var_ids);
        take(rows, batch.equity_variance_swap_instruments);
    }
    if (!eq_bar_ids.empty()) {
        service::equity_barrier_option_instrument_service svc(ctx);
        auto rows = svc.get_equity_barrier_option_instruments(eq_bar_ids);
        take(rows, batch.equity_barrier_option_instruments);
    }
    if (!eq_asn_ids.empty()) {
        service::equity_asian_option_instrument_service svc(ctx);
        auto rows = svc.get_equity_asian_option_instruments(eq_asn_ids);
        take(rows, batch.equity_asian_option_instruments);
    }
    if (!eq_dig_ids.empty()) {
        service::equity_digital_option_instrument_service svc(ctx);
        auto rows = svc.get_equity_digital_option_instruments(eq_dig_ids);
        take(rows, batch.equity_digital_option_instruments);
    }
    if (!eq_acc_ids.empty()) {
        service::equity_accumulator_instrument_service svc(ctx);
        auto rows = svc.get_equity_accumulator_instruments(eq_acc_ids);
        take(rows, batch.equity_accumulator_instruments);
    }
    if (!eq_pos_ids.empty()) {
        service::equity_position_instrument_service svc(ctx);
        auto rows = svc.get_equity_position_instruments(eq_pos_ids);
        take(rows, batch.equity_position_instruments);
    }

    // Phase 5: fill the trade-level envelope, which is keyed by the
    // trade rather than the instrument and so crosses product types.
    std::vector<std::string> trade_ids;
    trade_ids.reserve(items.size());
    for (const auto& item : items)
        trade_ids.push_back(boost::uuids::to_string(item.anchor.id));

    service::trade_envelope_reader envelope_reader(ctx);
    auto envelopes = envelope_reader.read_envelopes(trade_ids);
    for (auto& item : items) {
        const auto id = boost::uuids::to_string(item.anchor.id);
        if (auto it = envelopes.find(id); it != envelopes.end())
            item.envelope = std::move(it->second);
    }
}

}

trade_export_service::trade_export_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<trade_export_item>
trade_export_service::export_node(const std::string& node_id,
                                  std::uint32_t offset,
                                  std::uint32_t limit,
                                  domain::instrument_batch& instruments) const {
    using database::repository::execute_parameterized_string_query;
    const auto trade_ids = execute_parameterized_string_query(
        ctx_,
        "SELECT b.trade_id::text FROM ores_trading_trade_bookings_tbl b "
        "WHERE b.tenant_id = $1::uuid "
        "AND b.valid_to = ores_utility_infinity_timestamp_fn() "
        "AND (NULLIF($2::text, '') IS NULL "
        "OR b.book_id IN (SELECT t.id FROM ores_trading_get_book_ids_for_node_fn($1::uuid, "
        "NULLIF($2::text, '')::uuid) AS t(id))) "
        "ORDER BY b.trade_id OFFSET $3::integer LIMIT $4::integer",
        {ctx_.tenant_id().to_string(), node_id, std::to_string(offset), std::to_string(limit)},
        lg(),
        "Reading the trades booked under a node.");
    return export_trades(trade_ids, instruments);
}

std::vector<trade_export_item>
trade_export_service::export_books(const std::vector<std::string>& book_ids,
                                   std::uint32_t offset,
                                   std::uint32_t limit,
                                   domain::instrument_batch& instruments) const {
    using database::repository::execute_parameterized_string_query;
    if (book_ids.empty())
        return {};
    const auto trade_ids = execute_parameterized_string_query(
        ctx_,
        "SELECT b.trade_id::text FROM ores_trading_trade_bookings_tbl b "
        "WHERE b.tenant_id = $1::uuid "
        "AND b.valid_to = ores_utility_infinity_timestamp_fn() "
        "AND b.book_id = ANY($2::uuid[]) "
        "ORDER BY b.trade_id OFFSET $3::integer LIMIT $4::integer",
        {ctx_.tenant_id().to_string(),
         to_uuid_array(book_ids),
         std::to_string(offset),
         std::to_string(limit)},
        lg(),
        "Reading the trades booked in a set of books.");
    return export_trades(trade_ids, instruments);
}

std::vector<trade_export_item>
trade_export_service::export_trades(const std::vector<std::string>& trade_ids,
                                    domain::instrument_batch& instruments) const {
    using database::repository::execute_parameterized_multi_column_query;
    if (trade_ids.empty())
        return {};

    std::unordered_map<std::string, std::string> ore_ids;
    for (const auto& row : execute_parameterized_multi_column_query(
             ctx_,
             "SELECT trade_id::text, id_value FROM ores_trading_trade_identifiers_tbl "
             "WHERE tenant_id = ores_iam_current_tenant_id_fn() AND id_type = 'ORE' "
             "AND valid_to = ores_utility_infinity_timestamp_fn() "
             "AND trade_id = ANY($1::uuid[])",
             {to_uuid_array(trade_ids)},
             lg(),
             "Reading the ORE identifiers of the exported trades."))
        ore_ids.emplace(*row[0], row[1].value_or(""));

    auto anchors = repository::trade_repository().read_latest(ctx_, trade_ids);
    std::ranges::sort(anchors, {}, &domain::trade::id);

    std::vector<trade_export_item> items;
    items.reserve(anchors.size());
    for (auto& anchor : anchors) {
        const auto id = boost::uuids::to_string(anchor.id);
        const auto ore_id = ore_ids.find(id);
        items.push_back(
            {.anchor = std::move(anchor), .ore_id = ore_id != ore_ids.end() ? ore_id->second : id});
    }
    populate_instruments_for_trades(ctx_, items, instruments);
    BOOST_LOG_SEV(lg(), debug) << "Exported " << items.size() << " trades.";
    return items;
}

}
