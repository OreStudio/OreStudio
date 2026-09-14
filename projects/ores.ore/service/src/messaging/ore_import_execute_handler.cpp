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
#include "ores.ore.service/messaging/ore_import_execute_handler.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.ore.api/messaging/ore_import_engine_protocol.hpp"
#include "ores.ore.api/net/ore_storage.hpp"
#include "ores.ore.core/domain/trade_mapper.hpp"
#include "ores.ore.core/planner/ore_import_planner.hpp"
#include "ores.ore.core/scanner/ore_directory_scanner.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.refdata.api/messaging/portfolio_protocol.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.storage/net/storage_transfer.hpp"
#include "ores.trading.api/messaging/ascot_protocol.hpp"
#include "ores.trading.api/messaging/bond_forward_protocol.hpp"
#include "ores.trading.api/messaging/bond_future_delivery_basket_protocol.hpp"
#include "ores.trading.api/messaging/bond_future_protocol.hpp"
#include "ores.trading.api/messaging/bond_instrument_protocol.hpp"
#include "ores.trading.api/messaging/bond_issue_call_date_protocol.hpp"
#include "ores.trading.api/messaging/bond_issue_conversion_target_protocol.hpp"
#include "ores.trading.api/messaging/bond_issue_protocol.hpp"
#include "ores.trading.api/messaging/bond_leg_amortization_protocol.hpp"
#include "ores.trading.api/messaging/bond_leg_amount_protocol.hpp"
#include "ores.trading.api/messaging/bond_leg_protocol.hpp"
#include "ores.trading.api/messaging/bond_leg_rate_protocol.hpp"
#include "ores.trading.api/messaging/bond_option_protocol.hpp"
#include "ores.trading.api/messaging/bond_repo_protocol.hpp"
#include "ores.trading.api/messaging/bond_trs_protocol.hpp"
#include "ores.trading.api/messaging/equity_accumulator_instrument_protocol.hpp"
#include "ores.trading.api/messaging/equity_asian_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/equity_barrier_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/equity_digital_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/equity_forward_instrument_protocol.hpp"
#include "ores.trading.api/messaging/equity_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/equity_position_instrument_protocol.hpp"
#include "ores.trading.api/messaging/equity_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/equity_variance_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_accumulator_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_asian_forward_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_barrier_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_digital_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_forward_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_vanilla_option_instrument_protocol.hpp"
#include "ores.trading.api/messaging/fx_variance_swap_instrument_protocol.hpp"
#include "ores.trading.api/messaging/instrument_option_exercise_fee_protocol.hpp"
#include "ores.trading.api/messaging/instrument_option_payment_date_protocol.hpp"
#include "ores.trading.api/messaging/instrument_option_premium_protocol.hpp"
#include "ores.trading.api/messaging/instrument_option_protocol.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include "ores.trading.api/messaging/instrument_schedule_date_protocol.hpp"
#include "ores.trading.api/messaging/instrument_schedule_protocol.hpp"
#include "ores.trading.api/messaging/instrument_strike_protocol.hpp"
#include "ores.trading.api/messaging/trade_envelope_additional_field_protocol.hpp"
#include "ores.trading.api/messaging/trade_envelope_portfolio_id_protocol.hpp"
#include "ores.trading.api/messaging/trade_envelope_protocol.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstdint>
#include <format>
#include <rfl/json.hpp>
#include <set>
#include <unordered_map>

namespace ores::ore::service::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;

namespace {

/**
 * @brief Makes an authenticated NATS request and deserialises the response.
 *
 * Returns nullopt and populates out_error on any error.
 */
template <typename Req>
std::optional<typename Req::response_type>
nats_call(ores::nats::service::nats_client& nats, const Req& request, std::string& out_error) {
    using Resp = typename Req::response_type;
    try {
        const auto& codec = ores::nats::default_wire_codec();
        const auto msg = nats.authenticated_request(Req::nats_subject, codec.encode(request));

        const auto err_it = msg.headers.find("X-Error");
        if (err_it != msg.headers.end()) {
            out_error = std::format("Service error on {}: {}", Req::nats_subject, err_it->second);
            return std::nullopt;
        }
        auto result = codec.decode<Resp>(msg.data);
        if (!result) {
            out_error = std::format(
                "Failed to parse response from {}: {}", Req::nats_subject, result.error().what());
            return std::nullopt;
        }
        if constexpr (requires {
                          result->success;
                          result->message;
                      }) {
            if (!result->success)
                out_error = result->message;
        }
        return *result;
    } catch (const std::exception& e) {
        out_error = std::format("Exception calling {}: {}", Req::nats_subject, e.what());
        return std::nullopt;
    }
}

/**
 * @brief Saves one trade's envelope and the two lists it carries.
 *
 * The envelope row is keyed by the trade, so the trade must be saved
 * first. A document that stated no envelope writes no row. The list
 * ordinals are the document's order and start at one.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string
save_envelope(Nats& nats,
              const boost::uuids::uuid& trade_id,
              const std::optional<ores::trading::domain::trade_envelope_data>& envelope) {
    if (!envelope)
        return {};

    using ores::trading::messaging::save_trade_envelope_additional_field_request;
    using ores::trading::messaging::save_trade_envelope_portfolio_id_request;
    using ores::trading::messaging::save_trade_envelope_request;

    std::string error;
    save_trade_envelope_request envelope_req;
    envelope_req.data.trade_id = trade_id;
    envelope_req.data.counter_party = envelope->counter_party;
    envelope_req.data.netting_set_id = envelope->netting_set_id;
    envelope_req.data.has_portfolio_ids = envelope->portfolio_ids.has_value();
    envelope_req.data.has_additional_fields = envelope->additional_fields.has_value();
    auto resp = nats_call(nats, envelope_req, error);
    if (!resp || !resp->success)
        return error.empty() ? "save_trade_envelope failed" : error;

    if (envelope->portfolio_ids) {
        int sequence_number = 0;
        for (const auto& portfolio_id : *envelope->portfolio_ids) {
            save_trade_envelope_portfolio_id_request child_req;
            child_req.data.trade_id = trade_id;
            child_req.data.sequence_number = ++sequence_number;
            child_req.data.portfolio_id = portfolio_id;
            auto child_resp = nats_call(nats, child_req, error);
            if (!child_resp || !child_resp->success)
                return error.empty() ? "save_trade_envelope_portfolio_id failed" : error;
        }
    }

    if (envelope->additional_fields) {
        int sequence_number = 0;
        for (const auto& field : *envelope->additional_fields) {
            save_trade_envelope_additional_field_request child_req;
            child_req.data.trade_id = trade_id;
            child_req.data.sequence_number = ++sequence_number;
            child_req.data.name = field.name;
            child_req.data.value = field.value;
            auto child_resp = nats_call(nats, child_req, error);
            if (!child_resp || !child_resp->success)
                return error.empty() ? "save_trade_envelope_additional_field failed" : error;
        }
    }

    return {};
}

/**
 * @brief Reads the identifier of every stored bond issue, keyed by its security id.
 *
 * One issue row serves every trade of an ISIN and its security_id is
 * unique among the current rows, so an import that meets an ISIN already
 * stored adopts that row instead of minting a second one. The read is
 * paged, and one pass covers the whole run.
 *
 * @return The map, empty when the read fails, with out_error set.
 */
template <typename Nats>
std::unordered_map<std::string, std::string>
read_bond_issue_ids_by_security(Nats& nats, std::string& out_error) {
    using ores::trading::messaging::get_bond_issues_request;

    constexpr std::uint32_t page_size = 200;
    constexpr int max_pages = 500;

    std::unordered_map<std::string, std::string> result;
    std::uint32_t offset = 0;
    for (int page = 0; page < max_pages; ++page) {
        get_bond_issues_request req;
        req.offset = offset;
        req.limit = page_size;
        auto resp = nats_call(nats, req, out_error);
        if (!resp || !resp->success)
            return {};
        for (const auto& issue : resp->issues)
            result[issue.security_id] = boost::uuids::to_string(issue.issue_id);
        if (resp->issues.size() < page_size)
            break;
        offset += page_size;
    }
    return result;
}

/**
 * @brief Saves one schedule of one owner: the entries and their dates.
 *
 * The entries are numbered from one within their role, so a reader
 * reassembles the document's order. Each entry writes its own row and
 * each of its dates writes a child row keyed to that entry's ordinal.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string save_schedule(Nats& nats,
                          const boost::uuids::uuid& instrument_id,
                          const std::string& owner_role,
                          int owner_number,
                          const std::string& schedule_role,
                          const ores::trading::domain::bond_schedule_data& schedule) {
    using ores::trading::messaging::save_instrument_schedule_date_request;
    using ores::trading::messaging::save_instrument_schedule_request;

    std::string error;
    int sequence_number = 0;
    for (const auto& rule : schedule.rules) {
        save_instrument_schedule_request req;
        req.data.instrument_id = instrument_id;
        req.data.owner_role = owner_role;
        req.data.owner_number = owner_number;
        req.data.schedule_role = schedule_role;
        req.data.sequence_number = ++sequence_number;
        req.data.schedule_kind = "rules";
        req.data.start_date = rule.start_date;
        req.data.end_date = rule.end_date;
        req.data.adjust_end_date_to_previous_month_end = rule.adjust_end_date_to_previous_month_end;
        req.data.tenor = rule.tenor;
        req.data.calendar = rule.calendar;
        req.data.convention = rule.convention;
        req.data.term_convention = rule.term_convention;
        req.data.rule = rule.rule;
        req.data.end_of_month = rule.end_of_month;
        req.data.end_of_month_convention = rule.end_of_month_convention;
        req.data.first_date = rule.first_date;
        req.data.last_date = rule.last_date;
        req.data.remove_first_date = rule.remove_first_date;
        req.data.remove_last_date = rule.remove_last_date;
        auto resp = nats_call(nats, req, error);
        if (!resp || !resp->success)
            return error.empty() ? "save_instrument_schedule failed" : error;
    }

    for (const auto& dates : schedule.dates) {
        save_instrument_schedule_request req;
        req.data.instrument_id = instrument_id;
        req.data.owner_role = owner_role;
        req.data.owner_number = owner_number;
        req.data.schedule_role = schedule_role;
        req.data.sequence_number = ++sequence_number;
        req.data.schedule_kind = "dates";
        req.data.calendar = dates.calendar;
        req.data.convention = dates.convention;
        req.data.tenor = dates.tenor;
        req.data.end_of_month = dates.end_of_month;
        req.data.include_duplicate_dates = dates.include_duplicate_dates;
        auto resp = nats_call(nats, req, error);
        if (!resp || !resp->success)
            return error.empty() ? "save_instrument_schedule failed" : error;

        int date_number = 0;
        for (const auto& date : dates.dates) {
            save_instrument_schedule_date_request date_req;
            date_req.data.instrument_id = instrument_id;
            date_req.data.owner_role = owner_role;
            date_req.data.owner_number = owner_number;
            date_req.data.schedule_role = schedule_role;
            date_req.data.schedule_sequence_number = req.data.sequence_number;
            date_req.data.sequence_number = ++date_number;
            date_req.data.schedule_date = date;
            auto date_resp = nats_call(nats, date_req, error);
            if (!date_resp || !date_resp->success)
                return error.empty() ? "save_instrument_schedule_date failed" : error;
        }
    }

    return {};
}

/**
 * @brief Saves one of a leg's six amount lists under the role that names it.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string save_leg_amounts(Nats& nats,
                             const boost::uuids::uuid& instrument_id,
                             const std::string& leg_role,
                             int leg_number,
                             const std::string& amount_role,
                             const std::vector<ores::trading::domain::bond_float_data>& amounts) {
    using ores::trading::messaging::save_bond_leg_amount_request;

    std::string error;
    int sequence_number = 0;
    for (const auto& amount : amounts) {
        save_bond_leg_amount_request req;
        req.data.instrument_id = instrument_id;
        req.data.leg_role = leg_role;
        req.data.leg_number = leg_number;
        req.data.amount_role = amount_role;
        req.data.sequence_number = ++sequence_number;
        req.data.value = amount.value;
        req.data.start_date = amount.start_date;
        auto resp = nats_call(nats, req, error);
        if (!resp || !resp->success)
            return error.empty() ? "save_bond_leg_amount failed" : error;
    }
    return {};
}

/**
 * @brief Saves one bond leg: its row, its amounts, its rate, its
 * amortizations and its five schedules.
 *
 * A leg that carries nothing writes no row, which is what a container
 * that came from a row set rather than from a document holds.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string save_leg(Nats& nats,
                     const boost::uuids::uuid& instrument_id,
                     const std::string& leg_role,
                     int leg_number,
                     const ores::trading::domain::bond_leg_data& leg) {
    using ores::trading::messaging::save_bond_leg_amortization_request;
    using ores::trading::messaging::save_bond_leg_rate_request;
    using ores::trading::messaging::save_bond_leg_request;

    if (leg.is_empty())
        return {};

    std::string error;
    save_bond_leg_request leg_req;
    leg_req.data.instrument_id = instrument_id;
    leg_req.data.leg_role = leg_role;
    leg_req.data.leg_number = leg_number;
    leg_req.data.payer = leg.payer;
    leg_req.data.leg_type = leg.leg_type;
    leg_req.data.currency = leg.currency;
    leg_req.data.payment_convention = leg.payment_convention;
    leg_req.data.payment_lag = leg.payment_lag;
    leg_req.data.payment_calendar = leg.payment_calendar;
    leg_req.data.day_counter = leg.day_counter;
    leg_req.data.last_period_day_counter = leg.last_period_day_counter;
    leg_req.data.notional_payment_lag = leg.notional_payment_lag;
    leg_req.data.strict_notional_dates = leg.strict_notional_dates;
    leg_req.data.indexings_from_asset_leg = leg.indexings_from_asset_leg;
    if (leg.settlement) {
        leg_req.data.settlement_fx_index = leg.settlement->fx_index;
        leg_req.data.settlement_fixing_date = leg.settlement->fixing_date;
    }
    auto leg_resp = nats_call(nats, leg_req, error);
    if (!leg_resp || !leg_resp->success)
        return error.empty() ? "save_bond_leg failed" : error;

    if (auto failure =
            save_leg_amounts(nats, instrument_id, leg_role, leg_number, "notional", leg.notionals);
        !failure.empty())
        return failure;

    if (leg.rate && leg.rate->fixed) {
        if (auto failure = save_leg_amounts(
                nats, instrument_id, leg_role, leg_number, "rate", leg.rate->fixed->rates);
            !failure.empty())
            return failure;
    }

    if (leg.rate && leg.rate->floating) {
        const auto& floating = *leg.rate->floating;
        const std::pair<std::string, const std::vector<ores::trading::domain::bond_float_data>*>
            lists[] = {{"spread", &floating.spreads},
                       {"cap", &floating.caps},
                       {"floor", &floating.floors},
                       {"gearing", &floating.gearings}};
        for (const auto& [role, amounts] : lists) {
            if (auto failure =
                    save_leg_amounts(nats, instrument_id, leg_role, leg_number, role, *amounts);
                !failure.empty())
                return failure;
        }

        save_bond_leg_rate_request rate_req;
        rate_req.data.instrument_id = instrument_id;
        rate_req.data.leg_role = leg_role;
        rate_req.data.leg_number = leg_number;
        rate_req.data.rate_kind = "floating";
        rate_req.data.index = floating.index;
        rate_req.data.is_in_arrears = floating.is_in_arrears;
        rate_req.data.last_recent_period = floating.last_recent_period;
        rate_req.data.last_recent_period_calendar = floating.last_recent_period_calendar;
        if (floating.fixing_days)
            rate_req.data.fixing_days = static_cast<std::int64_t>(*floating.fixing_days);
        rate_req.data.lookback = floating.lookback;
        rate_req.data.rate_cutoff = floating.rate_cutoff;
        rate_req.data.is_averaged = floating.is_averaged;
        rate_req.data.has_sub_periods = floating.has_sub_periods;
        rate_req.data.include_spread = floating.include_spread;
        rate_req.data.is_not_resetting_xccy = floating.is_not_resetting_xccy;
        rate_req.data.naked_option = floating.naked_option;
        rate_req.data.local_cap_floor = floating.local_cap_floor;
        rate_req.data.stub_use_original_curve = floating.stub_use_original_curve;
        rate_req.data.observation_shift = floating.observation_shift;
        if (floating.front_stub_interpolation) {
            const auto& stub = *floating.front_stub_interpolation;
            rate_req.data.front_stub_short_index = stub.short_index;
            rate_req.data.front_stub_long_index = stub.long_index;
            rate_req.data.front_stub_rounding_type = stub.rounding_type;
            rate_req.data.front_stub_rounding_precision = stub.rounding_precision;
        }
        if (floating.back_stub_interpolation) {
            const auto& stub = *floating.back_stub_interpolation;
            rate_req.data.back_stub_short_index = stub.short_index;
            rate_req.data.back_stub_long_index = stub.long_index;
            rate_req.data.back_stub_rounding_type = stub.rounding_type;
            rate_req.data.back_stub_rounding_precision = stub.rounding_precision;
        }
        auto rate_resp = nats_call(nats, rate_req, error);
        if (!rate_resp || !rate_resp->success)
            return error.empty() ? "save_bond_leg_rate failed" : error;

        if (auto failure = save_schedule(nats,
                                         instrument_id,
                                         leg_role,
                                         leg_number,
                                         "fixing_schedule",
                                         floating.fixing_schedule);
            !failure.empty())
            return failure;
        if (auto failure = save_schedule(nats,
                                         instrument_id,
                                         leg_role,
                                         leg_number,
                                         "reset_schedule",
                                         floating.reset_schedule);
            !failure.empty())
            return failure;
    } else if (leg.rate && leg.rate->formula_based) {
        const auto& formula = *leg.rate->formula_based;
        save_bond_leg_rate_request rate_req;
        rate_req.data.instrument_id = instrument_id;
        rate_req.data.leg_role = leg_role;
        rate_req.data.leg_number = leg_number;
        rate_req.data.rate_kind = "formula_based";
        rate_req.data.index = formula.index;
        rate_req.data.is_in_arrears = formula.is_in_arrears;
        rate_req.data.fixing_days = formula.fixing_days;
        rate_req.data.fixing_calendar = formula.fixing_calendar;
        auto rate_resp = nats_call(nats, rate_req, error);
        if (!rate_resp || !rate_resp->success)
            return error.empty() ? "save_bond_leg_rate failed" : error;
    } else if (leg.rate && leg.rate->fixed) {
        save_bond_leg_rate_request rate_req;
        rate_req.data.instrument_id = instrument_id;
        rate_req.data.leg_role = leg_role;
        rate_req.data.leg_number = leg_number;
        rate_req.data.rate_kind = "fixed";
        auto rate_resp = nats_call(nats, rate_req, error);
        if (!rate_resp || !rate_resp->success)
            return error.empty() ? "save_bond_leg_rate failed" : error;
    }

    int sequence_number = 0;
    for (const auto& amortization : leg.amortizations) {
        save_bond_leg_amortization_request req;
        req.data.instrument_id = instrument_id;
        req.data.leg_role = leg_role;
        req.data.leg_number = leg_number;
        req.data.sequence_number = ++sequence_number;
        req.data.amortization_type = amortization.type;
        req.data.value = amortization.value;
        req.data.start_date = amortization.start_date;
        req.data.end_date = amortization.end_date;
        req.data.frequency = amortization.frequency;
        req.data.underflow = amortization.underflow;
        auto resp = nats_call(nats, req, error);
        if (!resp || !resp->success)
            return error.empty() ? "save_bond_leg_amortization failed" : error;
    }

    if (auto failure =
            save_schedule(nats, instrument_id, leg_role, leg_number, "schedule", leg.schedule);
        !failure.empty())
        return failure;
    if (auto failure = save_schedule(
            nats, instrument_id, leg_role, leg_number, "payment_schedule", leg.payment_schedule);
        !failure.empty())
        return failure;

    if (!leg.payment_dates.empty()) {
        ores::trading::domain::bond_schedule_data dates;
        ores::trading::domain::bond_schedule_dates block;
        block.dates = leg.payment_dates;
        dates.dates.push_back(std::move(block));
        if (auto failure =
                save_schedule(nats, instrument_id, leg_role, leg_number, "payment_dates", dates);
            !failure.empty())
            return failure;
    }

    return {};
}

/**
 * @brief Saves an option block: its row, its three child lists and the two
 * spellings of its exercise dates.
 *
 * Every product that states an option block writes the row, not only the
 * one whose fact row carries the type and the strike.
 *
 * The row carries a flag per optional sub-block, because a set of null
 * columns cannot say whether the document stated the sub-block and left
 * it bare or omitted it. An empty block writes nothing.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string save_option_block(Nats& nats,
                              const boost::uuids::uuid& instrument_id,
                              const ores::trading::domain::bond_instrument_data& data) {
    using ores::trading::messaging::save_instrument_option_exercise_fee_request;
    using ores::trading::messaging::save_instrument_option_payment_date_request;
    using ores::trading::messaging::save_instrument_option_premium_request;
    using ores::trading::messaging::save_instrument_option_request;

    std::string error;
    if (data.option_data) {
        const auto& block = *data.option_data;
        save_instrument_option_request req;
        req.data.instrument_id = instrument_id;
        req.data.long_short = block.long_short;
        req.data.option_type = block.option_type;
        req.data.payoff_type = block.payoff_type;
        req.data.payoff_type_2 = block.payoff_type_2;
        req.data.style = block.style;
        req.data.notice_period = block.notice_period;
        req.data.notice_calendar = block.notice_calendar;
        req.data.notice_convention = block.notice_convention;
        req.data.mid_coupon_exercise = block.mid_coupon_exercise;
        req.data.settlement = block.settlement;
        req.data.settlement_method = block.settlement_method;
        req.data.pay_off_at_expiry = block.pay_off_at_expiry;
        req.data.premium_amount = block.premium_amount;
        req.data.premium_currency = block.premium_currency;
        req.data.premium_pay_date = block.premium_pay_date;
        req.data.exercise_prices = block.exercise_prices;
        req.data.exercise_fee_settlement_period = block.exercise_fee_settlement_period;
        req.data.exercise_fee_settlement_calendar = block.exercise_fee_settlement_calendar;
        req.data.exercise_fee_settlement_convention = block.exercise_fee_settlement_convention;
        req.data.automatic_exercise = block.automatic_exercise;

        req.data.has_exercise_data = block.exercise_data.has_value();
        if (block.exercise_data) {
            req.data.exercise_date = block.exercise_data->date;
            req.data.exercise_price = block.exercise_data->price;
        }

        req.data.has_payment_data = block.payment_data.has_value();
        if (block.payment_data && block.payment_data->rules) {
            const auto& rules = *block.payment_data->rules;
            req.data.payment_lag = static_cast<std::int64_t>(rules.lag);
            req.data.payment_calendar = rules.calendar;
            req.data.payment_convention = rules.convention;
            req.data.payment_relative_to = rules.relative_to;
        }

        req.data.has_settlement_data = block.settlement_data.has_value();
        if (block.settlement_data) {
            req.data.settlement_pay_currency = block.settlement_data->pay_currency;
            req.data.settlement_fx_index = block.settlement_data->fx_index;
            req.data.settlement_fixing_date = block.settlement_data->fixing_date;
        }

        auto resp = nats_call(nats, req, error);
        if (!resp || !resp->success)
            return error.empty() ? "save_instrument_option failed" : error;

        int sequence_number = 0;
        for (const auto& premium : block.premiums) {
            save_instrument_option_premium_request child;
            child.data.instrument_id = instrument_id;
            child.data.sequence_number = ++sequence_number;
            child.data.amount = premium.amount;
            child.data.currency = premium.currency;
            child.data.pay_date = premium.pay_date;
            child.data.has_settlement = premium.settlement.has_value();
            if (premium.settlement) {
                child.data.settlement_pay_currency = premium.settlement->pay_currency;
                child.data.settlement_fx_index = premium.settlement->fx_index;
                child.data.settlement_fixing_date = premium.settlement->fixing_date;
            }
            auto child_resp = nats_call(nats, child, error);
            if (!child_resp || !child_resp->success)
                return error.empty() ? "save_instrument_option_premium failed" : error;
        }

        sequence_number = 0;
        for (const auto& fee : block.exercise_fees) {
            save_instrument_option_exercise_fee_request child;
            child.data.instrument_id = instrument_id;
            child.data.sequence_number = ++sequence_number;
            child.data.amount = fee.amount;
            child.data.type = fee.type;
            child.data.start_date = fee.start_date;
            child.data.currency = fee.currency;
            auto child_resp = nats_call(nats, child, error);
            if (!child_resp || !child_resp->success)
                return error.empty() ? "save_instrument_option_exercise_fee failed" : error;
        }

        if (block.payment_data) {
            int payment_number = 0;
            for (const auto& date : block.payment_data->dates) {
                save_instrument_option_payment_date_request child;
                child.data.instrument_id = instrument_id;
                child.data.sequence_number = ++payment_number;
                child.data.payment_date = date;
                auto child_resp = nats_call(nats, child, error);
                if (!child_resp || !child_resp->success)
                    return error.empty() ? "save_instrument_option_payment_date failed" : error;
            }
        }
    }

    if (!data.option_exercise_dates.empty()) {
        ores::trading::domain::bond_schedule_data schedule;
        ores::trading::domain::bond_schedule_dates dates;
        dates.dates = data.option_exercise_dates;
        schedule.dates.push_back(std::move(dates));
        if (auto failure =
                save_schedule(nats, instrument_id, "option", 1, "exercise_dates", schedule);
            !failure.empty())
            return failure;
    }

    if (data.option_exercise_schedule) {
        if (auto failure = save_schedule(nats,
                                         instrument_id,
                                         "option",
                                         1,
                                         "exercise_schedule",
                                         *data.option_exercise_schedule);
            !failure.empty())
            return failure;
    }

    return {};
}

/**
 * @brief Saves the strike group, which the option row has no column for.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string save_strike(Nats& nats,
                        const boost::uuids::uuid& instrument_id,
                        const ores::trading::domain::bond_strike_data& strike) {
    using ores::trading::messaging::save_instrument_strike_request;

    save_instrument_strike_request req;
    req.data.instrument_id = instrument_id;
    req.data.price_value = strike.price_value;
    req.data.price_currency = strike.price_currency;
    req.data.yield_value = strike.yield_value;
    req.data.yield_compounding = strike.yield_compounding;
    req.data.bare_value = strike.bare_value;
    req.data.bare_currency = strike.bare_currency;

    std::string error;
    auto resp = nats_call(nats, req, error);
    if (!resp || !resp->success)
        return error.empty() ? "save_instrument_strike failed" : error;
    return {};
}

/**
 * @brief Saves the forward terms the fact row has no column for.
 *
 * A container that states no forward member writes no row, which is what
 * a container built from a row set holds.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string save_forward(Nats& nats,
                         const boost::uuids::uuid& instrument_id,
                         const ores::trading::domain::bond_instrument_data& data) {
    using ores::trading::messaging::save_bond_forward_request;

    if (!data.forward_long_in_forward && !data.forward_settlement && !data.forward_premium)
        return {};

    save_bond_forward_request req;
    req.data.instrument_id = instrument_id;
    req.data.long_in_forward = data.forward_long_in_forward;
    if (data.forward_settlement) {
        const auto& settlement = *data.forward_settlement;
        req.data.forward_maturity_date = settlement.forward_maturity_date;
        req.data.forward_settlement_date = settlement.forward_settlement_date;
        req.data.settlement = settlement.settlement;
        req.data.amount = settlement.amount;
        req.data.lock_rate = settlement.lock_rate;
        req.data.dv01 = settlement.dv01;
        req.data.lock_rate_day_counter = settlement.lock_rate_day_counter;
        req.data.settlement_dirty = settlement.settlement_dirty;
    }
    if (data.forward_premium) {
        req.data.premium_amount = data.forward_premium->amount;
        req.data.premium_date = data.forward_premium->date;
    }

    std::string error;
    auto resp = nats_call(nats, req, error);
    if (!resp || !resp->success)
        return error.empty() ? "save_bond_forward failed" : error;
    return {};
}

/**
 * @brief Saves the delivery basket of a future, one row per identifier.
 *
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string save_delivery_basket(Nats& nats,
                                 const boost::uuids::uuid& instrument_id,
                                 const std::vector<std::string>& basket) {
    using ores::trading::messaging::save_bond_future_delivery_basket_request;

    std::string error;
    int sequence_number = 0;
    for (const auto& delivery_basket_id : basket) {
        save_bond_future_delivery_basket_request req;
        req.data.instrument_id = instrument_id;
        req.data.sequence_number = ++sequence_number;
        req.data.delivery_basket_id = delivery_basket_id;
        auto resp = nats_call(nats, req, error);
        if (!resp || !resp->success)
            return error.empty() ? "save_bond_future_delivery_basket failed" : error;
    }
    return {};
}

/**
 * @brief Saves one bond instrument: its issue row, its header row, the
 * issue's child rows, its legs, its option block and the product's fact row.
 *
 * The issue row comes first because both the header and the child rows
 * reference it. A document that stated no call dates and no conversion
 * targets writes none of either, and a trade type with no product row
 * writes none.
 *
 * @param issue_ids_by_security The stored issue identifiers, keyed by
 * security id. A miss mints a row and records it here for the trades that
 * follow.
 * @return An empty string on success, or the first failure.
 */
template <typename Nats>
std::string
save_bond_instrument(Nats& nats,
                     const ores::trading::domain::bond_instrument_data& data,
                     std::unordered_map<std::string, std::string>& issue_ids_by_security) {
    using ores::trading::messaging::save_ascot_request;
    using ores::trading::messaging::save_bond_future_request;
    using ores::trading::messaging::save_bond_instrument_request;
    using ores::trading::messaging::save_bond_issue_call_date_request;
    using ores::trading::messaging::save_bond_issue_conversion_target_request;
    using ores::trading::messaging::save_bond_issue_request;
    using ores::trading::messaging::save_bond_option_request;
    using ores::trading::messaging::save_bond_repo_request;
    using ores::trading::messaging::save_bond_trs_request;

    std::string error;
    auto instrument = data.instrument;
    auto issue = data.issue;

    const auto found = issue_ids_by_security.find(issue.security_id);
    const bool issue_is_new = found == issue_ids_by_security.end();
    if (!issue_is_new) {
        issue.issue_id = boost::lexical_cast<boost::uuids::uuid>(found->second);
        instrument.issue_id = issue.issue_id;
    }

    // A failed save leaves the security id out of the map, so the next
    // trade of it tries the issue again rather than opening an instrument
    // against a row that is not there.
    if (issue_is_new) {
        save_bond_issue_request issue_req;
        issue_req.data = issue;
        auto resp = nats_call(nats, issue_req, error);
        if (!resp || !resp->success)
            return error.empty() ? "save_bond_issue failed" : error;
        issue_ids_by_security[issue.security_id] = boost::uuids::to_string(issue.issue_id);
    }

    save_bond_instrument_request instrument_req;
    instrument_req.data = instrument;
    auto resp = nats_call(nats, instrument_req, error);
    if (!resp || !resp->success)
        return error.empty() ? "save_bond_instrument failed" : error;

    int sequence_number = 0;
    for (const auto& call_date : data.call_dates) {
        save_bond_issue_call_date_request child_req;
        child_req.data = call_date;
        child_req.data.issue_id = issue.issue_id;
        child_req.data.sequence_number = ++sequence_number;
        auto child_resp = nats_call(nats, child_req, error);
        if (!child_resp || !child_resp->success)
            return error.empty() ? "save_bond_issue_call_date failed" : error;
    }

    sequence_number = 0;
    for (const auto& target : data.conversion_targets) {
        save_bond_issue_conversion_target_request child_req;
        child_req.data = target;
        child_req.data.issue_id = issue.issue_id;
        child_req.data.sequence_number = ++sequence_number;
        auto child_resp = nats_call(nats, child_req, error);
        if (!child_resp || !child_resp->success)
            return error.empty() ? "save_bond_issue_conversion_target failed" : error;
    }

    const auto instrument_id = instrument.identity.instrument_id;
    int leg_number = 0;
    for (const auto& leg : data.bond_legs) {
        if (auto failure = save_leg(nats, instrument_id, "bond", ++leg_number, leg);
            !failure.empty())
            return failure;
    }
    if (auto failure = save_leg(nats, instrument_id, "trs_funding", 1, data.trs_funding_leg);
        !failure.empty())
        return failure;
    if (auto failure = save_leg(nats, instrument_id, "repo", 1, data.repo_leg); !failure.empty())
        return failure;
    if (auto failure = save_leg(nats, instrument_id, "ascot_swap", 1, data.ascot_swap_leg);
        !failure.empty())
        return failure;

    if (auto failure = save_option_block(nats, instrument_id, data); !failure.empty())
        return failure;

    if (data.strike_data) {
        if (auto failure = save_strike(nats, instrument_id, *data.strike_data); !failure.empty())
            return failure;
    }

    if (auto failure = save_forward(nats, instrument_id, data); !failure.empty())
        return failure;

    if (auto failure = save_delivery_basket(nats, instrument_id, data.future_delivery_basket);
        !failure.empty())
        return failure;

    if (auto failure = save_schedule(nats, instrument_id, "trs", 1, "schedule", data.trs_schedule);
        !failure.empty())
        return failure;

    const auto& ttc = instrument.identity.trade_type_code;
    if (ttc == "BondOption" && data.option) {
        save_bond_option_request fact_req;
        fact_req.data = *data.option;
        fact_req.data.instrument_id = instrument.identity.instrument_id;
        fact_req.data.redemption = data.option_redemption;
        fact_req.data.price_type = data.option_price_type;
        fact_req.data.knocks_out = data.option_knocks_out;
        auto fact_resp = nats_call(nats, fact_req, error);
        if (!fact_resp || !fact_resp->success)
            return error.empty() ? "save_bond_option failed" : error;
    } else if (ttc == "BondTRS" && data.trs) {
        save_bond_trs_request fact_req;
        fact_req.data = *data.trs;
        fact_req.data.instrument_id = instrument.identity.instrument_id;
        fact_req.data.payer = data.trs_payer;
        fact_req.data.initial_price = data.trs_initial_price;
        if (!data.trs_price_type.empty())
            fact_req.data.price_type = data.trs_price_type;
        auto fact_resp = nats_call(nats, fact_req, error);
        if (!fact_resp || !fact_resp->success)
            return error.empty() ? "save_bond_trs failed" : error;
    } else if (ttc == "BondRepo" && data.repo) {
        save_bond_repo_request fact_req;
        fact_req.data = *data.repo;
        fact_req.data.instrument_id = instrument.identity.instrument_id;
        auto fact_resp = nats_call(nats, fact_req, error);
        if (!fact_resp || !fact_resp->success)
            return error.empty() ? "save_bond_repo failed" : error;
    } else if (ttc == "BondFuture" && data.future) {
        save_bond_future_request fact_req;
        fact_req.data = *data.future;
        fact_req.data.instrument_id = instrument.identity.instrument_id;
        auto fact_resp = nats_call(nats, fact_req, error);
        if (!fact_resp || !fact_resp->success)
            return error.empty() ? "save_bond_future failed" : error;
    } else if (ttc == "Ascot" && data.ascot) {
        save_ascot_request fact_req;
        fact_req.data = *data.ascot;
        fact_req.data.instrument_id = instrument.identity.instrument_id;
        auto fact_resp = nats_call(nats, fact_req, error);
        if (!fact_resp || !fact_resp->success)
            return error.empty() ? "save_ascot failed" : error;
    }

    return {};
}

} // namespace

ore_import_execute_handler::ore_import_execute_handler(
    ores::nats::service::client& nats,
    ores::nats::service::nats_client outbound_nats,
    std::string http_base_url,
    std::string work_dir)
    : nats_(nats)
    , outbound_nats_(std::move(outbound_nats))
    , http_base_url_(std::move(http_base_url))
    , work_dir_(std::move(work_dir)) {}

void ore_import_execute_handler::execute(ores::nats::message msg) {
    using ores::ore::messaging::ore_import_execute_request;
    using ores::ore::messaging::ore_import_execute_result;

    const auto step_id = extract_workflow_header(msg, workflow_step_id_header);
    const auto inst_id = extract_workflow_header(msg, workflow_instance_id_header);

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<ore_import_execute_request>(sv);
    if (!parsed) {
        BOOST_LOG_SEV(lg(), error)
            << "ore.import.execute: failed to decode request | step=" << step_id;
        publish_step_completion(nats_,
                                step_id,
                                inst_id,
                                ores::workflow::messaging::step_outcome::failed,
                                "",
                                "Failed to decode ore_import_execute_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute starting | corr=" << req.correlation_id
                              << " request_id=" << req.request_id << " step=" << step_id;

    auto delegated_nats =
        outbound_nats_.with_delegation(req.bearer_token).with_correlation_id(req.correlation_id);

    ore_import_execute_result result;
    result.correlation_id = req.correlation_id;

    // -------------------------------------------------------------------------
    // Step 0: fetch tarball from storage and extract to work directory
    // -------------------------------------------------------------------------
    const auto import_dir = work_dir_ / req.request_id;
    BOOST_LOG_SEV(lg(), debug) << "ore.import.execute step 0: fetch_and_unpack | corr="
                               << req.correlation_id
                               << " bucket=" << ores::ore::net::ore_storage::bucket
                               << " key=" << ores::ore::net::ore_storage::import_key(req.request_id)
                               << " dest=" << import_dir.string();

    try {
        std::filesystem::create_directories(import_dir);
        ores::storage::net::storage_transfer transfer(http_base_url_);
        transfer.fetch_and_unpack(std::string(ores::ore::net::ore_storage::bucket),
                                  ores::ore::net::ore_storage::import_key(req.request_id),
                                  import_dir);
    } catch (const std::exception& e) {
        const auto failure = std::format("fetch_and_unpack failed: {}", e.what());
        BOOST_LOG_SEV(lg(), error)
            << "ore.import.execute step 0 failed | corr=" << req.correlation_id
            << " error=" << failure;
        publish_step_completion(
            nats_, step_id, inst_id, ores::workflow::messaging::step_outcome::failed, "", failure);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 0 complete | corr=" << req.correlation_id
                              << " import_dir=" << import_dir.string();

    // -------------------------------------------------------------------------
    // Step 1: scan the extracted directory
    // -------------------------------------------------------------------------
    ores::ore::scanner::scan_result scan;
    try {
        ores::ore::scanner::ore_directory_scanner scanner(import_dir);
        scan = scanner.scan();
    } catch (const std::exception& e) {
        const auto failure = std::format("Directory scan failed: {}", e.what());
        BOOST_LOG_SEV(lg(), error)
            << "ore.import.execute step 1 failed | corr=" << req.correlation_id
            << " error=" << failure;
        publish_step_completion(
            nats_, step_id, inst_id, ores::workflow::messaging::step_outcome::failed, "", failure);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 1 complete | corr=" << req.correlation_id
                              << " currency_files=" << scan.currency_files.size()
                              << " portfolio_files=" << scan.portfolio_files.size()
                              << " ignored_files=" << scan.ignored_files.size();

    // -------------------------------------------------------------------------
    // Step 2: list existing currency ISO codes
    // -------------------------------------------------------------------------
    std::set<std::string> existing_iso_codes;
    {
        ores::refdata::messaging::get_currencies_request list_req;
        constexpr int max_currency_fetch = 10'000;
        list_req.offset = 0;
        list_req.limit = max_currency_fetch;
        std::string err;
        auto list_resp = nats_call(delegated_nats, list_req, err);
        if (!list_resp) {
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.execute step 2 failed | corr=" << req.correlation_id
                << " error=" << err;
            publish_step_completion(
                nats_, step_id, inst_id, ores::workflow::messaging::step_outcome::failed, "", err);
            return;
        }
        for (const auto& c : list_resp->currencies)
            existing_iso_codes.insert(c.iso_code);
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 2 complete | corr=" << req.correlation_id
                              << " existing_iso_codes=" << existing_iso_codes.size();

    // -------------------------------------------------------------------------
    // Step 3: build the import plan
    // -------------------------------------------------------------------------
    ores::ore::planner::import_choices choices;
    if (!req.import_choices_json.empty()) {
        auto parsed_choices =
            rfl::json::read<ores::ore::planner::import_choices>(req.import_choices_json);
        if (!parsed_choices) {
            const auto failure = std::format("Failed to parse import_choices_json: {}",
                                             parsed_choices.error().what());
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.execute step 3 failed | corr=" << req.correlation_id
                << " error=" << failure;
            publish_step_completion(nats_,
                                    step_id,
                                    inst_id,
                                    ores::workflow::messaging::step_outcome::failed,
                                    "",
                                    failure);
            return;
        }
        choices = std::move(*parsed_choices);
    }

    ores::ore::planner::ore_import_plan plan;
    try {
        ores::ore::planner::ore_import_planner planner(
            std::move(scan), std::move(existing_iso_codes), std::move(choices));
        plan = planner.plan();
    } catch (const std::exception& e) {
        const auto failure = std::format("Import plan failed: {}", e.what());
        BOOST_LOG_SEV(lg(), error)
            << "ore.import.execute step 3 failed | corr=" << req.correlation_id
            << " error=" << failure;
        publish_step_completion(
            nats_, step_id, inst_id, ores::workflow::messaging::step_outcome::failed, "", failure);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 3 complete | corr=" << req.correlation_id
                              << " currencies=" << plan.currencies.size()
                              << " portfolios=" << plan.portfolios.size()
                              << " books=" << plan.books.size() << " trades=" << plan.trades.size();

    // -------------------------------------------------------------------------
    // Step 4: save currencies
    // -------------------------------------------------------------------------
    for (auto& currency : plan.currencies) {
        const auto iso = currency.iso_code;
        ores::refdata::messaging::save_currency_request save_req;
        save_req.data = std::move(currency);
        std::string err;
        auto resp = nats_call(delegated_nats, save_req, err);
        if (!resp || !resp->success) {
            const auto failure = err.empty() ? std::format("save_currency failed for {}: {}",
                                                           iso,
                                                           resp ? resp->message : "(no response)") :
                                               err;
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.execute step 4 failed | corr=" << req.correlation_id
                << " iso_code=" << iso << " error=" << failure;
            publish_step_completion(nats_,
                                    step_id,
                                    inst_id,
                                    ores::workflow::messaging::step_outcome::failed,
                                    "",
                                    failure);
            return;
        }
        result.saved_currency_iso_codes.push_back(iso);
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 4 complete | corr=" << req.correlation_id
                              << " saved=" << result.saved_currency_iso_codes.size();

    // -------------------------------------------------------------------------
    // Step 5: save portfolios (parents-first from planner)
    // -------------------------------------------------------------------------
    for (auto& portfolio : plan.portfolios) {
        const auto pid = boost::uuids::to_string(portfolio.id);
        const auto name = portfolio.name;
        ores::refdata::messaging::save_portfolio_request save_req;
        save_req.data = std::move(portfolio);
        std::string err;
        auto resp = nats_call(delegated_nats, save_req, err);
        if (!resp || !resp->success) {
            const auto failure = err.empty() ?
                                     std::format("save_portfolio failed for '{}' ({}): {}",
                                                 name,
                                                 pid,
                                                 resp ? resp->message : "(no response)") :
                                     err;
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.execute step 5 failed | corr=" << req.correlation_id
                << " portfolio=" << pid << " error=" << failure;
            publish_step_completion(nats_,
                                    step_id,
                                    inst_id,
                                    ores::workflow::messaging::step_outcome::failed,
                                    "",
                                    failure);
            return;
        }
        result.saved_portfolio_ids.push_back(pid);
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 5 complete | corr=" << req.correlation_id
                              << " saved=" << result.saved_portfolio_ids.size();

    // -------------------------------------------------------------------------
    // Step 6: save books
    // -------------------------------------------------------------------------
    for (auto& book : plan.books) {
        const auto bid = boost::uuids::to_string(book.id);
        const auto name = book.name;
        ores::refdata::messaging::save_book_request save_req;
        save_req.data = std::move(book);
        std::string err;
        auto resp = nats_call(delegated_nats, save_req, err);
        if (!resp || !resp->success) {
            const auto failure = err.empty() ? std::format("save_book failed for '{}' ({}): {}",
                                                           name,
                                                           bid,
                                                           resp ? resp->message : "(no response)") :
                                               err;
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.execute step 6 failed | corr=" << req.correlation_id
                << " book=" << bid << " error=" << failure;
            publish_step_completion(nats_,
                                    step_id,
                                    inst_id,
                                    ores::workflow::messaging::step_outcome::failed,
                                    "",
                                    failure);
            return;
        }
        result.saved_book_ids.push_back(bid);
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 6 complete | corr=" << req.correlation_id
                              << " saved=" << result.saved_book_ids.size();

    // -------------------------------------------------------------------------
    // Step 7: save trades (failures collected; saga continues)
    // -------------------------------------------------------------------------
    for (auto& item : plan.trades) {
        const auto trade_id = item.trade.identity.id;
        const auto tid = boost::uuids::to_string(trade_id);
        const auto src = item.source_file.string();
        const auto ext_id = item.trade.identity.external_id;

        ores::trading::messaging::save_trade_request save_req;
        save_req.trades = {std::move(item.trade)};

        std::string trade_error;
        auto resp = nats_call(delegated_nats, save_req, trade_error);
        if (!resp || !resp->success) {
            const auto trade_msg = resp ? resp->message : trade_error;
            BOOST_LOG_SEV(lg(), warn)
                << "ore.import.execute trade save failed | corr=" << req.correlation_id
                << " trade_id=" << tid << " source=" << src << " error=" << trade_msg;
            result.item_errors.push_back(
                {.source_file = src, .item_id = ext_id, .message = trade_msg});
        } else {
            result.saved_trade_ids.push_back(tid);
            const auto envelope_error = save_envelope(delegated_nats, trade_id, item.envelope);
            if (!envelope_error.empty()) {
                BOOST_LOG_SEV(lg(), warn)
                    << "ore.import.execute envelope save failed | corr=" << req.correlation_id
                    << " trade_id=" << tid << " source=" << src << " error=" << envelope_error;
                result.item_errors.push_back(
                    {.source_file = src, .item_id = ext_id, .message = envelope_error});
            }
        }
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 7 complete | corr=" << req.correlation_id
                              << " saved=" << result.saved_trade_ids.size()
                              << " failed=" << result.item_errors.size();

    // -------------------------------------------------------------------------
    // Step 8: save instruments (non-fatal — collect errors, continue)
    // -------------------------------------------------------------------------
    int instruments_saved = 0;
    std::unordered_map<std::string, std::string> issue_ids_by_security;
    bool bond_issues_loaded = false;
    for (const auto& item : plan.trades) {
        using namespace ores::trading::messaging;
        using ores::trading::domain::swap_instrument_data;
        using ores::trading::domain::fx_instrument_variant;
        using ores::trading::domain::bond_instrument_data;
        using ores::trading::domain::credit_instrument;
        using ores::trading::domain::equity_instrument_variant;
        using ores::trading::domain::commodity_instrument;
        using ores::trading::domain::composite_instrument_data;
        using ores::trading::domain::scripted_instrument;
        std::string instr_error;

        const auto save_ok = std::visit(
            [&](const auto& r) -> bool {
                using T = std::decay_t<decltype(r)>;
                if constexpr (std::is_same_v<T, std::monostate>) {
                    return true; // no instrument for this trade type
                } else if constexpr (std::is_same_v<T, swap_instrument_data>) {
                    return std::visit(
                        [&](const auto& instr) -> bool {
                            using InstrT = std::decay_t<decltype(instr)>;
                            using namespace ores::trading::domain;
                            if constexpr (std::is_same_v<InstrT, fra_instrument>) {
                                save_fra_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT, vanilla_swap_instrument>) {
                                save_vanilla_swap_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT, cap_floor_instrument>) {
                                save_cap_floor_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT, swaption_instrument>) {
                                save_swaption_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<
                                                     InstrT,
                                                     balance_guaranteed_swap_instrument>) {
                                save_balance_guaranteed_swap_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT, callable_swap_instrument>) {
                                save_callable_swap_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                knock_out_swap_instrument>) {
                                save_knock_out_swap_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                inflation_swap_instrument>) {
                                save_inflation_swap_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT, rpa_instrument>) {
                                save_rpa_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else {
                                return true;
                            }
                        },
                        r.instrument);
                } else if constexpr (std::is_same_v<T, fx_instrument_variant>) {
                    return std::visit(
                        [&](const auto& instr) -> bool {
                            using InstrT = std::decay_t<decltype(instr)>;
                            using namespace ores::trading::domain;
                            if constexpr (std::is_same_v<InstrT, fx_forward_instrument>) {
                                save_fx_forward_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                fx_vanilla_option_instrument>) {
                                save_fx_vanilla_option_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                fx_barrier_option_instrument>) {
                                save_fx_barrier_option_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                fx_digital_option_instrument>) {
                                save_fx_digital_option_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                fx_asian_forward_instrument>) {
                                save_fx_asian_forward_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                fx_accumulator_instrument>) {
                                save_fx_accumulator_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                fx_variance_swap_instrument>) {
                                save_fx_variance_swap_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else {
                                return true;
                            }
                        },
                        r);
                } else if constexpr (std::is_same_v<T, bond_instrument_data>) {
                    if (!bond_issues_loaded) {
                        bond_issues_loaded = true;
                        issue_ids_by_security =
                            read_bond_issue_ids_by_security(delegated_nats, instr_error);
                        if (!instr_error.empty())
                            return false;
                    }
                    instr_error = save_bond_instrument(delegated_nats, r, issue_ids_by_security);
                    return instr_error.empty();
                } else if constexpr (std::is_same_v<T, credit_instrument>) {
                    save_credit_instrument_request req;
                    req.data = r;
                    auto resp = nats_call(delegated_nats, req, instr_error);
                    return resp && resp->success;
                } else if constexpr (std::is_same_v<T, equity_instrument_variant>) {
                    return std::visit(
                        [&](const auto& instr) -> bool {
                            using InstrT = std::decay_t<decltype(instr)>;
                            using namespace ores::trading::domain;
                            if constexpr (std::is_same_v<InstrT, equity_option_instrument>) {
                                save_equity_option_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                equity_digital_option_instrument>) {
                                save_equity_digital_option_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                equity_barrier_option_instrument>) {
                                save_equity_barrier_option_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                equity_asian_option_instrument>) {
                                save_equity_asian_option_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                equity_forward_instrument>) {
                                save_equity_forward_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                equity_variance_swap_instrument>) {
                                save_equity_variance_swap_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT, equity_swap_instrument>) {
                                save_equity_swap_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                equity_accumulator_instrument>) {
                                save_equity_accumulator_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else if constexpr (std::is_same_v<InstrT,
                                                                equity_position_instrument>) {
                                save_equity_position_instrument_request req;
                                req.data = instr;
                                auto resp = nats_call(delegated_nats, req, instr_error);
                                return resp && resp->success;
                            } else {
                                // Unknown per-type alternative — fail loudly so a new
                                // variant added without updating this dispatch surfaces
                                // at import time instead of silently skipping saves.
                                instr_error = "equity variant alternative not handled "
                                              "by import dispatch";
                                return false;
                            }
                        },
                        r);
                } else if constexpr (std::is_same_v<T, commodity_instrument>) {
                    save_commodity_instrument_request req;
                    req.data = r;
                    auto resp = nats_call(delegated_nats, req, instr_error);
                    return resp && resp->success;
                } else if constexpr (std::is_same_v<T, composite_instrument_data>) {
                    save_composite_instrument_request req;
                    req.data = r.instrument;
                    auto resp = nats_call(delegated_nats, req, instr_error);
                    return resp && resp->success;
                } else if constexpr (std::is_same_v<T, scripted_instrument>) {
                    save_scripted_instrument_request req;
                    req.data = r;
                    auto resp = nats_call(delegated_nats, req, instr_error);
                    return resp && resp->success;
                } else {
                    return true;
                }
            },
            item.instrument);

        if (save_ok) {
            ++instruments_saved;
        } else {
            BOOST_LOG_SEV(lg(), warn)
                << "ore.import.execute instrument save failed | corr=" << req.correlation_id
                << " trade=" << item.trade.identity.external_id << " error=" << instr_error;
            result.item_errors.push_back({.source_file = item.source_file.string(),
                                          .item_id = item.trade.identity.external_id,
                                          .message = "Instrument save failed: " + instr_error});
        }
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 8 complete | corr=" << req.correlation_id
                              << " instruments_saved=" << instruments_saved;

    // -------------------------------------------------------------------------
    // Done — determine step outcome and publish
    // -------------------------------------------------------------------------
    // Total failure: all planned trades failed to save (and at least one was attempted).
    // Adjust this constant to change the threshold for saga compensation.
    const bool all_trades_failed =
        !plan.trades.empty() && result.saved_trade_ids.empty() && !result.item_errors.empty();

    using wf_outcome = ores::workflow::messaging::step_outcome;
    using wf_log_level = ores::workflow::messaging::step_log_level;
    using wf_log_entry = ores::workflow::messaging::step_log_entry;

    wf_outcome outcome;
    if (result.item_errors.empty()) {
        outcome = wf_outcome::completed;
    } else if (all_trades_failed) {
        outcome = wf_outcome::failed;
    } else {
        outcome = wf_outcome::completed_with_warnings;
    }

    std::vector<wf_log_entry> step_log;
    if (!result.saved_trade_ids.empty()) {
        step_log.push_back(
            {.level = wf_log_level::info,
             .message = std::format("Saved {} trade(s).", result.saved_trade_ids.size()),
             .context = {}});
    }
    for (const auto& ie : result.item_errors) {
        step_log.push_back(
            {.level = (outcome == wf_outcome::failed) ? wf_log_level::error : wf_log_level::warn,
             .message = ie.message,
             .context = ie.item_id.empty() ? ie.source_file : ie.item_id});
    }

    result.success = outcome != wf_outcome::failed;
    result.message =
        result.item_errors.empty() ?
            "ORE import completed." :
            std::format("ORE import completed with {} error(s).", result.item_errors.size());

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute complete | corr=" << req.correlation_id
                              << " currencies=" << result.saved_currency_iso_codes.size()
                              << " portfolios=" << result.saved_portfolio_ids.size()
                              << " books=" << result.saved_book_ids.size()
                              << " trades=" << result.saved_trade_ids.size()
                              << " item_errors=" << result.item_errors.size()
                              << " outcome=" << ores::workflow::messaging::to_string(outcome);

    if (outcome == wf_outcome::failed) {
        publish_step_completion(
            nats_,
            step_id,
            inst_id,
            wf_outcome::failed,
            rfl::json::write(result),
            std::format("All {} trade save(s) failed.", result.item_errors.size()),
            step_log);
    } else {
        publish_step_completion(
            nats_, step_id, inst_id, outcome, rfl::json::write(result), "", step_log);
    }
}

void ore_import_execute_handler::rollback(ores::nats::message msg) {
    using ores::ore::messaging::ore_import_rollback_request;

    const auto step_id = extract_workflow_header(msg, workflow_step_id_header);
    const auto inst_id = extract_workflow_header(msg, workflow_instance_id_header);

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<ore_import_rollback_request>(sv);
    if (!parsed) {
        BOOST_LOG_SEV(lg(), error)
            << "ore.import.rollback: failed to decode request | step=" << step_id;
        publish_step_completion(nats_,
                                step_id,
                                inst_id,
                                ores::workflow::messaging::step_outcome::failed,
                                "",
                                "Failed to decode ore_import_rollback_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "ore.import.rollback starting | corr=" << req.correlation_id
                              << " step=" << step_id;

    auto delegated_nats =
        outbound_nats_.with_delegation(req.bearer_token).with_correlation_id(req.correlation_id);

    // ── Delete trades ────────────────────────────────────────────────────────
    if (!req.saved_trade_ids.empty()) {
        BOOST_LOG_SEV(lg(), info) << "ore.import.rollback: delete trades | corr="
                                  << req.correlation_id << " count=" << req.saved_trade_ids.size();
        ores::trading::messaging::delete_trade_request del_req;
        del_req.ids = req.saved_trade_ids;
        std::string err;
        auto r = nats_call(delegated_nats, del_req, err);
        if (!r || !r->success) {
            const auto reason = (r && !r->message.empty()) ? r->message : err;
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.rollback delete_trades failed | corr=" << req.correlation_id
                << " error=" << reason;
        }
    }

    // ── Delete books ─────────────────────────────────────────────────────────
    if (!req.saved_book_ids.empty()) {
        BOOST_LOG_SEV(lg(), info) << "ore.import.rollback: delete books | corr="
                                  << req.correlation_id << " count=" << req.saved_book_ids.size();
        ores::refdata::messaging::delete_book_request del_req;
        del_req.ids = req.saved_book_ids;
        std::string err;
        auto r = nats_call(delegated_nats, del_req, err);
        if (!r || !r->success) {
            const auto reason = (r && !r->message.empty()) ? r->message : err;
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.rollback delete_books failed | corr=" << req.correlation_id
                << " error=" << reason;
        }
    }

    // ── Delete portfolios (reverse order — children before parents) ──────────
    if (!req.saved_portfolio_ids.empty()) {
        BOOST_LOG_SEV(lg(), info) << "ore.import.rollback: delete portfolios | corr="
                                  << req.correlation_id
                                  << " count=" << req.saved_portfolio_ids.size();
        ores::refdata::messaging::delete_portfolio_request del_req;
        del_req.ids = std::vector<std::string>(req.saved_portfolio_ids.rbegin(),
                                               req.saved_portfolio_ids.rend());
        std::string err;
        auto r = nats_call(delegated_nats, del_req, err);
        if (!r || !r->success) {
            const auto reason = (r && !r->message.empty()) ? r->message : err;
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.rollback delete_portfolios failed | corr=" << req.correlation_id
                << " error=" << reason;
        }
    }

    // ── Delete currencies ────────────────────────────────────────────────────
    if (!req.saved_currency_iso_codes.empty()) {
        BOOST_LOG_SEV(lg(), info) << "ore.import.rollback: delete currencies | corr="
                                  << req.correlation_id
                                  << " count=" << req.saved_currency_iso_codes.size();
        ores::refdata::messaging::delete_currency_request del_req;
        del_req.iso_codes = req.saved_currency_iso_codes;
        std::string err;
        auto r = nats_call(delegated_nats, del_req, err);
        if (!r || !r->success) {
            const auto reason = (r && !r->message.empty()) ? r->message : err;
            BOOST_LOG_SEV(lg(), error)
                << "ore.import.rollback delete_currencies failed | corr=" << req.correlation_id
                << " error=" << reason;
        }
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.rollback complete | corr=" << req.correlation_id;
    publish_step_completion(nats_,
                            step_id,
                            inst_id,
                            ores::workflow::messaging::step_outcome::completed,
                            "{\"success\":true}",
                            "");
}

}
