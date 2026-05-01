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
#ifndef ORES_TRADING_MESSAGING_INSTRUMENT_PROTOCOL_HPP
#define ORES_TRADING_MESSAGING_INSTRUMENT_PROTOCOL_HPP

#include <string>
#include <variant>
#include <vector>
#include "ores.trading.api/domain/product_type.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.api/domain/fx_forward_instrument.hpp"
#include "ores.trading.api/domain/fx_vanilla_option_instrument.hpp"
#include "ores.trading.api/domain/fx_barrier_option_instrument.hpp"
#include "ores.trading.api/domain/fx_digital_option_instrument.hpp"
#include "ores.trading.api/domain/fx_asian_forward_instrument.hpp"
#include "ores.trading.api/domain/fx_accumulator_instrument.hpp"
#include "ores.trading.api/domain/fx_variance_swap_instrument.hpp"
#include "ores.trading.api/domain/bond_instrument.hpp"
#include "ores.trading.api/domain/credit_instrument.hpp"
#include "ores.trading.api/domain/equity_option_instrument.hpp"
#include "ores.trading.api/domain/equity_digital_option_instrument.hpp"
#include "ores.trading.api/domain/equity_barrier_option_instrument.hpp"
#include "ores.trading.api/domain/equity_asian_option_instrument.hpp"
#include "ores.trading.api/domain/equity_forward_instrument.hpp"
#include "ores.trading.api/domain/equity_variance_swap_instrument.hpp"
#include "ores.trading.api/domain/equity_swap_instrument.hpp"
#include "ores.trading.api/domain/equity_accumulator_instrument.hpp"
#include "ores.trading.api/domain/equity_position_instrument.hpp"
#include "ores.trading.api/domain/commodity_instrument.hpp"
#include "ores.trading.api/domain/composite_instrument.hpp"
#include "ores.trading.api/domain/composite_leg.hpp"
#include "ores.trading.api/domain/scripted_instrument.hpp"
#include "ores.trading.api/domain/fra_instrument.hpp"
#include "ores.trading.api/domain/vanilla_swap_instrument.hpp"
#include "ores.trading.api/domain/cap_floor_instrument.hpp"
#include "ores.trading.api/domain/swaption_instrument.hpp"
#include "ores.trading.api/domain/balance_guaranteed_swap_instrument.hpp"
#include "ores.trading.api/domain/callable_swap_instrument.hpp"
#include "ores.trading.api/domain/knock_out_swap_instrument.hpp"
#include "ores.trading.api/domain/inflation_swap_instrument.hpp"
#include "ores.trading.api/domain/rpa_instrument.hpp"

namespace ores::trading::messaging {

// ---- Typed FX instrument protocol ----

struct save_fx_forward_instrument_request {
    using response_type = struct save_fx_forward_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_forward_instruments.save";
    ores::trading::domain::fx_forward_instrument data;
};

struct save_fx_forward_instrument_response {
    bool success = false;
    std::string message;
};

struct save_fx_vanilla_option_instrument_request {
    using response_type = struct save_fx_vanilla_option_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_vanilla_option_instruments.save";
    ores::trading::domain::fx_vanilla_option_instrument data;
};

struct save_fx_vanilla_option_instrument_response {
    bool success = false;
    std::string message;
};

struct save_fx_barrier_option_instrument_request {
    using response_type = struct save_fx_barrier_option_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_barrier_option_instruments.save";
    ores::trading::domain::fx_barrier_option_instrument data;
};

struct save_fx_barrier_option_instrument_response {
    bool success = false;
    std::string message;
};

struct save_fx_digital_option_instrument_request {
    using response_type = struct save_fx_digital_option_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_digital_option_instruments.save";
    ores::trading::domain::fx_digital_option_instrument data;
};

struct save_fx_digital_option_instrument_response {
    bool success = false;
    std::string message;
};

struct save_fx_asian_forward_instrument_request {
    using response_type = struct save_fx_asian_forward_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_asian_forward_instruments.save";
    ores::trading::domain::fx_asian_forward_instrument data;
};

struct save_fx_asian_forward_instrument_response {
    bool success = false;
    std::string message;
};

struct save_fx_accumulator_instrument_request {
    using response_type = struct save_fx_accumulator_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_accumulator_instruments.save";
    ores::trading::domain::fx_accumulator_instrument data;
};

struct save_fx_accumulator_instrument_response {
    bool success = false;
    std::string message;
};

struct save_fx_variance_swap_instrument_request {
    using response_type = struct save_fx_variance_swap_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_variance_swap_instruments.save";
    ores::trading::domain::fx_variance_swap_instrument data;
};

struct save_fx_variance_swap_instrument_response {
    bool success = false;
    std::string message;
};

// ---- Bond instrument protocol ----

struct get_bond_instruments_request {
    using response_type = struct get_bond_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.bond_instruments.list";
    int offset = 0;
    int limit = 100;
};

struct get_bond_instruments_response {
    std::vector<ores::trading::domain::bond_instrument> instruments;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_bond_instrument_request {
    using response_type = struct save_bond_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.bond_instruments.save";
    ores::trading::domain::bond_instrument data;
};

struct save_bond_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_bond_instrument_request {
    using response_type = struct delete_bond_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.bond_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_bond_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_bond_instrument_history_request {
    using response_type = struct get_bond_instrument_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.bond_instruments.history";
    std::string id;
};

struct get_bond_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::bond_instrument> history;
};

// ---- Credit instrument protocol ----

struct get_credit_instruments_request {
    using response_type = struct get_credit_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.credit_instruments.list";
    int offset = 0;
    int limit = 100;
};

struct get_credit_instruments_response {
    std::vector<ores::trading::domain::credit_instrument> instruments;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_credit_instrument_request {
    using response_type = struct save_credit_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.credit_instruments.save";
    ores::trading::domain::credit_instrument data;
};

struct save_credit_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_credit_instrument_request {
    using response_type = struct delete_credit_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.credit_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_credit_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_credit_instrument_history_request {
    using response_type = struct get_credit_instrument_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.credit_instruments.history";
    std::string id;
};

struct get_credit_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::credit_instrument> history;
};

// ---- Typed equity instrument protocol ----

struct save_equity_option_instrument_request {
    using response_type = struct save_equity_option_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_option_instruments.save";
    ores::trading::domain::equity_option_instrument data;
};

struct save_equity_option_instrument_response {
    bool success = false;
    std::string message;
};

struct save_equity_digital_option_instrument_request {
    using response_type = struct save_equity_digital_option_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_digital_option_instruments.save";
    ores::trading::domain::equity_digital_option_instrument data;
};

struct save_equity_digital_option_instrument_response {
    bool success = false;
    std::string message;
};

struct save_equity_barrier_option_instrument_request {
    using response_type = struct save_equity_barrier_option_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_barrier_option_instruments.save";
    ores::trading::domain::equity_barrier_option_instrument data;
};

struct save_equity_barrier_option_instrument_response {
    bool success = false;
    std::string message;
};

struct save_equity_asian_option_instrument_request {
    using response_type = struct save_equity_asian_option_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_asian_option_instruments.save";
    ores::trading::domain::equity_asian_option_instrument data;
};

struct save_equity_asian_option_instrument_response {
    bool success = false;
    std::string message;
};

struct save_equity_forward_instrument_request {
    using response_type = struct save_equity_forward_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_forward_instruments.save";
    ores::trading::domain::equity_forward_instrument data;
};

struct save_equity_forward_instrument_response {
    bool success = false;
    std::string message;
};

struct save_equity_variance_swap_instrument_request {
    using response_type = struct save_equity_variance_swap_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_variance_swap_instruments.save";
    ores::trading::domain::equity_variance_swap_instrument data;
};

struct save_equity_variance_swap_instrument_response {
    bool success = false;
    std::string message;
};

struct save_equity_swap_instrument_request {
    using response_type = struct save_equity_swap_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_swap_instruments.save";
    ores::trading::domain::equity_swap_instrument data;
};

struct save_equity_swap_instrument_response {
    bool success = false;
    std::string message;
};

struct save_equity_accumulator_instrument_request {
    using response_type = struct save_equity_accumulator_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_accumulator_instruments.save";
    ores::trading::domain::equity_accumulator_instrument data;
};

struct save_equity_accumulator_instrument_response {
    bool success = false;
    std::string message;
};

struct save_equity_position_instrument_request {
    using response_type = struct save_equity_position_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_position_instruments.save";
    ores::trading::domain::equity_position_instrument data;
};

struct save_equity_position_instrument_response {
    bool success = false;
    std::string message;
};

// ---- Commodity instrument protocol ----

struct get_commodity_instruments_request {
    using response_type = struct get_commodity_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.commodity_instruments.list";
    int offset = 0;
    int limit = 100;
};

struct get_commodity_instruments_response {
    std::vector<ores::trading::domain::commodity_instrument> instruments;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_commodity_instrument_request {
    using response_type = struct save_commodity_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.commodity_instruments.save";
    ores::trading::domain::commodity_instrument data;
};

struct save_commodity_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_commodity_instrument_request {
    using response_type = struct delete_commodity_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.commodity_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_commodity_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_commodity_instrument_history_request {
    using response_type = struct get_commodity_instrument_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.commodity_instruments.history";
    std::string id;
};

struct get_commodity_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::commodity_instrument> history;
};

// ---- Composite instrument protocol ----

struct get_composite_instrument_legs_request {
    using response_type = struct get_composite_instrument_legs_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.composite_instruments.legs.list";
    std::string instrument_id;
};

struct get_composite_instrument_legs_response {
    std::vector<ores::trading::domain::composite_leg> legs;
    bool success = true;
    std::string message;
};

struct get_composite_instruments_request {
    using response_type = struct get_composite_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.composite_instruments.list";
    int offset = 0;
    int limit = 100;
};

struct get_composite_instruments_response {
    std::vector<ores::trading::domain::composite_instrument> instruments;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_composite_instrument_request {
    using response_type = struct save_composite_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.composite_instruments.save";
    ores::trading::domain::composite_instrument data;
    std::vector<ores::trading::domain::composite_leg> legs;
};

struct save_composite_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_composite_instrument_request {
    using response_type = struct delete_composite_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.composite_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_composite_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_composite_instrument_history_request {
    using response_type = struct get_composite_instrument_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.composite_instruments.history";
    std::string id;
};

struct get_composite_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::composite_instrument> history;
};

// ---- Scripted instrument protocol ----

struct get_scripted_instruments_request {
    using response_type = struct get_scripted_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.scripted_instruments.list";
    int offset = 0;
    int limit = 100;
};

struct get_scripted_instruments_response {
    std::vector<ores::trading::domain::scripted_instrument> instruments;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_scripted_instrument_request {
    using response_type = struct save_scripted_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.scripted_instruments.save";
    ores::trading::domain::scripted_instrument data;
};

struct save_scripted_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_scripted_instrument_request {
    using response_type = struct delete_scripted_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.scripted_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_scripted_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_scripted_instrument_history_request {
    using response_type = struct get_scripted_instrument_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.scripted_instruments.history";
    std::string id;
};

struct get_scripted_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::scripted_instrument> history;
};

// ---- FRA instrument protocol ----

struct get_fra_instruments_request {
    using response_type = struct get_fra_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fra_instruments.list";
    int offset = 0;
    int limit = 100;
};

struct get_fra_instruments_response {
    std::vector<ores::trading::domain::fra_instrument> instruments;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_fra_instrument_request {
    using response_type = struct save_fra_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fra_instruments.save";
    ores::trading::domain::fra_instrument data;
};

struct save_fra_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_fra_instrument_request {
    using response_type = struct delete_fra_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fra_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_fra_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_fra_instrument_history_request {
    using response_type = struct get_fra_instrument_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fra_instruments.history";
    std::string id;
};

struct get_fra_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::fra_instrument> history;
};

// ---- Vanilla swap instrument protocol ----

struct get_vanilla_swap_instruments_request {
    using response_type = struct get_vanilla_swap_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.vanilla_swap_instruments.list";
    int offset = 0;
    int limit = 100;
};

struct get_vanilla_swap_instruments_response {
    std::vector<ores::trading::domain::vanilla_swap_instrument> instruments;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_vanilla_swap_instrument_request {
    using response_type = struct save_vanilla_swap_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.vanilla_swap_instruments.save";
    ores::trading::domain::vanilla_swap_instrument data;
    std::vector<ores::trading::domain::swap_leg> legs;
};

struct save_vanilla_swap_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_vanilla_swap_instrument_request {
    using response_type = struct delete_vanilla_swap_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.vanilla_swap_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_vanilla_swap_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_vanilla_swap_instrument_history_request {
    using response_type = struct get_vanilla_swap_instrument_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.vanilla_swap_instruments.history";
    std::string id;
};

struct get_vanilla_swap_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::vanilla_swap_instrument> history;
};

// ---- Cap/floor instrument protocol ----

struct get_cap_floor_instruments_request {
    using response_type = struct get_cap_floor_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.cap_floor_instruments.list";
    int offset = 0;
    int limit = 100;
};

struct get_cap_floor_instruments_response {
    std::vector<ores::trading::domain::cap_floor_instrument> instruments;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_cap_floor_instrument_request {
    using response_type = struct save_cap_floor_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.cap_floor_instruments.save";
    ores::trading::domain::cap_floor_instrument data;
    std::vector<ores::trading::domain::swap_leg> legs;
};

struct save_cap_floor_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_cap_floor_instrument_request {
    using response_type = struct delete_cap_floor_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.cap_floor_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_cap_floor_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_cap_floor_instrument_history_request {
    using response_type = struct get_cap_floor_instrument_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.cap_floor_instruments.history";
    std::string id;
};

struct get_cap_floor_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::cap_floor_instrument> history;
};

// ---- Swaption instrument protocol ----

struct get_swaption_instruments_request {
    using response_type = struct get_swaption_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.swaption_instruments.list";
    int offset = 0;
    int limit = 100;
};

struct get_swaption_instruments_response {
    std::vector<ores::trading::domain::swaption_instrument> instruments;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_swaption_instrument_request {
    using response_type = struct save_swaption_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.swaption_instruments.save";
    ores::trading::domain::swaption_instrument data;
    std::vector<ores::trading::domain::swap_leg> legs;
};

struct save_swaption_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_swaption_instrument_request {
    using response_type = struct delete_swaption_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.swaption_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_swaption_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_swaption_instrument_history_request {
    using response_type = struct get_swaption_instrument_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.swaption_instruments.history";
    std::string id;
};

struct get_swaption_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::swaption_instrument> history;
};

// ---- Balance guaranteed swap instrument protocol ----

struct get_balance_guaranteed_swap_instruments_request {
    using response_type = struct get_balance_guaranteed_swap_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.balance_guaranteed_swap_instruments.list";
    int offset = 0;
    int limit = 100;
};

struct get_balance_guaranteed_swap_instruments_response {
    std::vector<ores::trading::domain::balance_guaranteed_swap_instrument> instruments;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_balance_guaranteed_swap_instrument_request {
    using response_type = struct save_balance_guaranteed_swap_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.balance_guaranteed_swap_instruments.save";
    ores::trading::domain::balance_guaranteed_swap_instrument data;
    std::vector<ores::trading::domain::swap_leg> legs;
};

struct save_balance_guaranteed_swap_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_balance_guaranteed_swap_instrument_request {
    using response_type = struct delete_balance_guaranteed_swap_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.balance_guaranteed_swap_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_balance_guaranteed_swap_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_balance_guaranteed_swap_instrument_history_request {
    using response_type = struct get_balance_guaranteed_swap_instrument_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.balance_guaranteed_swap_instruments.history";
    std::string id;
};

struct get_balance_guaranteed_swap_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::balance_guaranteed_swap_instrument> history;
};

// ---- Callable swap instrument protocol ----

struct get_callable_swap_instruments_request {
    using response_type = struct get_callable_swap_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.callable_swap_instruments.list";
    int offset = 0;
    int limit = 100;
};

struct get_callable_swap_instruments_response {
    std::vector<ores::trading::domain::callable_swap_instrument> instruments;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_callable_swap_instrument_request {
    using response_type = struct save_callable_swap_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.callable_swap_instruments.save";
    ores::trading::domain::callable_swap_instrument data;
    std::vector<ores::trading::domain::swap_leg> legs;
};

struct save_callable_swap_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_callable_swap_instrument_request {
    using response_type = struct delete_callable_swap_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.callable_swap_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_callable_swap_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_callable_swap_instrument_history_request {
    using response_type = struct get_callable_swap_instrument_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.callable_swap_instruments.history";
    std::string id;
};

struct get_callable_swap_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::callable_swap_instrument> history;
};

// ---- Knock-out swap instrument protocol ----

struct get_knock_out_swap_instruments_request {
    using response_type = struct get_knock_out_swap_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.knock_out_swap_instruments.list";
    int offset = 0;
    int limit = 100;
};

struct get_knock_out_swap_instruments_response {
    std::vector<ores::trading::domain::knock_out_swap_instrument> instruments;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_knock_out_swap_instrument_request {
    using response_type = struct save_knock_out_swap_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.knock_out_swap_instruments.save";
    ores::trading::domain::knock_out_swap_instrument data;
    std::vector<ores::trading::domain::swap_leg> legs;
};

struct save_knock_out_swap_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_knock_out_swap_instrument_request {
    using response_type = struct delete_knock_out_swap_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.knock_out_swap_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_knock_out_swap_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_knock_out_swap_instrument_history_request {
    using response_type = struct get_knock_out_swap_instrument_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.knock_out_swap_instruments.history";
    std::string id;
};

struct get_knock_out_swap_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::knock_out_swap_instrument> history;
};

// ---- Inflation swap instrument protocol ----

struct get_inflation_swap_instruments_request {
    using response_type = struct get_inflation_swap_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.inflation_swap_instruments.list";
    int offset = 0;
    int limit = 100;
};

struct get_inflation_swap_instruments_response {
    std::vector<ores::trading::domain::inflation_swap_instrument> instruments;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_inflation_swap_instrument_request {
    using response_type = struct save_inflation_swap_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.inflation_swap_instruments.save";
    ores::trading::domain::inflation_swap_instrument data;
    std::vector<ores::trading::domain::swap_leg> legs;
};

struct save_inflation_swap_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_inflation_swap_instrument_request {
    using response_type = struct delete_inflation_swap_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.inflation_swap_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_inflation_swap_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_inflation_swap_instrument_history_request {
    using response_type = struct get_inflation_swap_instrument_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.inflation_swap_instruments.history";
    std::string id;
};

struct get_inflation_swap_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::inflation_swap_instrument> history;
};

// ---- RPA instrument protocol ----

struct get_rpa_instruments_request {
    using response_type = struct get_rpa_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.rpa_instruments.list";
    int offset = 0;
    int limit = 100;
};

struct get_rpa_instruments_response {
    std::vector<ores::trading::domain::rpa_instrument> instruments;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_rpa_instrument_request {
    using response_type = struct save_rpa_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.rpa_instruments.save";
    ores::trading::domain::rpa_instrument data;
    std::vector<ores::trading::domain::swap_leg> legs;
};

struct save_rpa_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_rpa_instrument_request {
    using response_type = struct delete_rpa_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.rpa_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_rpa_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_rpa_instrument_history_request {
    using response_type = struct get_rpa_instrument_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.rpa_instruments.history";
    std::string id;
};

struct get_rpa_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::rpa_instrument> history;
};

// ---- Instrument-for-trade fetch ----

/**
 * @brief Swap instrument with its legs, used in export/fetch results.
 *
 * The instrument field is a variant over all supported rates instrument types.
 */
struct swap_export_result {
    std::variant<
        ores::trading::domain::fra_instrument,
        ores::trading::domain::vanilla_swap_instrument,
        ores::trading::domain::cap_floor_instrument,
        ores::trading::domain::swaption_instrument,
        ores::trading::domain::balance_guaranteed_swap_instrument,
        ores::trading::domain::callable_swap_instrument,
        ores::trading::domain::knock_out_swap_instrument,
        ores::trading::domain::inflation_swap_instrument,
        ores::trading::domain::rpa_instrument
    > instrument;
    std::vector<ores::trading::domain::swap_leg> legs;
};

/**
 * @brief FX instrument, used in export/fetch results.
 *
 * The instrument field is a variant over all supported FX instrument types.
 */
struct fx_export_result {
    std::variant<
        ores::trading::domain::fx_forward_instrument,
        ores::trading::domain::fx_vanilla_option_instrument,
        ores::trading::domain::fx_barrier_option_instrument,
        ores::trading::domain::fx_digital_option_instrument,
        ores::trading::domain::fx_asian_forward_instrument,
        ores::trading::domain::fx_accumulator_instrument,
        ores::trading::domain::fx_variance_swap_instrument
    > instrument;
};

/**
 * @brief Equity instrument, used in export/fetch results.
 *
 * The instrument field is a variant over all supported equity instrument
 * types.
 */
struct equity_export_result {
    std::variant<
        ores::trading::domain::equity_option_instrument,
        ores::trading::domain::equity_digital_option_instrument,
        ores::trading::domain::equity_barrier_option_instrument,
        ores::trading::domain::equity_asian_option_instrument,
        ores::trading::domain::equity_forward_instrument,
        ores::trading::domain::equity_variance_swap_instrument,
        ores::trading::domain::equity_swap_instrument,
        ores::trading::domain::equity_accumulator_instrument,
        ores::trading::domain::equity_position_instrument
    > instrument;
};

/**
 * @brief Bond instrument, used in export/fetch results.
 */
struct bond_export_result {
    ores::trading::domain::bond_instrument instrument;
};

/**
 * @brief Credit instrument, used in export/fetch results.
 */
struct credit_export_result {
    ores::trading::domain::credit_instrument instrument;
};

/**
 * @brief Commodity instrument, used in export/fetch results.
 */
struct commodity_export_result {
    ores::trading::domain::commodity_instrument instrument;
};

/**
 * @brief Composite instrument with its constituent legs.
 */
struct composite_export_result {
    ores::trading::domain::composite_instrument instrument;
    std::vector<ores::trading::domain::composite_leg> legs;
};

/**
 * @brief Scripted instrument, used in export/fetch results.
 */
struct scripted_export_result {
    ores::trading::domain::scripted_instrument instrument;
};

/**
 * @brief Discriminated union of all possible instrument representations.
 *
 * std::monostate indicates the trade has no linked instrument (or the family
 * is not recognised). Populated per-trade inside get_trades_response /
 * export_portfolio_response bundles.
 */
using instrument_export_result = std::variant<
    std::monostate,
    swap_export_result,
    fx_export_result,
    bond_export_result,
    credit_export_result,
    equity_export_result,
    commodity_export_result,
    composite_export_result,
    scripted_export_result
>;

}

#endif
