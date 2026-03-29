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
#include <vector>
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.api/domain/fx_instrument.hpp"
#include "ores.trading.api/domain/bond_instrument.hpp"
#include "ores.trading.api/domain/credit_instrument.hpp"
#include "ores.trading.api/domain/equity_instrument.hpp"
#include "ores.trading.api/domain/commodity_instrument.hpp"

namespace ores::trading::messaging {

struct get_instruments_request {
    using response_type = struct get_instruments_response;
    static constexpr std::string_view nats_subject = "trading.v1.instruments.list";
    int offset = 0;
    int limit = 100;
};

struct get_instruments_response {
    std::vector<ores::trading::domain::instrument> instruments;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_instrument_request {
    using response_type = struct save_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.instruments.save";
    ores::trading::domain::instrument data;
    std::vector<ores::trading::domain::swap_leg> legs;
};

struct save_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_instrument_request {
    using response_type = struct delete_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.instruments.delete";
    std::vector<std::string> ids;
};

struct delete_instrument_response {
    bool success = false;
    std::string message;
};

struct get_instrument_history_request {
    using response_type = struct get_instrument_history_response;
    static constexpr std::string_view nats_subject = "trading.v1.instruments.history";
    std::string id;
};

struct get_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::instrument> history;
};

struct get_swap_legs_request {
    using response_type = struct get_swap_legs_response;
    static constexpr std::string_view nats_subject = "trading.v1.instruments.legs.list";
    std::string instrument_id;
};

struct get_swap_legs_response {
    std::vector<ores::trading::domain::swap_leg> legs;
    bool success = true;
    std::string message;
};

// ---- FX instrument protocol ----

struct get_fx_instruments_request {
    using response_type = struct get_fx_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_instruments.list";
    int offset = 0;
    int limit = 100;
};

struct get_fx_instruments_response {
    std::vector<ores::trading::domain::fx_instrument> instruments;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_fx_instrument_request {
    using response_type = struct save_fx_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_instruments.save";
    ores::trading::domain::fx_instrument data;
};

struct save_fx_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_fx_instrument_request {
    using response_type = struct delete_fx_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_fx_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_fx_instrument_history_request {
    using response_type = struct get_fx_instrument_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_instruments.history";
    std::string id;
};

struct get_fx_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::fx_instrument> history;
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

// ---- Equity instrument protocol ----

struct get_equity_instruments_request {
    using response_type = struct get_equity_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_instruments.list";
    int offset = 0;
    int limit = 100;
};

struct get_equity_instruments_response {
    std::vector<ores::trading::domain::equity_instrument> instruments;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_equity_instrument_request {
    using response_type = struct save_equity_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_instruments.save";
    ores::trading::domain::equity_instrument data;
};

struct save_equity_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_equity_instrument_request {
    using response_type = struct delete_equity_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_equity_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_equity_instrument_history_request {
    using response_type = struct get_equity_instrument_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_instruments.history";
    std::string id;
};

struct get_equity_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::equity_instrument> history;
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

}

#endif
