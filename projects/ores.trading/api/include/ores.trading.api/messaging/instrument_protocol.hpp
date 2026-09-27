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

#include "ores.trading.api/domain/composite_instrument.hpp"
#include "ores.trading.api/domain/composite_leg.hpp"
#include "ores.trading.api/domain/credit_instrument.hpp"
#include "ores.trading.api/domain/scripted_instrument.hpp"
#include <string>
#include <variant>
#include <vector>

namespace ores::trading::messaging {

// ---- Credit instrument protocol ----

struct get_credit_instruments_request {
    using response_type = struct get_credit_instruments_response;
    static constexpr std::string_view nats_subject = "trading.v1.credit_instruments.list";
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
    static constexpr std::string_view nats_subject = "trading.v1.credit_instruments.save";
    ores::trading::domain::credit_instrument data;
};

struct save_credit_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_credit_instrument_request {
    using response_type = struct delete_credit_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.credit_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_credit_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_credit_instrument_history_request {
    using response_type = struct get_credit_instrument_history_response;
    static constexpr std::string_view nats_subject = "trading.v1.credit_instruments.history";
    std::string id;
};

struct get_credit_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::credit_instrument> history;
};

// ---- Composite instrument protocol ----

struct get_composite_instrument_legs_request {
    using response_type = struct get_composite_instrument_legs_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_instruments.legs.list";
    std::string instrument_id;
};

struct get_composite_instrument_legs_response {
    std::vector<ores::trading::domain::composite_leg> legs;
    bool success = true;
    std::string message;
};

struct get_composite_instruments_request {
    using response_type = struct get_composite_instruments_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_instruments.list";
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
    static constexpr std::string_view nats_subject = "trading.v1.composite_instruments.save";
    ores::trading::domain::composite_instrument data;
    std::vector<ores::trading::domain::composite_leg> legs;
};

struct save_composite_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_composite_instrument_request {
    using response_type = struct delete_composite_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_composite_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_composite_instrument_history_request {
    using response_type = struct get_composite_instrument_history_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_instruments.history";
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
    static constexpr std::string_view nats_subject = "trading.v1.scripted_instruments.list";
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
    static constexpr std::string_view nats_subject = "trading.v1.scripted_instruments.save";
    ores::trading::domain::scripted_instrument data;
};

struct save_scripted_instrument_response {
    bool success = false;
    std::string message;
};

struct delete_scripted_instrument_request {
    using response_type = struct delete_scripted_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.scripted_instruments.delete";
    std::vector<std::string> ids;
};

struct delete_scripted_instrument_response {
    bool success = false;
    std::string message;
    std::vector<std::pair<std::string, std::pair<bool, std::string>>> results;
};

struct get_scripted_instrument_history_request {
    using response_type = struct get_scripted_instrument_history_response;
    static constexpr std::string_view nats_subject = "trading.v1.scripted_instruments.history";
    std::string id;
};

struct get_scripted_instrument_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::scripted_instrument> history;
};

}

#endif
