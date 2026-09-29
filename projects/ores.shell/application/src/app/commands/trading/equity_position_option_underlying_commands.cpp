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
#include "ores.shell/app/commands/trading/equity_position_option_underlying_commands.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.trading.api/messaging/equity_position_option_underlying_protocol.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/string_generator.hpp>
#include <cli/cli.h>
#include <ostream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;
namespace domain = ores::trading::domain;
namespace messaging = ores::trading::messaging;

namespace {

ores::utility::decimal::decimal parse_decimal(std::string_view value, std::string_view name) {
    auto parsed = ores::utility::decimal::decimal::from_string(value);
    if (!parsed)
        throw std::runtime_error(std::string("Invalid numeric value for ") + std::string(name) +
                                 ".");
    return *parsed;
}

void read_optional_text(std::string& field, const std::string& value) {
    field = value == "-" ? std::string() : value;
}

/**
 * Splits the entries argument. Each token states, in the child table's column
 * order, name:strike:long_short and then the optional weight and option terms.
 * A dash states no entries.
 */
std::vector<domain::equity_position_option_underlying> parse_entries(const std::string& text) {
    std::vector<domain::equity_position_option_underlying> entries;
    if (text.empty() || text == "-")
        return entries;
    std::size_t begin = 0;
    while (begin <= text.size()) {
        const auto end = text.find(',', begin);
        const auto token = text.substr(begin, end == std::string::npos ? end : end - begin);
        if (!token.empty()) {
            std::vector<std::string> fields;
            std::size_t field_begin = 0;
            while (field_begin <= token.size()) {
                const auto field_end = token.find(':', field_begin);
                fields.push_back(token.substr(
                    field_begin,
                    field_end == std::string::npos ? field_end : field_end - field_begin));
                if (field_end == std::string::npos)
                    break;
                field_begin = field_end + 1;
            }
            if (fields.size() < 3)
                throw std::runtime_error("An entry states at least name:strike:long_short, got '" +
                                         token + "'.");
            domain::equity_position_option_underlying entry;
            entry.sequence_number = static_cast<int>(entries.size()) + 1;
            entry.underlying_name = fields[0];
            entry.strike = parse_decimal(fields[1], "strike");
            entry.long_short = fields[2];
            if (fields.size() > 3 && !fields[3].empty() && fields[3] != "-")
                entry.weight = parse_decimal(fields[3], "weight");
            if (fields.size() > 4)
                read_optional_text(entry.option_type, fields[4]);
            if (fields.size() > 5)
                read_optional_text(entry.exercise_type, fields[5]);
            if (fields.size() > 6)
                read_optional_text(entry.settlement_type, fields[6]);
            entries.push_back(std::move(entry));
        }
        if (end == std::string::npos)
            break;
        begin = end + 1;
    }
    return entries;
}

} // namespace

void equity_position_option_underlying_commands::register_commands(cli::Menu& root_menu,
                                                                   nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("equity_position_option_underlyings");

    menu->Insert("set",
                 [&session](std::ostream& out, std::string trade_id, std::string entries) {
                     process_set_underlyings(
                         std::ref(out), std::ref(session), std::move(trade_id), std::move(entries));
                 },
                 "Write the option entries of one equity position instrument "
                 "(trade_id "
                 "\"name:strike:long_short[:weight[:option_type[:exercise_type[:settlement_type]]]]"
                 ",...\")",
                 {"trade_id", "entries"});

    root_menu.Insert(std::move(menu));
}

void equity_position_option_underlying_commands::process_set_underlyings(std::ostream& out,
                                                                         nats_client& session,
                                                                         std::string trade_id,
                                                                         std::string entries) {
    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to write equity position entries." << std::endl;
        return;
    }

    std::vector<domain::equity_position_option_underlying> parsed;
    try {
        parsed = parse_entries(entries);
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    boost::uuids::uuid id;
    try {
        id = boost::uuids::string_generator()(trade_id);
    } catch (const std::exception&) {
        fail(out) << "Invalid trade_id '" << trade_id << "'." << std::endl;
        return;
    }

    for (auto& entry : parsed) {
        messaging::put_equity_position_option_underlying_request req;
        req.change.write.trade_id = id;
        req.change.write.sequence_number = entry.sequence_number;
        req.change.write.underlying_name = std::move(entry.underlying_name);
        req.change.write.strike = entry.strike;
        req.change.write.weight = entry.weight;
        req.change.write.long_short = std::move(entry.long_short);
        req.change.write.option_type = std::move(entry.option_type);
        req.change.write.exercise_type = std::move(entry.exercise_type);
        req.change.write.settlement_type = std::move(entry.settlement_type);
        auto result = do_auth_request<messaging::put_equity_position_option_underlying_response>(
            out, session, std::string(req.nats_subject), req);
        if (!result || result->result.outcome != ores::utility::domain::outcome::ok) {
            const auto& message = result ? result->result.message : std::string("no response");
            BOOST_LOG_SEV(lg(), warn) << "Failed to write equity position entry: " << message;
            fail(out) << "Failed to write equity position entry: " << message << std::endl;
            return;
        }
    }

    out << "✓ Wrote " << parsed.size() << " equity position entries." << std::endl;
}

}
