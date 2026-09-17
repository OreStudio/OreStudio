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
#include "ores.shell/app/commands/trading/fx_digital_option_instrument_commands.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/command_token.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.trading.api/domain/fx_digital_option_instrument_table_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/fx_digital_option_instrument_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cli/cli.h>
#include <functional>
#include <ostream>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;
namespace domain = ores::trading::domain;

void fx_digital_option_instrument_commands::register_commands(cli::Menu& root_menu,
                                                              nats_client& session,
                                                              pagination_context& pagination) {
    auto fx_digital_option_instruments_menu =
        std::make_unique<cli::Menu>("fx_digital_option_instruments");

    fx_digital_option_instruments_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_fx_digital_option_instruments(
                std::ref(out), std::ref(session), std::ref(pagination));
        },
        "Retrieve FX digital option instruments from the server (paginated)");

    pagination.register_list_callback(
        "fx_digital_option_instruments", [&session, &pagination](std::ostream& out) {
            process_get_fx_digital_option_instruments(out, session, pagination);
        });

    fx_digital_option_instruments_menu->Insert(
        "add",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_add_fx_digital_option_instrument(std::ref(out), std::ref(session), args);
        },
        "Add FX digital option instrument (trade_type_code "
        "party_id "
        "trade_id "
        "foreign_currency "
        "domestic_currency "
        "payoff_currency "
        "payoff_amount "
        "option_type "
        "expiry_date "
        "long_short "
        "strike "
        "barrier_type "
        "lower_barrier "
        "upper_barrier "
        "description "
        "change_reason_code "
        "\"change_commentary\")");

    fx_digital_option_instruments_menu->Insert(
        "delete",
        [&session](std::ostream& out, std::string id) {
            process_delete_fx_digital_option_instrument(
                std::ref(out), std::ref(session), std::move(id));
        },
        "Delete FX digital option instrument by instrument_id");

    fx_digital_option_instruments_menu->Insert(
        "history",
        [&session](std::ostream& out, std::string id) {
            process_get_fx_digital_option_instrument_history(
                std::ref(out), std::ref(session), std::move(id));
        },
        "Show FX digital option instrument's version history");

    root_menu.Insert(std::move(fx_digital_option_instruments_menu));
}

void fx_digital_option_instrument_commands::process_get_fx_digital_option_instruments(
    std::ostream& out, nats_client& session, pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get FX digital option instrument request.";

    auto& state = pagination.state_for("fx_digital_option_instruments");

    trading::messaging::get_fx_digital_option_instruments_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<trading::messaging::get_fx_digital_option_instruments_response>(
        out, session, "trading.v1.fx_digital_option_instruments.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("fx_digital_option_instruments");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->fx_digital_option_instruments.size()
                              << " FX digital option instruments.";
    out << result->fx_digital_option_instruments << std::endl;

    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " ("
        << result->fx_digital_option_instruments.size() << " of " << state.total_count << " total)"
        << std::endl;
}

void fx_digital_option_instrument_commands::process_add_fx_digital_option_instrument(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add FX digital option instrument request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to add FX digital option instrument." << std::endl;
        return;
    }

    const auto parsed = parse_args(args, {});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 15 + 2;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    domain::fx_digital_option_instrument v;
    std::size_t next = 0;
    try {
        if (const auto tenant = utility::uuid::tenant_id::from_string(session.auth().tenant_id);
            tenant) {
            v.identity.tenant_id = *tenant;
        }
        v.identity.instrument_id = boost::uuids::random_generator()();
        v.identity.trade_type_code = ores::shell::app::from_token<std::string>(
            parsed->positionals[next++], "trade_type_code");
        v.identity.party_id = ores::shell::app::from_token<boost::uuids::uuid>(
            parsed->positionals[next++], "party_id");
        v.identity.trade_id = ores::shell::app::from_token<std::optional<boost::uuids::uuid>>(
            parsed->positionals[next++], "trade_id");
        v.foreign_currency = ores::shell::app::from_token<std::string>(parsed->positionals[next++],
                                                                       "foreign_currency");
        v.domestic_currency = ores::shell::app::from_token<std::string>(parsed->positionals[next++],
                                                                        "domestic_currency");
        v.payoff_currency = ores::shell::app::from_token<std::string>(parsed->positionals[next++],
                                                                      "payoff_currency");
        v.payoff_amount =
            ores::shell::app::from_token<double>(parsed->positionals[next++], "payoff_amount");
        v.option_type =
            ores::shell::app::from_token<std::string>(parsed->positionals[next++], "option_type");
        v.expiry_date =
            ores::shell::app::from_token<std::string>(parsed->positionals[next++], "expiry_date");
        v.long_short =
            ores::shell::app::from_token<std::string>(parsed->positionals[next++], "long_short");
        v.strike = ores::shell::app::from_token<std::optional<double>>(parsed->positionals[next++],
                                                                       "strike");
        v.barrier_type =
            ores::shell::app::from_token<std::string>(parsed->positionals[next++], "barrier_type");
        v.lower_barrier = ores::shell::app::from_token<std::optional<double>>(
            parsed->positionals[next++], "lower_barrier");
        v.upper_barrier = ores::shell::app::from_token<std::optional<double>>(
            parsed->positionals[next++], "upper_barrier");
        v.description =
            ores::shell::app::from_token<std::string>(parsed->positionals[next++], "description");
        v.audit.modified_by = session.auth().username;
        v.audit.change_reason_code = std::move(parsed->positionals[next++]);
        v.audit.change_commentary = std::move(parsed->positionals[next++]);
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto req = trading::messaging::save_fx_digital_option_instrument_request::from(std::move(v));

    auto result = do_auth_request<trading::messaging::save_fx_digital_option_instrument_response>(
        out, session, "trading.v1.fx_digital_option_instruments.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added FX digital option instrument.";
        out << "✓ FX digital option instrument added successfully!" << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add FX digital option instrument: " << msg;
        fail(out) << "Failed to add FX digital option instrument: " << msg << std::endl;
    }
}

void fx_digital_option_instrument_commands::process_delete_fx_digital_option_instrument(
    std::ostream& out, nats_client& session, std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete FX digital option instrument request for: "
                               << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete FX digital option instrument." << std::endl;
        return;
    }

    trading::messaging::delete_fx_digital_option_instrument_request req;
    req.ids = {std::move(instrument_id)};

    auto result = do_auth_request<trading::messaging::delete_fx_digital_option_instrument_response>(
        out, session, "trading.v1.fx_digital_option_instruments.delete", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted FX digital option instrument.";
        out << "✓ FX digital option instrument deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete FX digital option instrument: "
                                  << result->message;
        fail(out) << "Failed to delete FX digital option instrument: " << result->message
                  << std::endl;
    }
}

void fx_digital_option_instrument_commands::process_get_fx_digital_option_instrument_history(
    std::ostream& out, nats_client& session, std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get FX digital option instrument history for: "
                               << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get FX digital option instrument history."
                  << std::endl;
        return;
    }

    trading::messaging::get_fx_digital_option_instrument_history_request req;
    req.instrument_id = std::move(instrument_id);

    auto result =
        do_auth_request<trading::messaging::get_fx_digital_option_instrument_history_response>(
            out, session, "trading.v1.fx_digital_option_instruments.history", req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get FX digital option instrument history: "
                                  << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this FX digital option instrument." << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
