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
#include "ores.shell/app/commands/trading/commodity_instrument_commands.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.trading.api/domain/commodity_instrument_table_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cli/cli.h>
#include <functional>
#include <optional>
#include <ostream>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;
namespace domain = ores::trading::domain;

namespace {

constexpr std::string_view session_party_error =
    "The logged-in account has no default party; set one before adding instruments.";

boost::uuids::uuid party_uuid_for(nats_client& session) {
    const auto& party = session.auth().default_party_id;
    if (party.empty())
        throw std::runtime_error(std::string(session_party_error));
    return boost::lexical_cast<boost::uuids::uuid>(party);
}

std::optional<double> parse_optional_double(std::string_view value, std::string_view name) {
    if (value.empty() || value == "-")
        return std::nullopt;
    try {
        return std::stod(std::string(value));
    } catch (const std::exception&) {
        throw std::runtime_error(std::string("Invalid numeric value for ") + std::string(name) +
                                 ".");
    }
}

} // namespace

void commodity_instrument_commands::register_commands(cli::Menu& root_menu,
                                                      nats_client& session,
                                                      pagination_context& pagination) {
    auto commodity_instruments_menu = std::make_unique<cli::Menu>("commodity_instruments");

    commodity_instruments_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_commodity_instruments(
                std::ref(out), std::ref(session), std::ref(pagination));
        },
        "Retrieve Commodity instruments from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback(
        "commodity_instruments", [&session, &pagination](std::ostream& out) {
            process_get_commodity_instruments(out, session, pagination);
        });

    commodity_instruments_menu->Insert(
        "add",
        [&session](std::ostream& out,
                   std::string trade_type_code,
                   std::string commodity_code,
                   std::string currency,
                   double quantity,
                   std::string unit,
                   std::string fixed_price,
                   std::string start_date,
                   std::string maturity_date,
                   std::string day_count_code,
                   std::string payment_frequency_code,
                   std::string option_type,
                   std::string strike_price,
                   std::string exercise_type,
                   std::string swaption_expiry_date,
                   std::string average_type,
                   std::string averaging_start_date,
                   std::string averaging_end_date,
                   std::string spread_commodity_code,
                   std::string spread_amount,
                   std::string strip_frequency_code,
                   std::string variance_strike,
                   std::string accumulation_amount,
                   std::string knock_out_barrier,
                   std::string barrier_type,
                   std::string lower_barrier,
                   std::string upper_barrier,
                   std::string basket_json,
                   std::string description,
                   std::string change_reason_code,
                   std::string change_commentary) {
            process_add_commodity_instrument(std::ref(out),
                                             std::ref(session),
                                             std::move(trade_type_code),
                                             std::move(commodity_code),
                                             std::move(currency),
                                             quantity,
                                             std::move(unit),
                                             std::move(fixed_price),
                                             std::move(start_date),
                                             std::move(maturity_date),
                                             std::move(day_count_code),
                                             std::move(payment_frequency_code),
                                             std::move(option_type),
                                             std::move(strike_price),
                                             std::move(exercise_type),
                                             std::move(swaption_expiry_date),
                                             std::move(average_type),
                                             std::move(averaging_start_date),
                                             std::move(averaging_end_date),
                                             std::move(spread_commodity_code),
                                             std::move(spread_amount),
                                             std::move(strip_frequency_code),
                                             std::move(variance_strike),
                                             std::move(accumulation_amount),
                                             std::move(knock_out_barrier),
                                             std::move(barrier_type),
                                             std::move(lower_barrier),
                                             std::move(upper_barrier),
                                             std::move(basket_json),
                                             std::move(description),
                                             std::move(change_reason_code),
                                             std::move(change_commentary));
        },
        "Add an Commodity instrument (trade_type_code commodity_code currency quantity unit "
        "[fixed_price] [start_date] [maturity_date] [day_count_code] [payment_frequency_code] "
        "[option_type] [strike_price] [exercise_type] [swaption_expiry_date] [average_type] "
        "[averaging_start_date] [averaging_end_date] [spread_commodity_code] [spread_amount] "
        "[strip_frequency_code] [variance_strike] [accumulation_amount] [knock_out_barrier] "
        "[barrier_type] [lower_barrier] [upper_barrier] [basket_json] description "
        "change_reason_code \"change_commentary\")",
        {"trade_type_code commodity_code currency quantity unit fixed_price start_date "
         "maturity_date day_count_code payment_frequency_code option_type strike_price "
         "exercise_type swaption_expiry_date average_type averaging_start_date averaging_end_date "
         "spread_commodity_code spread_amount strip_frequency_code variance_strike "
         "accumulation_amount knock_out_barrier barrier_type lower_barrier upper_barrier "
         "basket_json description change_reason_code change_commentary"});

    commodity_instruments_menu->Insert(
        "delete",
        [&session](std::ostream& out, std::string instrument_id) {
            process_delete_commodity_instrument(
                std::ref(out), std::ref(session), std::move(instrument_id));
        },
        "Delete an Commodity instrument by instrument id",
        {"instrument_id"});

    commodity_instruments_menu->Insert(
        "history",
        [&session](std::ostream& out, std::string instrument_id) {
            process_get_commodity_instrument_history(
                std::ref(out), std::ref(session), std::move(instrument_id));
        },
        "Show an Commodity instrument's version history",
        {"instrument_id"});

    root_menu.Insert(std::move(commodity_instruments_menu));
}

void commodity_instrument_commands::process_get_commodity_instruments(
    std::ostream& out, nats_client& session, pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get Commodity instruments request.";

    auto& state = pagination.state_for("commodity_instruments");

    trading::messaging::get_commodity_instruments_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<trading::messaging::get_commodity_instruments_response>(
        out, session, "trading.v1.commodity_instruments.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("commodity_instruments");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << result->instruments.size()
                              << " Commodity instruments.";
    out << result->instruments << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " (" << result->instruments.size()
        << " of " << state.total_count << " total)" << std::endl;
}

void commodity_instrument_commands::process_add_commodity_instrument(
    std::ostream& out,
    nats_client& session,
    std::string trade_type_code,
    std::string commodity_code,
    std::string currency,
    double quantity,
    std::string unit,
    std::string fixed_price,
    std::string start_date,
    std::string maturity_date,
    std::string day_count_code,
    std::string payment_frequency_code,
    std::string option_type,
    std::string strike_price,
    std::string exercise_type,
    std::string swaption_expiry_date,
    std::string average_type,
    std::string averaging_start_date,
    std::string averaging_end_date,
    std::string spread_commodity_code,
    std::string spread_amount,
    std::string strip_frequency_code,
    std::string variance_strike,
    std::string accumulation_amount,
    std::string knock_out_barrier,
    std::string barrier_type,
    std::string lower_barrier,
    std::string upper_barrier,
    std::string basket_json,
    std::string description,
    std::string change_reason_code,
    std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add Commodity instrument request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to add an Commodity instrument." << std::endl;
        return;
    }

    domain::commodity_instrument v;
    v.identity.instrument_id = boost::uuids::random_generator()();
    v.identity.trade_type_code = std::move(trade_type_code);
    try {
        v.identity.party_id = party_uuid_for(session);
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }
    const auto& tenant = session.auth().tenant_id;
    if (auto tid = utility::uuid::tenant_id::from_string(tenant); tid)
        v.identity.tenant_id = *tid;

    v.terms.commodity_code = std::move(commodity_code);
    v.terms.currency = std::move(currency);
    v.terms.quantity = quantity;
    v.terms.unit = std::move(unit);
    v.terms.start_date = (start_date == "-") ? "" : std::move(start_date);
    v.terms.maturity_date = (maturity_date == "-") ? "" : std::move(maturity_date);
    v.terms.day_count_code = (day_count_code == "-") ? "" : std::move(day_count_code);
    v.terms.payment_frequency_code =
        (payment_frequency_code == "-") ? "" : std::move(payment_frequency_code);
    v.option.option_type = (option_type == "-") ? "" : std::move(option_type);
    v.option.exercise_type = (exercise_type == "-") ? "" : std::move(exercise_type);
    v.option.swaption_expiry_date =
        (swaption_expiry_date == "-") ? "" : std::move(swaption_expiry_date);
    v.pricing.average_type = (average_type == "-") ? "" : std::move(average_type);
    v.pricing.averaging_start_date =
        (averaging_start_date == "-") ? "" : std::move(averaging_start_date);
    v.pricing.averaging_end_date = (averaging_end_date == "-") ? "" : std::move(averaging_end_date);
    v.pricing.spread_commodity_code =
        (spread_commodity_code == "-") ? "" : std::move(spread_commodity_code);
    v.pricing.strip_frequency_code =
        (strip_frequency_code == "-") ? "" : std::move(strip_frequency_code);
    v.exotic.barrier_type = (barrier_type == "-") ? "" : std::move(barrier_type);
    v.exotic.basket_json = (basket_json == "-") ? "" : std::move(basket_json);
    v.description = std::move(description);

    try {
        v.terms.fixed_price = parse_optional_double(fixed_price, "fixed_price");
        v.option.strike_price = parse_optional_double(strike_price, "strike_price");
        v.pricing.spread_amount = parse_optional_double(spread_amount, "spread_amount");
        v.exotic.variance_strike = parse_optional_double(variance_strike, "variance_strike");
        v.exotic.accumulation_amount =
            parse_optional_double(accumulation_amount, "accumulation_amount");
        v.exotic.knock_out_barrier = parse_optional_double(knock_out_barrier, "knock_out_barrier");
        v.exotic.lower_barrier = parse_optional_double(lower_barrier, "lower_barrier");
        v.exotic.upper_barrier = parse_optional_double(upper_barrier, "upper_barrier");
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    // The trading tables require modified_by to name a real account
    // username; the logged-in account is the acting principal.
    v.audit.modified_by = session.auth().username;
    v.audit.change_reason_code = std::move(change_reason_code);
    v.audit.change_commentary = std::move(change_commentary);

    auto req = trading::messaging::save_commodity_instrument_request{.data = std::move(v)};

    auto result = do_auth_request<trading::messaging::save_commodity_instrument_response>(
        out, session, "trading.v1.commodity_instruments.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added Commodity instrument.";
        out << "✓ Commodity instrument added successfully!" << std::endl;
        out << "Instrument id: " << boost::uuids::to_string(req.data.identity.instrument_id)
            << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add Commodity instrument: " << msg;
        fail(out) << "Failed to add Commodity instrument: " << msg << std::endl;
    }
}

void commodity_instrument_commands::process_delete_commodity_instrument(std::ostream& out,
                                                                        nats_client& session,
                                                                        std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete Commodity instrument request for: "
                               << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete an Commodity instrument." << std::endl;
        return;
    }

    trading::messaging::delete_commodity_instrument_request req;
    req.ids = {std::move(instrument_id)};

    auto result = do_auth_request<trading::messaging::delete_commodity_instrument_response>(
        out, session, "trading.v1.commodity_instruments.delete", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted Commodity instrument.";
        out << "✓ Commodity instrument deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete Commodity instrument: " << result->message;
        fail(out) << "Failed to delete Commodity instrument: " << result->message << std::endl;
    }
}

void commodity_instrument_commands::process_get_commodity_instrument_history(
    std::ostream& out, nats_client& session, std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get Commodity instrument history for: "
                               << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get Commodity instrument history." << std::endl;
        return;
    }

    trading::messaging::get_commodity_instrument_history_request req;
    req.id = std::move(instrument_id);

    auto result = do_auth_request<trading::messaging::get_commodity_instrument_history_response>(
        out, session, "trading.v1.commodity_instruments.history", req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get Commodity instrument history: "
                                  << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this Commodity instrument." << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
