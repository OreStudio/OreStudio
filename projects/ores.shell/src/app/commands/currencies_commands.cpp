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
#include "ores.shell/app/commands/currencies_commands.hpp"

#include <ostream>
#include <functional>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <cli/cli.h>
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.refdata.api/messaging/currency_history_protocol.hpp"
#include "ores.refdata.api/domain/currency_table_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/domain/currency_version_table_io.hpp" // IWYU pragma: keep.

namespace ores::shell::app::commands {

using namespace logging;
using service::nats_session;

namespace {

template<typename Response>
std::optional<Response> do_request(std::ostream& out, nats_session& session,
    const std::string& subject, const std::string& body) {
    try {
        auto reply = session.request(subject, body);
        auto data_str = std::string(
            reinterpret_cast<const char*>(reply.data.data()), reply.data.size());
        auto result = rfl::json::read<Response>(data_str);
        if (!result) {
            out << "✗ Failed to parse response" << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        out << "✗ Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

template<typename Response>
std::optional<Response> do_auth_request(std::ostream& out, nats_session& session,
    const std::string& subject, const std::string& body) {
    try {
        auto reply = session.authenticated_request(subject, body);
        auto data_str = std::string(
            reinterpret_cast<const char*>(reply.data.data()), reply.data.size());
        auto result = rfl::json::read<Response>(data_str);
        if (!result) {
            out << "✗ Failed to parse response" << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        out << "✗ Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

} // anonymous namespace

void currencies_commands::
register_commands(cli::Menu& root_menu, nats_session& session,
                  pagination_context& pagination) {
    auto currencies_menu =
        std::make_unique<cli::Menu>("currencies");

    currencies_menu->Insert("get", [&session, &pagination](std::ostream& out) {
        process_get_currencies(std::ref(out), std::ref(session),
                               std::ref(pagination));
    }, "Retrieve currencies from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback("currencies",
        [&session, &pagination](std::ostream& out) {
            process_get_currencies(out, session, pagination);
        });

    currencies_menu->Insert("add", [&session](std::ostream& out,
            std::string iso_code, std::string name,
            std::string numeric_code, std::string symbol,
            std::string fractions_per_unit, std::string change_reason_code,
            std::string change_commentary) {
        process_add_currency(std::ref(out), std::ref(session),
            std::move(iso_code), std::move(name),
            std::move(numeric_code), std::move(symbol),
            std::move(fractions_per_unit), std::move(change_reason_code),
            std::move(change_commentary));
    }, "Add a currency (iso_code name numeric_code symbol fractions reason_code \"commentary\")");

    currencies_menu->Insert("delete", [&session](std::ostream& out,
            std::string iso_code) {
        process_delete_currency(std::ref(out), std::ref(session),
            std::move(iso_code));
    }, "Delete a currency by ISO code");

    currencies_menu->Insert("history", [&session](std::ostream& out,
            std::string iso_code) {
        process_get_currency_history(std::ref(out), std::ref(session),
            std::move(iso_code));
    }, "Get version history for a currency by ISO code");

    root_menu.Insert(std::move(currencies_menu));
}

void currencies_commands::
process_get_currencies(std::ostream& out, nats_session& session,
                       pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get currencies request.";

    auto& state = pagination.state_for("currencies");

    refdata::messaging::get_currencies_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_request<refdata::messaging::get_currencies_response>(
        out, session, "refdata.v1.currencies.list", rfl::json::write(req));
    if (!result) return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("currencies");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->currencies.size() << " currencies.";
    out << result->currencies << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages = state.total_count > 0
        ? ((state.total_count + pagination.page_size() - 1) / pagination.page_size())
        : 1;
    out << "\nPage " << page << " of " << total_pages
        << " (" << result->currencies.size() << " of "
        << state.total_count << " total)" << std::endl;
}

void currencies_commands::
process_add_currency(std::ostream& out, nats_session& session,
    std::string iso_code, std::string name,
    std::string numeric_code, std::string symbol,
    std::string fractions_per_unit, std::string change_reason_code,
    std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add currency request for: "
                               << iso_code;

    // Get modified_by from logged-in user
    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to add a currency." << std::endl;
        return;
    }
    const auto& modified_by = session.auth().username;

    // Parse fractions_per_unit with default
    int fractions = 100;
    if (!fractions_per_unit.empty()) {
        try {
            fractions = std::stoi(fractions_per_unit);
        } catch (...) {
            out << "✗ Invalid fractions_per_unit value: " << fractions_per_unit
                << std::endl;
            return;
        }
    }

    auto req = refdata::messaging::save_currency_request::from(
        refdata::domain::currency{
            .version = 0,
            .iso_code = std::move(iso_code),
            .name = std::move(name),
            .numeric_code = std::move(numeric_code),
            .symbol = std::move(symbol),
            .fraction_symbol = "",
            .fractions_per_unit = fractions,
            .rounding_type = "Nearest",
            .rounding_precision = 2,
            .format = "",
            .monetary_nature = "fiat",
            .market_tier = "g10",
            .modified_by = modified_by,
            .change_reason_code = std::move(change_reason_code),
            .change_commentary = std::move(change_commentary),
            .recorded_at = std::chrono::system_clock::now()
        });

    auto result = do_auth_request<refdata::messaging::save_currency_response>(
        out, session, "refdata.v1.currencies.save", rfl::json::write(req));
    if (!result) return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added currency.";
        out << "✓ Currency added successfully!" << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add currency: " << msg;
        out << "✗ Failed to add currency: " << msg << std::endl;
    }
}

void currencies_commands::
process_delete_currency(std::ostream& out, nats_session& session,
    std::string iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete currency request for: "
                               << iso_code;

    // Check if logged in
    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to delete a currency." << std::endl;
        return;
    }

    refdata::messaging::delete_currency_request req;
    req.iso_codes = {iso_code};

    auto result = do_auth_request<refdata::messaging::delete_currency_response>(
        out, session, "refdata.v1.currencies.delete", rfl::json::write(req));
    if (!result) return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted currency: " << iso_code;
        out << "✓ Currency " << iso_code << " deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete currency: " << result->message;
        out << "✗ Failed to delete currency: " << result->message << std::endl;
    }
}

void currencies_commands::
process_get_currency_history(std::ostream& out, nats_session& session,
    std::string iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get currency history for: "
                               << iso_code;

    // Check if logged in
    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to get currency history." << std::endl;
        return;
    }

    refdata::messaging::get_currency_history_request req;
    req.iso_code = std::move(iso_code);

    auto result = do_auth_request<refdata::messaging::get_currency_history_response>(
        out, session, "refdata.v1.currencies.history", rfl::json::write(req));
    if (!result) return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get currency history: "
                                  << result->message;
        out << "✗ " << result->message << std::endl;
        return;
    }

    if (result->history.versions.empty()) {
        out << "No history found for this currency." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->history.versions.size()
                              << " history records.";
    out << result->history.versions << std::endl;
}

}
