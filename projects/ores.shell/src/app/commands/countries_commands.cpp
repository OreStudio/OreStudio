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
#include "ores.shell/app/commands/countries_commands.hpp"

#include <ostream>
#include <functional>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <cli/cli.h>
#include "ores.refdata/messaging/country_protocol.hpp"
#include "ores.refdata/domain/country_table_io.hpp" // IWYU pragma: keep.

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

void countries_commands::
register_commands(cli::Menu& root_menu, nats_session& session,
                  pagination_context& pagination) {
    auto countries_menu =
        std::make_unique<cli::Menu>("countries");

    countries_menu->Insert("get", [&session, &pagination](std::ostream& out) {
        process_get_countries(std::ref(out), std::ref(session),
                              std::ref(pagination));
    }, "Retrieve countries from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback("countries",
        [&session, &pagination](std::ostream& out) {
            process_get_countries(out, session, pagination);
        });

    countries_menu->Insert("add", [&session](std::ostream& out,
            std::string alpha2_code, std::string alpha3_code,
            std::string numeric_code, std::string name,
            std::string official_name, std::string change_reason_code,
            std::string change_commentary) {
        process_add_country(std::ref(out), std::ref(session),
            std::move(alpha2_code), std::move(alpha3_code),
            std::move(numeric_code), std::move(name),
            std::move(official_name), std::move(change_reason_code),
            std::move(change_commentary));
    }, "Add a country (alpha2 alpha3 numeric name official_name reason_code \"commentary\")");

    countries_menu->Insert("delete", [&session](std::ostream& out,
            std::string alpha2_code) {
        process_delete_country(std::ref(out), std::ref(session),
            std::move(alpha2_code));
    }, "Delete a country by alpha-2 code");

    countries_menu->Insert("history", [&session](std::ostream& out,
            std::string alpha2_code) {
        process_get_country_history(std::ref(out), std::ref(session),
            std::move(alpha2_code));
    }, "Get version history for a country by alpha-2 code");

    root_menu.Insert(std::move(countries_menu));
}

void countries_commands::
process_get_countries(std::ostream& out, nats_session& session,
                      pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get countries request.";

    auto& state = pagination.state_for("countries");

    refdata::messaging::get_countries_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_request<refdata::messaging::get_countries_response>(
        out, session, "refdata.v1.countries.list", rfl::json::write(req));
    if (!result) return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("countries");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->countries.size() << " countries.";
    out << result->countries << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages = state.total_count > 0
        ? ((state.total_count + pagination.page_size() - 1) / pagination.page_size())
        : 1;
    out << "\nPage " << page << " of " << total_pages
        << " (" << result->countries.size() << " of "
        << state.total_count << " total)" << std::endl;
}

void countries_commands::
process_add_country(std::ostream& out, nats_session& session,
    std::string alpha2_code, std::string alpha3_code,
    std::string numeric_code, std::string name,
    std::string official_name, std::string change_reason_code,
    std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add country request for: "
                               << alpha2_code;

    // Get modified_by from logged-in user
    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to add a country." << std::endl;
        return;
    }
    const auto& modified_by = session.auth().username;

    auto req = refdata::messaging::save_country_request::from(
        refdata::domain::country{
            .version = 0,
            .alpha2_code = std::move(alpha2_code),
            .alpha3_code = std::move(alpha3_code),
            .numeric_code = std::move(numeric_code),
            .name = std::move(name),
            .official_name = std::move(official_name),
            .image_id = std::nullopt,
            .modified_by = modified_by,
            .change_reason_code = std::move(change_reason_code),
            .change_commentary = std::move(change_commentary),
            .recorded_at = std::chrono::system_clock::now()
        });

    auto result = do_auth_request<refdata::messaging::save_country_response>(
        out, session, "refdata.v1.countries.save", rfl::json::write(req));
    if (!result) return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added country.";
        out << "✓ Country added successfully!" << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add country: " << msg;
        out << "✗ Failed to add country: " << msg << std::endl;
    }
}

void countries_commands::
process_delete_country(std::ostream& out, nats_session& session,
    std::string alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete country request for: "
                               << alpha2_code;

    // Check if logged in
    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to delete a country." << std::endl;
        return;
    }

    refdata::messaging::delete_country_request req;
    req.alpha2_codes = {alpha2_code};

    auto result = do_auth_request<refdata::messaging::delete_country_response>(
        out, session, "refdata.v1.countries.delete", rfl::json::write(req));
    if (!result) return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted country: " << alpha2_code;
        out << "✓ Country " << alpha2_code << " deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete country: " << result->message;
        out << "✗ Failed to delete country: " << result->message << std::endl;
    }
}

void countries_commands::
process_get_country_history(std::ostream& out, nats_session& session,
    std::string alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get country history for: "
                               << alpha2_code;

    // Check if logged in
    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to get country history." << std::endl;
        return;
    }

    refdata::messaging::get_country_history_request req;
    req.alpha2_code = std::move(alpha2_code);

    auto result = do_auth_request<refdata::messaging::get_country_history_response>(
        out, session, "refdata.v1.countries.history", rfl::json::write(req));
    if (!result) return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get country history: "
                                  << result->message;
        out << "✗ " << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this country." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->history.size() << " history records.";
    out << result->history << std::endl;
}

}
