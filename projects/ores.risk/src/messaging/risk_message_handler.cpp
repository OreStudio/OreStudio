/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.utility/rfl/reflectors.hpp" // Must be before rfl/json.hpp
#include "ores.risk/messaging/risk_message_handler.hpp"

#include <sstream>
#include <rfl/json.hpp>
#include "ores.risk/messaging/protocol.hpp"

namespace ores::risk::messaging {

using namespace ores::telemetry::log;

risk_message_handler::risk_message_handler(database::context ctx,
    std::shared_ptr<variability::service::system_flags_service> system_flags)
    : ctx_(std::move(ctx))
    , system_flags_(std::move(system_flags))
    , currency_service_(ctx_)
    , country_service_(ctx_) {}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
risk_message_handler::handle_message(comms::messaging::message_type type,
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling risk message type " << type;

    // Block all risk operations when in bootstrap mode
    if (system_flags_->is_bootstrap_mode_enabled()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Blocked risk operation " << type << " - system in bootstrap mode";
        co_return std::unexpected(ores::utility::serialization::error_code::bootstrap_mode_only);
    }

    switch (type) {
    // Currency handlers
    case comms::messaging::message_type::get_currencies_request:
        co_return co_await handle_get_currencies_request(payload);
    case comms::messaging::message_type::save_currency_request:
        co_return co_await handle_save_currency_request(payload);
    case comms::messaging::message_type::delete_currency_request:
        co_return co_await handle_delete_currency_request(payload);
    case comms::messaging::message_type::get_currency_history_request:
        co_return co_await handle_get_currency_history_request(payload);
    // Country handlers
    case comms::messaging::message_type::get_countries_request:
        co_return co_await handle_get_countries_request(payload);
    case comms::messaging::message_type::save_country_request:
        co_return co_await handle_save_country_request(payload);
    case comms::messaging::message_type::delete_country_request:
        co_return co_await handle_delete_country_request(payload);
    case comms::messaging::message_type::get_country_history_request:
        co_return co_await handle_get_country_history_request(payload);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown risk message type " << std::hex
                                   << static_cast<std::uint16_t>(type);
        co_return std::unexpected(ores::utility::serialization::error_code::invalid_message_type);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
risk_message_handler::
handle_save_currency_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_currency_request.";

    // Deserialize request
    auto request_result = save_currency_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_currency_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Saving currency: " << request.currency.iso_code;

    save_currency_response response;
    try {
        currency_service_.save_currency(request.currency);
        response.success = true;
        response.message = "Currency saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved currency: "
                                  << request.currency.iso_code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save currency: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving currency "
                                   << request.currency.iso_code << ": " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
risk_message_handler::
handle_get_currencies_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_currencies_request.";

    // Deserialize request
    auto request_result = get_currencies_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_currencies_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;

    // Validate pagination parameters
    constexpr std::uint32_t max_limit = 1000;
    if (request.limit == 0 || request.limit > max_limit) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid limit: " << request.limit
                                  << ". Must be between 1 and " << max_limit;
        co_return std::unexpected(ores::utility::serialization::error_code::limit_exceeded);
    }

    BOOST_LOG_SEV(lg(), debug) << "Fetching currencies with offset: "
                               << request.offset << ", limit: " << request.limit;

    auto currencies = currency_service_.list_currencies(request.offset, request.limit);
    auto total_count = currency_service_.count_currencies();

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << currencies.size()
                              << " currencies (total available: " << total_count << ")";

    // TEMP: Log first currency as JSON for debugging
    if (!currencies.empty()) {
        std::ostringstream oss;
        oss << "First currency JSON: " << rfl::json::write(currencies[0]);
        BOOST_LOG_SEV(lg(), debug) << oss.str();
    }

    // Create and serialize response
    get_currencies_response response{
        .currencies = std::move(currencies),
        .total_available_count = total_count
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
risk_message_handler::
handle_delete_currency_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_currency_request.";

    // Deserialize request
    auto request_result = delete_currency_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_currency_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.iso_codes.size()
                              << " currency/currencies";

    delete_currency_response response;

    // Process each currency in the batch
    for (const auto& iso_code : request.iso_codes) {
        delete_currency_result result;
        result.iso_code = iso_code;

        try {
            currency_service_.delete_currency(iso_code);
            result.success = true;
            result.message = "Currency deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted currency: " << iso_code;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete currency: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting currency "
                                       << iso_code << ": " << e.what();
        }

        response.results.push_back(std::move(result));
    }

    BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
                              << response.results.size() << " results";

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
risk_message_handler::
handle_get_currency_history_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_currency_history_request.";

    // Deserialize request
    auto request_result = get_currency_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_currency_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for currency: " << request.iso_code;

    get_currency_history_response response;
    try {
        auto history_opt = currency_service_.get_currency_version_history(request.iso_code);

        if (!history_opt) {
            response.success = false;
            response.message = "Currency not found: " + request.iso_code;
            BOOST_LOG_SEV(lg(), warn) << "No history found for currency: " << request.iso_code;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.history = std::move(*history_opt);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.history.versions.size()
                                  << " versions for currency: " << request.iso_code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for "
                                   << request.iso_code << ": " << e.what();
    }

    co_return response.serialize();
}

// Country handlers

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
risk_message_handler::
handle_get_countries_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_countries_request.";

    // Deserialize request
    auto request_result = get_countries_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_countries_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;

    // Validate pagination parameters
    constexpr std::uint32_t max_limit = 1000;
    if (request.limit == 0 || request.limit > max_limit) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid limit: " << request.limit
                                  << ". Must be between 1 and " << max_limit;
        co_return std::unexpected(ores::utility::serialization::error_code::limit_exceeded);
    }

    BOOST_LOG_SEV(lg(), debug) << "Fetching countries with offset: "
                               << request.offset << ", limit: " << request.limit;

    auto countries = country_service_.list_countries(request.offset, request.limit);
    auto total_count = country_service_.count_countries();

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << countries.size()
                              << " countries (total available: " << total_count << ")";

    // Create and serialize response
    get_countries_response response{
        .countries = std::move(countries),
        .total_available_count = total_count
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
risk_message_handler::
handle_save_country_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_country_request.";

    // Deserialize request
    auto request_result = save_country_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_country_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Saving country: " << request.country.alpha2_code;

    save_country_response response;
    try {
        country_service_.save_country(request.country);
        response.success = true;
        response.message = "Country saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved country: "
                                  << request.country.alpha2_code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save country: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving country "
                                   << request.country.alpha2_code << ": " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
risk_message_handler::
handle_delete_country_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_country_request.";

    // Deserialize request
    auto request_result = delete_country_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_country_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.alpha2_codes.size()
                              << " country/countries";

    delete_country_response response;

    // Process each country in the batch
    for (const auto& alpha2_code : request.alpha2_codes) {
        delete_country_result result;
        result.alpha2_code = alpha2_code;

        try {
            country_service_.delete_country(alpha2_code);
            result.success = true;
            result.message = "Country deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted country: " << alpha2_code;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete country: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting country "
                                       << alpha2_code << ": " << e.what();
        }

        response.results.push_back(std::move(result));
    }

    BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
                              << response.results.size() << " results";

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
risk_message_handler::
handle_get_country_history_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_country_history_request.";

    // Deserialize request
    auto request_result = get_country_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_country_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for country: " << request.alpha2_code;

    get_country_history_response response;
    try {
        auto history = country_service_.get_country_history(request.alpha2_code);

        if (history.empty()) {
            response.success = false;
            response.message = "Country not found: " + request.alpha2_code;
            BOOST_LOG_SEV(lg(), warn) << "No history found for country: " << request.alpha2_code;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.history = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.history.size()
                                  << " versions for country: " << request.alpha2_code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for "
                                   << request.alpha2_code << ": " << e.what();
    }

    co_return response.serialize();
}

}

