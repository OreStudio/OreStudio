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
#include "ores.risk/messaging/risk_message_handler.hpp"

#include "ores.risk/messaging/protocol.hpp"

namespace ores::risk::messaging {

using namespace ores::utility::log;

risk_message_handler::risk_message_handler(database::context ctx,
    std::shared_ptr<variability::service::system_flags_service> system_flags)
    : ctx_(ctx), system_flags_(std::move(system_flags)) {}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     comms::messaging::error_code>>
risk_message_handler::handle_message(comms::messaging::message_type type,
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling risk message type " << type;

    // Block all risk operations when in bootstrap mode
    if (system_flags_->is_bootstrap_mode_enabled()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Blocked risk operation " << type << " - system in bootstrap mode";
        co_return std::unexpected(comms::messaging::error_code::bootstrap_mode_only);
    }

    switch (type) {
    case comms::messaging::message_type::get_currencies_request:
        co_return co_await handle_get_currencies_request(payload);
    case comms::messaging::message_type::save_currency_request:
        co_return co_await handle_save_currency_request(payload);
    case comms::messaging::message_type::delete_currency_request:
        co_return co_await handle_delete_currency_request(payload);
    case comms::messaging::message_type::get_currency_history_request:
        co_return co_await handle_get_currency_history_request(payload);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown risk message type " << std::hex
                                   << static_cast<std::uint16_t>(type);
        co_return std::unexpected(comms::messaging::error_code::invalid_message_type);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     comms::messaging::error_code>>
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
        // Write currency to repository (bitemporal - creates new version)
        currency_repo_.write(ctx_, request.currency);
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
                                     comms::messaging::error_code>>
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
        co_return std::unexpected(comms::messaging::error_code::invalid_request);
    }

    BOOST_LOG_SEV(lg(), debug) << "Fetching currencies with offset: "
                               << request.offset << ", limit: " << request.limit;

    // Retrieve paginated currencies and total count from repository
    auto currencies = currency_repo_.read_latest(ctx_, request.offset, request.limit);
    auto total_count = currency_repo_.get_total_currency_count(ctx_);

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << currencies.size()
                              << " currencies (total available: " << total_count << ")";

    // Create and serialize response
    get_currencies_response response{
        .currencies = std::move(currencies),
        .total_available_count = total_count
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     comms::messaging::error_code>>
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
            // Remove currency from repository
            currency_repo_.remove(ctx_, iso_code);
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
                                     comms::messaging::error_code>>
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
        // Get all versions of the currency from repository
        auto currencies = currency_repo_.read_all(ctx_, request.iso_code);

        if (currencies.empty()) {
            response.success = false;
            response.message = "Currency not found: " + request.iso_code;
            BOOST_LOG_SEV(lg(), warn) << "No history found for currency: " << request.iso_code;
            co_return response.serialize();
        }

        // Convert currencies to currency_version objects
        domain::currency_version_history history;
        history.iso_code = request.iso_code;

        // Sort by version descending (newest first) - use database version field
        std::sort(currencies.begin(), currencies.end(),
            [](const auto& a, const auto& b) {
                return a.version > b.version;
            });

        for (const auto& currency : currencies) {
            domain::currency_version version;
            version.data = currency;
            version.version_number = currency.version;  // Use database version field
            version.recorded_by = currency.recorded_by;
            version.recorded_at = currency.recorded_at;
            version.change_summary = "Version " + std::to_string(version.version_number);

            BOOST_LOG_SEV(lg(), trace) << "Adding version: iso_code=" << currency.iso_code
                                       << ", db_version=" << currency.version
                                       << ", version_number=" << version.version_number;

            history.versions.push_back(std::move(version));
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.history = std::move(history);

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

}
