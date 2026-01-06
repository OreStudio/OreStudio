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

using namespace ores::telemetry::log;

risk_message_handler::risk_message_handler(database::context ctx,
    std::shared_ptr<variability::service::system_flags_service> system_flags)
    : ctx_(std::move(ctx))
    , system_flags_(std::move(system_flags))
    , currency_service_(ctx_) {}

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

    auto currencies = currency_service_.list_currencies(request.offset, request.limit);
    auto total_count = currency_service_.count_currencies();

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

        if (currency_service_.delete_currency(iso_code)) {
            result.success = true;
            result.message = "Currency deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted currency: " << iso_code;
        } else {
            result.success = false;
            result.message = "Failed to delete currency";
            BOOST_LOG_SEV(lg(), error) << "Error deleting currency " << iso_code;
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

}
