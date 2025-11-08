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

risk_message_handler::risk_message_handler(utility::repository::context ctx)
    : ctx_(std::move(ctx)) {}

boost::asio::awaitable<std::expected<std::vector<std::uint8_t>,
                                     comms::protocol::error_code>>
risk_message_handler::handle_message(comms::protocol::message_type type,
    std::span<const std::uint8_t> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling risk message type "
                               << std::hex << static_cast<std::uint16_t>(type);

    switch (type) {
    case comms::protocol::message_type::get_currencies_request:
        co_return co_await handle_get_currencies_request(payload);
    case comms::protocol::message_type::update_currency_request:
        co_return co_await handle_update_currency_request(payload);
    case comms::protocol::message_type::delete_currency_request:
        co_return co_await handle_delete_currency_request(payload);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown risk message type " << std::hex
                                   << static_cast<std::uint16_t>(type);
        co_return std::unexpected(comms::protocol::error_code::invalid_message_type);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::uint8_t>,
                                     comms::protocol::error_code>>
risk_message_handler::
handle_get_currencies_request(std::span<const std::uint8_t> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_currencies_request.";

    // Deserialize request
    auto request_result = get_currencies_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_currencies_request";
        co_return std::unexpected(request_result.error());
    }

    // Retrieve currencies from repository
    auto currencies = currency_repo_.read_latest(ctx_);
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << currencies.size()
                              << " currencies";

    // Create and serialize response
    get_currencies_response response{std::move(currencies)};
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::uint8_t>,
                                     comms::protocol::error_code>>
risk_message_handler::
handle_update_currency_request(std::span<const std::uint8_t> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing update_currency_request.";

    // Deserialize request
    auto request_result = update_currency_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize update_currency_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Updating currency: " << request.currency.iso_code;

    update_currency_response response;
    try {
        // Update currency in repository
        currency_repo_.update(ctx_, request.currency);
        response.success = true;
        response.message = "Currency updated successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully updated currency: "
                                  << request.currency.iso_code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to update currency: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error updating currency "
                                   << request.currency.iso_code << ": " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::uint8_t>,
                                     comms::protocol::error_code>>
risk_message_handler::
handle_delete_currency_request(std::span<const std::uint8_t> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_currency_request.";

    // Deserialize request
    auto request_result = delete_currency_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_currency_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting currency: " << request.iso_code;

    delete_currency_response response;
    try {
        // Remove currency from repository
        currency_repo_.remove(ctx_, request.iso_code);
        response.success = true;
        response.message = "Currency deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted currency: "
                                  << request.iso_code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete currency: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error deleting currency "
                                   << request.iso_code << ": " << e.what();
    }

    co_return response.serialize();
}

}
