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
#include "ores.variability/messaging/variability_message_handler.hpp"

#include "ores.variability/messaging/feature_flags_protocol.hpp"

namespace ores::variability::messaging {

using namespace ores::utility::log;

variability_message_handler::variability_message_handler(utility::database::context ctx)
    : feature_flags_repo_(std::move(ctx)) {}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     comms::messaging::error_code>>
variability_message_handler::handle_message(comms::messaging::message_type type,
    std::span<const std::byte> payload, [[maybe_unused]] const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling variability message type " << type;

    switch (type) {
    case comms::messaging::message_type::list_feature_flags_request:
        co_return co_await handle_list_feature_flags_request(payload);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown variability message type " << std::hex
                                   << static_cast<std::uint16_t>(type);
        co_return std::unexpected(comms::messaging::error_code::invalid_message_type);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     comms::messaging::error_code>>
variability_message_handler::
handle_list_feature_flags_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing list_feature_flags_request.";

    // Deserialize request
    auto request_result = list_feature_flags_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize list_feature_flags_request";
        co_return std::unexpected(request_result.error());
    }

    list_feature_flags_response response;
    try {
        // Read latest feature flags from repository
        response.feature_flags = feature_flags_repo_.read_latest();

        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.feature_flags.size()
                                  << " feature flags";

        // Serialize response
        co_return response.serialize();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Database error reading feature flags: " << e.what();
        co_return std::unexpected(comms::messaging::error_code::database_error);
    }
}

}
