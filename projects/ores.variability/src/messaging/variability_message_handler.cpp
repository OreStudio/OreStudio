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

#include <algorithm>

#include "ores.variability/messaging/feature_flags_protocol.hpp"
#include "ores.variability/repository/feature_flags_repository.hpp"

namespace ores::variability::messaging {

using namespace ores::logging;

variability_message_handler::variability_message_handler(database::context ctx,
    std::shared_ptr<comms::service::auth_session_service> sessions)
    : tenant_aware_handler(std::move(ctx), std::move(sessions)) {}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
variability_message_handler::handle_message(comms::messaging::message_type type,
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling variability message type " << type;

    switch (type) {
    case comms::messaging::message_type::get_feature_flags_request:
        co_return co_await handle_get_feature_flags_request(payload, remote_address);
    case comms::messaging::message_type::save_feature_flag_request:
        co_return co_await handle_save_feature_flag_request(payload, remote_address);
    case comms::messaging::message_type::delete_feature_flag_request:
        co_return co_await handle_delete_feature_flag_request(payload, remote_address);
    case comms::messaging::message_type::get_feature_flag_history_request:
        co_return co_await handle_get_feature_flag_history_request(payload, remote_address);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown variability message type " << std::hex
                                   << static_cast<std::uint16_t>(type);
        co_return std::unexpected(ores::utility::serialization::error_code::invalid_message_type);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
variability_message_handler::
handle_get_feature_flags_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_feature_flags_request.";

    // Require authentication
    auto auth = require_authentication(remote_address, "Get feature flags");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth);

    // Create per-request repository
    repository::feature_flags_repository repo;

    // Deserialize request
    auto request_result = get_feature_flags_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_feature_flags_request";
        co_return std::unexpected(request_result.error());
    }

    get_feature_flags_response response;
    try {
        // Read latest feature flags from repository
        response.feature_flags = repo.read_latest(ctx);

        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.feature_flags.size()
                                  << " feature flags";

        // Serialize response
        co_return response.serialize();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Database error reading feature flags: " << e.what();
        co_return std::unexpected(ores::utility::serialization::error_code::database_error);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
variability_message_handler::
handle_save_feature_flag_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_feature_flag_request.";

    // Require authentication
    auto auth = require_authentication(remote_address, "Save feature flag");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth);

    // Create per-request repository
    repository::feature_flags_repository repo;

    // Deserialize request
    auto request_result = save_feature_flag_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_feature_flag_request";
        co_return std::unexpected(request_result.error());
    }

    save_feature_flag_response response;
    try {
        // Override tenant_id from server-side context (don't trust client)
        auto flag = request_result->flag;
        if (ctx.tenant_id().has_value()) {
            flag.tenant_id = ctx.tenant_id()->to_string();
        }
        flag.recorded_by = auth->username;

        // Save the feature flag
        repo.write(ctx, flag);

        BOOST_LOG_SEV(lg(), info) << "Saved feature flag: " << flag.name
                                  << " by " << auth->username;

        response.success = true;
        co_return response.serialize();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Database error saving feature flag: " << e.what();
        response.success = false;
        response.error_message = e.what();
        co_return response.serialize();
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
variability_message_handler::
handle_delete_feature_flag_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_feature_flag_request.";

    // Require authentication
    auto auth = require_authentication(remote_address, "Delete feature flag");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth);

    // Create per-request repository
    repository::feature_flags_repository repo;

    // Deserialize request
    auto request_result = delete_feature_flag_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_feature_flag_request";
        co_return std::unexpected(request_result.error());
    }

    delete_feature_flag_response response;
    try {
        // Delete the feature flag
        repo.remove(ctx, request_result->name);

        BOOST_LOG_SEV(lg(), info) << "Deleted feature flag: " << request_result->name;

        response.success = true;
        co_return response.serialize();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Database error deleting feature flag: " << e.what();
        response.success = false;
        response.error_message = e.what();
        co_return response.serialize();
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
variability_message_handler::
handle_get_feature_flag_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_feature_flag_history_request.";

    // Require authentication
    auto auth = require_authentication(remote_address, "Get feature flag history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth);

    // Create per-request repository
    repository::feature_flags_repository repo;

    // Deserialize request
    auto request_result = get_feature_flag_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_feature_flag_history_request";
        co_return std::unexpected(request_result.error());
    }

    get_feature_flag_history_response response;
    try {
        // Read all versions of this feature flag
        auto history = repo.read_all(ctx, request_result->name);

        // Sort by version descending (newest first)
        std::ranges::sort(history, [](const auto& a, const auto& b) {
            return a.version > b.version;
        });

        BOOST_LOG_SEV(lg(), info) << "Retrieved " << history.size()
                                  << " history records for feature flag: "
                                  << request_result->name;

        response.success = true;
        response.history = std::move(history);
        co_return response.serialize();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Database error reading feature flag history: " << e.what();
        response.success = false;
        response.message = e.what();
        co_return response.serialize();
    }
}

}
