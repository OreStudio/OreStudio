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
#include "ores.utility/rfl/reflectors.hpp" // Must be before rfl/json.hpp
#include "ores.trading/messaging/trade_message_handler.hpp"

#include <boost/uuid/uuid_io.hpp>
#include "ores.comms/messaging/save_result.hpp"
#include "ores.trading/messaging/trade_type_protocol.hpp"
#include "ores.trading/messaging/lifecycle_event_protocol.hpp"
#include "ores.trading/messaging/party_role_type_protocol.hpp"
#include "ores.trading/messaging/trade_id_type_protocol.hpp"
#include "ores.trading/messaging/trade_protocol.hpp"
#include "ores.trading/messaging/trade_identifier_protocol.hpp"
#include "ores.trading/messaging/trade_party_role_protocol.hpp"
#include "ores.trading/service/trade_type_service.hpp"
#include "ores.trading/service/lifecycle_event_service.hpp"
#include "ores.trading/service/party_role_type_service.hpp"
#include "ores.trading/service/trade_id_type_service.hpp"
#include "ores.trading/service/trade_service.hpp"
#include "ores.trading/service/trade_identifier_service.hpp"
#include "ores.trading/service/trade_party_role_service.hpp"

namespace ores::trading::messaging {

using namespace ores::logging;
using comms::messaging::message_type;

trade_message_handler::trade_message_handler(database::context ctx,
    std::shared_ptr<comms::service::auth_session_service> sessions)
    : tenant_aware_handler(std::move(ctx), std::move(sessions)) {}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_message(message_type type,
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling trade message type " << type;

    switch (type) {
    // Trade type handlers
    case message_type::get_trade_types_request:
        co_return co_await handle_get_trade_types_request(payload, remote_address);
    case message_type::save_trade_type_request:
        co_return co_await handle_save_trade_type_request(payload, remote_address);
    case message_type::delete_trade_type_request:
        co_return co_await handle_delete_trade_type_request(payload, remote_address);
    case message_type::get_trade_type_history_request:
        co_return co_await handle_get_trade_type_history_request(payload, remote_address);
    // Lifecycle event handlers
    case message_type::get_lifecycle_events_request:
        co_return co_await handle_get_lifecycle_events_request(payload, remote_address);
    case message_type::save_lifecycle_event_request:
        co_return co_await handle_save_lifecycle_event_request(payload, remote_address);
    case message_type::delete_lifecycle_event_request:
        co_return co_await handle_delete_lifecycle_event_request(payload, remote_address);
    case message_type::get_lifecycle_event_history_request:
        co_return co_await handle_get_lifecycle_event_history_request(payload, remote_address);
    // Party role type handlers
    case message_type::get_party_role_types_request:
        co_return co_await handle_get_party_role_types_request(payload, remote_address);
    case message_type::save_party_role_type_request:
        co_return co_await handle_save_party_role_type_request(payload, remote_address);
    case message_type::delete_party_role_type_request:
        co_return co_await handle_delete_party_role_type_request(payload, remote_address);
    case message_type::get_party_role_type_history_request:
        co_return co_await handle_get_party_role_type_history_request(payload, remote_address);
    // Trade ID type handlers
    case message_type::get_trade_id_types_request:
        co_return co_await handle_get_trade_id_types_request(payload, remote_address);
    case message_type::save_trade_id_type_request:
        co_return co_await handle_save_trade_id_type_request(payload, remote_address);
    case message_type::delete_trade_id_type_request:
        co_return co_await handle_delete_trade_id_type_request(payload, remote_address);
    case message_type::get_trade_id_type_history_request:
        co_return co_await handle_get_trade_id_type_history_request(payload, remote_address);
    // Trade handlers
    case message_type::get_trades_request:
        co_return co_await handle_get_trades_request(payload, remote_address);
    case message_type::save_trade_request:
        co_return co_await handle_save_trade_request(payload, remote_address);
    case message_type::delete_trade_request:
        co_return co_await handle_delete_trade_request(payload, remote_address);
    case message_type::get_trade_history_request:
        co_return co_await handle_get_trade_history_request(payload, remote_address);
    // Trade identifier handlers
    case message_type::get_trade_identifiers_request:
        co_return co_await handle_get_trade_identifiers_request(payload, remote_address);
    case message_type::save_trade_identifier_request:
        co_return co_await handle_save_trade_identifier_request(payload, remote_address);
    case message_type::delete_trade_identifier_request:
        co_return co_await handle_delete_trade_identifier_request(payload, remote_address);
    case message_type::get_trade_identifier_history_request:
        co_return co_await handle_get_trade_identifier_history_request(payload, remote_address);
    // Trade party role handlers
    case message_type::get_trade_party_roles_request:
        co_return co_await handle_get_trade_party_roles_request(payload, remote_address);
    case message_type::save_trade_party_role_request:
        co_return co_await handle_save_trade_party_role_request(payload, remote_address);
    case message_type::delete_trade_party_role_request:
        co_return co_await handle_delete_trade_party_role_request(payload, remote_address);
    case message_type::get_trade_party_role_history_request:
        co_return co_await handle_get_trade_party_role_history_request(payload, remote_address);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown trade message type " << std::hex
                                   << static_cast<std::uint16_t>(type);
        co_return std::unexpected(ores::utility::serialization::error_code::invalid_message_type);
    }
}

// ============================================================================
// Trade Type Handlers
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_get_trade_types_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_trade_types_request.";

    auto auth = require_authentication(remote_address, "Get trade types");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_type_service svc(ctx);

    auto request_result = get_trade_types_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_trade_types_request";
        co_return std::unexpected(request_result.error());
    }

    auto types = svc.list_types();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << types.size() << " trade types";

    get_trade_types_response response{ .types = std::move(types) };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_save_trade_type_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_trade_type_request.";

    auto auth = require_authentication(remote_address, "Save trade type");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_type_service svc(ctx);

    auto request_result = save_trade_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_trade_type_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.types.size() << " trade type(s)";
    for (auto& t : request.types) {
        t.modified_by = auth->username;
        t.performed_by.clear();
    }

    save_trade_type_response response;
    try {
        svc.save_types(request.types);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved " << request.types.size()
                                  << " trade type(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save trade types: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving trade types: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_delete_trade_type_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_trade_type_request.";

    auto auth = require_authentication(remote_address, "Delete trade type");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_type_service svc(ctx);

    auto request_result = delete_trade_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_trade_type_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    delete_trade_type_response response;
    try {
        for (const auto& code : request.codes) {
            svc.remove_type(code);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.codes.size() << " trade type(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete trade type: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_get_trade_type_history_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_trade_type_history_request.";

    auto auth = require_authentication(remote_address, "Get trade type history");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_type_service svc(ctx);

    auto request_result = get_trade_type_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_trade_type_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    get_trade_type_history_response response;
    try {
        auto history = svc.get_type_history(request.code);
        if (history.empty()) {
            response.success = false;
            response.message = "Trade type not found: " + request.code;
        } else {
            response.success = true;
            response.message = "History retrieved successfully";
            response.versions = std::move(history);
        }
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving trade type history: " << e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Lifecycle Event Handlers
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_get_lifecycle_events_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_lifecycle_events_request.";

    auto auth = require_authentication(remote_address, "Get lifecycle events");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::lifecycle_event_service svc(ctx);

    auto request_result = get_lifecycle_events_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_lifecycle_events_request";
        co_return std::unexpected(request_result.error());
    }

    auto events = svc.list_events();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << events.size() << " lifecycle events";

    get_lifecycle_events_response response{ .events = std::move(events) };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_save_lifecycle_event_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_lifecycle_event_request.";

    auto auth = require_authentication(remote_address, "Save lifecycle event");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::lifecycle_event_service svc(ctx);

    auto request_result = save_lifecycle_event_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_lifecycle_event_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.events.size() << " lifecycle event(s)";
    for (auto& e : request.events) {
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_lifecycle_event_response response;
    try {
        svc.save_events(request.events);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved " << request.events.size()
                                  << " lifecycle event(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save lifecycle events: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving lifecycle events: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_delete_lifecycle_event_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_lifecycle_event_request.";

    auto auth = require_authentication(remote_address, "Delete lifecycle event");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::lifecycle_event_service svc(ctx);

    auto request_result = delete_lifecycle_event_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_lifecycle_event_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    delete_lifecycle_event_response response;
    try {
        for (const auto& code : request.codes) {
            svc.remove_event(code);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.codes.size() << " lifecycle event(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete lifecycle event: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_get_lifecycle_event_history_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_lifecycle_event_history_request.";

    auto auth = require_authentication(remote_address, "Get lifecycle event history");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::lifecycle_event_service svc(ctx);

    auto request_result = get_lifecycle_event_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_lifecycle_event_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    get_lifecycle_event_history_response response;
    try {
        auto history = svc.get_event_history(request.code);
        if (history.empty()) {
            response.success = false;
            response.message = "Lifecycle event not found: " + request.code;
        } else {
            response.success = true;
            response.message = "History retrieved successfully";
            response.versions = std::move(history);
        }
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving lifecycle event history: " << e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Party Role Type Handlers
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_get_party_role_types_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_party_role_types_request.";

    auto auth = require_authentication(remote_address, "Get party role types");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::party_role_type_service svc(ctx);

    auto request_result = get_party_role_types_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_party_role_types_request";
        co_return std::unexpected(request_result.error());
    }

    auto role_types = svc.list_role_types();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << role_types.size() << " party role types";

    get_party_role_types_response response{ .role_types = std::move(role_types) };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_save_party_role_type_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_party_role_type_request.";

    auto auth = require_authentication(remote_address, "Save party role type");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::party_role_type_service svc(ctx);

    auto request_result = save_party_role_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_party_role_type_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.role_types.size() << " party role type(s)";
    for (auto& rt : request.role_types) {
        rt.modified_by = auth->username;
        rt.performed_by.clear();
    }

    save_party_role_type_response response;
    try {
        svc.save_role_types(request.role_types);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved " << request.role_types.size()
                                  << " party role type(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save party role types: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving party role types: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_delete_party_role_type_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_party_role_type_request.";

    auto auth = require_authentication(remote_address, "Delete party role type");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::party_role_type_service svc(ctx);

    auto request_result = delete_party_role_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_party_role_type_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    delete_party_role_type_response response;
    try {
        for (const auto& code : request.codes) {
            svc.remove_role_type(code);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.codes.size() << " party role type(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete party role type: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_get_party_role_type_history_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_party_role_type_history_request.";

    auto auth = require_authentication(remote_address, "Get party role type history");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::party_role_type_service svc(ctx);

    auto request_result = get_party_role_type_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_party_role_type_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    get_party_role_type_history_response response;
    try {
        auto history = svc.get_role_type_history(request.code);
        if (history.empty()) {
            response.success = false;
            response.message = "Party role type not found: " + request.code;
        } else {
            response.success = true;
            response.message = "History retrieved successfully";
            response.versions = std::move(history);
        }
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving party role type history: " << e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Trade ID Type Handlers
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_get_trade_id_types_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_trade_id_types_request.";

    auto auth = require_authentication(remote_address, "Get trade ID types");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_id_type_service svc(ctx);

    auto request_result = get_trade_id_types_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_trade_id_types_request";
        co_return std::unexpected(request_result.error());
    }

    auto id_types = svc.list_id_types();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << id_types.size() << " trade ID types";

    get_trade_id_types_response response{ .id_types = std::move(id_types) };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_save_trade_id_type_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_trade_id_type_request.";

    auto auth = require_authentication(remote_address, "Save trade ID type");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_id_type_service svc(ctx);

    auto request_result = save_trade_id_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_trade_id_type_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.id_types.size() << " trade ID type(s)";
    for (auto& t : request.id_types) {
        t.modified_by = auth->username;
        t.performed_by.clear();
    }

    save_trade_id_type_response response;
    try {
        svc.save_id_types(request.id_types);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved " << request.id_types.size()
                                  << " trade ID type(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save trade ID types: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving trade ID types: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_delete_trade_id_type_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_trade_id_type_request.";

    auto auth = require_authentication(remote_address, "Delete trade ID type");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_id_type_service svc(ctx);

    auto request_result = delete_trade_id_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_trade_id_type_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    delete_trade_id_type_response response;
    try {
        for (const auto& code : request.codes) {
            svc.remove_id_type(code);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.codes.size() << " trade id type(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete trade id type: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_get_trade_id_type_history_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_trade_id_type_history_request.";

    auto auth = require_authentication(remote_address, "Get trade ID type history");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_id_type_service svc(ctx);

    auto request_result = get_trade_id_type_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_trade_id_type_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    get_trade_id_type_history_response response;
    try {
        auto history = svc.get_id_type_history(request.code);
        if (history.empty()) {
            response.success = false;
            response.message = "Trade ID type not found: " + request.code;
        } else {
            response.success = true;
            response.message = "History retrieved successfully";
            response.versions = std::move(history);
        }
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving trade ID type history: " << e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Trade Handlers
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_get_trades_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_trades_request.";

    auto auth = require_authentication(remote_address, "Get trades");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_service svc(ctx);

    auto request_result = get_trades_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_trades_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;

    std::vector<domain::trade> trades;
    std::uint32_t total;
    if (request.book_id.has_value() || request.portfolio_id.has_value() ||
            request.business_unit_id.has_value()) {
        trades = svc.list_trades_filtered(
            request.offset, request.limit,
            request.book_id, request.portfolio_id, request.business_unit_id);
        total = svc.count_trades_filtered(
            request.book_id, request.portfolio_id, request.business_unit_id);
    } else {
        trades = svc.list_trades(request.offset, request.limit);
        total = svc.count_trades();
    }

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << trades.size()
                              << " trades, total available: " << total;

    get_trades_response response{
        .trades = std::move(trades),
        .total_available_count = total
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_save_trade_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_trade_request.";

    auto auth = require_authentication(remote_address, "Save trade");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_service svc(ctx);

    auto request_result = save_trade_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_trade_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.trades.size() << " trade(s)";
    for (auto& t : request.trades) {
        t.modified_by = auth->username;
        t.performed_by.clear();
    }

    save_trade_response response;
    try {
        svc.save_trades(request.trades);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved " << request.trades.size()
                                  << " trade(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save trades: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving trades: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_delete_trade_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_trade_request.";

    auto auth = require_authentication(remote_address, "Delete trade");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_service svc(ctx);

    auto request_result = delete_trade_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_trade_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    delete_trade_response response;

    try {
        for (const auto& id : request.ids) {
            svc.remove_trade(boost::uuids::to_string(id));
            BOOST_LOG_SEV(lg(), info) << "Deleted trade: " << id;
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
                                  << request.ids.size() << " trade(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete trade: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_get_trade_history_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_trade_history_request.";

    auto auth = require_authentication(remote_address, "Get trade history");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_service svc(ctx);

    auto request_result = get_trade_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_trade_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    get_trade_history_response response;
    try {
        auto history = svc.get_trade_history(boost::uuids::to_string(request.id));
        if (history.empty()) {
            response.success = false;
            response.message = "Trade not found: " + boost::uuids::to_string(request.id);
        } else {
            response.success = true;
            response.message = "History retrieved successfully";
            response.versions = std::move(history);
        }
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving trade history: " << e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Trade Identifier Handlers
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_get_trade_identifiers_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_trade_identifiers_request.";

    auto auth = require_authentication(remote_address, "Get trade identifiers");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_identifier_service svc(ctx);

    auto request_result = get_trade_identifiers_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_trade_identifiers_request";
        co_return std::unexpected(request_result.error());
    }

    auto identifiers = svc.list_identifiers();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << identifiers.size() << " trade identifiers";

    get_trade_identifiers_response response{ .identifiers = std::move(identifiers) };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_save_trade_identifier_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_trade_identifier_request.";

    auto auth = require_authentication(remote_address, "Save trade identifier");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_identifier_service svc(ctx);

    auto request_result = save_trade_identifier_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_trade_identifier_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.identifiers.size() << " trade identifier(s)";
    for (auto& i : request.identifiers) {
        i.modified_by = auth->username;
        i.performed_by.clear();
    }

    save_trade_identifier_response response;
    try {
        svc.save_identifiers(request.identifiers);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved " << request.identifiers.size()
                                  << " trade identifier(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save trade identifiers: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving trade identifiers: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_delete_trade_identifier_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_trade_identifier_request.";

    auto auth = require_authentication(remote_address, "Delete trade identifier");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_identifier_service svc(ctx);

    auto request_result = delete_trade_identifier_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_trade_identifier_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    delete_trade_identifier_response response;

    try {
        for (const auto& id : request.ids) {
            svc.remove_identifier(boost::uuids::to_string(id));
            BOOST_LOG_SEV(lg(), info) << "Deleted trade identifier: " << id;
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
                                  << request.ids.size() << " trade identifier(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete trade identifier: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_get_trade_identifier_history_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_trade_identifier_history_request.";

    auto auth = require_authentication(remote_address, "Get trade identifier history");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_identifier_service svc(ctx);

    auto request_result = get_trade_identifier_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_trade_identifier_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    get_trade_identifier_history_response response;
    try {
        auto history = svc.get_identifier_history(boost::uuids::to_string(request.id));
        if (history.empty()) {
            response.success = false;
            response.message = "Trade identifier not found: " + boost::uuids::to_string(request.id);
        } else {
            response.success = true;
            response.message = "History retrieved successfully";
            response.versions = std::move(history);
        }
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving trade identifier history: " << e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Trade Party Role Handlers
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_get_trade_party_roles_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_trade_party_roles_request.";

    auto auth = require_authentication(remote_address, "Get trade party roles");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_party_role_service svc(ctx);

    auto request_result = get_trade_party_roles_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_trade_party_roles_request";
        co_return std::unexpected(request_result.error());
    }

    auto roles = svc.list_roles();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << roles.size() << " trade party roles";

    get_trade_party_roles_response response{ .roles = std::move(roles) };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_save_trade_party_role_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_trade_party_role_request.";

    auto auth = require_authentication(remote_address, "Save trade party role");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_party_role_service svc(ctx);

    auto request_result = save_trade_party_role_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_trade_party_role_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.roles.size() << " trade party role(s)";
    for (auto& r : request.roles) {
        r.modified_by = auth->username;
        r.performed_by.clear();
    }

    save_trade_party_role_response response;
    try {
        svc.save_roles(request.roles);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved " << request.roles.size()
                                  << " trade party role(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save trade party roles: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving trade party roles: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_delete_trade_party_role_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_trade_party_role_request.";

    auto auth = require_authentication(remote_address, "Delete trade party role");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_party_role_service svc(ctx);

    auto request_result = delete_trade_party_role_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_trade_party_role_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    delete_trade_party_role_response response;

    try {
        for (const auto& id : request.ids) {
            svc.remove_role(boost::uuids::to_string(id));
            BOOST_LOG_SEV(lg(), info) << "Deleted trade party role: " << id;
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
                                  << request.ids.size() << " trade party role(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete trade party role: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
trade_message_handler::handle_get_trade_party_role_history_request(
    std::span<const std::byte> payload, const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_trade_party_role_history_request.";

    auto auth = require_authentication(remote_address, "Get trade party role history");
    if (!auth) co_return std::unexpected(auth.error());

    auto ctx = make_request_context(*auth);
    service::trade_party_role_service svc(ctx);

    auto request_result = get_trade_party_role_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_trade_party_role_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    get_trade_party_role_history_response response;
    try {
        auto history = svc.get_role_history(boost::uuids::to_string(request.id));
        if (history.empty()) {
            response.success = false;
            response.message = "Trade party role not found: " + boost::uuids::to_string(request.id);
        } else {
            response.success = true;
            response.message = "History retrieved successfully";
            response.versions = std::move(history);
        }
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving trade party role history: " << e.what();
    }

    co_return response.serialize();
}

}
