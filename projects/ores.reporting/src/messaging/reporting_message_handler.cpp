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
#include "ores.reporting/messaging/reporting_message_handler.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ores.reporting/messaging/report_type_protocol.hpp"
#include "ores.reporting/messaging/concurrency_policy_protocol.hpp"
#include "ores.reporting/messaging/report_definition_protocol.hpp"
#include "ores.reporting/messaging/report_instance_protocol.hpp"
#include "ores.reporting/service/report_type_service.hpp"
#include "ores.reporting/service/concurrency_policy_service.hpp"
#include "ores.reporting/service/report_definition_service.hpp"
#include "ores.reporting/service/report_instance_service.hpp"
#include "ores.scheduler/service/cron_scheduler.hpp"
#include "ores.scheduler/domain/cron_expression.hpp"

namespace ores::reporting::messaging {

using namespace ores::logging;
using comms::messaging::message_type;

reporting_message_handler::reporting_message_handler(
    database::context ctx,
    std::shared_ptr<comms::service::auth_session_service> sessions)
    : tenant_aware_handler(std::move(ctx), std::move(sessions)) {}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_message(message_type type,
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling reporting message type " << type;

    switch (type) {
    // Report types
    case message_type::get_report_types_request:
        co_return co_await handle_get_report_types_request(payload, remote_address);
    case message_type::save_report_type_request:
        co_return co_await handle_save_report_type_request(payload, remote_address);
    case message_type::delete_report_type_request:
        co_return co_await handle_delete_report_type_request(payload, remote_address);
    case message_type::get_report_type_history_request:
        co_return co_await handle_get_report_type_history_request(payload, remote_address);
    // Concurrency policies
    case message_type::get_concurrency_policies_request:
        co_return co_await handle_get_concurrency_policies_request(payload, remote_address);
    case message_type::save_concurrency_policy_request:
        co_return co_await handle_save_concurrency_policy_request(payload, remote_address);
    case message_type::delete_concurrency_policy_request:
        co_return co_await handle_delete_concurrency_policy_request(payload, remote_address);
    case message_type::get_concurrency_policy_history_request:
        co_return co_await handle_get_concurrency_policy_history_request(payload, remote_address);
    // Report definitions
    case message_type::get_report_definitions_request:
        co_return co_await handle_get_report_definitions_request(payload, remote_address);
    case message_type::save_report_definition_request:
        co_return co_await handle_save_report_definition_request(payload, remote_address);
    case message_type::delete_report_definition_request:
        co_return co_await handle_delete_report_definition_request(payload, remote_address);
    case message_type::get_report_definition_history_request:
        co_return co_await handle_get_report_definition_history_request(payload, remote_address);
    case message_type::schedule_report_definitions_request:
        co_return co_await handle_schedule_report_definitions_request(payload, remote_address);
    case message_type::unschedule_report_definitions_request:
        co_return co_await handle_unschedule_report_definitions_request(payload, remote_address);
    // Report instances
    case message_type::get_report_instances_request:
        co_return co_await handle_get_report_instances_request(payload, remote_address);
    case message_type::save_report_instance_request:
        co_return co_await handle_save_report_instance_request(payload, remote_address);
    case message_type::delete_report_instance_request:
        co_return co_await handle_delete_report_instance_request(payload, remote_address);
    case message_type::get_report_instance_history_request:
        co_return co_await handle_get_report_instance_history_request(payload, remote_address);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown reporting message type " << std::hex
                                   << static_cast<std::uint16_t>(type);
        co_return std::unexpected(ores::utility::serialization::error_code::invalid_message_type);
    }
}

// ============================================================================
// Report Types
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_get_report_types_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_report_types_request.";

    auto auth = require_authentication(remote_address, "Get report types");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = get_report_types_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_report_types_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);
    service::report_type_service svc(ctx);

    auto types = svc.list_types();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << types.size() << " report types";

    get_report_types_response response{ .types = std::move(types) };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_save_report_type_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing save_report_type_request.";

    auto auth = require_authentication(remote_address, "Save report type");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = save_report_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_report_type_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    request.type.modified_by = auth->username;
    request.type.performed_by = auth->username;

    auto ctx = make_request_context(*auth);
    service::report_type_service svc(ctx);

    save_report_type_response response;
    try {
        svc.save_type(request.type);
        response.success = true;
        response.message = "Report type saved successfully.";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save report type: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_delete_report_type_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing delete_report_type_request.";

    auto auth = require_authentication(remote_address, "Delete report type");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = delete_report_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_report_type_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);
    service::report_type_service svc(ctx);

    delete_report_type_response response;
    for (const auto& code : request_result->codes) {
        delete_report_type_result r;
        r.code = code;
        try {
            svc.remove_type(code);
            r.success = true;
            r.message = "Deleted successfully.";
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to delete report type " << code
                                       << ": " << e.what();
            r.success = false;
            r.message = e.what();
        }
        response.results.push_back(std::move(r));
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_get_report_type_history_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_report_type_history_request.";

    auto auth = require_authentication(remote_address, "Get report type history");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = get_report_type_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_report_type_history_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);
    service::report_type_service svc(ctx);

    get_report_type_history_response response;
    try {
        response.versions = svc.get_type_history(request_result->code);
        response.success = true;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get report type history: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Concurrency Policies
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_get_concurrency_policies_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_concurrency_policies_request.";

    auto auth = require_authentication(remote_address, "Get concurrency policies");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = get_concurrency_policies_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_concurrency_policies_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);
    service::concurrency_policy_service svc(ctx);

    auto policies = svc.list_policies();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << policies.size() << " concurrency policies";

    get_concurrency_policies_response response{ .policies = std::move(policies) };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_save_concurrency_policy_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing save_concurrency_policy_request.";

    auto auth = require_authentication(remote_address, "Save concurrency policy");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = save_concurrency_policy_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_concurrency_policy_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    request.policy.modified_by = auth->username;
    request.policy.performed_by = auth->username;

    auto ctx = make_request_context(*auth);
    service::concurrency_policy_service svc(ctx);

    save_concurrency_policy_response response;
    try {
        svc.save_policy(request.policy);
        response.success = true;
        response.message = "Concurrency policy saved successfully.";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save concurrency policy: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_delete_concurrency_policy_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing delete_concurrency_policy_request.";

    auto auth = require_authentication(remote_address, "Delete concurrency policy");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = delete_concurrency_policy_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_concurrency_policy_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);
    service::concurrency_policy_service svc(ctx);

    delete_concurrency_policy_response response;
    for (const auto& code : request_result->codes) {
        delete_concurrency_policy_result r;
        r.code = code;
        try {
            svc.remove_policy(code);
            r.success = true;
            r.message = "Deleted successfully.";
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to delete concurrency policy " << code
                                       << ": " << e.what();
            r.success = false;
            r.message = e.what();
        }
        response.results.push_back(std::move(r));
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_get_concurrency_policy_history_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_concurrency_policy_history_request.";

    auto auth = require_authentication(remote_address, "Get concurrency policy history");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = get_concurrency_policy_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_concurrency_policy_history_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);
    service::concurrency_policy_service svc(ctx);

    get_concurrency_policy_history_response response;
    try {
        response.versions = svc.get_policy_history(request_result->code);
        response.success = true;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get concurrency policy history: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Report Definitions
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_get_report_definitions_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_report_definitions_request.";

    auto auth = require_authentication(remote_address, "Get report definitions");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = get_report_definitions_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_report_definitions_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);
    service::report_definition_service svc(ctx);

    auto definitions = svc.list_definitions();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << definitions.size() << " report definitions";

    get_report_definitions_response response{ .definitions = std::move(definitions) };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_save_report_definition_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing save_report_definition_request.";

    auto auth = require_authentication(remote_address, "Save report definition");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = save_report_definition_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_report_definition_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    request.definition.modified_by = auth->username;
    request.definition.performed_by = auth->username;
    request.definition.tenant_id = auth->tenant_id;

    auto ctx = make_request_context(*auth);
    service::report_definition_service svc(ctx);

    save_report_definition_response response;
    try {
        svc.save_definition(request.definition);
        response.success = true;
        response.message = "Report definition saved successfully.";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save report definition: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_delete_report_definition_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing delete_report_definition_request.";

    auto auth = require_authentication(remote_address, "Delete report definition");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = delete_report_definition_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_report_definition_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);
    service::report_definition_service svc(ctx);

    delete_report_definition_response response;
    for (const auto& id : request_result->ids) {
        delete_report_definition_result r;
        r.id = id;
        try {
            svc.remove_definition(boost::uuids::to_string(id));
            r.success = true;
            r.message = "Deleted successfully.";
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to delete report definition: " << e.what();
            r.success = false;
            r.message = e.what();
        }
        response.results.push_back(std::move(r));
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_get_report_definition_history_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_report_definition_history_request.";

    auto auth = require_authentication(remote_address, "Get report definition history");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = get_report_definition_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_report_definition_history_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);
    service::report_definition_service svc(ctx);

    get_report_definition_history_response response;
    try {
        response.versions = svc.get_definition_history(
            boost::uuids::to_string(request_result->id));
        response.success = true;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get report definition history: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Report Instances
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_get_report_instances_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_report_instances_request.";

    auto auth = require_authentication(remote_address, "Get report instances");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = get_report_instances_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_report_instances_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);
    service::report_instance_service svc(ctx);

    auto instances = svc.list_instances();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << instances.size() << " report instances";

    get_report_instances_response response{ .instances = std::move(instances) };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_save_report_instance_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing save_report_instance_request.";

    auto auth = require_authentication(remote_address, "Save report instance");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = save_report_instance_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_report_instance_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    request.instance.modified_by = auth->username;
    request.instance.performed_by = auth->username;
    request.instance.tenant_id = auth->tenant_id;

    auto ctx = make_request_context(*auth);
    service::report_instance_service svc(ctx);

    save_report_instance_response response;
    try {
        svc.save_instance(request.instance);
        response.success = true;
        response.message = "Report instance saved successfully.";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save report instance: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_delete_report_instance_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing delete_report_instance_request.";

    auto auth = require_authentication(remote_address, "Delete report instance");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = delete_report_instance_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_report_instance_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);
    service::report_instance_service svc(ctx);

    delete_report_instance_response response;
    for (const auto& id : request_result->ids) {
        delete_report_instance_result r;
        r.id = id;
        try {
            svc.remove_instance(boost::uuids::to_string(id));
            r.success = true;
            r.message = "Deleted successfully.";
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to delete report instance: " << e.what();
            r.success = false;
            r.message = e.what();
        }
        response.results.push_back(std::move(r));
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_get_report_instance_history_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_report_instance_history_request.";

    auto auth = require_authentication(remote_address, "Get report instance history");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = get_report_instance_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_report_instance_history_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);
    service::report_instance_service svc(ctx);

    get_report_instance_history_response response;
    try {
        response.versions = svc.get_instance_history(
            boost::uuids::to_string(request_result->id));
        response.success = true;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get report instance history: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Report Scheduling
// ============================================================================

namespace {

/**
 * @brief Escape single-quote characters for embedding in a SQL string literal.
 *
 * PostgreSQL uses '' (two single quotes) to represent a literal single quote
 * inside a single-quoted string. Used when baking a report name into the
 * pg_cron command SQL (a pgmq.send() call) at scheduling time.
 */
std::string sql_single_quote_escape(const std::string& s) {
    std::string result;
    result.reserve(s.size());
    for (char c : s) {
        if (c == '\'') result += "''";
        else result += c;
    }
    return result;
}

} // anonymous namespace

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_schedule_report_definitions_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing schedule_report_definitions_request.";

    auto auth = require_authentication(remote_address, "Schedule report definitions");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = schedule_report_definitions_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize schedule_report_definitions_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    auto ctx = make_request_context(*auth);
    service::report_definition_service report_svc(ctx);
    scheduler::service::cron_scheduler sched_svc(ctx);

    schedule_report_definitions_response response;
    response.success = true;

    for (const auto& id : request.ids) {
        const std::string id_str = boost::uuids::to_string(id);

        auto def = report_svc.find_definition(id_str);
        if (!def) {
            BOOST_LOG_SEV(lg(), warn) << "Report definition not found for scheduling: " << id_str;
            response.success = false;
            response.message = "Report definition not found: " + id_str;
            continue;
        }

        // Parse the stored cron expression string into a typed cron_expression.
        auto cron_expr = scheduler::domain::cron_expression::from_string(
            def->schedule_expression);
        if (!cron_expr) {
            BOOST_LOG_SEV(lg(), error) << "Invalid cron expression '"
                << def->schedule_expression << "' for definition " << id_str
                << ": " << cron_expr.error();
            response.success = false;
            response.message = "Invalid cron expression: " + cron_expr.error();
            continue;
        }

        // Build the SQL command that pg_cron will execute on each firing.
        // Posts a message to the pgmq 'report_events' queue. The queue name,
        // tenant UUID, and report name are baked in at scheduling time;
        // now() is evaluated at execution time by pg_cron.
        const std::string tenant_str = auth->tenant_id.to_string();
        const std::string safe_name = sql_single_quote_escape(def->name);
        const std::string command =
            "SELECT pgmq.send('report_events', "
            "jsonb_build_object("
            "'report_name', '" + safe_name + "', "
            "'event', 'scheduled', "
            "'tenant_id', '" + tenant_str + "', "
            "'triggered_at', now()::text), 0)";

        // Create the job definition. The unique job_name is keyed on the
        // report definition UUID so that re-scheduling the same report
        // updates the existing pg_cron entry in-place.
        scheduler::domain::job_definition job_def;
        job_def.id = boost::uuids::random_generator()();
        job_def.tenant_id = auth->tenant_id;
        job_def.party_id = auth->party_id;
        job_def.job_name = "ores:report:" + id_str;
        job_def.description = "Scheduled run for report: " + def->name;
        job_def.command = command;
        job_def.schedule_expression = *cron_expr;
        job_def.is_active = true;
        job_def.modified_by = auth->username;
        job_def.performed_by = auth->username;
        job_def.change_reason_code = request.change_reason_code;
        job_def.change_commentary = request.change_commentary;

        try {
            auto scheduled = sched_svc.schedule(
                std::move(job_def),
                request.change_reason_code,
                request.change_commentary);

            BOOST_LOG_SEV(lg(), info)
                << "Scheduled report definition " << id_str
                << " as pg_cron job " << scheduled.job_name;

            // Link the scheduler job back to the report definition.
            def->scheduler_job_id = scheduled.id;
            def->modified_by = auth->username;
            def->performed_by = auth->username;
            def->change_reason_code = request.change_reason_code;
            def->change_commentary = request.change_commentary;
            report_svc.save_definition(*def);

            ++response.scheduled_count;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to schedule report definition " << id_str
                << ": " << e.what();
            response.success = false;
            response.message = e.what();
        }
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
reporting_message_handler::handle_unschedule_report_definitions_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing unschedule_report_definitions_request.";

    auto auth = require_authentication(remote_address, "Unschedule report definitions");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = unschedule_report_definitions_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize unschedule_report_definitions_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    auto ctx = make_request_context(*auth);
    service::report_definition_service report_svc(ctx);
    scheduler::service::cron_scheduler sched_svc(ctx);

    unschedule_report_definitions_response response;
    response.success = true;

    for (const auto& id : request.ids) {
        const std::string id_str = boost::uuids::to_string(id);

        auto def = report_svc.find_definition(id_str);
        if (!def) {
            BOOST_LOG_SEV(lg(), warn) << "Report definition not found for unscheduling: " << id_str;
            response.success = false;
            response.message = "Report definition not found: " + id_str;
            continue;
        }

        if (!def->scheduler_job_id.has_value()) {
            BOOST_LOG_SEV(lg(), warn)
                << "Report definition " << id_str << " has no active scheduler job; skipping.";
            continue;
        }

        try {
            sched_svc.unschedule(
                *def->scheduler_job_id,
                auth->username,
                request.change_reason_code,
                request.change_commentary);

            BOOST_LOG_SEV(lg(), info)
                << "Unscheduled report definition " << id_str;

            // Clear the scheduler link on the report definition.
            def->scheduler_job_id = std::nullopt;
            def->modified_by = auth->username;
            def->performed_by = auth->username;
            def->change_reason_code = request.change_reason_code;
            def->change_commentary = request.change_commentary;
            report_svc.save_definition(*def);

            ++response.unscheduled_count;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to unschedule report definition " << id_str
                << ": " << e.what();
            response.success = false;
            response.message = e.what();
        }
    }

    co_return response.serialize();
}

}
