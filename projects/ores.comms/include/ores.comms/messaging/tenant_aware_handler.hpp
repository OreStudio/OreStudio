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
#ifndef ORES_COMMS_MESSAGING_TENANT_AWARE_HANDLER_HPP
#define ORES_COMMS_MESSAGING_TENANT_AWARE_HANDLER_HPP

#include <memory>
#include <boost/uuid/uuid_io.hpp>
#include "ores.comms/messaging/message_handler.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/serialization/error_code.hpp"

namespace ores::comms::messaging {

/**
 * @brief Base class for message handlers that require tenant isolation.
 *
 * Provides common functionality for handlers that need to create per-request
 * database contexts based on the authenticated session's tenant ID. This
 * ensures proper tenant isolation when multiple tenants send concurrent
 * requests.
 *
 * Usage pattern in derived handlers:
 * @code
 * auto auth_result = require_authentication(remote_address, "Operation name");
 * if (!auth_result) {
 *     co_return std::unexpected(auth_result.error());
 * }
 * auto ctx = make_request_context(*auth_result);
 * MyService svc(ctx);
 * // Use svc for this request...
 * @endcode
 *
 * For handlers that need permission checks, add an authorization_service
 * member and implement check_authorization() using require_authentication()
 * as a building block.
 */
class tenant_aware_handler : public message_handler {
protected:
    /**
     * @brief Result type for authentication/authorization checks.
     */
    using auth_result = std::expected<
        service::session_info,
        ores::utility::serialization::error_code
    >;

    /**
     * @brief Construct a tenant-aware handler.
     *
     * @param ctx Base database context (will be cloned with tenant for each request)
     * @param sessions Shared auth session service for authentication
     */
    tenant_aware_handler(database::context ctx,
        std::shared_ptr<service::auth_session_service> sessions)
        : ctx_(std::move(ctx)), sessions_(std::move(sessions)) {}

    /**
     * @brief Create a per-request database context for the session's tenant.
     *
     * Creates a new context with the tenant ID from the session, ensuring
     * that database operations use the correct tenant isolation. Each
     * request should use its own context to avoid race conditions.
     *
     * @param session The authenticated session containing the tenant ID
     * @return A new context configured for the session's tenant
     */
    [[nodiscard]] database::context
    make_request_context(const service::session_info& session) const {
        if (!session.visible_party_ids.empty()) {
            return ctx_.with_party(session.tenant_id, session.party_id,
                                   session.visible_party_ids);
        }
        return ctx_.with_tenant(session.tenant_id);
    }

    /**
     * @brief Require authentication for a request.
     *
     * Verifies that the remote address has an active authenticated session.
     * Use this for operations that require authentication but no specific
     * permission.
     *
     * @param remote_address The remote endpoint address
     * @param operation_name Human-readable name for logging (e.g., "List images")
     * @return The session info if authenticated, or error code if not
     */
    [[nodiscard]] auth_result
    require_authentication(const std::string& remote_address,
                          std::string_view operation_name) const {
        auto session = sessions_->get_session(remote_address);
        if (!session) {
            BOOST_LOG_SEV(lg(), logging::warn) << operation_name
                                              << " denied: no active session for "
                                              << remote_address;
            return std::unexpected(
                ores::utility::serialization::error_code::authentication_failed);
        }
        return *session;
    }

    /**
     * @brief Get the sessions service.
     *
     * Provides access to the sessions service for derived classes that need
     * additional session operations.
     */
    [[nodiscard]] const std::shared_ptr<service::auth_session_service>&
    sessions() const { return sessions_; }

    /**
     * @brief Get the base context.
     *
     * Provides access to the base context for derived classes. Note: prefer
     * using make_request_context() for operations that need tenant isolation.
     */
    [[nodiscard]] const database::context& base_context() const { return ctx_; }

    database::context ctx_;
    std::shared_ptr<service::auth_session_service> sessions_;

private:
    inline static std::string_view logger_name_ =
        "ores.comms.messaging.tenant_aware_handler";

    [[nodiscard]] static logging::logger_t& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name_);
        return instance;
    }
};

}

#endif
