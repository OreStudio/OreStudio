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
#ifndef ORES_IAM_CORE_MESSAGING_TENANT_SESSION_HANDLER_HPP
#define ORES_IAM_CORE_MESSAGING_TENANT_SESSION_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.iam.api/messaging/tenant_session_protocol.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.iam.core/service/tenant_session_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/error_code.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <exception>
#include <functional>
#include <optional>
#include <string>
#include <string_view>

namespace ores::iam::messaging {

/**
 * @brief Serves iam.v1.ops.enter_tenant and iam.v1.ops.leave_tenant.
 *
 * The caller is read from the bearer token the request carries, and the
 * caller's permissions from the database, because a person's token carries
 * none. The rules live in the service.
 */
class tenant_session_handler {
private:
    inline static std::string_view logger_name = "ores.iam.messaging.tenant_session_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using lifetime_fn = std::function<std::chrono::seconds()>;

    tenant_session_handler(ores::nats::service::client& nats,
                           ores::database::context ctx,
                           ores::security::jwt::jwt_authenticator signer,
                           lifetime_fn lifetime)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , signer_(std::move(signer))
        , lifetime_(std::move(lifetime)) {}

    void enter(ores::nats::message msg) {
        using ores::service::messaging::decode;
        using ores::service::messaging::error_reply;
        using ores::service::messaging::reply;
        const auto caller = read_caller(msg);
        if (!caller) {
            error_reply(nats_, msg, ores::service::error_code::unauthorized);
            return;
        }
        const auto req = decode<enter_tenant_request>(msg);
        if (!req) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        try {
            reply(nats_, msg, service().enter(*caller, *req));
        } catch (const std::exception& e) {
            using namespace ores::logging;
            BOOST_LOG_SEV(lg(), error) << msg.subject << " failed: " << e.what();
            reply(
                nats_,
                msg,
                enter_tenant_response{.success = false, .message = "The tenant was not entered."});
        }
    }

    void leave(ores::nats::message msg) {
        using ores::service::messaging::error_reply;
        using ores::service::messaging::reply;
        const auto caller = read_caller(msg);
        if (!caller) {
            error_reply(nats_, msg, ores::service::error_code::unauthorized);
            return;
        }
        try {
            reply(nats_, msg, service().leave(*caller));
        } catch (const std::exception& e) {
            using namespace ores::logging;
            BOOST_LOG_SEV(lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_,
                  msg,
                  leave_tenant_response{.success = false, .message = "The exit was not recorded."});
        }
    }

private:
    service::tenant_session_service service() const {
        return service::tenant_session_service(ctx_, signer_, lifetime_());
    }

    /**
     * @brief The caller, from a valid bearer token, or nothing.
     *
     * The permissions are read in the caller's own tenant: an administrator
     * inside another tenant is still the system tenant's account.
     */
    std::optional<service::tenant_session_caller>
    read_caller(const ores::nats::message& msg) const {
        const auto it = msg.headers.find(std::string(ores::nats::headers::authorization));
        if (it == msg.headers.end() || !it->second.starts_with(ores::nats::headers::bearer_prefix))
            return std::nullopt;
        const auto claims =
            signer_.validate(it->second.substr(ores::nats::headers::bearer_prefix.size()));
        if (!claims || !claims->tenant_id)
            return std::nullopt;
        const auto tenant = utility::uuid::tenant_id::from_string(*claims->tenant_id);
        if (!tenant)
            return std::nullopt;

        boost::uuids::uuid account_id;
        try {
            account_id = boost::lexical_cast<boost::uuids::uuid>(claims->subject);
        } catch (const boost::bad_lexical_cast&) {
            return std::nullopt;
        }
        service::tenant_session_caller caller{.account_id = account_id,
                                              .username = claims->username.value_or(""),
                                              .session_id = claims->session_id.value_or(""),
                                              .tenant_id = *tenant,
                                              .party_id = claims->party_id,
                                              .acting_from_tenant_id =
                                                  claims->acting_from_tenant_id,
                                              .permissions = {}};
        // A session already inside a tenant carries no permissions here: the
        // service refuses its entry as already inside before it reads them,
        // and leaving needs none.
        if (!caller.acting_from_tenant_id) {
            caller.permissions =
                service::authorization_service(ctx_.with_tenant(*tenant, caller.username))
                    .get_effective_permissions(caller.account_id);
        }
        return caller;
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
    lifetime_fn lifetime_;
};

}

#endif
