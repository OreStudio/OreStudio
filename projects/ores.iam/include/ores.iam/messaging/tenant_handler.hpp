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
#ifndef ORES_IAM_MESSAGING_TENANT_HANDLER_HPP
#define ORES_IAM_MESSAGING_TENANT_HANDLER_HPP

#include <stdexcept>
#include <boost/uuid/string_generator.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.iam/messaging/tenant_protocol.hpp"
#include "ores.iam/repository/tenant_repository.hpp"

namespace ores::iam::messaging {

namespace {

inline auto& tenant_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.iam.messaging.tenant_handler");
    return instance;
}

} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;

class tenant_handler {
public:
    tenant_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        ores::security::jwt::jwt_authenticator signer)
        : nats_(nats), ctx_(std::move(ctx)), signer_(std::move(signer)) {}

    void list(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(tenant_handler_lg(), debug)
            << "Handling " << msg.subject;
        try {
            repository::tenant_repository repo(ctx_);
            get_tenants_response resp;
            resp.tenants = repo.read_latest();
            BOOST_LOG_SEV(tenant_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_tenants_response{});
        }
    }

    void save(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(tenant_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<save_tenant_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenant_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            const auto ctx = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            repository::tenant_repository repo(ctx);
            stamp(req->data, ctx);
            repo.write(req->data);
            BOOST_LOG_SEV(tenant_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                save_tenant_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_tenant_response{
                .success = false, .message = e.what()});
        }
    }

    void del(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(tenant_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<delete_tenant_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenant_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            repository::tenant_repository repo(ctx_);
            boost::uuids::string_generator sg;
            for (const auto& id_str : req->ids)
                repo.remove(sg(id_str));
            BOOST_LOG_SEV(tenant_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                delete_tenant_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_tenant_response{
                .success = false, .message = e.what()});
        }
    }

    void history(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(tenant_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<get_tenant_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenant_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            repository::tenant_repository repo(ctx_);
            boost::uuids::string_generator sg;
            auto hist = repo.read_history(sg(req->id));
            BOOST_LOG_SEV(tenant_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, get_tenant_history_response{
                .success = true,
                .versions = std::move(hist)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_tenant_history_response{
                .success = false, .message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
};

} // namespace ores::iam::messaging
#endif
