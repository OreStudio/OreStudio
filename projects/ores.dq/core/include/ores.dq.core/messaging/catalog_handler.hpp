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
#ifndef ORES_DQ_CORE_MESSAGING_CATALOG_HANDLER_HPP
#define ORES_DQ_CORE_MESSAGING_CATALOG_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/messaging/catalog_protocol.hpp"
#include "ores.dq.core/service/catalog_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::dq::messaging {

namespace {
inline auto& catalog_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.dq.messaging.catalog_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for catalog operations.
 */
class catalog_handler {
public:
    catalog_handler(ores::nats::service::client& nats,
                    ores::database::context ctx,
                    std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(catalog_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::catalog_service svc(req_ctx);
        get_catalogs_response resp;
        if (auto req = decode<get_catalogs_request>(msg)) {
            try {
                resp.catalogs = svc.list_catalogs(req->offset, req->limit);
                resp.total_available_count = static_cast<int>(svc.count_catalogs());
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(catalog_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
        } else {
            BOOST_LOG_SEV(catalog_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        BOOST_LOG_SEV(catalog_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(catalog_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "dq::catalogs:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::catalog_service svc(req_ctx);
        if (auto req = decode<save_catalog_request>(msg)) {
            try {
                svc.save_catalog(req->data);
                BOOST_LOG_SEV(catalog_handler_lg(), debug) << "Completed " << msg.subject;
                reply(nats_, msg, save_catalog_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(catalog_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, save_catalog_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(catalog_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(catalog_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::catalog_service svc(req_ctx);
        if (auto req = decode<get_catalog_history_request>(msg)) {
            try {
                auto hist = svc.get_catalog_history(req->name);
                BOOST_LOG_SEV(catalog_handler_lg(), debug) << "Completed " << msg.subject;
                reply(nats_,
                      msg,
                      get_catalog_history_response{.history = std::move(hist), .success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(catalog_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      get_catalog_history_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(catalog_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(catalog_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "dq::catalogs:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::catalog_service svc(req_ctx);
        if (auto req = decode<delete_catalog_request>(msg)) {
            try {
                svc.delete_catalogs(req->names);
                BOOST_LOG_SEV(catalog_handler_lg(), debug) << "Completed " << msg.subject;
                reply(nats_, msg, delete_catalog_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(catalog_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, delete_catalog_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(catalog_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::dq::messaging

#endif
