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
#ifndef ORES_REFDATA_CORE_MESSAGING_REGULATORY_BOOK_TYPE_HANDLER_HPP
#define ORES_REFDATA_CORE_MESSAGING_REGULATORY_BOOK_TYPE_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/messaging/regulatory_book_type_protocol.hpp"
#include "ores.refdata.core/service/regulatory_book_type_service.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::refdata::messaging {

namespace {
inline auto& regulatory_book_type_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.refdata.messaging.regulatory_book_type_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for regulatory book type operations.
 */
class regulatory_book_type_handler {
public:
    regulatory_book_type_handler(ores::nats::service::client& nats,
                                 ores::database::context ctx,
                                 std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(regulatory_book_type_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::regulatory_book_type_service svc(req_ctx);
        get_regulatory_book_types_response resp;
        if (auto req = decode<get_regulatory_book_types_request>(msg)) {
            try {
                if (!req->as_of.empty()) {
                    resp.types = svc.list_types_at_timepoint(req->as_of);
                    resp.total_available_count = static_cast<int>(resp.types.size());
                } else {
                    resp.types = svc.list_types(req->offset, req->limit);
                    resp.total_available_count = static_cast<int>(svc.count_types());
                }
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(regulatory_book_type_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
        } else {
            BOOST_LOG_SEV(regulatory_book_type_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        BOOST_LOG_SEV(regulatory_book_type_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(regulatory_book_type_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::regulatory_book_types:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::regulatory_book_type_service svc(req_ctx);
        if (auto req = decode<save_regulatory_book_type_request>(msg)) {
            try {
                svc.save_type(req->data);
                BOOST_LOG_SEV(regulatory_book_type_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, save_regulatory_book_type_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(regulatory_book_type_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      save_regulatory_book_type_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(regulatory_book_type_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(regulatory_book_type_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::regulatory_book_type_service svc(req_ctx);
        if (auto req = decode<get_regulatory_book_type_history_request>(msg)) {
            try {
                auto hist = svc.get_type_history(req->code);
                BOOST_LOG_SEV(regulatory_book_type_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_,
                      msg,
                      get_regulatory_book_type_history_response{.history = std::move(hist),
                                                                .success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(regulatory_book_type_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      get_regulatory_book_type_history_response{.success = false,
                                                                .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(regulatory_book_type_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(regulatory_book_type_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::regulatory_book_types:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::regulatory_book_type_service svc(req_ctx);
        if (auto req = decode<delete_regulatory_book_type_request>(msg)) {
            try {
                svc.delete_types(req->codes);
                BOOST_LOG_SEV(regulatory_book_type_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, delete_regulatory_book_type_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(regulatory_book_type_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      delete_regulatory_book_type_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(regulatory_book_type_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::refdata::messaging

#endif
