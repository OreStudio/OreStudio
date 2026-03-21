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
#ifndef ORES_DQ_MESSAGING_CODING_SCHEME_HANDLER_HPP
#define ORES_DQ_MESSAGING_CODING_SCHEME_HANDLER_HPP

#include <optional>
#include <stdexcept>
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.dq/messaging/coding_scheme_protocol.hpp"
#include "ores.dq/service/coding_scheme_service.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::dq::messaging {

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
using namespace ores::logging;

namespace {
inline auto& coding_scheme_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.dq.messaging.coding_scheme_handler");
    return instance;
}
} // namespace

class coding_scheme_handler {
public:
    coding_scheme_handler(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    // =========================================================================
    // Coding Scheme Authority Types
    // =========================================================================

    void list_authority_types(ores::nats::message msg) {
        BOOST_LOG_SEV(coding_scheme_handler_lg(), debug) << "Handling " << msg.subject;
        auto req =
            decode<get_coding_scheme_authority_types_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(coding_scheme_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::coding_scheme_service svc(ctx);
        try {
            const auto items = svc.list_authority_types();
            get_coding_scheme_authority_types_response resp;
            resp.coding_scheme_authority_types = items;
            resp.total_available_count = static_cast<int>(items.size());
            BOOST_LOG_SEV(coding_scheme_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(coding_scheme_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_coding_scheme_authority_types_response resp;
            resp.total_available_count = 0;
            reply(nats_, msg, resp);
        }
    }

    void save_authority_type(ores::nats::message msg) {
        BOOST_LOG_SEV(coding_scheme_handler_lg(), debug) << "Handling " << msg.subject;
        auto req =
            decode<save_coding_scheme_authority_type_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(coding_scheme_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::coding_scheme_service svc(ctx);
        try {
            stamp(req->data, ctx);
            svc.save_authority_type(req->data);
            BOOST_LOG_SEV(coding_scheme_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg,
                save_coding_scheme_authority_type_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(coding_scheme_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg,
                save_coding_scheme_authority_type_response{false, e.what()});
        }
    }

    void delete_authority_types(ores::nats::message msg) {
        BOOST_LOG_SEV(coding_scheme_handler_lg(), debug) << "Handling " << msg.subject;
        auto req =
            decode<delete_coding_scheme_authority_type_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(coding_scheme_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::coding_scheme_service svc(ctx);
        try {
            svc.remove_authority_types(req->types);
            BOOST_LOG_SEV(coding_scheme_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg,
                delete_coding_scheme_authority_type_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(coding_scheme_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg,
                delete_coding_scheme_authority_type_response{false, e.what()});
        }
    }

    void authority_type_history(ores::nats::message msg) {
        BOOST_LOG_SEV(coding_scheme_handler_lg(), debug) << "Handling " << msg.subject;
        auto req =
            decode<get_coding_scheme_authority_type_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(coding_scheme_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::coding_scheme_service svc(ctx);
        try {
            const auto history = svc.get_authority_type_history(req->type);
            get_coding_scheme_authority_type_history_response resp;
            resp.success = true;
            resp.history = history;
            BOOST_LOG_SEV(coding_scheme_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(coding_scheme_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_coding_scheme_authority_type_history_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

    // =========================================================================
    // Coding Schemes
    // =========================================================================

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(coding_scheme_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_coding_schemes_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(coding_scheme_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::coding_scheme_service svc(ctx);
        try {
            const auto items = svc.list_coding_schemes(
                static_cast<std::uint32_t>(req->offset),
                static_cast<std::uint32_t>(req->limit));
            const auto count = svc.get_coding_scheme_count();
            get_coding_schemes_response resp;
            resp.coding_schemes = items;
            resp.total_available_count = static_cast<int>(count);
            BOOST_LOG_SEV(coding_scheme_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(coding_scheme_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_coding_schemes_response resp;
            resp.total_available_count = 0;
            reply(nats_, msg, resp);
        }
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(coding_scheme_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<save_coding_scheme_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(coding_scheme_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::coding_scheme_service svc(ctx);
        try {
            stamp(req->data, ctx);
            svc.save_coding_scheme(req->data);
            BOOST_LOG_SEV(coding_scheme_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_coding_scheme_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(coding_scheme_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_coding_scheme_response{false, e.what()});
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(coding_scheme_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<delete_coding_scheme_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(coding_scheme_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::coding_scheme_service svc(ctx);
        try {
            svc.remove_coding_schemes(req->codes);
            BOOST_LOG_SEV(coding_scheme_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_coding_scheme_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(coding_scheme_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_coding_scheme_response{false, e.what()});
        }
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(coding_scheme_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_coding_scheme_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(coding_scheme_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::coding_scheme_service svc(ctx);
        try {
            const auto hist = svc.get_coding_scheme_history(req->code);
            get_coding_scheme_history_response resp;
            resp.success = true;
            resp.history = hist;
            BOOST_LOG_SEV(coding_scheme_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(coding_scheme_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_coding_scheme_history_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

private:

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::dq::messaging

#endif
