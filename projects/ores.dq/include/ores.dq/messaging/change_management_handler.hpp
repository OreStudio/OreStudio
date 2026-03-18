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
#ifndef ORES_DQ_MESSAGING_CHANGE_MANAGEMENT_HANDLER_HPP
#define ORES_DQ_MESSAGING_CHANGE_MANAGEMENT_HANDLER_HPP

#include <optional>
#include <stdexcept>
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.dq/messaging/change_management_protocol.hpp"
#include "ores.dq/service/change_management_service.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::dq::messaging {

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using namespace ores::logging;

namespace {
inline auto& change_management_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.dq.messaging.change_management_handler");
    return instance;
}
} // namespace

class change_management_handler {
public:
    change_management_handler(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list_categories(ores::nats::message msg) {
        BOOST_LOG_SEV(change_management_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_change_reason_categories_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(change_management_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::change_management_service svc(ctx);
        try {
            const auto items = svc.list_categories();
            get_change_reason_categories_response resp;
            resp.categories = items;
            BOOST_LOG_SEV(change_management_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(change_management_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_change_reason_categories_response{});
        }
    }

    void save_category(ores::nats::message msg) {
        BOOST_LOG_SEV(change_management_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<save_change_reason_category_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(change_management_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::change_management_service svc(ctx);
        try {
            stamp(req->data, ctx);
            svc.save_category(req->data);
            BOOST_LOG_SEV(change_management_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_change_reason_category_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(change_management_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg,
                save_change_reason_category_response{false, e.what()});
        }
    }

    void delete_categories(ores::nats::message msg) {
        BOOST_LOG_SEV(change_management_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<delete_change_reason_category_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(change_management_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::change_management_service svc(ctx);
        try {
            svc.remove_categories(req->codes);
            BOOST_LOG_SEV(change_management_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_change_reason_category_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(change_management_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg,
                delete_change_reason_category_response{false, e.what()});
        }
    }

    void category_history(ores::nats::message msg) {
        BOOST_LOG_SEV(change_management_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_change_reason_category_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(change_management_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::change_management_service svc(ctx);
        try {
            const auto history = svc.get_category_history(req->code);
            get_change_reason_category_history_response resp;
            resp.success = true;
            resp.versions = history;
            BOOST_LOG_SEV(change_management_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(change_management_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_change_reason_category_history_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

    void list_reasons(ores::nats::message msg) {
        BOOST_LOG_SEV(change_management_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_change_reasons_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(change_management_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::change_management_service svc(ctx);
        try {
            const auto items = svc.list_reasons();
            get_change_reasons_response resp;
            resp.reasons = items;
            BOOST_LOG_SEV(change_management_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(change_management_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_change_reasons_response{});
        }
    }

    void save_reason(ores::nats::message msg) {
        BOOST_LOG_SEV(change_management_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<save_change_reason_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(change_management_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::change_management_service svc(ctx);
        try {
            stamp(req->data, ctx);
            svc.save_reason(req->data);
            BOOST_LOG_SEV(change_management_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_change_reason_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(change_management_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_change_reason_response{false, e.what()});
        }
    }

    void delete_reasons(ores::nats::message msg) {
        BOOST_LOG_SEV(change_management_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<delete_change_reason_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(change_management_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::change_management_service svc(ctx);
        try {
            svc.remove_reasons(req->codes);
            BOOST_LOG_SEV(change_management_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_change_reason_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(change_management_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_change_reason_response{false, e.what()});
        }
    }

    void reason_history(ores::nats::message msg) {
        BOOST_LOG_SEV(change_management_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_change_reason_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(change_management_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::change_management_service svc(ctx);
        try {
            const auto history = svc.get_reason_history(req->code);
            get_change_reason_history_response resp;
            resp.success = true;
            resp.versions = history;
            BOOST_LOG_SEV(change_management_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(change_management_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_change_reason_history_response resp;
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
