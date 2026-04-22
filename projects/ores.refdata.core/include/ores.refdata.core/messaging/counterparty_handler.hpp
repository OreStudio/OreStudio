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
#ifndef ORES_REFDATA_CORE_MESSAGING_COUNTERPARTY_HANDLER_HPP
#define ORES_REFDATA_CORE_MESSAGING_COUNTERPARTY_HANDLER_HPP

#include <optional>
#include <boost/uuid/string_generator.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.core/service/counterparty_service.hpp"

namespace ores::refdata::messaging {

namespace {
inline auto& counterparty_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.refdata.messaging.counterparty_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using namespace ores::logging;

class counterparty_handler {
public:
    counterparty_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(counterparty_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::counterparty_service svc(ctx);
        get_counterparties_response resp;
        auto req = decode<get_counterparties_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            reply(nats_, msg, resp);
            return;
        }
        try {
            resp.counterparties = svc.list_counterparties(
                static_cast<std::uint32_t>(req->offset),
                static_cast<std::uint32_t>(req->limit));
            resp.total_available_count =
                static_cast<int>(svc.count_counterparties());
            BOOST_LOG_SEV(counterparty_handler_lg(), debug)
                << "Completed " << msg.subject;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(counterparty_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
        }
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(counterparty_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "refdata::counterparties:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::counterparty_service svc(ctx);
        auto req = decode<save_counterparty_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            svc.save_counterparty(req->data);
            BOOST_LOG_SEV(counterparty_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                save_counterparty_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(counterparty_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_counterparty_response{
                .success = false, .message = e.what()});
        }
    }

    void remove(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(counterparty_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "refdata::counterparties:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::counterparty_service svc(ctx);
        auto req = decode<delete_counterparty_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            boost::uuids::string_generator gen;
            for (const auto& id_str : req->ids)
                svc.remove_counterparty(gen(id_str));
            BOOST_LOG_SEV(counterparty_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                delete_counterparty_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(counterparty_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_counterparty_response{
                .success = false, .message = e.what()});
        }
    }

    void history(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(counterparty_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::counterparty_service svc(ctx);
        auto req = decode<get_counterparty_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            boost::uuids::string_generator gen;
            auto h = svc.get_counterparty_history(gen(req->id));
            BOOST_LOG_SEV(counterparty_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, get_counterparty_history_response{
                .success = true, .history = std::move(h)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(counterparty_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_counterparty_history_response{
                .success = false, .message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::refdata::messaging
#endif
