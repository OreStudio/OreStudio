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
#ifndef ORES_REFDATA_CORE_MESSAGING_BOOK_HANDLER_HPP
#define ORES_REFDATA_CORE_MESSAGING_BOOK_HANDLER_HPP

#include <optional>
#include <boost/uuid/string_generator.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.core/service/book_service.hpp"

namespace ores::refdata::messaging {

namespace {
inline auto& book_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.refdata.messaging.book_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

class book_handler {
public:
    book_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(book_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::book_service svc(ctx);
        get_books_response resp;
        try {
            resp.books = svc.list_books();
            resp.total_available_count =
                static_cast<int>(resp.books.size());
            BOOST_LOG_SEV(book_handler_lg(), debug)
                << "Completed " << msg.subject;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(book_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
        }
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(book_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "refdata::books:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::book_service svc(ctx);
        auto req = decode<save_book_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(book_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            svc.save_book(req->data);
            BOOST_LOG_SEV(book_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, save_book_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(book_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_book_response{
                .success = false, .message = e.what()});
        }
    }

    void del(ores::nats::message msg) {
        BOOST_LOG_SEV(book_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "refdata::books:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::book_service svc(ctx);
        auto req = decode<delete_book_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(book_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            boost::uuids::string_generator gen;
            for (const auto& id_str : req->ids)
                svc.remove_book(gen(id_str));
            BOOST_LOG_SEV(book_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, delete_book_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(book_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_book_response{
                .success = false, .message = e.what()});
        }
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(book_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::book_service svc(ctx);
        auto req = decode<get_book_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(book_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            boost::uuids::string_generator gen;
            auto h = svc.get_book_history(gen(req->id));
            BOOST_LOG_SEV(book_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, get_book_history_response{
                .success = true, .versions = std::move(h)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(book_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_book_history_response{
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
