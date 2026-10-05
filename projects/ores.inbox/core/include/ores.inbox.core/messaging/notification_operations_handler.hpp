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
#ifndef ORES_INBOX_CORE_MESSAGING_NOTIFICATION_OPERATIONS_HANDLER_HPP
#define ORES_INBOX_CORE_MESSAGING_NOTIFICATION_OPERATIONS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/messaging/notification_operations_protocol.hpp"
#include "ores.inbox.core/service/notification_center.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <algorithm>
#include <optional>
#include <string>
#include <vector>

namespace ores::inbox::messaging {

namespace {

inline auto& notification_operations_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.inbox.messaging.notification_operations_handler");
    return instance;
}

inline ores::utility::domain::result
notification_result(ores::utility::domain::outcome o, std::string code, std::string message) {
    return ores::utility::domain::result{
        .outcome = o, .code = std::move(code), .message = std::move(message), .fields = {}};
}

} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief Answers raising notifications and a person's reads of their own.
 *
 * Raising needs inbox::notifications:write, because it is how a component
 * tells people something: a person does not broadcast. The reads, marking and
 * clearing need only a signed-in account, and touch only that account's rows.
 */
class notification_operations_handler {
public:
    notification_operations_handler(ores::nats::service::client& nats,
                                    ores::database::context ctx,
                                    std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void raise(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        if (!has_permission(*ctx, "inbox::notifications:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<raise_notification_request>(msg);
        if (!req) {
            reply(nats_,
                  msg,
                  raise_notification_response{
                      .result = notification_result(
                          outcome::invalid, "bad_request", "The request could not be read.")});
            return;
        }
        try {
            service::notification_center center(*ctx);
            if (!center.kind_exists(req->kind_code)) {
                reply(nats_,
                      msg,
                      raise_notification_response{
                          .result = notification_result(outcome::invalid,
                                                        "unknown_kind",
                                                        "No such kind of notification: " +
                                                            req->kind_code)});
                return;
            }
            if (req->link_route.empty()) {
                reply(nats_,
                      msg,
                      raise_notification_response{
                          .result = notification_result(
                              outcome::invalid,
                              "link_required",
                              "A notification links to where it is dealt with.")});
                return;
            }
            const auto me = center.actor_account_id();
            if (!me) {
                reply(nats_,
                      msg,
                      raise_notification_response{
                          .result = notification_result(outcome::denied,
                                                        "no_account",
                                                        "The signed-in account was not found.")});
                return;
            }

            auto recipients = req->account_ids;
            if (!req->audience_permission_code.empty()) {
                const auto holders = center.holders_of(req->audience_permission_code);
                recipients.insert(recipients.end(), holders.begin(), holders.end());
            }
            std::ranges::sort(recipients);
            const auto [first, last] = std::ranges::unique(recipients);
            recipients.erase(first, last);
            if (recipients.empty()) {
                reply(nats_,
                      msg,
                      raise_notification_response{
                          .result = notification_result(outcome::ok,
                                                        "no_recipients",
                                                        "Nobody holds the audience permission."),
                          .recipient_count = 0});
                return;
            }

            const auto raised = center.raise(*req, recipients, *me);
            reply(nats_,
                  msg,
                  raise_notification_response{.result = notification_result(outcome::ok, "", ""),
                                              .notification_id = raised.notification_id,
                                              .recipient_count = raised.recipient_count});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(notification_operations_handler_lg(), error)
                << "Error raising a notification: " << e.what();
            reply(nats_,
                  msg,
                  raise_notification_response{
                      .result = notification_result(outcome::failed, "raise_failed", e.what())});
        }
    }

    void mine(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = decode<list_my_notifications_request>(msg);
        if (!req) {
            reply(nats_,
                  msg,
                  list_my_notifications_response{
                      .result = notification_result(
                          outcome::invalid, "bad_request", "The request could not be read.")});
            return;
        }
        try {
            service::notification_center center(*ctx);
            const auto me = center.actor_account_id();
            if (!me) {
                reply(nats_,
                      msg,
                      list_my_notifications_response{
                          .result = notification_result(outcome::denied,
                                                        "no_account",
                                                        "The signed-in account was not found.")});
                return;
            }
            auto page = center.mine(*me, req->unread_only, req->offset, req->limit);
            reply(nats_,
                  msg,
                  list_my_notifications_response{.result = notification_result(outcome::ok, "", ""),
                                                 .notifications = std::move(page.notifications),
                                                 .total = page.total});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(notification_operations_handler_lg(), error)
                << "Error reading one's notifications: " << e.what();
            reply(nats_,
                  msg,
                  list_my_notifications_response{
                      .result = notification_result(outcome::failed, "mine_failed", e.what())});
        }
    }

    void unread(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        try {
            service::notification_center center(*ctx);
            const auto me = center.actor_account_id();
            if (!me) {
                reply(nats_,
                      msg,
                      count_unread_notifications_response{
                          .result = notification_result(outcome::denied,
                                                        "no_account",
                                                        "The signed-in account was not found.")});
                return;
            }
            reply(nats_,
                  msg,
                  count_unread_notifications_response{.result =
                                                          notification_result(outcome::ok, "", ""),
                                                      .unread = center.unread(*me)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(notification_operations_handler_lg(), error)
                << "Error counting unread notifications: " << e.what();
            reply(nats_,
                  msg,
                  count_unread_notifications_response{
                      .result = notification_result(outcome::failed, "count_failed", e.what())});
        }
    }

    void mark_read(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = decode<mark_notifications_read_request>(msg);
        if (!req) {
            reply(nats_,
                  msg,
                  mark_notifications_read_response{
                      .result = notification_result(
                          outcome::invalid, "bad_request", "The request could not be read.")});
            return;
        }
        try {
            service::notification_center center(*ctx);
            const auto me = center.actor_account_id();
            if (!me) {
                reply(nats_,
                      msg,
                      mark_notifications_read_response{
                          .result = notification_result(outcome::denied,
                                                        "no_account",
                                                        "The signed-in account was not found.")});
                return;
            }
            reply(nats_,
                  msg,
                  mark_notifications_read_response{
                      .result = notification_result(outcome::ok, "", ""),
                      .marked = center.mark_read(*me, req->notification_ids)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(notification_operations_handler_lg(), error)
                << "Error marking notifications read: " << e.what();
            reply(nats_,
                  msg,
                  mark_notifications_read_response{
                      .result = notification_result(outcome::failed, "mark_failed", e.what())});
        }
    }

    void clear(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = decode<clear_notifications_request>(msg);
        if (!req) {
            reply(nats_,
                  msg,
                  clear_notifications_response{
                      .result = notification_result(
                          outcome::invalid, "bad_request", "The request could not be read.")});
            return;
        }
        try {
            service::notification_center center(*ctx);
            const auto me = center.actor_account_id();
            if (!me) {
                reply(nats_,
                      msg,
                      clear_notifications_response{
                          .result = notification_result(outcome::denied,
                                                        "no_account",
                                                        "The signed-in account was not found.")});
                return;
            }
            reply(
                nats_,
                msg,
                clear_notifications_response{.result = notification_result(outcome::ok, "", ""),
                                             .cleared = center.clear(*me, req->notification_ids)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(notification_operations_handler_lg(), error)
                << "Error clearing notifications: " << e.what();
            reply(nats_,
                  msg,
                  clear_notifications_response{
                      .result = notification_result(outcome::failed, "clear_failed", e.what())});
        }
    }

private:
    std::optional<ores::database::context> context_for(const ores::nats::message& msg) {
        BOOST_LOG_SEV(notification_operations_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            error_reply(nats_, msg, ctx.error());
            return std::nullopt;
        }
        return *ctx;
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
