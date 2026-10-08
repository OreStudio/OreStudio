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
#include "ores.inbox.core/messaging/registrar.hpp"
#include "ores.history.core/messaging/registrar.hpp"
#include "ores.history.core/service/dispatch_registry.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.inbox.api/messaging/notification_operations_protocol.hpp"
#include "ores.inbox.core/messaging/approval_decision_history_provider_registrar.hpp"
#include "ores.inbox.core/messaging/approval_decision_registrar.hpp"
#include "ores.inbox.core/messaging/approval_decision_type_history_provider_registrar.hpp"
#include "ores.inbox.core/messaging/approval_decision_type_registrar.hpp"
#include "ores.inbox.core/messaging/approval_kind_history_provider_registrar.hpp"
#include "ores.inbox.core/messaging/approval_kind_registrar.hpp"
#include "ores.inbox.core/messaging/approval_operations_handler.hpp"
#include "ores.inbox.core/messaging/approval_request_history_provider_registrar.hpp"
#include "ores.inbox.core/messaging/approval_request_registrar.hpp"
#include "ores.inbox.core/messaging/approval_request_state_history_provider_registrar.hpp"
#include "ores.inbox.core/messaging/approval_request_state_registrar.hpp"
#include "ores.inbox.core/messaging/delivery_outcome_type_registrar.hpp"
#include "ores.inbox.core/messaging/notification_argument_registrar.hpp"
#include "ores.inbox.core/messaging/notification_channel_history_provider_registrar.hpp"
#include "ores.inbox.core/messaging/notification_channel_registrar.hpp"
#include "ores.inbox.core/messaging/notification_delivery_history_provider_registrar.hpp"
#include "ores.inbox.core/messaging/notification_delivery_registrar.hpp"
#include "ores.inbox.core/messaging/notification_history_provider_registrar.hpp"
#include "ores.inbox.core/messaging/notification_kind_history_provider_registrar.hpp"
#include "ores.inbox.core/messaging/notification_kind_registrar.hpp"
#include "ores.inbox.core/messaging/notification_operations_handler.hpp"
#include "ores.inbox.core/messaging/notification_preference_history_provider_registrar.hpp"
#include "ores.inbox.core/messaging/notification_preference_registrar.hpp"
#include "ores.inbox.core/messaging/notification_recipient_registrar.hpp"
#include "ores.inbox.core/messaging/notification_registrar.hpp"
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::inbox::messaging {

namespace {

auto& lg() {
    static auto instance = ores::logging::make_logger("ores.inbox.messaging.registrar");
    return instance;
}

constexpr std::string_view queue_group = "ores.inbox.service";

// The registry must outlive the history.v1.get subscription, and
// register_handlers is only ever called once per service process.
ores::history::service::dispatch_registry& history_registry() {
    static ores::history::service::dispatch_registry instance;
    return instance;
}

} // namespace

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier,
                             std::chrono::seconds answered_window) {

    std::vector<ores::nats::service::subscription> subs;
    const auto add = [&subs](std::vector<ores::nats::service::subscription> more) {
        for (auto& sub : more)
            subs.push_back(std::move(sub));
    };

    // Every inbox entity stack is generated: the CRUD, version and change
    // verbs come from each entity's registrar.
    add(register_approval_kind_handlers(nats, ctx, verifier));
    add(register_approval_request_state_handlers(nats, ctx, verifier));
    add(register_approval_decision_type_handlers(nats, ctx, verifier));
    add(register_approval_request_handlers(nats, ctx, verifier));
    add(register_approval_decision_handlers(nats, ctx, verifier));
    add(register_notification_kind_handlers(nats, ctx, verifier));
    add(register_notification_channel_handlers(nats, ctx, verifier));
    add(register_delivery_outcome_type_handlers(nats, ctx, verifier));
    add(register_notification_handlers(nats, ctx, verifier));
    add(register_notification_argument_handlers(nats, ctx, verifier));
    add(register_notification_recipient_handlers(nats, ctx, verifier));
    add(register_notification_delivery_handlers(nats, ctx, verifier));
    add(register_notification_preference_handlers(nats, ctx, verifier));

    // The approval lifecycle is operations rather than entity verbs: raising,
    // withdrawing and deciding apply the lifecycle rules, and the queue and
    // one's own requests are reads a person works from.
    {
        auto h = std::make_shared<approval_operations_handler>(
            nats, ctx, verifier, answered_window);
        subs.push_back(
            nats.queue_subscribe(raise_approval_request_request::nats_subject,
                                 queue_group,
                                 [h](ores::nats::message msg) { h->raise(std::move(msg)); }));
        subs.push_back(
            nats.queue_subscribe(withdraw_approval_request_request::nats_subject,
                                 queue_group,
                                 [h](ores::nats::message msg) { h->withdraw(std::move(msg)); }));
        subs.push_back(
            nats.queue_subscribe(decide_approval_request_request::nats_subject,
                                 queue_group,
                                 [h](ores::nats::message msg) { h->decide(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            list_approval_queue_request::nats_subject, queue_group, [h](ores::nats::message msg) {
                h->queue(std::move(msg));
            }));
        subs.push_back(nats.queue_subscribe(
            get_approval_request::nats_subject, queue_group, [h](ores::nats::message msg) {
                h->get_request(std::move(msg));
            }));
        subs.push_back(nats.queue_subscribe(get_approval_history_request::nats_subject,
                                            queue_group,
                                            [h](ores::nats::message msg) {
                                                h->get_history(std::move(msg));
                                            }));
        subs.push_back(
            nats.queue_subscribe(list_my_approval_requests_request::nats_subject,
                                 queue_group,
                                 [h](ores::nats::message msg) { h->mine(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            expire_overdue_approvals_request::nats_subject,
            queue_group,
            [h](ores::nats::message msg) { h->expire_overdue(std::move(msg)); }));
        BOOST_LOG_SEV(lg(), info) << "inbox registrar: approval operations subscribed, "
                                  << "including get_approval at "
                                  << get_approval_request::nats_subject;
    }

    // Notifications: a component raises one, and a person reads, marks and
    // clears their own.
    {
        auto h = std::make_shared<notification_operations_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            raise_notification_request::nats_subject, queue_group, [h](ores::nats::message msg) {
                h->raise(std::move(msg));
            }));
        subs.push_back(nats.queue_subscribe(
            list_my_notifications_request::nats_subject, queue_group, [h](ores::nats::message msg) {
                h->mine(std::move(msg));
            }));
        subs.push_back(
            nats.queue_subscribe(count_unread_notifications_request::nats_subject,
                                 queue_group,
                                 [h](ores::nats::message msg) { h->unread(std::move(msg)); }));
        subs.push_back(
            nats.queue_subscribe(mark_notifications_read_request::nats_subject,
                                 queue_group,
                                 [h](ores::nats::message msg) { h->mark_read(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            clear_notifications_request::nats_subject, queue_group, [h](ores::nats::message msg) {
                h->clear(std::move(msg));
            }));
    }

    // Inbox history comes from the generic history provider.
    {
        auto& hist_registry = history_registry();
        register_approval_kind_history_provider(hist_registry);
        register_approval_request_state_history_provider(hist_registry);
        register_approval_decision_type_history_provider(hist_registry);
        register_approval_request_history_provider(hist_registry);
        register_approval_decision_history_provider(hist_registry);
        register_notification_kind_history_provider(hist_registry);
        register_notification_channel_history_provider(hist_registry);
        register_notification_history_provider(hist_registry);
        register_notification_delivery_history_provider(hist_registry);
        register_notification_preference_history_provider(hist_registry);
        subs.push_back(ores::history::messaging::register_history_handlers(
            nats, hist_registry, "inbox", queue_group, ctx, verifier));
    }

    BOOST_LOG_SEV(lg(), info) << "inbox registrar: registered " << subs.size()
                              << " subscription(s).";

    return subs;
}

}
