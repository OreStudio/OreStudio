/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
// subscribeToEvent() and unsubscribeFromEvent() are isolated here so that the
// rfl::json::read<entity_change_event> instantiation does not contribute to
// the per-TU template depth in ClientManager.cpp (MSVC C1202).
#include "ores.qt/ClientManager.hpp"

#include <QDateTime>
#include <QTimeZone>
#include "ores.nats/service/client.hpp"
#include "ores.eventing/domain/entity_change_event.hpp"

namespace ores::qt {

using namespace ores::logging;

void ClientManager::subscribeToEvent(const std::string& subject) {
    if (nats_subscriptions_.count(subject))
        return;

    auto cl = session_.get_client();
    if (!cl) {
        BOOST_LOG_SEV(lg(), warn) << "subscribeToEvent: not connected, skipping '"
                                  << subject << "'";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Subscribing to NATS event: " << subject;
    try {
        auto sub = cl->subscribe(subject,
            [this, subject](nats::message msg) {
                try {
                    const std::string_view json(
                        reinterpret_cast<const char*>(msg.data.data()),
                        msg.data.size());
                    auto result =
                        rfl::json::read<eventing::domain::entity_change_event>(json);
                    if (!result) {
                        BOOST_LOG_SEV(lg(), warn)
                            << "Failed to parse event on '" << subject << "': "
                            << result.error().what();
                        return;
                    }
                    const auto& ev = *result;
                    const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
                        ev.timestamp.time_since_epoch()).count();
                    QDateTime ts = QDateTime::fromMSecsSinceEpoch(ms, QTimeZone::UTC);
                    QStringList ids;
                    ids.reserve(static_cast<int>(ev.entity_ids.size()));
                    for (const auto& id : ev.entity_ids)
                        ids.append(QString::fromStdString(id));
                    QString eventType = QString::fromStdString(subject);
                    QString tenantId  = QString::fromStdString(ev.tenant_id);
                    QMetaObject::invokeMethod(this,
                        [this, eventType, ts, ids, tenantId]() {
                            emit notificationReceived(
                                eventType, ts, ids, tenantId, 0, {});
                        }, Qt::QueuedConnection);
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Exception in event handler for '" << subject
                        << "': " << e.what();
                }
            });
        nats_subscriptions_.emplace(subject, std::move(sub));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to subscribe to '" << subject
                                   << "': " << e.what();
    }
}

void ClientManager::unsubscribeFromEvent(const std::string& subject) {
    if (nats_subscriptions_.erase(subject) > 0) {
        try {
            BOOST_LOG_SEV(lg(), debug) << "Unsubscribed from event: " << subject;
        } catch (...) {
            // Boost.Log may already be torn down during static destruction.
        }
    }
}

}
