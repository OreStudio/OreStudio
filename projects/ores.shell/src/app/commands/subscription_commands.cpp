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
#include "ores.shell/app/commands/subscription_commands.hpp"

#include <ostream>
#include <vector>
#include <functional>
#include <cli/cli.h>
#include "ores.utility/datetime/datetime.hpp"

namespace ores::shell::app::commands {

using namespace ores::utility::log;
using comms::net::client_session;
using utility::datetime::datetime;

void subscription_commands::
register_commands(cli::Menu& root_menu, client_session& session) {
    root_menu.Insert("listen", [&session](std::ostream& out,
            std::string event_type) {
        process_listen(std::ref(out), std::ref(session),
            std::move(event_type));
    }, "Subscribe to notifications for an event type");

    root_menu.Insert("unlisten", [&session](std::ostream& out,
            std::string event_type) {
        process_unlisten(std::ref(out), std::ref(session),
            std::move(event_type));
    }, "Unsubscribe from notifications (event_type or * for all)");

    root_menu.Insert("subscriptions", [&session](std::ostream& out) {
        process_subscriptions(std::ref(out), std::ref(session));
    }, "List active subscriptions");

    root_menu.Insert("notifications", [&session](std::ostream& out) {
        process_notifications(std::ref(out), std::ref(session));
    }, "Display and clear pending notifications");
}

void subscription_commands::
process_listen(std::ostream& out, client_session& session,
    std::string event_type) {

    if (!session.is_connected()) {
        out << "✗ Not connected to server." << std::endl;
        return;
    }

    if (event_type.empty()) {
        out << "✗ Event type required. Usage: listen <event_type>" << std::endl;
        out << "  Example: listen ores.risk.currency_changed" << std::endl;
        return;
    }

    if (session.is_subscribed(event_type)) {
        out << "⚠ Already subscribed to " << event_type << std::endl;
        return;
    }

    if (session.subscribe(event_type)) {
        out << "✓ Listening on " << event_type << std::endl;
    } else {
        out << "✗ Failed to subscribe to " << event_type << std::endl;
    }
}

void subscription_commands::
process_unlisten(std::ostream& out, client_session& session,
    std::string event_type) {

    if (!session.is_connected()) {
        out << "✗ Not connected to server." << std::endl;
        return;
    }

    auto subscriptions = session.get_subscriptions();

    if (event_type.empty() || event_type == "*") {
        // Unsubscribe from all
        if (subscriptions.empty()) {
            out << "⚠ No active subscriptions." << std::endl;
            return;
        }

        std::size_t count = 0;
        std::vector<std::string> failed;
        for (const auto& sub : subscriptions) {
            if (session.unsubscribe(sub)) {
                ++count;
            } else {
                failed.push_back(sub);
            }
        }
        out << "✓ Unsubscribed from " << count << " event type(s)." << std::endl;
        if (!failed.empty()) {
            out << "✗ Failed to unsubscribe from " << failed.size()
                << " event type(s):" << std::endl;
            for (const auto& sub : failed) {
                out << "  - " << sub << std::endl;
            }
        }
    } else {
        // Unsubscribe from specific event type
        if (!session.is_subscribed(event_type)) {
            out << "⚠ Not subscribed to " << event_type << std::endl;
            return;
        }

        if (session.unsubscribe(event_type)) {
            out << "✓ Stopped listening on " << event_type << std::endl;
        } else {
            out << "✗ Failed to unsubscribe from " << event_type << std::endl;
        }
    }
}

void subscription_commands::
process_subscriptions(std::ostream& out, client_session& session) {
    if (!session.is_connected()) {
        out << "✗ Not connected to server." << std::endl;
        return;
    }

    auto subscriptions = session.get_subscriptions();

    if (subscriptions.empty()) {
        out << "No active subscriptions." << std::endl;
        return;
    }

    out << "Active subscriptions:" << std::endl;
    for (const auto& sub : subscriptions) {
        out << "  • " << sub << std::endl;
    }
}

void subscription_commands::
process_notifications(std::ostream& out, client_session& session) {
    auto notifications = session.take_pending_notifications();

    if (notifications.empty()) {
        out << "No pending notifications." << std::endl;
        return;
    }

    out << "Received " << notifications.size() << " notification(s):" << std::endl;
    for (const auto& notif : notifications) {
        out << "  Async notification \"" << notif.event_type
            << "\" received at " << datetime::format_time_point(notif.timestamp)
            << std::endl;
    }
}

std::size_t subscription_commands::
display_pending_notifications(std::ostream& out, client_session& session) {
    auto notifications = session.take_pending_notifications();

    for (const auto& notif : notifications) {
        out << "Asynchronous notification \"" << notif.event_type
            << "\" received from server at " << datetime::format_time_point(notif.timestamp)
            << "." << std::endl;
    }

    return notifications.size();
}

}
