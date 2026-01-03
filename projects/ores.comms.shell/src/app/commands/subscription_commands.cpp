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
#include "ores.comms.shell/app/commands/subscription_commands.hpp"

#include <array>
#include <ostream>
#include <vector>
#include <functional>
#include <cli/cli.h>
#include "ores.platform/time/datetime.hpp"

namespace ores::comms::shell::app::commands {

using namespace ores::telemetry::log;
using comms::net::client_session;
using platform::time::datetime;

namespace {

/**
 * @brief Known event channels available for subscription.
 *
 * These are the event types that can be subscribed to via the listen command.
 * Event types follow the pattern: ores.<module>.<event_name>
 */
constexpr std::array known_channels = {
    std::pair{"ores.risk.currency_changed", "Currency data modified"},
    std::pair{"ores.iam.account_changed", "Account data modified"},
    std::pair{"ores.iam.role_assigned", "Role assigned to account"},
    std::pair{"ores.iam.role_revoked", "Role revoked from account"},
    std::pair{"ores.iam.permissions_changed", "Account permissions modified"},
    std::pair{"ores.variability.feature_flags_changed", "Feature flags modified"},
};

} // anonymous namespace

void subscription_commands::
register_commands(cli::Menu& root_menu, client_session& session) {
    auto events_menu = std::make_unique<cli::Menu>("events");

    events_menu->Insert("channels", [](std::ostream& out) {
        process_channels(out);
    }, "List available event channels");

    events_menu->Insert("listen", [&session](std::ostream& out,
            std::string event_type) {
        process_listen(std::ref(out), std::ref(session),
            std::move(event_type));
    }, "Subscribe to notifications for an event type");

    events_menu->Insert("unlisten", [&session](std::ostream& out,
            std::string event_type) {
        process_unlisten(std::ref(out), std::ref(session),
            std::move(event_type));
    }, "Unsubscribe from notifications (event_type or * for all)");

    events_menu->Insert("subscriptions", [&session](std::ostream& out) {
        process_subscriptions(std::ref(out), std::ref(session));
    }, "List active subscriptions");

    events_menu->Insert("notifications", [&session](std::ostream& out) {
        process_notifications(std::ref(out), std::ref(session));
    }, "Display and clear pending notifications");

    root_menu.Insert(std::move(events_menu));
}

void subscription_commands::
process_channels(std::ostream& out) {
    out << "Available event channels:" << std::endl;
    for (const auto& [channel, description] : known_channels) {
        out << "  " << channel << " - " << description << std::endl;
    }
    out << std::endl;
    out << "Use 'listen <channel>' to subscribe, or 'listen *' for all." << std::endl;
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
        out << "  Use 'listen *' to subscribe to all channels." << std::endl;
        return;
    }

    if (event_type == "*") {
        // Subscribe to all known channels
        std::size_t count = 0;
        std::vector<std::string> failed;
        for (const auto& [channel, _] : known_channels) {
            if (session.is_subscribed(channel)) {
                continue;  // Skip already subscribed
            }
            if (session.subscribe(channel)) {
                ++count;
            } else {
                failed.emplace_back(channel);
            }
        }
        out << "✓ Subscribed to " << count << " channel(s)." << std::endl;
        if (!failed.empty()) {
            out << "✗ Failed to subscribe to " << failed.size()
                << " channel(s):" << std::endl;
            for (const auto& ch : failed) {
                out << "  - " << ch << std::endl;
            }
        }
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
