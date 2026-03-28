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
#include "ores.shell/app/commands/subscription_commands.hpp"

#include <ostream>
#include <functional>
#include <cli/cli.h>

namespace ores::shell::app::commands {

using ores::nats::service::nats_client;

void subscription_commands::
register_commands(cli::Menu& root_menu, nats_client& session) {
    auto events_menu = std::make_unique<cli::Menu>("events");

    events_menu->Insert("channels", [&session](std::ostream& out) {
        process_channels(std::ref(out), std::ref(session));
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
process_channels(std::ostream& out, nats_client& /*session*/) {
    out << "Event subscriptions are not available in NATS mode." << std::endl;
}

void subscription_commands::
process_listen(std::ostream& out, nats_client& /*session*/,
    std::string /*event_type*/) {
    out << "Event subscriptions are not available in NATS mode." << std::endl;
}

void subscription_commands::
process_unlisten(std::ostream& out, nats_client& /*session*/,
    std::string /*event_type*/) {
    out << "Event subscriptions are not available in NATS mode." << std::endl;
}

void subscription_commands::
process_subscriptions(std::ostream& out, nats_client& /*session*/) {
    out << "Event subscriptions are not available in NATS mode." << std::endl;
}

void subscription_commands::
process_notifications(std::ostream& out, nats_client& /*session*/) {
    out << "No pending notifications." << std::endl;
}

std::size_t subscription_commands::
display_pending_notifications(std::ostream& /*out*/, nats_client& /*session*/) {
    return 0;
}

}
