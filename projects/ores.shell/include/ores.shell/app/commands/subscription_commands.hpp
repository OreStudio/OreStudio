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
#ifndef ORES_SHELL_APP_COMMANDS_SUBSCRIPTION_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_SUBSCRIPTION_COMMANDS_HPP

#include <string>
#include "ores.utility/log/make_logger.hpp"
#include "ores.comms/net/client_session.hpp"

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Manages commands related to event subscriptions and notifications.
 *
 * Provides PSQL-like LISTEN/NOTIFY functionality for the shell:
 * - listen: Subscribe to server notifications for an event type
 * - unlisten: Unsubscribe from notifications
 * - subscriptions: List active subscriptions
 * - notifications: Display pending notifications
 */
class subscription_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.subscription";

    auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register subscription management commands.
     *
     * Adds listen, unlisten, subscriptions, and notifications commands
     * to the root menu.
     *
     * @param root The root menu to add commands to
     * @param session Client session for server communication
     */
    static void register_commands(cli::Menu& root,
        comms::net::client_session& session);

    /**
     * @brief Process a listen (subscribe) request.
     *
     * @param out Output stream for user feedback
     * @param session Client session for server communication
     * @param event_type The event type to subscribe to
     */
    static void process_listen(std::ostream& out,
        comms::net::client_session& session,
        std::string event_type);

    /**
     * @brief Process an unlisten (unsubscribe) request.
     *
     * @param out Output stream for user feedback
     * @param session Client session for server communication
     * @param event_type The event type to unsubscribe from (empty = all)
     */
    static void process_unlisten(std::ostream& out,
        comms::net::client_session& session,
        std::string event_type);

    /**
     * @brief Display current subscriptions.
     *
     * @param out Output stream for user feedback
     * @param session Client session for server communication
     */
    static void process_subscriptions(std::ostream& out,
        comms::net::client_session& session);

    /**
     * @brief Display and clear pending notifications.
     *
     * @param out Output stream for user feedback
     * @param session Client session for server communication
     */
    static void process_notifications(std::ostream& out,
        comms::net::client_session& session);

    /**
     * @brief Display pending notifications without clearing them.
     *
     * Helper function that can be called after each command to show
     * notifications like PSQL does.
     *
     * @param out Output stream for user feedback
     * @param session Client session for server communication
     * @return Number of notifications displayed
     */
    static std::size_t display_pending_notifications(std::ostream& out,
        comms::net::client_session& session);
};

}

#endif
