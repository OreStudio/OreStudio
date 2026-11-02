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
#ifndef ORES_SHELL_APP_COMMANDS_MARKETDATA_PUBLIC_TERMS_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_MARKETDATA_PUBLIC_TERMS_COMMANDS_HPP

#include "ores.nats/service/nats_client.hpp"
#include <ostream>

namespace cli {
class Menu;
}

namespace ores::shell::app::commands {

/**
 * @brief Accepting the terms of every public source at once.
 *
 * A new system fetches no public fixings until someone accepts each source's terms. The
 * setup of a system does this for the administrator it creates, and a system set up
 * without it does it with @c provider_terms_operations @c accept-all. Each provider that
 * states terms is accepted as it stands, through the service's one accept, so the record
 * names the signed-in account and the server's time.
 */
class public_terms_commands {
public:
    /**
     * @brief Adds @c accept-all to the @c provider_terms_operations menu.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief Accepts the terms of every provider that states them, one line a provider.
     *
     * @return true when every one was accepted, or there was none to accept.
     */
    static bool accept_all(std::ostream& out, ores::nats::service::nats_client& session);
};

}

#endif
