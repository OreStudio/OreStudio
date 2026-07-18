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
#ifndef ORES_SHELL_APP_COMMANDS_CRM_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_CRM_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <string>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Commands for the FX cross-rates matrix (CRM).
 *
 * ores.shell's entry point into marketdata.v1.crm.rates -- the same
 * request/response the Qt Cross-Rates Matrix window uses -- formatted
 * via crm_rate_display_service/crm_rate_formatter so the shell doesn't
 * repeat the reciprocal/delta/precision logic those already solved.
 */
class crm_commands {
private:
    inline static std::string_view logger_name = "ores.shell.app.commands.crm_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register CRM commands.
     *
     * Creates the crm submenu with the rates operation.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief List CRM rates: crm rates [<crm-name>] [--party
     * <id-or-full-name>] [--reciprocal] [--matrix].
     *
     * <crm-name> is optional; omitted, every enabled CRM the party has
     * is returned, each row tagged with the CRM it came from. Runs in
     * the logged-in account's own default party by default (same as
     * ClientManager::currentPartyId() on the Qt side); --party
     * overrides that for an account that manages more than one party,
     * accepting either a UUID or an exact full name resolved the same
     * way `provision party <party>` does. --reciprocal backfills a pair
     * with no direct quote from its reverse pair's computed reciprocal,
     * same as the Qt matrix's own "Show Reciprocal" toggle. --matrix
     * renders an NxN grid (base currencies as rows, quote as columns)
     * instead of the default flat list, mirroring the Qt Cross-Rates
     * Matrix window's own layout -- requires a named CRM, since a grid
     * can't hold rows from more than one CRM at once.
     */
    static void process_rates(std::ostream& out,
                              ores::nats::service::nats_client& session,
                              const std::vector<std::string>& args);
};

}

#endif
