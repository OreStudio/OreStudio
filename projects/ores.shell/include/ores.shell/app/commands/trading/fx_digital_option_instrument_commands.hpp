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
#ifndef ORES_SHELL_APP_COMMANDS_TRADING_FX_DIGITAL_OPTION_INSTRUMENT_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_TRADING_FX_DIGITAL_OPTION_INSTRUMENT_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.shell/app/pagination_context.hpp"
#include <string>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Manages commands related to FX DIGITAL OPTION INSTRUMENTS.
 */
class fx_digital_option_instrument_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.trading.fx_digital_option_instrument_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register fx-digital-option-instrument related commands.
     */
    static void register_commands(cli::Menu& root_menu,
                                  ores::nats::service::nats_client& session,
                                  pagination_context& pagination);

    /**
     * @brief Process a get fx digital option instruments request.
     */
    static void process_get_fx_digital_option_instruments(std::ostream& out,
                                                          ores::nats::service::nats_client& session,
                                                          pagination_context& pagination);

    /**
     * @brief Process an add fx digital option instrument request.
     *
     * The instrument id is minted client-side; the tenant and party
     * scope come from the logged-in session. The audit change fields
     * arrive as the trailing arguments, matching the reference-entity
     * add verbs.
     */
    static void process_add_fx_digital_option_instrument(std::ostream& out,
                                                         ores::nats::service::nats_client& session,
                                                         std::string trade_type_code,
                                                         std::string foreign_currency,
                                                         std::string domestic_currency,
                                                         std::string payoff_currency,
                                                         double payoff_amount,
                                                         std::string option_type,
                                                         std::string expiry_date,
                                                         std::string long_short,
                                                         std::string strike,
                                                         std::string barrier_type,
                                                         std::string lower_barrier,
                                                         std::string upper_barrier,
                                                         std::string description,
                                                         std::string change_reason_code,
                                                         std::string change_commentary);

    /**
     * @brief Process a delete fx digital option instrument request.
     */
    static void process_delete_fx_digital_option_instrument(std::ostream& out,
        ores::nats::service::nats_client& session,
        std::string instrument_id);

    /**
     * @brief Process an fx digital option instrument history request.
     *
     * Unlike the reference entities, instruments have no ores.history
     * provider: history renders over the per-product subject, so the
     * diff flags are not offered.
     */
    static void process_get_fx_digital_option_instrument_history(std::ostream& out,
        ores::nats::service::nats_client& session,
        std::string instrument_id);
};

}

#endif
