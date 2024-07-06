/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 */
#include "ores.utility/log/logger.hpp"
#include "ores.console/application.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.console.application"));

}

namespace ores::console {

void application::
perform_importing(const std::optional<importing_configuration>& ocfg) const
{
    if (!ocfg.has_value())
    {
        BOOST_LOG_SEV(lg, debug) << "No importing configuration found.";
        return;
    }

    const auto& cfg(ocfg.value());
    for(const auto& ccy_cfg : cfg.currency_configurations())
    {
        BOOST_LOG_SEV(lg, debug) << "Processing currency configuration: "
                                 << ccy_cfg;
        xml_importer_.import_currency_config(ccy_cfg);
    }
}

void application::run(const configuration& cfg) const {
    BOOST_LOG_SEV(lg, info) << "Started application.";

    perform_importing(cfg.importing());

    BOOST_LOG_SEV(lg, info) << "Finished application.";
}

}
