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
#include <iostream>
#include "ores.utility/log/logger.hpp"
#include "ores.core/db/currency_table.hpp"
#include "ores.core/types/currency_config.hpp"
#include "ores.core/json/currency_config_serialiser.hpp"
#include "ores.console/application.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.console.application"));

}

namespace ores::console {

void application::
import_data(const std::optional<importing_configuration>& ocfg) const
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
        auto cc(importer_.import_currency_config(ccy_cfg));
        core::db::currency_table ct;
        ct.write(cc.currencies());
        using core::json::currency_config_serialiser;
        std::cout << currency_config_serialiser::serialise(cc) << std::endl;
    }
}

void application::
dump_data(const std::optional<dumping_configuration>& ocfg) const
{
    if (!ocfg.has_value())
    {
        BOOST_LOG_SEV(lg, debug) << "No dumping configuration found.";
        return;
    }

    const auto& cfg(ocfg.value());
    if (cfg.currency_configurations())
    {
        BOOST_LOG_SEV(lg, debug) << "Dumping currency configurations.";
        core::db::currency_table ct;

        using ores::core::types::currency_config;
        using core::json::currency_config_serialiser;
        const auto reader([&]() {
            if (cfg.all_versions()) {
                BOOST_LOG_SEV(lg, debug) << "Reading all versions for currencies.";
                return ct.read_all(cfg.key());
            } else if (cfg.as_of().empty()) {
                BOOST_LOG_SEV(lg, debug) << "Reading latest currencies.";
                return ct.read_latest(cfg.key());
            }
            BOOST_LOG_SEV(lg, debug) << "Reading currencies at timepoint: "
                                     << cfg.as_of();
            return ct.read_at_timepoint(cfg.as_of(), cfg.key());
        });
        const currency_config cc(reader());
        std::cout << currency_config_serialiser::serialise(cc) << std::endl;
    }
}

void application::run(const configuration& cfg) const {
    BOOST_LOG_SEV(lg, info) << "Started application.";

    import_data(cfg.importing());
    dump_data(cfg.dumping());

    BOOST_LOG_SEV(lg, info) << "Finished application.";
}

}
