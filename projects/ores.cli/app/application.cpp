/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include <optional>
#include <boost/throw_exception.hpp>
#include <sqlgen/postgres.hpp>
#include <magic_enum/magic_enum.hpp>
#include "ores.cli/config/export_options.hpp"
#include "ores.utility/log/logger.hpp"
#include "ores.risk/db/currency_table.hpp"
#include "ores.risk/types/currency_config.hpp"
#include "ores.cli/app/application_exception.hpp"
#include "ores.cli/app/application.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.cli.application"));

}

namespace ores::cli::app {

using connection = sqlgen::Result<rfl::Ref<sqlgen::postgres::Connection>>;

namespace {

connection connect() {
    const auto credentials = sqlgen::postgres::Credentials {
        .user = "ores",
        .password = "ahV6aehuij6eingohsiajaiT0",
        .host = "localhost",
        .dbname = "oresdb",
        .port = 5434
    };
    BOOST_LOG_SEV(lg, debug) << "connected";

    return sqlgen::postgres::connect(credentials);
}

}

void application::
import_currencies(const std::vector<std::filesystem::path> files) const {
    for(const auto& f : files)
    {
        BOOST_LOG_SEV(lg, debug) << "Processing file: " << f;
        auto cc(importer_.import_currency_config(f));
        risk::db::currency_table ct;
        ct.write(connect(), cc.currencies);
        std::cout << cc << std::endl;
    }
}


void application::
import_data(const std::optional<config::import_options>& ocfg) const {
    if (!ocfg.has_value()) {
        BOOST_LOG_SEV(lg, debug) << "No importing configuration found.";
        return;
    }

    const auto& cfg(ocfg.value());
    switch (cfg.target_entity) {
        case config::entity::currency_config:
            import_currencies(cfg.targets);
            break;
        default:
            BOOST_THROW_EXCEPTION(
                application_exception(std::format("Unsupported entity: {}",
                        magic_enum::enum_name(cfg.target_entity))));
    }
}

void application::
export_currencies(const config::export_options& cfg) const {
    BOOST_LOG_SEV(lg, debug) << "Exporting currency configurations.";
    risk::db::currency_table ct;

    using ores::risk::types::currency_config;
    const auto reader([&]() {
        if (cfg.all_versions) {
            BOOST_LOG_SEV(lg, debug) << "Reading all versions for currencies.";
            return ct.read_all(connect(), cfg.key);
        } else if (cfg.as_of.empty()) {
            BOOST_LOG_SEV(lg, debug) << "Reading latest currencies.";
            return ct.read_latest(connect(), cfg.key);
        }
        BOOST_LOG_SEV(lg, debug) << "Reading currencies as of: " << cfg.as_of;
        return ct.read_at_timepoint(connect(), cfg.as_of, cfg.key);
    });
    const currency_config cc(reader());
    std::cout << cc << std::endl;
}

void application::
export_data(const std::optional<config::export_options>& ocfg) const {
    if (!ocfg.has_value()) {
        BOOST_LOG_SEV(lg, debug) << "No dumping configuration found.";
        return;
    }

    const auto& cfg(ocfg.value());
    switch (cfg.target_entity) {
        case config::entity::currency_config:
            export_currencies(cfg);
            break;
        default:
            BOOST_THROW_EXCEPTION(
                application_exception(std::format("Unsupported entity: {}",
                        magic_enum::enum_name(cfg.target_entity))));
    }
}

void application::run(const config::options& cfg) const {
    BOOST_LOG_SEV(lg, info) << "Started application.";

    import_data(cfg.importing);
    export_data(cfg.exporting);

    BOOST_LOG_SEV(lg, info) << "Finished application.";
}

}
