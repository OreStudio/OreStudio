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
#include <optional>
#include <boost/throw_exception.hpp>
#include <sqlgen/postgres.hpp>
#include <magic_enum/magic_enum.hpp>
#include "ores.cli/config/export_options.hpp"
#include "ores.utility/streaming/std_vector.hpp"
#include "ores.utility/repository/context_factory.hpp"
#include "ores.utility/database/database_options.hpp"
#include "ores.risk/orexml/importer.hpp"
#include "ores.risk/orexml/exporter.hpp"
#include "ores.risk/repository/currency_repository.hpp"
#include "ores.cli/app/application_exception.hpp"
#include "ores.cli/app/application.hpp"

namespace ores::cli::app {

using risk::orexml::importer;
using risk::orexml::exporter;
using namespace ores::utility::log;
using ores::risk::domain::currency;
using risk::repository::currency_repository;
using connection = sqlgen::Result<rfl::Ref<sqlgen::postgres::Connection>>;

utility::repository::context application::make_context(
    const std::optional<utility::database::database_options>& db_opts) {
    using utility::repository::context_factory;

    if (!db_opts.has_value()) {
        BOOST_THROW_EXCEPTION(
            application_exception("Database configuration is required."));
    }

    const auto& db(db_opts.value());
    context_factory::configuration cfg {
        .user = db.user,
        .password = db.password,
        .host = db.host,
        .database = db.database,
        .port = db.port,
        .pool_size = 4,
        .num_attempts = 10,
        .wait_time_in_seconds = 1
    };

    return context_factory::make_context(cfg);
}

application::application(std::ostream& output_stream,
    const std::optional<utility::database::database_options>& db_opts)
    : output_stream_(output_stream), context_(make_context(db_opts)) {
    BOOST_LOG_SEV(lg(), debug) << "Creating application.";
}

void application::
import_currencies(const std::vector<std::filesystem::path> files) const {
    for (const auto& f : files) {
        BOOST_LOG_SEV(lg(), debug) << "Processing file: " << f;
        auto ccys(importer::import_currency_config(f));
        currency_repository rp;
        rp.write(context_, ccys);
        output_stream_ << ccys << std::endl;
    }
}

void application::
import_data(const std::optional<config::import_options>& ocfg) const {
    if (!ocfg.has_value()) {
        BOOST_LOG_SEV(lg(), debug) << "No importing configuration found.";
        return;
    }

    const auto& cfg(ocfg.value());
    switch (cfg.target_entity) {
        case config::entity::currencies:
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
    BOOST_LOG_SEV(lg(), debug) << "Exporting currency configurations.";
    risk::repository::currency_repository rp;

    const auto reader([&]() {
        if (cfg.all_versions) {
            BOOST_LOG_SEV(lg(), debug) << "Reading all versions for currencies.";
            if (cfg.key.empty())
                return rp.read_all(context_);
            else
                return rp.read_all(context_, cfg.key);
        } else if (cfg.as_of.empty()) {
            BOOST_LOG_SEV(lg(), debug) << "Reading latest currencies.";
            if (cfg.key.empty())
                return rp.read_latest(context_);
            else
                return rp.read_latest(context_, cfg.key);
        }
        BOOST_LOG_SEV(lg(), debug) << "Reading currencies as of: " << cfg.as_of;
        if (cfg.key.empty())
            return rp.read_at_timepoint(context_, cfg.as_of);
        else
            return rp.read_at_timepoint(context_, cfg.as_of, cfg.key);
    });

    const std::vector<currency> ccys(reader());
    if (cfg.target_format == config::format::xml) {
        std::string ccy_cfgs = exporter::export_currency_config(ccys);
        output_stream_ << ccy_cfgs << std::endl;
    } else if (cfg.target_format == config::format::json) {
        output_stream_ << ccys << std::endl;
    }
}

void application::
export_data(const std::optional<config::export_options>& ocfg) const {
    if (!ocfg.has_value()) {
        BOOST_LOG_SEV(lg(), debug) << "No dumping configuration found.";
        return;
    }

    const auto& cfg(ocfg.value());
    switch (cfg.target_entity) {
        case config::entity::currencies:
            export_currencies(cfg);
            break;
        default:
            BOOST_THROW_EXCEPTION(
                application_exception(std::format("Unsupported entity: {}",
                        magic_enum::enum_name(cfg.target_entity))));
    }
}

boost::asio::awaitable<void> application::run(const config::options& cfg) const {
    BOOST_LOG_SEV(lg(), info) << "Started application.";

    import_data(cfg.importing);
    export_data(cfg.exporting);

    BOOST_LOG_SEV(lg(), info) << "Finished application.";
    co_return;
}

}
