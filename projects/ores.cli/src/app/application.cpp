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
#include "ores.cli/app/application.hpp"

#include <optional>
#include <boost/lexical_cast.hpp>
#include <boost/throw_exception.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/postgres.hpp>
#include <magic_enum/magic_enum.hpp>
#include "ores.cli/config/export_options.hpp"
#include "ores.utility/streaming/std_vector.hpp"
#include "ores.utility/repository/context_factory.hpp"
#include "ores.utility/database/database_options.hpp"
#include "ores.risk/orexml/importer.hpp"
#include "ores.risk/orexml/exporter.hpp"
#include "ores.risk/csv/exporter.hpp"
#include "ores.risk/domain/currency_table_io.hpp"
#include "ores.risk/repository/currency_repository.hpp"
#include "ores.accounts/domain/account_table_io.hpp"
#include "ores.accounts/domain/feature_flags_table_io.hpp"
#include "ores.accounts/repository/account_repository.hpp"
#include "ores.cli/app/application_exception.hpp"

namespace ores::cli::app {

using ore_importer = risk::orexml::importer;
using ore_exporter = risk::orexml::exporter;
using csv_exporter = risk::csv::exporter;
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
        auto ccys(ore_importer::import_currency_config(f));
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
        std::string ccy_cfgs = ore_exporter::export_currency_config(ccys);
        output_stream_ << ccy_cfgs << std::endl;
    } else if (cfg.target_format == config::format::json) {
        output_stream_ << ccys << std::endl;
    } else if (cfg.target_format == config::format::csv) {
        std::string ccy_cfgs = csv_exporter::export_currency_config(ccys);
        output_stream_ << ccy_cfgs << std::endl;
    } else if (cfg.target_format == config::format::table) {
        risk::domain::print_currency_table(output_stream_, ccys);
    }
}

void application::
export_accounts(const config::export_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Exporting accounts.";
    accounts::repository::account_repository repo(context_);

    const auto reader([&]() {
        if (cfg.all_versions) {
            BOOST_LOG_SEV(lg(), debug) << "Reading all versions for accounts.";
            // Note: account repository doesn't support reading all versions by username yet
            if (cfg.key.empty())
                return repo.read_all();
            else
                return repo.read_latest_by_username(cfg.key);
        } else if (cfg.as_of.empty()) {
            BOOST_LOG_SEV(lg(), debug) << "Reading latest accounts.";
            if (cfg.key.empty())
                return repo.read_latest();
            else
                return repo.read_latest_by_username(cfg.key);
        }
        BOOST_LOG_SEV(lg(), debug) << "Reading accounts as of: " << cfg.as_of;
        // Note: account repository doesn't have read_at_timepoint yet
        return repo.read_latest();
    });

    const std::vector<accounts::domain::account> accts(reader());
    if (cfg.target_format == config::format::json) {
        output_stream_ << accts << std::endl;
    } else if (cfg.target_format == config::format::table) {
        accounts::domain::print_account_table(output_stream_, accts);
    }
}

void application::
export_feature_flags(const config::export_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Exporting feature flags.";
    // TODO: Implement when feature_flags repository is available
    BOOST_THROW_EXCEPTION(
        application_exception("Feature flags export not yet implemented"));
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
        case config::entity::accounts:
            export_accounts(cfg);
            break;
        case config::entity::feature_flags:
            export_feature_flags(cfg);
            break;
        default:
            BOOST_THROW_EXCEPTION(
                application_exception(std::format("Unsupported entity: {}",
                        magic_enum::enum_name(cfg.target_entity))));
    }
}

void application::run(const config::options& cfg) const {
    BOOST_LOG_SEV(lg(), info) << "Started application.";

    import_data(cfg.importing);
    export_data(cfg.exporting);
    delete_data(cfg.deleting);

    BOOST_LOG_SEV(lg(), info) << "Finished application.";
    return;
}

void application::
delete_account(const config::delete_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Deleting account: " << cfg.key;
    accounts::repository::account_repository repo(context_);

    // Try to parse as UUID first
    boost::uuids::uuid account_id;
    try {
        account_id = boost::lexical_cast<boost::uuids::uuid>(cfg.key);
        BOOST_LOG_SEV(lg(), debug) << "Parsed key as UUID: "
                                   << boost::uuids::to_string(account_id);
    } catch (const boost::bad_lexical_cast&) {
        // If not a UUID, treat as username and look it up
        BOOST_LOG_SEV(lg(), debug) << "Key is not a UUID, treating as username";
        const auto accounts = repo.read_latest_by_username(cfg.key);
        if (accounts.empty()) {
            BOOST_THROW_EXCEPTION(
                application_exception(std::format("Account not found: {}", cfg.key)));
        }
        account_id = accounts.front().id;
        BOOST_LOG_SEV(lg(), debug) << "Found account ID: "
                                   << boost::uuids::to_string(account_id);
    }

    repo.remove(account_id);
    output_stream_ << "Account deleted successfully: "
                   << boost::uuids::to_string(account_id) << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Deleted account: "
                              << boost::uuids::to_string(account_id);
}

void application::
delete_data(const std::optional<config::delete_options>& ocfg) const {
    if (!ocfg.has_value()) {
        BOOST_LOG_SEV(lg(), debug) << "No deletion configuration found.";
        return;
    }

    const auto& cfg(ocfg.value());
    switch (cfg.target_entity) {
        case config::entity::accounts:
            delete_account(cfg);
            break;
        default:
            BOOST_THROW_EXCEPTION(
                application_exception(std::format("Delete not supported for entity: {}",
                        magic_enum::enum_name(cfg.target_entity))));
    }
}

}
