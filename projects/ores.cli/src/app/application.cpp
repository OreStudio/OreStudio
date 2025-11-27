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

#include <chrono>
#include <optional>
#include <boost/lexical_cast.hpp>
#include <boost/throw_exception.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <rfl/json.hpp>
#include <sqlgen/postgres.hpp>
#include <magic_enum/magic_enum.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.cli/config/export_options.hpp"
#include "ores.utility/streaming/std_vector.hpp"
#include "ores.utility/repository/context_factory.hpp"
#include "ores.utility/database/database_options.hpp"
#include "ores.utility/datetime/datetime.hpp"
#include "ores.risk/orexml/importer.hpp"
#include "ores.risk/orexml/exporter.hpp"
#include "ores.accounts/security/password_policy_validator.hpp"
#include "ores.risk/csv/exporter.hpp"
#include "ores.risk/domain/currency_table.hpp"
#include "ores.risk/domain/currency_json.hpp"
#include "ores.risk/repository/currency_repository.hpp"
#include "ores.accounts/service/bootstrap_mode_service.hpp"
#include "ores.accounts/domain/account_table.hpp"
#include "ores.accounts/domain/account_json.hpp"
#include "ores.accounts/domain/feature_flags_table.hpp"
#include "ores.accounts/domain/feature_flags_json.hpp"
#include "ores.accounts/repository/account_repository.hpp"
#include "ores.accounts/repository/feature_flags_repository.hpp"
#include "ores.accounts/security/password_manager.hpp"
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

    context_factory::configuration cfg {
        .database_options = db_opts.value(),
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
        output_stream_ << risk::domain::convert_to_json(ccys) << std::endl;
    } else if (cfg.target_format == config::format::csv) {
        std::string ccy_cfgs = csv_exporter::export_currency_config(ccys);
        output_stream_ << ccy_cfgs << std::endl;
    } else if (cfg.target_format == config::format::table) {
        output_stream_ << risk::domain::convert_to_table(ccys) << std::endl;
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
        output_stream_ << accounts::domain::convert_to_json(accts) << std::endl;
    } else if (cfg.target_format == config::format::table) {
        output_stream_ << accounts::domain::convert_to_table(accts) << std::endl;
    }
}

void application::
export_feature_flags(const config::export_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Exporting feature flags.";

    accounts::repository::feature_flags_repository repo(context_);
    std::vector<accounts::domain::feature_flags> flags;

    if (!cfg.key.empty()) {
        // Export specific feature flag by name
        if (cfg.all_versions) {
            flags = repo.read_all(cfg.key);
        } else {
            flags = repo.read_latest(cfg.key);
        }
    } else {
        // Export all feature flags
        if (cfg.all_versions) {
            flags = repo.read_all();
        } else {
            flags = repo.read_latest();
        }
    }

    // Output in the requested format
    if (cfg.target_format == config::format::json ||
        cfg.target_format == config::format::table) {
        if (cfg.target_format == config::format::json) {
            output_stream_ << accounts::domain::convert_to_json(flags) << std::endl;
        } else if (cfg.target_format == config::format::table) {
            output_stream_ << accounts::domain::convert_to_table(flags) << std::endl;
        }
    } else {
        BOOST_THROW_EXCEPTION(
            application_exception("Only JSON and table formats are supported for feature flags"));
    }

    BOOST_LOG_SEV(lg(), debug) << "Exported " << flags.size() << " feature flag(s).";
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
    add_data(cfg.adding);

    BOOST_LOG_SEV(lg(), info) << "Finished application.";
    return;
}

void application::
delete_currency(const config::delete_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Deleting currency: " << cfg.key;
    currency_repository rp;
    rp.remove(context_, cfg.key);
    output_stream_ << "Currency deleted successfully: " << cfg.key << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Deleted currency: " << cfg.key;
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
delete_feature_flag(const config::delete_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Deleting feature flag: " << cfg.key;
    accounts::repository::feature_flags_repository repo(context_);
    repo.remove(cfg.key);
    output_stream_ << "Feature flag deleted successfully: " << cfg.key << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Deleted feature flag: " << cfg.key;
}

void application::
delete_data(const std::optional<config::delete_options>& ocfg) const {
    if (!ocfg.has_value()) {
        BOOST_LOG_SEV(lg(), debug) << "No deletion configuration found.";
        return;
    }

    const auto& cfg(ocfg.value());
    switch (cfg.target_entity) {
        case config::entity::currencies:
            delete_currency(cfg);
            break;
        case config::entity::accounts:
            delete_account(cfg);
            break;
        case config::entity::feature_flags:
            delete_feature_flag(cfg);
            break;
        default:
            BOOST_THROW_EXCEPTION(
                application_exception(std::format("Delete not supported for entity: {}",
                        magic_enum::enum_name(cfg.target_entity))));
    }
}

void application::
add_currency(const config::add_options& cfg) const {
    BOOST_LOG_SEV(lg(), info) << "Adding currency: " << cfg.iso_code.value_or("");

    // Construct currency from command-line arguments
    risk::domain::currency currency;
    currency.iso_code = cfg.iso_code.value();
    currency.name = cfg.name.value();
    currency.numeric_code = cfg.numeric_code.value_or("0");
    currency.symbol = cfg.symbol.value_or("");
    currency.fraction_symbol = cfg.fraction_symbol.value_or("");
    currency.fractions_per_unit = cfg.fractions_per_unit.value_or(100);
    currency.rounding_type = cfg.rounding_type.value_or("Closest");
    currency.rounding_precision = cfg.rounding_precision.value_or(2);
    currency.format = cfg.format.value_or("");
    currency.currency_type = cfg.currency_type.value_or("");
    currency.modified_by = cfg.modified_by.value();

    // Set timestamps to current time
    const auto now = std::chrono::system_clock::now();
    const auto timestamp = utility::datetime::datetime::format_time_point(
        now, "%Y-%m-%d %H:%M:%S");
    currency.valid_from = timestamp;
    currency.valid_to = timestamp;

    // Write to database
    currency_repository repo;
    repo.write(context_, currency);

    output_stream_ << "Successfully added currency: " << currency.iso_code << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Added currency: " << currency.iso_code;
}

void application::
add_account(const config::add_options& cfg) const {
    BOOST_LOG_SEV(lg(), info) << "Adding account: " << cfg.username.value_or("");

    // Generate UUID for the account
    boost::uuids::random_generator gen;
    const auto account_id = gen();

    // Validate password policy
    using accounts::security::password_policy_validator;
    using accounts::repository::feature_flags_repository;

    // Check if password validation is disabled via feature flag
    feature_flags_repository flag_repo(context_);
    const auto disable_validation_flags =
        flag_repo.read_latest("system.disable_password_validation");
    const bool enforce_policy = disable_validation_flags.empty() ||
                                !disable_validation_flags[0].enabled;

    const auto validation_result =
        password_policy_validator::validate(cfg.password.value(), enforce_policy);

    if (!validation_result.is_valid) {
        BOOST_LOG_SEV(lg(), error) << "Password validation failed: "
                                   << validation_result.error_message;
        output_stream_ << "Error: " << validation_result.error_message << std::endl;
        BOOST_THROW_EXCEPTION(
            application_exception(validation_result.error_message));
    }

    // Hash the password
    using accounts::security::password_manager;
    const auto password_hash =
        password_manager::create_password_hash(cfg.password.value());

    // Construct account from command-line arguments
    accounts::domain::account account;
    account.version = 0;
    account.is_admin = cfg.is_admin.value_or(false);
    account.id = account_id;
    account.modified_by = cfg.modified_by.value();
    account.username = cfg.username.value();
    account.password_hash = password_hash;
    account.password_salt = "";  // Not used - hash contains salt
    account.totp_secret = "";    // Can be set later
    account.email = cfg.email.value();

    accounts::service::bootstrap_mode_service bootstrap_svc(context_);
    bootstrap_svc.initialize_bootstrap_state();
    if (bootstrap_svc.is_in_bootstrap_mode())
        output_stream_ << "System is currently in bootstrap mode." << std::endl;

    accounts::repository::account_repository repo(context_);
    repo.write(account);

    output_stream_ << "Successfully added account: " << account.username
                   << " (ID: " << boost::uuids::to_string(account_id) << ")" << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Added account: " << account.username;

    if (account.is_admin && bootstrap_svc.is_in_bootstrap_mode()) {
        bootstrap_svc.exit_bootstrap_mode();
        BOOST_LOG_SEV(lg(), info) << "Created first admin account - exiting bootstrap mode.";
        output_stream_ << "System exited bootstrap mode." << std::endl;
    }
}

void application::
add_feature_flag(const config::add_options& cfg) const {
    BOOST_LOG_SEV(lg(), info) << "Adding feature flag: " << cfg.flag_name.value_or("");

    // Construct feature flag from command-line arguments
    accounts::domain::feature_flags flag;
    flag.name = cfg.flag_name.value();
    flag.description = cfg.description.value_or("");
    flag.enabled = cfg.enabled.value_or(false);
    flag.modified_by = cfg.modified_by.value();

    // Write to database
    accounts::repository::feature_flags_repository repo(context_);
    repo.write(flag);

    output_stream_ << "Successfully added feature flag: " << flag.name << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Added feature flag: " << flag.name;
}

void application::
add_data(const std::optional<config::add_options>& ocfg) const {
    if (!ocfg.has_value()) {
        BOOST_LOG_SEV(lg(), debug) << "No add configuration found.";
        return;
    }

    const auto& cfg(ocfg.value());
    switch (cfg.target_entity) {
        case config::entity::currencies:
            add_currency(cfg);
            break;
        case config::entity::accounts:
            add_account(cfg);
            break;
        case config::entity::feature_flags:
            add_feature_flag(cfg);
            break;
        default:
            BOOST_THROW_EXCEPTION(
                application_exception(std::format("Add not supported for entity: {}",
                        magic_enum::enum_name(cfg.target_entity))));
    }
}

}
