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
#include <type_traits>
#include "ores.utility/version/version.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/throw_exception.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <rfl/json.hpp>
#include <sqlgen/postgres.hpp>
#include <magic_enum/magic_enum.hpp>
#include "ores.database/service/tenant_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp"  // IWYU pragma: keep.
#include "ores.database/service/context_factory.hpp"
#include "ores.database/domain/database_options.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.ore/xml/importer.hpp"
#include "ores.ore/xml/exporter.hpp"
#include "ores.refdata/csv/exporter.hpp"
#include "ores.refdata/domain/currency_table.hpp"
#include "ores.refdata/domain/currency_json.hpp"
#include "ores.refdata/repository/currency_repository.hpp"
#include "ores.iam/service/bootstrap_mode_service.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.iam/domain/role.hpp"
#include "ores.iam/domain/account_json.hpp"
#include "ores.iam/domain/account_table.hpp"
#include "ores.security/crypto/password_hasher.hpp"
#include "ores.security/validation/password_validator.hpp"
#include "ores.iam/repository/account_repository.hpp"
#include "ores.variability/domain/feature_flags_json.hpp"
#include "ores.variability/domain/feature_flags_table.hpp"
#include "ores.variability/repository/feature_flags_repository.hpp"
#include "ores.iam/domain/login_info_json.hpp"
#include "ores.iam/domain/login_info_table.hpp"
#include "ores.iam/repository/login_info_repository.hpp"
#include "ores.iam/domain/role_json.hpp"
#include "ores.iam/domain/role_table.hpp"
#include "ores.iam/repository/role_repository.hpp"
#include "ores.iam/domain/permission_json.hpp"
#include "ores.iam/domain/permission_table.hpp"
#include "ores.iam/repository/permission_repository.hpp"
#include "ores.dq/domain/change_reason_json_io.hpp"
#include "ores.dq/domain/change_reason_table_io.hpp"
#include "ores.dq/repository/change_reason_repository.hpp"
#include "ores.dq/domain/change_reason_category_json_io.hpp"
#include "ores.dq/domain/change_reason_category_table_io.hpp"
#include "ores.dq/repository/change_reason_category_repository.hpp"
#include "ores.refdata/domain/country_json_io.hpp"
#include "ores.refdata/domain/country_table_io.hpp"
#include "ores.refdata/repository/country_repository.hpp"
#include "ores.cli/config/export_options.hpp"
#include "ores.cli/config/add_currency_options.hpp"
#include "ores.cli/config/add_account_options.hpp"
#include "ores.cli/config/add_feature_flag_options.hpp"
#include "ores.cli/config/add_login_info_options.hpp"
#include "ores.cli/config/add_role_options.hpp"
#include "ores.cli/config/add_permission_options.hpp"
#include "ores.cli/config/add_country_options.hpp"
#include "ores.cli/config/add_change_reason_options.hpp"
#include "ores.cli/config/add_change_reason_category_options.hpp"
#include "ores.cli/app/application_exception.hpp"

namespace ores::cli::app {

using ore_importer = ore::xml::importer;
using ore_exporter = ore::xml::exporter;
using csv_exporter = refdata::csv::exporter;
using namespace ores::logging;
using ores::refdata::domain::currency;
using refdata::repository::currency_repository;
using connection = sqlgen::Result<rfl::Ref<sqlgen::postgres::Connection>>;

database::context application::make_context(
    const std::optional<database::database_options>& db_opts) {
    using database::context_factory;

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
    const std::optional<database::database_options>& db_opts)
    : output_stream_(output_stream), context_(make_context(db_opts)) {
    BOOST_LOG_SEV(lg(), debug) << "Creating application.";

    // Set tenant context if provided
    if (db_opts.has_value() && !db_opts->tenant.empty()) {
        set_tenant_context(db_opts->tenant);
    }
}

void application::set_tenant_context(const std::string& tenant) {
    try {
        database::service::tenant_context::set(context_, tenant);
    } catch (const std::exception& e) {
        BOOST_THROW_EXCEPTION(application_exception(e.what()));
    }
}

void application::
import_currencies(const std::vector<std::filesystem::path> files) const {
    currency_repository rp;

    for (const auto& f : files) {
        BOOST_LOG_SEV(lg(), debug) << "Processing file: " << f;
        auto ccys(ore_importer::import_currency_config(f));
        rp.write(context_, ccys);
        output_stream_ << f.filename() << ": Imported a total of "
                       << ccys.size() << " currencies." << std::endl;
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
    refdata::repository::currency_repository rp;

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
        output_stream_ << refdata::domain::convert_to_json(ccys) << std::endl;
    } else if (cfg.target_format == config::format::csv) {
        std::string ccy_cfgs = csv_exporter::export_currency_config(ccys);
        output_stream_ << ccy_cfgs << std::endl;
    } else if (cfg.target_format == config::format::table) {
        output_stream_ << refdata::domain::convert_to_table(ccys) << std::endl;
    }
}

void application::
export_accounts(const config::export_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Exporting accounts.";
    iam::repository::account_repository repo(context_);

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

    const std::vector<iam::domain::account> accts(reader());
    if (cfg.target_format == config::format::json) {
        output_stream_ << iam::domain::convert_to_json(accts) << std::endl;
    } else if (cfg.target_format == config::format::table) {
        output_stream_ << iam::domain::convert_to_table(accts) << std::endl;
    }
}

void application::
export_feature_flags(const config::export_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Exporting feature flags.";

    variability::repository::feature_flags_repository repo(context_);
    std::vector<variability::domain::feature_flags> flags;

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
            output_stream_ << variability::domain::convert_to_json(flags) << std::endl;
        } else if (cfg.target_format == config::format::table) {
            output_stream_ << variability::domain::convert_to_table(flags) << std::endl;
        }
    } else {
        BOOST_THROW_EXCEPTION(
            application_exception("Only JSON and table formats are supported for feature flags"));
    }

    BOOST_LOG_SEV(lg(), debug) << "Exported " << flags.size() << " feature flag(s).";
}

void application::
export_login_info(const config::export_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Exporting login info.";

    iam::repository::login_info_repository repo(context_);
    std::vector<iam::domain::login_info> infos;

    if (!cfg.key.empty()) {
        // Export specific login info by account ID
        try {
            const auto account_id = boost::lexical_cast<boost::uuids::uuid>(cfg.key);
            infos = repo.read(account_id);
        } catch (const boost::bad_lexical_cast&) {
            BOOST_THROW_EXCEPTION(
                application_exception(std::format("Invalid account ID: {}", cfg.key)));
        }
    } else {
        // Export all login info records
        infos = repo.read();
    }

    // Output in the requested format
    if (cfg.target_format == config::format::json) {
        output_stream_ << iam::domain::convert_to_json(infos) << std::endl;
    } else if (cfg.target_format == config::format::table) {
        output_stream_ << iam::domain::convert_to_table(infos) << std::endl;
    } else {
        BOOST_THROW_EXCEPTION(
            application_exception("Only JSON and table formats are supported for login info"));
    }

    BOOST_LOG_SEV(lg(), debug) << "Exported " << infos.size() << " login info record(s).";
}

void application::
export_roles(const config::export_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Exporting roles.";

    iam::repository::role_repository repo(context_);
    std::vector<iam::domain::role> items;

    if (!cfg.key.empty()) {
        // Export specific role by ID (UUID)
        try {
            const auto role_id = boost::lexical_cast<boost::uuids::uuid>(cfg.key);
            items = repo.read_latest(role_id);
        } catch (const boost::bad_lexical_cast&) {
            // Try by name
            items = repo.read_latest_by_name(cfg.key);
        }
    } else {
        items = repo.read_latest();
    }

    // Output in the requested format
    if (cfg.target_format == config::format::json) {
        output_stream_ << iam::domain::convert_to_json(items) << std::endl;
    } else if (cfg.target_format == config::format::table) {
        output_stream_ << iam::domain::convert_to_table(items) << std::endl;
    } else {
        BOOST_THROW_EXCEPTION(
            application_exception("Only JSON and table formats are supported for roles"));
    }

    BOOST_LOG_SEV(lg(), debug) << "Exported " << items.size() << " role(s).";
}

void application::
export_permissions(const config::export_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Exporting permissions.";

    iam::repository::permission_repository repo(context_);
    std::vector<iam::domain::permission> items;

    if (!cfg.key.empty()) {
        // Export specific permission by ID (UUID)
        try {
            const auto perm_id = boost::lexical_cast<boost::uuids::uuid>(cfg.key);
            items = repo.read_latest(perm_id);
        } catch (const boost::bad_lexical_cast&) {
            // Try by code
            items = repo.read_latest_by_code(cfg.key);
        }
    } else {
        items = repo.read_latest();
    }

    // Output in the requested format
    if (cfg.target_format == config::format::json) {
        output_stream_ << iam::domain::convert_to_json(items) << std::endl;
    } else if (cfg.target_format == config::format::table) {
        output_stream_ << iam::domain::convert_to_table(items) << std::endl;
    } else {
        BOOST_THROW_EXCEPTION(
            application_exception("Only JSON and table formats are supported for permissions"));
    }

    BOOST_LOG_SEV(lg(), debug) << "Exported " << items.size() << " permission(s).";
}

void application::
export_countries(const config::export_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Exporting countries.";

    refdata::repository::country_repository repo;
    std::vector<refdata::domain::country> items;

    if (!cfg.key.empty()) {
        if (cfg.all_versions) {
            items = repo.read_all(context_, cfg.key);
        } else if (cfg.as_of.empty()) {
            items = repo.read_latest(context_, cfg.key);
        } else {
            items = repo.read_at_timepoint(context_, cfg.as_of, cfg.key);
        }
    } else {
        if (cfg.all_versions) {
            items = repo.read_all(context_);
        } else if (cfg.as_of.empty()) {
            items = repo.read_latest(context_);
        } else {
            items = repo.read_at_timepoint(context_, cfg.as_of);
        }
    }

    // Output in the requested format
    if (cfg.target_format == config::format::json) {
        output_stream_ << "[";
        const char* sep = "";
        for (const auto& item : items) {
            output_stream_ << sep << item;
            sep = ",";
        }
        output_stream_ << "]" << std::endl;
    } else if (cfg.target_format == config::format::table) {
        output_stream_ << items << std::endl;
    } else {
        BOOST_THROW_EXCEPTION(
            application_exception("Only JSON and table formats are supported for countries"));
    }

    BOOST_LOG_SEV(lg(), debug) << "Exported " << items.size() << " country(ies).";
}

void application::
export_change_reasons(const config::export_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Exporting change reasons.";

    dq::repository::change_reason_repository repo(context_);
    std::vector<dq::domain::change_reason> items;

    if (!cfg.key.empty()) {
        if (cfg.all_versions) {
            items = repo.read_all(cfg.key);
        } else {
            items = repo.read_latest(cfg.key);
        }
    } else {
        items = repo.read_latest();
    }

    // Output in the requested format
    if (cfg.target_format == config::format::json) {
        output_stream_ << "[";
        for (size_t i = 0; i < items.size(); ++i) {
            output_stream_ << items[i];
            if (i < items.size() - 1)
                output_stream_ << ",";
        }
        output_stream_ << "]" << std::endl;
    } else if (cfg.target_format == config::format::table) {
        output_stream_ << items << std::endl;
    } else {
        BOOST_THROW_EXCEPTION(
            application_exception("Only JSON and table formats are supported for change reasons"));
    }

    BOOST_LOG_SEV(lg(), debug) << "Exported " << items.size() << " change reason(s).";
}

void application::
export_change_reason_categories(const config::export_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Exporting change reason categories.";

    dq::repository::change_reason_category_repository repo(context_);
    std::vector<dq::domain::change_reason_category> items;

    if (!cfg.key.empty()) {
        if (cfg.all_versions) {
            items = repo.read_all(cfg.key);
        } else {
            items = repo.read_latest(cfg.key);
        }
    } else {
        items = repo.read_latest();
    }

    // Output in the requested format
    if (cfg.target_format == config::format::json) {
        output_stream_ << "[";
        for (size_t i = 0; i < items.size(); ++i) {
            output_stream_ << items[i];
            if (i < items.size() - 1)
                output_stream_ << ",";
        }
        output_stream_ << "]" << std::endl;
    } else if (cfg.target_format == config::format::table) {
        output_stream_ << items << std::endl;
    } else {
        BOOST_THROW_EXCEPTION(
            application_exception("Only JSON and table formats are supported for change reason categories"));
    }

    BOOST_LOG_SEV(lg(), debug) << "Exported " << items.size() << " change reason category(ies).";
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
        case config::entity::login_info:
            export_login_info(cfg);
            break;
        case config::entity::roles:
            export_roles(cfg);
            break;
        case config::entity::permissions:
            export_permissions(cfg);
            break;
        case config::entity::countries:
            export_countries(cfg);
            break;
        case config::entity::change_reasons:
            export_change_reasons(cfg);
            break;
        case config::entity::change_reason_categories:
            export_change_reason_categories(cfg);
            break;
    }
}

void application::run(const config::options& cfg) const {
    BOOST_LOG_SEV(lg(), info) << utility::version::format_startup_message(
        "ORE Studio CLI");

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
    iam::repository::account_repository repo(context_);

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
    variability::repository::feature_flags_repository repo(context_);
    repo.remove(cfg.key);
    output_stream_ << "Feature flag deleted successfully: " << cfg.key << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Deleted feature flag: " << cfg.key;
}

void application::
delete_login_info(const config::delete_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Deleting login info for account: " << cfg.key;
    iam::repository::login_info_repository repo(context_);

    // Parse as UUID
    boost::uuids::uuid account_id;
    try {
        account_id = boost::lexical_cast<boost::uuids::uuid>(cfg.key);
    } catch (const boost::bad_lexical_cast&) {
        BOOST_THROW_EXCEPTION(
            application_exception(std::format("Invalid account ID: {}", cfg.key)));
    }

    repo.remove(account_id);
    output_stream_ << "Login info deleted successfully for account: "
                   << boost::uuids::to_string(account_id) << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Deleted login info for account: "
                              << boost::uuids::to_string(account_id);
}

void application::
delete_role(const config::delete_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Deleting role: " << cfg.key;
    iam::repository::role_repository repo(context_);

    // Parse as UUID
    boost::uuids::uuid role_id;
    try {
        role_id = boost::lexical_cast<boost::uuids::uuid>(cfg.key);
    } catch (const boost::bad_lexical_cast&) {
        // Try to find by name
        const auto roles = repo.read_latest_by_name(cfg.key);
        if (roles.empty()) {
            BOOST_THROW_EXCEPTION(
                application_exception(std::format("Role not found: {}", cfg.key)));
        }
        role_id = roles.front().id;
    }

    repo.remove(role_id);
    output_stream_ << "Role deleted successfully: "
                   << boost::uuids::to_string(role_id) << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Deleted role: "
                              << boost::uuids::to_string(role_id);
}

void application::
delete_permission(const config::delete_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Deleting permission: " << cfg.key;
    iam::repository::permission_repository repo(context_);

    // Parse as UUID
    boost::uuids::uuid perm_id;
    try {
        perm_id = boost::lexical_cast<boost::uuids::uuid>(cfg.key);
    } catch (const boost::bad_lexical_cast&) {
        // Try to find by code
        const auto perms = repo.read_latest_by_code(cfg.key);
        if (perms.empty()) {
            BOOST_THROW_EXCEPTION(
                application_exception(std::format("Permission not found: {}", cfg.key)));
        }
        perm_id = perms.front().id;
    }

    repo.remove(perm_id);
    output_stream_ << "Permission deleted successfully: "
                   << boost::uuids::to_string(perm_id) << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Deleted permission: "
                              << boost::uuids::to_string(perm_id);
}

void application::
delete_country(const config::delete_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Deleting country: " << cfg.key;
    refdata::repository::country_repository repo;
    repo.remove(context_, cfg.key);
    output_stream_ << "Country deleted successfully: " << cfg.key << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Deleted country: " << cfg.key;
}

void application::
delete_change_reason(const config::delete_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Deleting change reason: " << cfg.key;
    dq::repository::change_reason_repository repo(context_);
    repo.remove(cfg.key);
    output_stream_ << "Change reason deleted successfully: " << cfg.key << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Deleted change reason: " << cfg.key;
}

void application::
delete_change_reason_category(const config::delete_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Deleting change reason category: " << cfg.key;
    dq::repository::change_reason_category_repository repo(context_);
    repo.remove(cfg.key);
    output_stream_ << "Change reason category deleted successfully: " << cfg.key << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Deleted change reason category: " << cfg.key;
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
        case config::entity::login_info:
            delete_login_info(cfg);
            break;
        case config::entity::roles:
            delete_role(cfg);
            break;
        case config::entity::permissions:
            delete_permission(cfg);
            break;
        case config::entity::countries:
            delete_country(cfg);
            break;
        case config::entity::change_reasons:
            delete_change_reason(cfg);
            break;
        case config::entity::change_reason_categories:
            delete_change_reason_category(cfg);
            break;
    }
}

void application::
add_currency(const config::add_currency_options& cfg) const {
    BOOST_LOG_SEV(lg(), info) << "Adding currency: " << cfg.iso_code;

    // Construct currency from command-line arguments
    refdata::domain::currency currency;
    currency.iso_code = cfg.iso_code;
    currency.name = cfg.name;
    currency.numeric_code = cfg.numeric_code.value_or("0");
    currency.symbol = cfg.symbol.value_or("");
    currency.fraction_symbol = cfg.fraction_symbol.value_or("");
    currency.fractions_per_unit = cfg.fractions_per_unit.value_or(100);
    currency.rounding_type = cfg.rounding_type.value_or("Closest");
    currency.rounding_precision = cfg.rounding_precision.value_or(2);
    currency.format = cfg.format.value_or("");
    currency.currency_type = cfg.currency_type.value_or("");
    currency.recorded_by = cfg.modified_by;
    // Note: recorded_at is set by the database triggers via valid_from

    // Write to database
    currency_repository repo;
    repo.write(context_, currency);

    output_stream_ << "Successfully added currency: " << currency.iso_code << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Added currency: " << currency.iso_code;
}

void application::
add_account(const config::add_account_options& cfg) const {
    BOOST_LOG_SEV(lg(), info) << "Adding account: " << cfg.username;

    // Generate UUID for the account
    boost::uuids::random_generator gen;
    const auto account_id = gen();

    // Validate password policy
    using security::validation::password_validator;
    using variability::repository::feature_flags_repository;

    // Check if password validation is disabled via feature flag
    feature_flags_repository flag_repo(context_);
    const auto disable_validation_flags =
        flag_repo.read_latest("system.disable_password_validation");
    const bool enforce_policy = disable_validation_flags.empty() ||
                                !disable_validation_flags[0].enabled;

    const auto validation_result =
        password_validator::validate(cfg.password, enforce_policy);

    if (!validation_result.is_valid) {
        BOOST_LOG_SEV(lg(), error) << "Password validation failed: "
                                   << validation_result.error_message;
        output_stream_ << "Error: " << validation_result.error_message << std::endl;
        BOOST_THROW_EXCEPTION(
            application_exception(validation_result.error_message));
    }

    // Hash the password
    using security::crypto::password_hasher;
    const auto password_hash = password_hasher::hash(cfg.password);

    // Create authorization service for RBAC operations
    // (Permissions and roles are seeded via SQL scripts in the database template)
    auto auth_service = std::make_shared<iam::service::authorization_service>(context_);

    // Construct account from command-line arguments
    iam::domain::account account;
    account.version = 0;
    account.id = account_id;
    account.recorded_by = cfg.modified_by;
    account.username = cfg.username;
    account.password_hash = password_hash;
    account.password_salt = "";  // Not used - hash contains salt
    account.totp_secret = "";    // Can be set later
    account.email = cfg.email;

    iam::service::bootstrap_mode_service bootstrap_svc(
        context_, database::service::tenant_context::system_tenant_id, auth_service);
    bootstrap_svc.initialize_bootstrap_state();
    if (bootstrap_svc.is_in_bootstrap_mode())
        output_stream_ << "System is currently in bootstrap mode." << std::endl;

    iam::repository::account_repository repo(context_);
    repo.write(account);

    output_stream_ << "Successfully added account: " << account.username
                   << " (ID: " << boost::uuids::to_string(account_id) << ")" << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Added account: " << account.username;

    // If --admin was specified, assign Admin role via RBAC
    const bool assign_admin = cfg.admin.value_or(false);
    if (assign_admin) {
        auto admin_role = auth_service->find_role_by_name(iam::domain::roles::admin);
        if (admin_role) {
            auth_service->assign_role(account_id, admin_role->id, cfg.modified_by);
            BOOST_LOG_SEV(lg(), info) << "Assigned Admin role to account: "
                                      << account.username;
            output_stream_ << "Assigned Admin role to account." << std::endl;
        }
    }

    if (assign_admin && bootstrap_svc.is_in_bootstrap_mode()) {
        bootstrap_svc.exit_bootstrap_mode();
        BOOST_LOG_SEV(lg(), info) << "Created first admin account - exiting bootstrap mode.";
        output_stream_ << "System exited bootstrap mode." << std::endl;
    }
}

void application::
add_feature_flag(const config::add_feature_flag_options& cfg) const {
    BOOST_LOG_SEV(lg(), info) << "Adding feature flag: " << cfg.flag_name;

    // Construct feature flag from command-line arguments
    variability::domain::feature_flags flag;
    flag.name = cfg.flag_name;
    flag.description = cfg.description.value_or("");
    flag.enabled = cfg.enabled.value_or(false);
    flag.recorded_by = cfg.modified_by;

    // Write to database
    variability::repository::feature_flags_repository repo(context_);
    repo.write(flag);

    output_stream_ << "Successfully added feature flag: " << flag.name << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Added feature flag: " << flag.name;
}

void application::
add_login_info(const config::add_login_info_options& cfg) const {
    BOOST_LOG_SEV(lg(), info) << "Adding login info for account: " << cfg.account_id;

    // Parse account ID
    boost::uuids::uuid account_id;
    try {
        account_id = boost::lexical_cast<boost::uuids::uuid>(cfg.account_id);
    } catch (const boost::bad_lexical_cast&) {
        BOOST_THROW_EXCEPTION(
            application_exception(std::format("Invalid account ID: {}", cfg.account_id)));
    }

    // Construct login info from command-line arguments
    iam::domain::login_info record;
    record.account_id = account_id;
    record.locked = cfg.locked.value_or(false);
    record.failed_logins = cfg.failed_logins.value_or(0);
    record.online = false;
    record.last_login = std::chrono::system_clock::now();

    // Write to database
    iam::repository::login_info_repository repo(context_);
    repo.write({record});

    output_stream_ << "Successfully added login info for account: "
                   << boost::uuids::to_string(account_id) << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Added login info for account: "
                              << boost::uuids::to_string(account_id);
}

void application::
add_role(const config::add_role_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Adding role: " << cfg.name;

    iam::domain::role record;
    record.id = boost::uuids::random_generator()();
    record.name = cfg.name;
    record.description = cfg.description;
    record.recorded_by = cfg.recorded_by;
    record.recorded_at = std::chrono::system_clock::now();
    record.permission_codes = cfg.permission_codes;

    if (cfg.change_reason_code)
        record.change_reason_code = *cfg.change_reason_code;
    if (cfg.change_commentary)
        record.change_commentary = *cfg.change_commentary;

    iam::repository::role_repository repo(context_);
    repo.write({record});

    output_stream_ << "Successfully added role: " << cfg.name << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Added role: " << cfg.name;
}

void application::
add_permission(const config::add_permission_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Adding permission: " << cfg.code;

    iam::domain::permission record;
    record.id = boost::uuids::random_generator()();
    record.code = cfg.code;
    if (cfg.description)
        record.description = *cfg.description;

    iam::repository::permission_repository repo(context_);
    repo.write({record});

    output_stream_ << "Successfully added permission: " << cfg.code << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Added permission: " << cfg.code;
}

void application::
add_country(const config::add_country_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Adding country: " << cfg.alpha2_code;

    refdata::domain::country record;
    record.alpha2_code = cfg.alpha2_code;
    record.alpha3_code = cfg.alpha3_code;
    record.name = cfg.name;
    record.recorded_by = cfg.recorded_by;
    record.recorded_at = std::chrono::system_clock::now();

    if (cfg.numeric_code)
        record.numeric_code = *cfg.numeric_code;
    if (cfg.official_name)
        record.official_name = *cfg.official_name;
    if (cfg.change_reason_code)
        record.change_reason_code = *cfg.change_reason_code;
    if (cfg.change_commentary)
        record.change_commentary = *cfg.change_commentary;

    refdata::repository::country_repository repo;
    repo.write(context_, record);

    output_stream_ << "Successfully added country: " << cfg.alpha2_code << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Added country: " << cfg.alpha2_code;
}

void application::
add_change_reason(const config::add_change_reason_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Adding change reason: " << cfg.code;

    dq::domain::change_reason record;
    record.code = cfg.code;
    record.description = cfg.description;
    record.category_code = cfg.category_code;
    record.recorded_by = cfg.recorded_by;
    record.recorded_at = std::chrono::system_clock::now();

    if (cfg.applies_to_amend)
        record.applies_to_amend = *cfg.applies_to_amend;
    if (cfg.applies_to_delete)
        record.applies_to_delete = *cfg.applies_to_delete;
    if (cfg.requires_commentary)
        record.requires_commentary = *cfg.requires_commentary;
    if (cfg.display_order)
        record.display_order = *cfg.display_order;
    if (cfg.change_commentary)
        record.change_commentary = *cfg.change_commentary;

    dq::repository::change_reason_repository repo(context_);
    repo.write({record});

    output_stream_ << "Successfully added change reason: " << cfg.code << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Added change reason: " << cfg.code;
}

void application::
add_change_reason_category(const config::add_change_reason_category_options& cfg) const {
    BOOST_LOG_SEV(lg(), debug) << "Adding change reason category: " << cfg.code;

    dq::domain::change_reason_category record;
    record.code = cfg.code;
    record.description = cfg.description;
    record.recorded_by = cfg.recorded_by;
    record.recorded_at = std::chrono::system_clock::now();

    if (cfg.change_commentary)
        record.change_commentary = *cfg.change_commentary;

    dq::repository::change_reason_category_repository repo(context_);
    repo.write({record});

    output_stream_ << "Successfully added change reason category: " << cfg.code << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Added change reason category: " << cfg.code;
}

void application::
add_data(const std::optional<config::add_options>& ocfg) const {
    if (!ocfg.has_value()) {
        BOOST_LOG_SEV(lg(), debug) << "No add configuration found.";
        return;
    }

    const auto& cfg(ocfg.value());
    std::visit([this](const auto& opts) {
        using T = std::decay_t<decltype(opts)>;
        if constexpr (std::is_same_v<T, config::add_currency_options>) {
            add_currency(opts);
        } else if constexpr (std::is_same_v<T, config::add_account_options>) {
            add_account(opts);
        } else if constexpr (std::is_same_v<T, config::add_feature_flag_options>) {
            add_feature_flag(opts);
        } else if constexpr (std::is_same_v<T, config::add_login_info_options>) {
            add_login_info(opts);
        } else if constexpr (std::is_same_v<T, config::add_role_options>) {
            add_role(opts);
        } else if constexpr (std::is_same_v<T, config::add_permission_options>) {
            add_permission(opts);
        } else if constexpr (std::is_same_v<T, config::add_country_options>) {
            add_country(opts);
        } else if constexpr (std::is_same_v<T, config::add_change_reason_options>) {
            add_change_reason(opts);
        } else if constexpr (std::is_same_v<T, config::add_change_reason_category_options>) {
            add_change_reason_category(opts);
        } else {
            []<bool flag = false>() {
                static_assert(flag, "unhandled type in std::visit for add_data");
            }();
        }
    }, cfg);
}

}
