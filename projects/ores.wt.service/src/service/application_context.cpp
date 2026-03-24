/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.wt.service/service/application_context.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.iam.core/service/bootstrap_mode_service.hpp"
#include "ores.variability.core/service/system_settings_service.hpp"
#include "ores.variability.api/eventing/system_setting_changed_event.hpp"
#include "ores.eventing/service/registrar.hpp"
#include "ores.logging/make_logger.hpp"

namespace {

const std::string logger_name = "ores.wt.service.service.application_context";

auto& lg() {
    using namespace ores::logging;
    static auto instance = make_logger(logger_name);
    return instance;
}

}

namespace ores::wt::service {

application_context& application_context::instance() {
    static application_context ctx;
    return ctx;
}

void application_context::initialize(const database::database_options& db_opts) {
    using namespace ores::logging;

    if (initialized_) {
        BOOST_LOG_SEV(lg(), warn) << "Application context already initialized";
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Initializing application context";
    BOOST_LOG_SEV(lg(), info) << "Database: " << db_opts.host << ":"
                              << db_opts.port << "/" << db_opts.database;

    database::context_factory::configuration cfg {
        .database_options = db_opts,
        .pool_size = 4,
        .num_attempts = 10,
        .wait_time_in_seconds = 1,
        .service_account = db_opts.user
    };

    db_context_ = std::make_unique<database::context>(
        database::context_factory::make_context(cfg));

    BOOST_LOG_SEV(lg(), info) << "Database context created";

    setup_services();
    setup_eventing();
    check_bootstrap_mode();

    initialized_ = true;
    BOOST_LOG_SEV(lg(), info) << "Application context initialized successfully";
}

void application_context::setup_services() {
    using namespace ores::logging;

    BOOST_LOG_SEV(lg(), info) << "Setting up services";

    // Create authorization service for RBAC operations
    // (Permissions and roles are seeded via SQL scripts in the database template)
    authorization_service_ = std::make_shared<iam::service::authorization_service>(
        *db_context_);

    // Create system settings service and refresh cache from database
    // (System settings are seeded via SQL scripts in the database template)
    system_settings_service_ = std::make_shared<variability::service::system_settings_service>(
        *db_context_, database::service::tenant_context::system_tenant_id);
    system_settings_service_->refresh();

    account_service_ = std::make_unique<iam::service::account_service>(
        *db_context_);

    account_setup_service_ = std::make_unique<iam::service::account_setup_service>(
        *account_service_, authorization_service_);

    currency_service_ = std::make_unique<refdata::service::currency_service>(
        *db_context_);

    country_service_ = std::make_unique<refdata::service::country_service>(
        *db_context_);

    BOOST_LOG_SEV(lg(), info) << "Services setup complete";
}

void application_context::setup_eventing() {
    using namespace ores::logging;

    BOOST_LOG_SEV(lg(), info) << "Setting up eventing infrastructure";

    // Create event bus and postgres event source
    event_bus_ = std::make_unique<eventing::service::event_bus>();
    event_source_ = std::make_unique<eventing::service::postgres_event_source>(
        *db_context_, *event_bus_);

    // Register system settings mapping for cache invalidation
    eventing::service::registrar::register_mapping<
        variability::eventing::system_setting_changed_event>(
        *event_source_, "ores.variability.system_setting", "ores_system_settings");

    // Subscribe to system settings changes to refresh service cache
    flags_subscription_ = event_bus_->subscribe<variability::eventing::system_setting_changed_event>(
        [this](const variability::eventing::system_setting_changed_event& e) {
            BOOST_LOG_SEV(lg(), info) << "System settings changed notification received, "
                                      << "refreshing settings cache ("
                                      << e.setting_names.size() << " settings changed)";
            system_settings_service_->refresh();
            is_bootstrap_mode_ = system_settings_service_->is_bootstrap_mode_enabled();
        });

    BOOST_LOG_SEV(lg(), info) << "Eventing infrastructure setup complete";
}

void application_context::start_eventing() {
    using namespace ores::logging;

    if (event_source_) {
        BOOST_LOG_SEV(lg(), info) << "Starting event source";
        event_source_->start();
    }
}

void application_context::stop_eventing() {
    using namespace ores::logging;

    // Tear down all eventing objects here, while Boost.Log is still alive.
    // The application_context is a Meyer's singleton: its destructor runs
    // during static destruction, after lifecycle_manager (and Boost.Log) are
    // already gone.  Any logging in those destructors would throw bad_alloc
    // inside a noexcept destructor and call terminate().
    if (event_source_) {
        BOOST_LOG_SEV(lg(), info) << "Stopping event source";
        event_source_->stop();
        event_source_.reset();
    }
    flags_subscription_.unsubscribe();
    event_bus_.reset();
}

void application_context::check_bootstrap_mode() {
    using namespace ores::logging;

    iam::service::bootstrap_mode_service bootstrap_svc(
        *db_context_, database::service::tenant_context::system_tenant_id,
        authorization_service_);
    bootstrap_svc.initialize_bootstrap_state();

    system_settings_service_->refresh();
    is_bootstrap_mode_ = system_settings_service_->is_bootstrap_mode_enabled();

    if (is_bootstrap_mode_) {
        BOOST_LOG_SEV(lg(), warn) << "*** SYSTEM IN BOOTSTRAP MODE ***";
        BOOST_LOG_SEV(lg(), warn) << "Create an admin account to exit bootstrap mode";
    } else {
        BOOST_LOG_SEV(lg(), info) << "System is in normal mode";
    }
}

}
