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
#include "ores.wt/service/application_context.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.iam/service/bootstrap_mode_service.hpp"
#include "ores.variability/service/system_flags_service.hpp"
#include "ores.variability/eventing/feature_flags_changed_event.hpp"
#include "ores.eventing/service/registrar.hpp"
#include "ores.telemetry/log/make_logger.hpp"

namespace {

const std::string logger_name = "ores.wt.service.application_context";

auto& lg() {
    using namespace ores::telemetry::log;
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
    using namespace ores::telemetry::log;

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
        .wait_time_in_seconds = 1
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
    using namespace ores::telemetry::log;

    BOOST_LOG_SEV(lg(), info) << "Setting up services";

    // Create authorization service for RBAC operations
    // (Permissions and roles are seeded via SQL scripts in the database template)
    authorization_service_ = std::make_shared<iam::service::authorization_service>(
        *db_context_);

    // Create system flags service and refresh cache from database
    // (System flags are seeded via SQL scripts in the database template)
    system_flags_service_ = std::make_shared<variability::service::system_flags_service>(
        *db_context_);
    system_flags_service_->refresh();

    account_service_ = std::make_unique<iam::service::account_service>(
        *db_context_);

    account_setup_service_ = std::make_unique<iam::service::account_setup_service>(
        *account_service_, authorization_service_);

    currency_service_ = std::make_unique<risk::service::currency_service>(
        *db_context_);

    country_service_ = std::make_unique<risk::service::country_service>(
        *db_context_);

    BOOST_LOG_SEV(lg(), info) << "Services setup complete";
}

void application_context::setup_eventing() {
    using namespace ores::telemetry::log;

    BOOST_LOG_SEV(lg(), info) << "Setting up eventing infrastructure";

    // Create event bus and postgres event source
    event_bus_ = std::make_unique<eventing::service::event_bus>();
    event_source_ = std::make_unique<eventing::service::postgres_event_source>(
        *db_context_, *event_bus_);

    // Register feature flags mapping for system_flags cache invalidation
    eventing::service::registrar::register_mapping<
        variability::eventing::feature_flags_changed_event>(
        *event_source_, "ores.variability.feature_flag", "ores_feature_flags");

    // Subscribe to feature flag changes to refresh system_flags cache
    flags_subscription_ = event_bus_->subscribe<variability::eventing::feature_flags_changed_event>(
        [this](const variability::eventing::feature_flags_changed_event& e) {
            BOOST_LOG_SEV(lg(), info) << "Feature flags changed notification received, "
                                      << "refreshing system_flags cache ("
                                      << e.flag_names.size() << " flags changed)";
            system_flags_service_->refresh();
            is_bootstrap_mode_ = system_flags_service_->is_bootstrap_mode_enabled();
        });

    BOOST_LOG_SEV(lg(), info) << "Eventing infrastructure setup complete";
}

void application_context::start_eventing() {
    using namespace ores::telemetry::log;

    if (event_source_) {
        BOOST_LOG_SEV(lg(), info) << "Starting event source";
        event_source_->start();
    }
}

void application_context::stop_eventing() {
    using namespace ores::telemetry::log;

    if (event_source_) {
        BOOST_LOG_SEV(lg(), info) << "Stopping event source";
        event_source_->stop();
    }
}

void application_context::check_bootstrap_mode() {
    using namespace ores::telemetry::log;

    iam::service::bootstrap_mode_service bootstrap_svc(
        *db_context_, authorization_service_);
    bootstrap_svc.initialize_bootstrap_state();

    system_flags_service_->refresh();
    is_bootstrap_mode_ = system_flags_service_->is_bootstrap_mode_enabled();

    if (is_bootstrap_mode_) {
        BOOST_LOG_SEV(lg(), warn) << "*** SYSTEM IN BOOTSTRAP MODE ***";
        BOOST_LOG_SEV(lg(), warn) << "Create an admin account to exit bootstrap mode";
    } else {
        BOOST_LOG_SEV(lg(), info) << "System is in normal mode";
    }
}

}
