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
#include "ores.variability/service/system_flags_seeder.hpp"
#include "ores.iam/service/bootstrap_mode_service.hpp"
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
    check_bootstrap_mode();

    initialized_ = true;
    BOOST_LOG_SEV(lg(), info) << "Application context initialized successfully";
}

void application_context::setup_services() {
    using namespace ores::telemetry::log;

    BOOST_LOG_SEV(lg(), info) << "Setting up services";

    variability::service::system_flags_seeder flags_seeder(*db_context_);
    flags_seeder.seed();

    authorization_service_ = std::make_shared<iam::service::authorization_service>(
        *db_context_);
    authorization_service_->seed_rbac();

    system_flags_service_ = std::make_shared<variability::service::system_flags_service>(
        *db_context_);
    system_flags_service_->refresh();

    account_service_ = std::make_unique<iam::service::account_service>(
        *db_context_);

    account_setup_service_ = std::make_unique<iam::service::account_setup_service>(
        *account_service_, authorization_service_);

    BOOST_LOG_SEV(lg(), info) << "Services setup complete";
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
