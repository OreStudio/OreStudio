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
#ifndef ORES_WT_SERVICE_APPLICATION_CONTEXT_HPP
#define ORES_WT_SERVICE_APPLICATION_CONTEXT_HPP

#include <memory>
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/database_options.hpp"
#include "ores.iam/service/account_service.hpp"
#include "ores.iam/service/account_setup_service.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.iam/service/bootstrap_mode_service.hpp"
#include "ores.variability/service/system_flags_service.hpp"
#include "ores.variability/eventing/feature_flags_changed_event.hpp"
#include "ores.risk/service/currency_service.hpp"
#include "ores.risk/service/country_service.hpp"
#include "ores.eventing/service/event_bus.hpp"
#include "ores.eventing/service/postgres_event_source.hpp"

namespace ores::wt::service {

/**
 * @brief Holds all shared services for the ores.wt application.
 *
 * This is a singleton that is initialized at application startup and shared
 * across all Wt sessions. It manages the database connection and domain
 * services.
 */
class application_context {
public:
    static application_context& instance();

    void initialize(const database::database_options& db_opts);
    bool is_initialized() const { return initialized_; }

    /**
     * @brief Start the event source to listen for PostgreSQL notifications.
     *
     * Call this after initialize() to begin receiving database change events.
     */
    void start_eventing();

    /**
     * @brief Stop the event source.
     *
     * Call this during shutdown to cleanly stop listening for notifications.
     */
    void stop_eventing();

    database::context& db_context() { return *db_context_; }

    iam::service::account_service& account_service() {
        return *account_service_;
    }

    iam::service::account_setup_service& account_setup_service() {
        return *account_setup_service_;
    }

    iam::service::authorization_service& authorization_service() {
        return *authorization_service_;
    }

    variability::service::system_flags_service& system_flags_service() {
        return *system_flags_service_;
    }

    risk::service::currency_service& currency_service() {
        return *currency_service_;
    }

    risk::service::country_service& country_service() {
        return *country_service_;
    }

    bool is_bootstrap_mode() const { return is_bootstrap_mode_; }

private:
    application_context() = default;
    application_context(const application_context&) = delete;
    application_context& operator=(const application_context&) = delete;

    void setup_services();
    void setup_eventing();
    void check_bootstrap_mode();

    bool initialized_ = false;
    bool is_bootstrap_mode_ = false;

    std::unique_ptr<database::context> db_context_;
    std::unique_ptr<iam::service::account_service> account_service_;
    std::unique_ptr<iam::service::account_setup_service> account_setup_service_;
    std::shared_ptr<iam::service::authorization_service> authorization_service_;
    std::shared_ptr<variability::service::system_flags_service> system_flags_service_;
    std::unique_ptr<risk::service::currency_service> currency_service_;
    std::unique_ptr<risk::service::country_service> country_service_;

    // Eventing infrastructure for cross-service cache invalidation
    std::unique_ptr<eventing::service::event_bus> event_bus_;
    std::unique_ptr<eventing::service::postgres_event_source> event_source_;
    eventing::service::subscription flags_subscription_;
};

}

#endif
