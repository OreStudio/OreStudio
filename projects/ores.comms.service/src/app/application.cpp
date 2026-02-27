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
#include "ores.comms.service/app/application.hpp"
#include "ores.comms/messaging/protocol.hpp"

#include <boost/throw_exception.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include "ores.refdata/messaging/registrar.hpp"
#include "ores.refdata/eventing/currency_changed_event.hpp"
#include "ores.refdata/eventing/country_changed_event.hpp"
#include "ores.refdata/eventing/party_changed_event.hpp"
#include "ores.refdata/eventing/party_identifier_changed_event.hpp"
#include "ores.refdata/eventing/party_contact_information_changed_event.hpp"
#include "ores.refdata/eventing/counterparty_changed_event.hpp"
#include "ores.refdata/eventing/counterparty_identifier_changed_event.hpp"
#include "ores.refdata/eventing/counterparty_contact_information_changed_event.hpp"
#include "ores.refdata/eventing/book_changed_event.hpp"
#include "ores.refdata/eventing/portfolio_changed_event.hpp"
#include "ores.iam/messaging/registrar.hpp"
#include "ores.dq/messaging/registrar.hpp"
#include "ores.synthetic/messaging/registrar.hpp"
#include "ores.iam/eventing/account_changed_event.hpp"
#include "ores.iam/eventing/tenant_changed_event.hpp"
#include "ores.dq/eventing/change_reason_changed_event.hpp"
#include "ores.dq/eventing/change_reason_category_changed_event.hpp"
#include "ores.dq/eventing/origin_dimension_changed_event.hpp"
#include "ores.dq/eventing/nature_dimension_changed_event.hpp"
#include "ores.dq/eventing/treatment_dimension_changed_event.hpp"
#include "ores.dq/eventing/catalog_changed_event.hpp"
#include "ores.dq/eventing/dataset_changed_event.hpp"
#include "ores.dq/eventing/methodology_changed_event.hpp"
#include "ores.dq/eventing/coding_scheme_changed_event.hpp"
#include "ores.dq/eventing/coding_scheme_authority_type_changed_event.hpp"
#include "ores.dq/eventing/data_domain_changed_event.hpp"
#include "ores.dq/eventing/subject_area_changed_event.hpp"
#include "ores.iam/eventing/role_changed_event.hpp"
#include "ores.iam/eventing/permission_changed_event.hpp"
#include "ores.assets/eventing/assets_changed_event.hpp"
#include "ores.variability/eventing/feature_flags_changed_event.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.variability/messaging/registrar.hpp"
#include "ores.assets/messaging/registrar.hpp"
#include "ores.telemetry/messaging/registrar.hpp"
#include "ores.trading/messaging/registrar.hpp"
#include "ores.trading/eventing/trade_changed_event.hpp"
#include "ores.scheduler/messaging/registrar.hpp"
#include "ores.mq/domain/queue_message_event.hpp"
#include "ores.variability/service/system_flags_service.hpp"
#include "ores.iam/service/bootstrap_mode_service.hpp"
#include "ores.eventing/service/event_bus.hpp"
#include "ores.eventing/service/event_channel_registry.hpp"
#include "ores.eventing/service/postgres_event_source.hpp"
#include "ores.eventing/service/registrar.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.database/repository/database_info_repository.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.database/service/health_monitor.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.comms/net/server.hpp"
#include "ores.comms/service/subscription_manager.hpp"
#include "ores.comms/service/subscription_handler.hpp"
#include "ores.geo/service/geolocation_service.hpp"
#include "ores.dq/service/dataset_bundle_service.hpp"
#include "ores.comms.service/app/application_exception.hpp"
#include "ores.comms.service/messaging/system_info_handler.hpp"

namespace ores::comms::service::app {
using namespace ores::logging;

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

application::application() = default;

boost::asio::awaitable<void> application::
run(boost::asio::io_context& io_ctx, const config::options& cfg) const {
    BOOST_LOG_SEV(lg(), info) << utility::version::format_startup_message(
        "ORE Studio Service",
        comms::messaging::PROTOCOL_VERSION_MAJOR,
        comms::messaging::PROTOCOL_VERSION_MINOR);

    // Create database health monitor
    database::health_monitor db_health_monitor(
        cfg.database, std::chrono::seconds(5));

    // Perform initial database connectivity check
    BOOST_LOG_SEV(lg(), info) << "Checking database connectivity...";
    bool db_initially_available = db_health_monitor.check_health();

    if (!db_initially_available) {
        BOOST_LOG_SEV(lg(), warn) << "================================================";
        BOOST_LOG_SEV(lg(), warn) << "DATABASE CONNECTIVITY CHECK FAILED";
        BOOST_LOG_SEV(lg(), warn) << "Error: " << db_health_monitor.last_error();
        BOOST_LOG_SEV(lg(), warn) << "Waiting for database to become available...";
        BOOST_LOG_SEV(lg(), warn) << "================================================";

        // Wait for database to become available before proceeding
        while (!db_health_monitor.check_health()) {
            BOOST_LOG_SEV(lg(), info) << "Database still unavailable, retrying in 5 seconds...";
            boost::asio::steady_timer timer(co_await boost::asio::this_coro::executor);
            timer.expires_after(std::chrono::seconds(5));
            co_await timer.async_wait(boost::asio::use_awaitable);
        }

        BOOST_LOG_SEV(lg(), info) << "Database connectivity restored!";
    } else {
        BOOST_LOG_SEV(lg(), info) << "Database connectivity check PASSED";
    }

    auto ctx = make_context(cfg.database);

    // Set system tenant context for bootstrap initialization
    ctx = database::service::tenant_context::with_system_tenant(ctx);

    // Log database build metadata to correlate service and schema versions
    try {
        database::repository::database_info_repository db_info_repo;
        const auto db_infos = db_info_repo.read(ctx);
        if (!db_infos.empty()) {
            const auto& di = db_infos.front();
            BOOST_LOG_SEV(lg(), info) << "Database schema: v" << di.schema_version
                << " (" << di.build_environment
                << " " << di.git_commit
                << " " << di.git_date << ")";
        } else {
            BOOST_LOG_SEV(lg(), warn) << "Database info record not found";
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Could not read database info: " << e.what();
    }

    // Create shared authorization service for RBAC checks
    // (Permissions and roles are seeded via SQL scripts in the database template)
    auto auth_service = std::make_shared<iam::service::authorization_service>(ctx);

    // Create shared system flags service and refresh cache from database
    // (System flags are seeded via SQL scripts in the database template)
    auto system_flags = std::make_shared<variability::service::system_flags_service>(
        ctx, database::service::tenant_context::system_tenant_id);
    system_flags->refresh();

    // Initialize and check bootstrap mode
    iam::service::bootstrap_mode_service bootstrap_svc(
        ctx, database::service::tenant_context::system_tenant_id, auth_service);
    bootstrap_svc.initialize_bootstrap_state();

    // Refresh system flags cache after bootstrap state initialization
    system_flags->refresh();

    const bool in_bootstrap_mode = system_flags->is_bootstrap_mode_enabled();

    if (in_bootstrap_mode) {
        BOOST_LOG_SEV(lg(), warn) << "================================================";
        BOOST_LOG_SEV(lg(), warn) << "!!!  SYSTEM IN BOOTSTRAP MODE  !!!";
        BOOST_LOG_SEV(lg(), warn) << "================================================";
        BOOST_LOG_SEV(lg(), warn) << "Security Status: INSECURE - No admin account exists";
        BOOST_LOG_SEV(lg(), warn) << "Available Endpoints: create-initial-admin ONLY";
        BOOST_LOG_SEV(lg(), warn) << "Access Restriction: localhost (127.0.0.1) only";
        BOOST_LOG_SEV(lg(), warn) << "Action Required: Create initial admin account";
        BOOST_LOG_SEV(lg(), warn) << "================================================";
    } else {
        BOOST_LOG_SEV(lg(), info) << "System in SECURE MODE";
        BOOST_LOG_SEV(lg(), info) << "Authentication and authorization enforcement enabled";
    }

    // Initialize the event bus, channel registry, and event sources
    eventing::service::event_bus event_bus;
    auto channel_registry = std::make_shared<eventing::service::event_channel_registry>();
    eventing::service::postgres_event_source event_source(ctx, event_bus);

    // Register entity-to-event mappings for each component
    eventing::service::registrar::register_mapping<
        refdata::eventing::currency_changed_event>(
        event_source, "ores.refdata.currency", "ores_currencies",
        *channel_registry, "Currency data modified");
    eventing::service::registrar::register_mapping<
        refdata::eventing::country_changed_event>(
        event_source, "ores.refdata.country", "ores_countries",
        *channel_registry, "Country data modified");
    eventing::service::registrar::register_mapping<
        refdata::eventing::party_changed_event>(
        event_source, "ores.refdata.party", "ores_parties",
        *channel_registry, "Party data modified");
    eventing::service::registrar::register_mapping<
        refdata::eventing::party_identifier_changed_event>(
        event_source, "ores.refdata.party_identifier", "ores_party_identifiers",
        *channel_registry, "Party identifier data modified");
    eventing::service::registrar::register_mapping<
        refdata::eventing::party_contact_information_changed_event>(
        event_source, "ores.refdata.party_contact_information", "ores_party_contact_informations",
        *channel_registry, "Party contact information data modified");
    eventing::service::registrar::register_mapping<
        refdata::eventing::counterparty_changed_event>(
        event_source, "ores.refdata.counterparty", "ores_counterparties",
        *channel_registry, "Counterparty data modified");
    eventing::service::registrar::register_mapping<
        refdata::eventing::counterparty_identifier_changed_event>(
        event_source, "ores.refdata.counterparty_identifier", "ores_counterparty_identifiers",
        *channel_registry, "Counterparty identifier data modified");
    eventing::service::registrar::register_mapping<
        refdata::eventing::counterparty_contact_information_changed_event>(
        event_source, "ores.refdata.counterparty_contact_information", "ores_counterparty_contact_informations",
        *channel_registry, "Counterparty contact information data modified");
    eventing::service::registrar::register_mapping<
        iam::eventing::account_changed_event>(
        event_source, "ores.iam.account", "ores_accounts",
        *channel_registry, "Account data modified");
    eventing::service::registrar::register_mapping<
        iam::eventing::tenant_changed_event>(
        event_source, "ores.iam.tenant", "ores_tenants",
        *channel_registry, "Tenant data modified");
    eventing::service::registrar::register_mapping<
        dq::eventing::change_reason_changed_event>(
        event_source, "ores.dq.change_reason", "ores_change_reasons",
        *channel_registry, "Change reason data modified");
    eventing::service::registrar::register_mapping<
        dq::eventing::change_reason_category_changed_event>(
        event_source, "ores.dq.change_reason_category", "ores_change_reason_categories",
        *channel_registry, "Change reason category data modified");
    eventing::service::registrar::register_mapping<
        dq::eventing::origin_dimension_changed_event>(
        event_source, "ores.dq.origin_dimension", "ores_origin_dimensions",
        *channel_registry, "Origin dimension data modified");
    eventing::service::registrar::register_mapping<
        dq::eventing::nature_dimension_changed_event>(
        event_source, "ores.dq.nature_dimension", "ores_nature_dimensions",
        *channel_registry, "Nature dimension data modified");
    eventing::service::registrar::register_mapping<
        dq::eventing::treatment_dimension_changed_event>(
        event_source, "ores.dq.treatment_dimension", "ores_treatment_dimensions",
        *channel_registry, "Treatment dimension data modified");
    eventing::service::registrar::register_mapping<
        dq::eventing::catalog_changed_event>(
        event_source, "ores.dq.catalog", "ores_catalogs",
        *channel_registry, "Catalog data modified");
    eventing::service::registrar::register_mapping<
        dq::eventing::dataset_changed_event>(
        event_source, "ores.dq.dataset", "ores_datasets",
        *channel_registry, "Dataset data modified");
    eventing::service::registrar::register_mapping<
        dq::eventing::methodology_changed_event>(
        event_source, "ores.dq.methodology", "ores_methodologies",
        *channel_registry, "Methodology data modified");
    eventing::service::registrar::register_mapping<
        dq::eventing::coding_scheme_changed_event>(
        event_source, "ores.dq.coding_scheme", "ores_coding_schemes",
        *channel_registry, "Coding scheme data modified");
    eventing::service::registrar::register_mapping<
        dq::eventing::coding_scheme_authority_type_changed_event>(
        event_source, "ores.dq.coding_scheme_authority_type", "ores_coding_scheme_authority_types",
        *channel_registry, "Coding scheme authority type data modified");
    eventing::service::registrar::register_mapping<
        dq::eventing::data_domain_changed_event>(
        event_source, "ores.dq.data_domain", "ores_data_domains",
        *channel_registry, "Data domain data modified");
    eventing::service::registrar::register_mapping<
        dq::eventing::subject_area_changed_event>(
        event_source, "ores.dq.subject_area", "ores_subject_areas",
        *channel_registry, "Subject area data modified");
    eventing::service::registrar::register_mapping<
        iam::eventing::role_changed_event>(
        event_source, "ores.iam.role", "ores_roles",
        *channel_registry, "Role data modified");
    eventing::service::registrar::register_mapping<
        iam::eventing::permission_changed_event>(
        event_source, "ores.iam.permission", "ores_permissions",
        *channel_registry, "Permission data modified");
    eventing::service::registrar::register_mapping<
        assets::eventing::assets_changed_event>(
        event_source, "ores.assets.currency_image", "ores_currency_images",
        *channel_registry, "Currency image assets modified");
    eventing::service::registrar::register_mapping<
        variability::eventing::feature_flags_changed_event>(
        event_source, "ores.variability.feature_flag", "ores_feature_flags",
        *channel_registry, "Feature flags modified");
    eventing::service::registrar::register_mapping<
        trading::eventing::trade_changed_event>(
        event_source, "ores.trading.trade", "ores_trading_trades",
        *channel_registry, "Trade data modified");
    eventing::service::registrar::register_mapping<
        refdata::eventing::book_changed_event>(
        event_source, "ores.refdata.book", "ores_books",
        *channel_registry, "Book data modified");
    eventing::service::registrar::register_mapping<
        refdata::eventing::portfolio_changed_event>(
        event_source, "ores.refdata.portfolio", "ores_portfolios",
        *channel_registry, "Portfolio data modified");

    // Start the event source to begin listening for database notifications
    event_source.start();

    // Create subscription manager for client notifications
    auto subscription_mgr = std::make_shared<comms::service::subscription_manager>();

    // Bridge event bus to subscription manager - when domain events occur,
    // notify all clients subscribed to those event types
    auto currency_sub = event_bus.subscribe<refdata::eventing::currency_changed_event>(
        [&subscription_mgr](const refdata::eventing::currency_changed_event& e) {
            using traits = eventing::domain::event_traits<
                refdata::eventing::currency_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.iso_codes, e.tenant_id);
        });

    auto country_sub = event_bus.subscribe<refdata::eventing::country_changed_event>(
        [&subscription_mgr](const refdata::eventing::country_changed_event& e) {
            using traits = eventing::domain::event_traits<
                refdata::eventing::country_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.alpha2_codes, e.tenant_id);
        });

    auto party_sub = event_bus.subscribe<refdata::eventing::party_changed_event>(
        [&subscription_mgr](const refdata::eventing::party_changed_event& e) {
            using traits = eventing::domain::event_traits<
                refdata::eventing::party_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.ids, e.tenant_id);
        });

    auto party_identifier_sub = event_bus.subscribe<refdata::eventing::party_identifier_changed_event>(
        [&subscription_mgr](const refdata::eventing::party_identifier_changed_event& e) {
            using traits = eventing::domain::event_traits<
                refdata::eventing::party_identifier_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.ids, e.tenant_id);
        });

    auto party_contact_sub = event_bus.subscribe<refdata::eventing::party_contact_information_changed_event>(
        [&subscription_mgr](const refdata::eventing::party_contact_information_changed_event& e) {
            using traits = eventing::domain::event_traits<
                refdata::eventing::party_contact_information_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.ids, e.tenant_id);
        });

    auto counterparty_sub = event_bus.subscribe<refdata::eventing::counterparty_changed_event>(
        [&subscription_mgr](const refdata::eventing::counterparty_changed_event& e) {
            using traits = eventing::domain::event_traits<
                refdata::eventing::counterparty_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.ids, e.tenant_id);
        });

    auto counterparty_identifier_sub = event_bus.subscribe<refdata::eventing::counterparty_identifier_changed_event>(
        [&subscription_mgr](const refdata::eventing::counterparty_identifier_changed_event& e) {
            using traits = eventing::domain::event_traits<
                refdata::eventing::counterparty_identifier_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.ids, e.tenant_id);
        });

    auto counterparty_contact_sub = event_bus.subscribe<refdata::eventing::counterparty_contact_information_changed_event>(
        [&subscription_mgr](const refdata::eventing::counterparty_contact_information_changed_event& e) {
            using traits = eventing::domain::event_traits<
                refdata::eventing::counterparty_contact_information_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.ids, e.tenant_id);
        });

    auto account_sub = event_bus.subscribe<iam::eventing::account_changed_event>(
        [&subscription_mgr](const iam::eventing::account_changed_event& e) {
            using traits = eventing::domain::event_traits<
                iam::eventing::account_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.account_ids, e.tenant_id);
        });

    auto tenant_sub = event_bus.subscribe<iam::eventing::tenant_changed_event>(
        [&subscription_mgr](const iam::eventing::tenant_changed_event& e) {
            using traits = eventing::domain::event_traits<
                iam::eventing::tenant_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.tenant_ids, e.tenant_id);
        });

    auto assets_sub = event_bus.subscribe<assets::eventing::assets_changed_event>(
        [&subscription_mgr](const assets::eventing::assets_changed_event& e) {
            using traits = eventing::domain::event_traits<
                assets::eventing::assets_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.iso_codes, e.tenant_id);
        });

    auto change_reason_sub = event_bus.subscribe<dq::eventing::change_reason_changed_event>(
        [&subscription_mgr](const dq::eventing::change_reason_changed_event& e) {
            using traits = eventing::domain::event_traits<
                dq::eventing::change_reason_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.reason_codes, e.tenant_id);
        });

    auto change_reason_category_sub = event_bus.subscribe<dq::eventing::change_reason_category_changed_event>(
        [&subscription_mgr](const dq::eventing::change_reason_category_changed_event& e) {
            using traits = eventing::domain::event_traits<
                dq::eventing::change_reason_category_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.category_codes, e.tenant_id);
        });

    auto origin_dimension_sub = event_bus.subscribe<dq::eventing::origin_dimension_changed_event>(
        [&subscription_mgr](const dq::eventing::origin_dimension_changed_event& e) {
            using traits = eventing::domain::event_traits<
                dq::eventing::origin_dimension_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.codes, e.tenant_id);
        });

    auto nature_dimension_sub = event_bus.subscribe<dq::eventing::nature_dimension_changed_event>(
        [&subscription_mgr](const dq::eventing::nature_dimension_changed_event& e) {
            using traits = eventing::domain::event_traits<
                dq::eventing::nature_dimension_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.codes, e.tenant_id);
        });

    auto treatment_dimension_sub = event_bus.subscribe<dq::eventing::treatment_dimension_changed_event>(
        [&subscription_mgr](const dq::eventing::treatment_dimension_changed_event& e) {
            using traits = eventing::domain::event_traits<
                dq::eventing::treatment_dimension_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.codes, e.tenant_id);
        });

    auto catalog_sub = event_bus.subscribe<dq::eventing::catalog_changed_event>(
        [&subscription_mgr](const dq::eventing::catalog_changed_event& e) {
            using traits = eventing::domain::event_traits<
                dq::eventing::catalog_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.names, e.tenant_id);
        });

    auto dataset_sub = event_bus.subscribe<dq::eventing::dataset_changed_event>(
        [&subscription_mgr](const dq::eventing::dataset_changed_event& e) {
            using traits = eventing::domain::event_traits<
                dq::eventing::dataset_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.ids, e.tenant_id);
        });

    auto methodology_sub = event_bus.subscribe<dq::eventing::methodology_changed_event>(
        [&subscription_mgr](const dq::eventing::methodology_changed_event& e) {
            using traits = eventing::domain::event_traits<
                dq::eventing::methodology_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.ids, e.tenant_id);
        });

    auto coding_scheme_sub = event_bus.subscribe<dq::eventing::coding_scheme_changed_event>(
        [&subscription_mgr](const dq::eventing::coding_scheme_changed_event& e) {
            using traits = eventing::domain::event_traits<
                dq::eventing::coding_scheme_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.codes, e.tenant_id);
        });

    auto coding_scheme_authority_type_sub = event_bus.subscribe<dq::eventing::coding_scheme_authority_type_changed_event>(
        [&subscription_mgr](const dq::eventing::coding_scheme_authority_type_changed_event& e) {
            using traits = eventing::domain::event_traits<
                dq::eventing::coding_scheme_authority_type_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.codes, e.tenant_id);
        });

    auto data_domain_sub = event_bus.subscribe<dq::eventing::data_domain_changed_event>(
        [&subscription_mgr](const dq::eventing::data_domain_changed_event& e) {
            using traits = eventing::domain::event_traits<
                dq::eventing::data_domain_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.names, e.tenant_id);
        });

    auto subject_area_sub = event_bus.subscribe<dq::eventing::subject_area_changed_event>(
        [&subscription_mgr](const dq::eventing::subject_area_changed_event& e) {
            using traits = eventing::domain::event_traits<
                dq::eventing::subject_area_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.keys, e.tenant_id);
        });

    auto role_sub = event_bus.subscribe<iam::eventing::role_changed_event>(
        [&subscription_mgr](const iam::eventing::role_changed_event& e) {
            using traits = eventing::domain::event_traits<
                iam::eventing::role_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.role_ids, e.tenant_id);
        });

    auto permission_sub = event_bus.subscribe<iam::eventing::permission_changed_event>(
        [&subscription_mgr](const iam::eventing::permission_changed_event& e) {
            using traits = eventing::domain::event_traits<
                iam::eventing::permission_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.permission_ids, e.tenant_id);
        });

    // Subscribe to feature flag changes to refresh system_flags cache.
    // This handles cross-service cache invalidation (e.g., HTTP server changes a flag,
    // this service gets notified via PostgreSQL NOTIFY and refreshes its cache).
    auto flags_sub = event_bus.subscribe<variability::eventing::feature_flags_changed_event>(
        [&system_flags, &subscription_mgr](const variability::eventing::feature_flags_changed_event& e) {
            BOOST_LOG_SEV(lg(), info) << "Feature flags changed notification received, "
                                      << "refreshing system_flags cache";
            system_flags->refresh();

            // Also notify subscribed clients
            using traits = eventing::domain::event_traits<
                variability::eventing::feature_flags_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.flag_names, e.tenant_id);
        });

    auto trade_sub = event_bus.subscribe<trading::eventing::trade_changed_event>(
        [&subscription_mgr](const trading::eventing::trade_changed_event& e) {
            using traits = eventing::domain::event_traits<
                trading::eventing::trade_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.trade_ids, e.tenant_id);
        });

    auto book_sub = event_bus.subscribe<refdata::eventing::book_changed_event>(
        [&subscription_mgr](const refdata::eventing::book_changed_event& e) {
            using traits = eventing::domain::event_traits<
                refdata::eventing::book_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.ids, e.tenant_id);
        });

    auto portfolio_sub = event_bus.subscribe<refdata::eventing::portfolio_changed_event>(
        [&subscription_mgr](const refdata::eventing::portfolio_changed_event& e) {
            using traits = eventing::domain::event_traits<
                refdata::eventing::portfolio_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.ids, e.tenant_id);
        });

    // Bridge pgmq queue messages to remote subscribers.
    // Each message is published under "ores.mq.q.<queue_name>" so clients can
    // subscribe to individual queues. The raw JSONB body is forwarded as the
    // binary payload with json encoding.
    auto mq_msg_sub = event_bus.subscribe<ores::mq::domain::queue_message_event>(
        [&subscription_mgr](const ores::mq::domain::queue_message_event& e) {
            const auto event_type = "ores.mq.q." + e.queue_name;
            const std::vector<std::string> entity_ids = {std::to_string(e.msg_id)};
            subscription_mgr->notify(
                event_type, e.timestamp, entity_ids, e.tenant_id,
                comms::messaging::payload_type::json,
                std::make_optional(e.payload));
        });

    // Create server with subscription manager
    auto srv = std::make_shared<ores::comms::net::server>(cfg.server, subscription_mgr);

    // Wire sessions service into subscription manager for tenant-aware filtering
    subscription_mgr->set_sessions_service(srv->sessions());

    // Create geolocation service using PostgreSQL geoip tables
    auto geo_service = std::make_shared<geo::service::geolocation_service>(ctx);

    // Create bundle provider for bootstrap wizard
    dq::service::dataset_bundle_service bundle_service(ctx);
    auto bundle_provider = [&bundle_service]() -> std::vector<iam::messaging::bootstrap_bundle_info> {
        std::vector<iam::messaging::bootstrap_bundle_info> result;
        for (const auto& bundle : bundle_service.list_bundles()) {
            result.push_back({
                .code = bundle.code,
                .name = bundle.name,
                .description = bundle.description
            });
        }
        return result;
    };

    // Register subsystem handlers
    ores::refdata::messaging::registrar::register_handlers(*srv, ctx, system_flags,
        srv->sessions());
    ores::iam::messaging::registrar::register_handlers(*srv, ctx, system_flags, auth_service,
        geo_service, bundle_provider);
    ores::variability::messaging::registrar::register_handlers(*srv, ctx, srv->sessions());
    ores::assets::messaging::registrar::register_handlers(*srv, ctx, srv->sessions());
    ores::telemetry::messaging::registrar::register_handlers(*srv, ctx, srv->sessions());
    ores::dq::messaging::registrar::register_handlers(*srv, ctx, auth_service);
    ores::synthetic::messaging::registrar::register_handlers(*srv, ctx, auth_service);
    ores::trading::messaging::registrar::register_handlers(*srv, ctx, srv->sessions());
    ores::scheduler::messaging::registrar::register_handlers(*srv, ctx, srv->sessions());

    // Register system info handler for get_system_info_request (no auth required)
    auto si_handler =
        std::make_shared<comms::service::messaging::system_info_handler>(ctx);
    srv->register_handler(
        {static_cast<std::uint16_t>(comms::messaging::message_type::get_system_info_request),
         static_cast<std::uint16_t>(comms::messaging::message_type::get_system_info_response)},
        si_handler);

    // Register subscription handler for subscribe/unsubscribe/list_event_channels messages
    // Range starts at 0x0009 to avoid overlapping with the system_info handler (0x0007-0x0008)
    auto subscription_handler =
        std::make_shared<comms::service::subscription_handler>(subscription_mgr, channel_registry);
    srv->register_handler(
        {0x0009, comms::messaging::CORE_SUBSYSTEM_MAX},
        subscription_handler);

    // Set up database health monitor callback to broadcast status changes to clients
    db_health_monitor.set_status_change_callback(
        [&srv](bool available, const std::string& error_message) {
            BOOST_LOG_SEV(lg(), info)
                << "Database status changed: "
                << (available ? "AVAILABLE" : "UNAVAILABLE");
            srv->broadcast_database_status(available, error_message);
        });

    // Start the database health monitor to continuously poll
    boost::asio::co_spawn(io_ctx,
        [&db_health_monitor, &io_ctx]() -> boost::asio::awaitable<void> {
            co_await db_health_monitor.run(io_ctx);
        },
        boost::asio::detached);

    co_await srv->run(io_ctx);

    // Stop the database health monitor
    db_health_monitor.stop();

    // Stop the event source
    event_source.stop();

    // Shutdown logging
    if (system_flags->is_bootstrap_mode_enabled()) {
        BOOST_LOG_SEV(lg(), warn) << "================================================";
        BOOST_LOG_SEV(lg(), warn) << "ORES Service stopped - STILL IN BOOTSTRAP MODE";
        BOOST_LOG_SEV(lg(), warn) << "Initial admin account was NOT created";
        BOOST_LOG_SEV(lg(), warn) << "================================================";
    } else {
        BOOST_LOG_SEV(lg(), info) << "ORES Service stopped normally (SECURE MODE).";
    }
}

}
