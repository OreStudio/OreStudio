/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.refdata.service/app/application.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.eventing.api/domain/entity_change_event.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.eventing.core/service/registrar.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/eventing/business_centre_changed_event.hpp"
#include "ores.refdata.api/eventing/counterparty_changed_event.hpp"
#include "ores.refdata.api/eventing/counterparty_contact_information_changed_event.hpp"
#include "ores.refdata.api/eventing/counterparty_identifier_changed_event.hpp"
#include "ores.refdata.api/eventing/currency_market_tier_changed_event.hpp"
#include "ores.refdata.api/eventing/monetary_nature_changed_event.hpp"
#include "ores.refdata.api/eventing/party_changed_event.hpp"
#include "ores.refdata.api/eventing/party_contact_information_changed_event.hpp"
#include "ores.refdata.api/eventing/party_identifier_changed_event.hpp"
#include "ores.refdata.api/eventing/party_status_changed_event.hpp"
#include "ores.refdata.api/eventing/portfolio_changed_event.hpp"
#include "ores.refdata.core/messaging/registrar.hpp"
#include "ores.refdata.service/app/application_exception.hpp"
#include "ores.refdata.service/messaging/event_registrar.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/version/version.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <algorithm>
#include <rfl/json.hpp>

namespace ores::refdata::service::app {

using namespace ores::logging;
namespace ev = ores::eventing;
namespace rdev = ores::refdata::eventing;

ores::database::context application::make_context(const ores::database::database_options& db_opts) {
    using ores::database::context_factory;

    context_factory::configuration cfg{.database_options = db_opts,
                                       .pool_size = 4,
                                       .num_attempts = 10,
                                       .wait_time_in_seconds = 1,
                                       .service_account = db_opts.user};

    return context_factory::make_context(cfg);
}

application::application() = default;

namespace {

constexpr std::string_view service_name = "ores.refdata.service";
constexpr std::string_view service_version = ORES_VERSION;

auto& pub_lg() {
    static auto instance = make_logger("ores.refdata.service.app");
    return instance;
}

void publish_entity_event(ores::nats::service::client& nats,
                          const std::string& subject,
                          const ev::domain::entity_change_event& notif) {
    try {
        const auto json = rfl::json::write(notif);
        std::vector<std::byte> data(json.size());
        std::transform(json.begin(), json.end(), data.begin(), [](char c) { return std::byte(c); });
        nats.publish(subject, std::move(data), {});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(pub_lg(), error)
            << "Failed to publish event to '" << subject << "': " << e.what();
    }
}

} // namespace

boost::asio::awaitable<void> application::run(boost::asio::io_context& io_ctx,
                                              const config::options& cfg) const {

    BOOST_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.refdata.service", 0, 1);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();
    BOOST_LOG_SEV(lg(), info) << "Connected to NATS: " << cfg.nats.url << " (namespace: '"
                              << (cfg.nats.subject_prefix.empty() ? "(none)" :
                                                                    cfg.nats.subject_prefix)
                              << "')";

    // =========================================================================
    // Entity change event pipeline: PostgreSQL LISTEN/NOTIFY → NATS publish
    // =========================================================================
    ev::service::event_bus event_bus;
    ev::service::postgres_event_source event_source(make_context(cfg.database), event_bus);

    // Generated per-entity event mappings (book, business_day_convention_type,
    // business_unit, business_unit_type, country, currency, party_id_scheme,
    // party_type, purpose_type). See event_registrar.cpp for why not every
    // entity is migrated here yet.
    auto generated_event_subs =
        messaging::event_registrar::register_event_mappings(event_source, event_bus, nats);

    ev::service::registrar::register_mapping<rdev::business_centre_changed_event>(
        event_source, "ores.refdata.business_centre", "ores_refdata_business_centres");
    ev::service::registrar::register_mapping<rdev::counterparty_changed_event>(
        event_source, "ores.refdata.counterparty", "ores_refdata_counterparties");
    ev::service::registrar::register_mapping<rdev::counterparty_contact_information_changed_event>(
        event_source,
        "ores.refdata.counterparty_contact_information",
        "ores_refdata_counterparty_contact_informations");
    ev::service::registrar::register_mapping<rdev::counterparty_identifier_changed_event>(
        event_source,
        "ores.refdata.counterparty_identifier",
        "ores_refdata_counterparty_identifiers");
    ev::service::registrar::register_mapping<rdev::currency_market_tier_changed_event>(
        event_source, "ores.refdata.currency_market_tier", "ores_refdata_currency_market_tiers");
    ev::service::registrar::register_mapping<rdev::monetary_nature_changed_event>(
        event_source, "ores.refdata.monetary_nature", "ores_refdata_monetary_natures");
    ev::service::registrar::register_mapping<rdev::party_changed_event>(
        event_source, "ores.refdata.party", "ores_refdata_parties");
    ev::service::registrar::register_mapping<rdev::party_contact_information_changed_event>(
        event_source,
        "ores.refdata.party_contact_information",
        "ores_refdata_party_contact_informations");
    ev::service::registrar::register_mapping<rdev::party_identifier_changed_event>(
        event_source, "ores.refdata.party_identifier", "ores_refdata_party_identifiers");
    ev::service::registrar::register_mapping<rdev::party_status_changed_event>(
        event_source, "ores.refdata.party_status", "ores_refdata_party_statuses");
    ev::service::registrar::register_mapping<rdev::portfolio_changed_event>(
        event_source, "ores.refdata.portfolio", "ores_refdata_portfolios");

    auto business_centre_sub = event_bus.subscribe<rdev::business_centre_changed_event>(
        [&nats](const rdev::business_centre_changed_event& e) {
            publish_entity_event(
                nats,
                std::string(ev::domain::event_traits<rdev::business_centre_changed_event>::name),
                ev::domain::entity_change_event{.entity = "ores.refdata.business_centre",
                                                .timestamp = e.timestamp,
                                                .entity_ids = e.codes,
                                                .tenant_id = e.tenant_id});
        });

    auto counterparty_sub = event_bus.subscribe<rdev::counterparty_changed_event>(
        [&nats](const rdev::counterparty_changed_event& e) {
            publish_entity_event(
                nats,
                std::string(ev::domain::event_traits<rdev::counterparty_changed_event>::name),
                ev::domain::entity_change_event{.entity = "ores.refdata.counterparty",
                                                .timestamp = e.timestamp,
                                                .entity_ids = e.ids,
                                                .tenant_id = e.tenant_id});
        });

    auto counterparty_contact_sub =
        event_bus.subscribe<rdev::counterparty_contact_information_changed_event>(
            [&nats](const rdev::counterparty_contact_information_changed_event& e) {
                publish_entity_event(
                    nats,
                    std::string(ev::domain::event_traits<
                                rdev::counterparty_contact_information_changed_event>::name),
                    ev::domain::entity_change_event{
                        .entity = "ores.refdata.counterparty_contact_information",
                        .timestamp = e.timestamp,
                        .entity_ids = e.counterparty_contact_information_ids,
                        .tenant_id = e.tenant_id});
            });

    auto counterparty_id_sub = event_bus.subscribe<rdev::counterparty_identifier_changed_event>(
        [&nats](const rdev::counterparty_identifier_changed_event& e) {
            publish_entity_event(
                nats,
                std::string(
                    ev::domain::event_traits<rdev::counterparty_identifier_changed_event>::name),
                ev::domain::entity_change_event{.entity = "ores.refdata.counterparty_identifier",
                                                .timestamp = e.timestamp,
                                                .entity_ids = e.counterparty_identifier_ids,
                                                .tenant_id = e.tenant_id});
        });

    auto currency_market_tier_sub = event_bus.subscribe<rdev::currency_market_tier_changed_event>(
        [&nats](const rdev::currency_market_tier_changed_event& e) {
            publish_entity_event(
                nats,
                std::string(
                    ev::domain::event_traits<rdev::currency_market_tier_changed_event>::name),
                ev::domain::entity_change_event{.entity = "ores.refdata.currency_market_tier",
                                                .timestamp = e.timestamp,
                                                .entity_ids = e.codes,
                                                .tenant_id = e.tenant_id});
        });

    auto monetary_nature_sub = event_bus.subscribe<rdev::monetary_nature_changed_event>(
        [&nats](const rdev::monetary_nature_changed_event& e) {
            publish_entity_event(
                nats,
                std::string(ev::domain::event_traits<rdev::monetary_nature_changed_event>::name),
                ev::domain::entity_change_event{.entity = "ores.refdata.monetary_nature",
                                                .timestamp = e.timestamp,
                                                .entity_ids = e.codes,
                                                .tenant_id = e.tenant_id});
        });

    auto party_sub =
        event_bus.subscribe<rdev::party_changed_event>([&nats](const rdev::party_changed_event& e) {
            publish_entity_event(
                nats,
                std::string(ev::domain::event_traits<rdev::party_changed_event>::name),
                ev::domain::entity_change_event{.entity = "ores.refdata.party",
                                                .timestamp = e.timestamp,
                                                .entity_ids = e.ids,
                                                .tenant_id = e.tenant_id});
        });

    auto party_contact_sub = event_bus.subscribe<rdev::party_contact_information_changed_event>(
        [&nats](const rdev::party_contact_information_changed_event& e) {
            publish_entity_event(
                nats,
                std::string(
                    ev::domain::event_traits<rdev::party_contact_information_changed_event>::name),
                ev::domain::entity_change_event{.entity = "ores.refdata.party_contact_information",
                                                .timestamp = e.timestamp,
                                                .entity_ids = e.party_contact_information_ids,
                                                .tenant_id = e.tenant_id});
        });

    auto party_id_sub = event_bus.subscribe<rdev::party_identifier_changed_event>(
        [&nats](const rdev::party_identifier_changed_event& e) {
            publish_entity_event(
                nats,
                std::string(ev::domain::event_traits<rdev::party_identifier_changed_event>::name),
                ev::domain::entity_change_event{.entity = "ores.refdata.party_identifier",
                                                .timestamp = e.timestamp,
                                                .entity_ids = e.party_identifier_ids,
                                                .tenant_id = e.tenant_id});
        });

    auto party_status_sub = event_bus.subscribe<rdev::party_status_changed_event>(
        [&nats](const rdev::party_status_changed_event& e) {
            publish_entity_event(
                nats,
                std::string(ev::domain::event_traits<rdev::party_status_changed_event>::name),
                ev::domain::entity_change_event{.entity = "ores.refdata.party_status",
                                                .timestamp = e.timestamp,
                                                .entity_ids = e.codes,
                                                .tenant_id = e.tenant_id});
        });

    auto portfolio_sub = event_bus.subscribe<rdev::portfolio_changed_event>(
        [&nats](const rdev::portfolio_changed_event& e) {
            publish_entity_event(
                nats,
                std::string(ev::domain::event_traits<rdev::portfolio_changed_event>::name),
                ev::domain::entity_change_event{.entity = "ores.refdata.portfolio",
                                                .timestamp = e.timestamp,
                                                .entity_ids = e.ids,
                                                .tenant_id = e.tenant_id});
        });

    event_source.start();
    BOOST_LOG_SEV(lg(), info) << "Entity change event pipeline started.";

    co_await ores::service::service::run(
        io_ctx,
        nats,
        make_context(cfg.database),
        "ores.refdata.service",
        [](auto& n, auto c, auto v) {
            return ores::refdata::messaging::registrar::register_handlers(
                n, std::move(c), std::move(v));
        },
        [&nats](boost::asio::io_context& ioc) {
            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats);
            boost::asio::co_spawn(ioc, [hb]() { return hb->run(); }, boost::asio::detached);
        });

    event_source.stop();
    co_return;
}

}
