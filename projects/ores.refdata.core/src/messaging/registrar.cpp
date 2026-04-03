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
#include "ores.refdata.core/messaging/registrar.hpp"

#include <memory>
#include <optional>
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.refdata.core/messaging/country_handler.hpp"
#include "ores.refdata.core/messaging/currency_handler.hpp"
#include "ores.refdata.core/messaging/currency_market_tier_handler.hpp"
#include "ores.refdata.core/messaging/party_handler.hpp"
#include "ores.refdata.core/messaging/party_type_handler.hpp"
#include "ores.refdata.core/messaging/party_status_handler.hpp"
#include "ores.refdata.core/messaging/party_identifier_handler.hpp"
#include "ores.refdata.core/messaging/party_id_scheme_handler.hpp"
#include "ores.refdata.core/messaging/party_contact_handler.hpp"
#include "ores.refdata.core/messaging/counterparty_handler.hpp"
#include "ores.refdata.core/messaging/counterparty_identifier_handler.hpp"
#include "ores.refdata.core/messaging/counterparty_contact_handler.hpp"
#include "ores.refdata.core/messaging/book_handler.hpp"
#include "ores.refdata.core/messaging/book_status_handler.hpp"
#include "ores.refdata.core/messaging/portfolio_handler.hpp"
#include "ores.refdata.core/messaging/business_unit_handler.hpp"
#include "ores.refdata.core/messaging/business_unit_type_handler.hpp"
#include "ores.refdata.core/messaging/business_centre_handler.hpp"
#include "ores.refdata.core/messaging/contact_type_handler.hpp"
#include "ores.refdata.core/messaging/monetary_nature_handler.hpp"
#include "ores.refdata.core/messaging/rounding_type_handler.hpp"
#include "ores.refdata.core/messaging/purpose_type_handler.hpp"
#include "ores.refdata.core/messaging/asset_class_handler.hpp"
#include "ores.refdata.api/messaging/country_protocol.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.refdata.api/messaging/currency_history_protocol.hpp"
#include "ores.refdata.api/messaging/currency_market_tier_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.refdata.api/messaging/party_type_protocol.hpp"
#include "ores.refdata.api/messaging/party_status_protocol.hpp"
#include "ores.refdata.api/messaging/party_identifier_protocol.hpp"
#include "ores.refdata.api/messaging/party_id_scheme_protocol.hpp"
#include "ores.refdata.api/messaging/party_contact_information_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_identifier_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_contact_information_protocol.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.api/messaging/book_status_protocol.hpp"
#include "ores.refdata.api/messaging/portfolio_protocol.hpp"
#include "ores.refdata.api/messaging/business_unit_protocol.hpp"
#include "ores.refdata.api/messaging/business_unit_type_protocol.hpp"
#include "ores.refdata.api/messaging/business_centre_protocol.hpp"
#include "ores.refdata.api/messaging/contact_type_protocol.hpp"
#include "ores.refdata.api/messaging/monetary_nature_protocol.hpp"
#include "ores.refdata.api/messaging/rounding_type_protocol.hpp"
#include "ores.refdata.api/messaging/purpose_type_protocol.hpp"
#include "ores.refdata.api/messaging/asset_class_protocol.hpp"

namespace ores::refdata::messaging {

namespace {
static constexpr std::string_view queue_group = "ores.refdata.service";
} // namespace

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    // ----------------------------------------------------------------
    // Countries
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<country_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_countries_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_country_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_country_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_country_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Currencies
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<currency_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_currencies_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_currency_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_currency_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_currency_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Currency market tiers
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<currency_market_tier_handler>(
            nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_currency_market_tiers_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_currency_market_tier_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_currency_market_tier_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_currency_market_tier_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Parties
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<party_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_parties_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_party_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_party_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_party_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Party types
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<party_type_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_party_types_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_party_type_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_party_type_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_party_type_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Party statuses
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<party_status_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_party_statuses_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_party_status_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_party_status_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_party_status_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Party identifiers
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<party_identifier_handler>(
            nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_party_identifiers_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_party_identifier_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_party_identifier_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Party ID schemes
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<party_id_scheme_handler>(
            nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_party_id_schemes_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_party_id_scheme_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_party_id_scheme_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_party_id_scheme_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Party contacts
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<party_contact_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_party_contact_informations_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_party_contact_information_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_party_contact_information_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Counterparties
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<counterparty_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_counterparties_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_counterparty_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_counterparty_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_counterparty_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Counterparty identifiers
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<counterparty_identifier_handler>(
            nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_counterparty_identifiers_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_counterparty_identifier_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_counterparty_identifier_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Counterparty contacts
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<counterparty_contact_handler>(
            nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_counterparty_contact_informations_request::nats_subject,
            queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_counterparty_contact_information_request::nats_subject,
            queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_counterparty_contact_information_request::nats_subject,
            queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Books
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<book_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_books_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_book_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_book_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_book_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Book statuses
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<book_status_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_book_statuses_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_book_status_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_book_status_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_book_status_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Portfolios
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<portfolio_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_portfolios_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_portfolio_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_portfolio_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_portfolio_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Business units
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<business_unit_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_business_units_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_business_unit_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_business_unit_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_business_unit_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Business unit types
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<business_unit_type_handler>(
            nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_business_unit_types_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_business_unit_type_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_business_unit_type_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_business_unit_type_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Business centres
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<business_centre_handler>(
            nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_business_centres_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_business_centre_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_business_centre_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_business_centre_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Contact types
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<contact_type_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_contact_types_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_contact_type_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_contact_type_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_contact_type_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Monetary natures
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<monetary_nature_handler>(
            nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_monetary_natures_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_monetary_nature_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_monetary_nature_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_monetary_nature_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Rounding types
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<rounding_type_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_rounding_types_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_rounding_type_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_rounding_type_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_rounding_type_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Purpose types
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<purpose_type_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_purpose_types_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            save_purpose_type_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            delete_purpose_type_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->del(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            get_purpose_type_history_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Asset classes
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<asset_class_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_asset_classes_request::nats_subject, queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
    }

    return subs;
}

} // namespace ores::refdata::messaging
