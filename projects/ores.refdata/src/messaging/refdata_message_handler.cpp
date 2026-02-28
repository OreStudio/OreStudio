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
#include "ores.utility/rfl/reflectors.hpp" // Must be before rfl/json.hpp
#include "ores.refdata/messaging/refdata_message_handler.hpp"

#include <sstream>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.refdata/messaging/protocol.hpp"
#include "ores.refdata/service/currency_service.hpp"
#include "ores.refdata/service/country_service.hpp"
#include "ores.refdata/service/business_centre_service.hpp"
#include "ores.refdata/service/party_type_service.hpp"
#include "ores.refdata/service/party_status_service.hpp"
#include "ores.refdata/service/party_id_scheme_service.hpp"
#include "ores.refdata/service/contact_type_service.hpp"
#include "ores.refdata/service/party_service.hpp"
#include "ores.refdata/service/counterparty_service.hpp"
#include "ores.refdata/service/party_identifier_service.hpp"
#include "ores.refdata/service/party_contact_information_service.hpp"
#include "ores.refdata/service/counterparty_identifier_service.hpp"
#include "ores.refdata/service/counterparty_contact_information_service.hpp"
#include "ores.refdata/service/business_unit_service.hpp"
#include "ores.refdata/repository/business_unit_type_repository.hpp"
#include "ores.refdata/messaging/business_unit_type_protocol.hpp"
#include "ores.refdata/service/portfolio_service.hpp"
#include "ores.refdata/service/book_service.hpp"
#include "ores.refdata/service/book_status_service.hpp"
#include "ores.refdata/messaging/book_status_protocol.hpp"
#include "ores.refdata/service/purpose_type_service.hpp"
#include "ores.refdata/messaging/purpose_type_protocol.hpp"
#include "ores.refdata/service/rounding_type_service.hpp"
#include "ores.refdata/messaging/rounding_type_protocol.hpp"
#include "ores.refdata/service/monetary_nature_service.hpp"
#include "ores.refdata/messaging/monetary_nature_protocol.hpp"
#include "ores.refdata/messaging/monetary_nature_history_protocol.hpp"
#include "ores.refdata/service/currency_market_tier_service.hpp"
#include "ores.refdata/messaging/currency_market_tier_protocol.hpp"
#include "ores.refdata/messaging/currency_market_tier_history_protocol.hpp"

namespace ores::refdata::messaging {

using namespace ores::logging;

refdata_message_handler::refdata_message_handler(database::context ctx,
    std::shared_ptr<variability::service::system_flags_service> system_flags,
    std::shared_ptr<comms::service::auth_session_service> sessions)
    : tenant_aware_handler(std::move(ctx), std::move(sessions))
    , system_flags_(std::move(system_flags)) {}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::handle_message(comms::messaging::message_type type,
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling refdata message type " << type;

    // Block all refdata operations when in bootstrap mode
    if (system_flags_->is_bootstrap_mode_enabled()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Blocked refdata operation " << type << " - system in bootstrap mode";
        co_return std::unexpected(ores::utility::serialization::error_code::bootstrap_mode_only);
    }

    switch (type) {
    // Currency handlers
    case comms::messaging::message_type::get_currencies_request:
        co_return co_await handle_get_currencies_request(payload, remote_address);
    case comms::messaging::message_type::save_currency_request:
        co_return co_await handle_save_currency_request(payload, remote_address);
    case comms::messaging::message_type::delete_currency_request:
        co_return co_await handle_delete_currency_request(payload, remote_address);
    case comms::messaging::message_type::get_currency_history_request:
        co_return co_await handle_get_currency_history_request(payload, remote_address);
    // Business centre handlers
    case comms::messaging::message_type::get_business_centres_request:
        co_return co_await handle_get_business_centres_request(payload, remote_address);
    case comms::messaging::message_type::save_business_centre_request:
        co_return co_await handle_save_business_centre_request(payload, remote_address);
    case comms::messaging::message_type::delete_business_centre_request:
        co_return co_await handle_delete_business_centre_request(payload, remote_address);
    case comms::messaging::message_type::get_business_centre_history_request:
        co_return co_await handle_get_business_centre_history_request(payload, remote_address);
    // Country handlers
    case comms::messaging::message_type::get_countries_request:
        co_return co_await handle_get_countries_request(payload, remote_address);
    case comms::messaging::message_type::save_country_request:
        co_return co_await handle_save_country_request(payload, remote_address);
    case comms::messaging::message_type::delete_country_request:
        co_return co_await handle_delete_country_request(payload, remote_address);
    case comms::messaging::message_type::get_country_history_request:
        co_return co_await handle_get_country_history_request(payload, remote_address);
    // Party type handlers
    case comms::messaging::message_type::get_party_types_request:
        co_return co_await handle_get_party_types_request(payload, remote_address);
    case comms::messaging::message_type::save_party_type_request:
        co_return co_await handle_save_party_type_request(payload, remote_address);
    case comms::messaging::message_type::delete_party_type_request:
        co_return co_await handle_delete_party_type_request(payload, remote_address);
    case comms::messaging::message_type::get_party_type_history_request:
        co_return co_await handle_get_party_type_history_request(payload, remote_address);
    // Party status handlers
    case comms::messaging::message_type::get_party_statuses_request:
        co_return co_await handle_get_party_statuses_request(payload, remote_address);
    case comms::messaging::message_type::save_party_status_request:
        co_return co_await handle_save_party_status_request(payload, remote_address);
    case comms::messaging::message_type::delete_party_status_request:
        co_return co_await handle_delete_party_status_request(payload, remote_address);
    case comms::messaging::message_type::get_party_status_history_request:
        co_return co_await handle_get_party_status_history_request(payload, remote_address);
    // Party ID scheme handlers
    case comms::messaging::message_type::get_party_id_schemes_request:
        co_return co_await handle_get_party_id_schemes_request(payload, remote_address);
    case comms::messaging::message_type::save_party_id_scheme_request:
        co_return co_await handle_save_party_id_scheme_request(payload, remote_address);
    case comms::messaging::message_type::delete_party_id_scheme_request:
        co_return co_await handle_delete_party_id_scheme_request(payload, remote_address);
    case comms::messaging::message_type::get_party_id_scheme_history_request:
        co_return co_await handle_get_party_id_scheme_history_request(payload, remote_address);
    // Contact type handlers
    case comms::messaging::message_type::get_contact_types_request:
        co_return co_await handle_get_contact_types_request(payload, remote_address);
    case comms::messaging::message_type::save_contact_type_request:
        co_return co_await handle_save_contact_type_request(payload, remote_address);
    case comms::messaging::message_type::delete_contact_type_request:
        co_return co_await handle_delete_contact_type_request(payload, remote_address);
    case comms::messaging::message_type::get_contact_type_history_request:
        co_return co_await handle_get_contact_type_history_request(payload, remote_address);
    // Party handlers
    case comms::messaging::message_type::get_parties_request:
        co_return co_await handle_get_parties_request(payload, remote_address);
    case comms::messaging::message_type::save_party_request:
        co_return co_await handle_save_party_request(payload, remote_address);
    case comms::messaging::message_type::delete_party_request:
        co_return co_await handle_delete_party_request(payload, remote_address);
    case comms::messaging::message_type::get_party_history_request:
        co_return co_await handle_get_party_history_request(payload, remote_address);
    // Counterparty handlers
    case comms::messaging::message_type::get_counterparties_request:
        co_return co_await handle_get_counterparties_request(payload, remote_address);
    case comms::messaging::message_type::save_counterparty_request:
        co_return co_await handle_save_counterparty_request(payload, remote_address);
    case comms::messaging::message_type::delete_counterparty_request:
        co_return co_await handle_delete_counterparty_request(payload, remote_address);
    case comms::messaging::message_type::get_counterparty_history_request:
        co_return co_await handle_get_counterparty_history_request(payload, remote_address);
    // Party identifier handlers
    case comms::messaging::message_type::get_party_identifiers_request:
        co_return co_await handle_get_party_identifiers_request(payload, remote_address);
    case comms::messaging::message_type::save_party_identifier_request:
        co_return co_await handle_save_party_identifier_request(payload, remote_address);
    case comms::messaging::message_type::delete_party_identifier_request:
        co_return co_await handle_delete_party_identifier_request(payload, remote_address);
    case comms::messaging::message_type::get_party_identifier_history_request:
        co_return co_await handle_get_party_identifier_history_request(payload, remote_address);
    // Party contact information handlers
    case comms::messaging::message_type::get_party_contact_informations_request:
        co_return co_await handle_get_party_contact_informations_request(payload, remote_address);
    case comms::messaging::message_type::save_party_contact_information_request:
        co_return co_await handle_save_party_contact_information_request(payload, remote_address);
    case comms::messaging::message_type::delete_party_contact_information_request:
        co_return co_await handle_delete_party_contact_information_request(payload, remote_address);
    case comms::messaging::message_type::get_party_contact_information_history_request:
        co_return co_await handle_get_party_contact_information_history_request(payload, remote_address);
    // Counterparty identifier handlers
    case comms::messaging::message_type::get_counterparty_identifiers_request:
        co_return co_await handle_get_counterparty_identifiers_request(payload, remote_address);
    case comms::messaging::message_type::save_counterparty_identifier_request:
        co_return co_await handle_save_counterparty_identifier_request(payload, remote_address);
    case comms::messaging::message_type::delete_counterparty_identifier_request:
        co_return co_await handle_delete_counterparty_identifier_request(payload, remote_address);
    case comms::messaging::message_type::get_counterparty_identifier_history_request:
        co_return co_await handle_get_counterparty_identifier_history_request(payload, remote_address);
    // Counterparty contact information handlers
    case comms::messaging::message_type::get_counterparty_contact_informations_request:
        co_return co_await handle_get_counterparty_contact_informations_request(payload, remote_address);
    case comms::messaging::message_type::save_counterparty_contact_information_request:
        co_return co_await handle_save_counterparty_contact_information_request(payload, remote_address);
    case comms::messaging::message_type::delete_counterparty_contact_information_request:
        co_return co_await handle_delete_counterparty_contact_information_request(payload, remote_address);
    case comms::messaging::message_type::get_counterparty_contact_information_history_request:
        co_return co_await handle_get_counterparty_contact_information_history_request(payload, remote_address);
    // Business unit handlers
    case comms::messaging::message_type::get_business_units_request:
        co_return co_await handle_get_business_units_request(payload, remote_address);
    case comms::messaging::message_type::save_business_unit_request:
        co_return co_await handle_save_business_unit_request(payload, remote_address);
    case comms::messaging::message_type::delete_business_unit_request:
        co_return co_await handle_delete_business_unit_request(payload, remote_address);
    case comms::messaging::message_type::get_business_unit_history_request:
        co_return co_await handle_get_business_unit_history_request(payload, remote_address);
    // Business unit type handlers
    case comms::messaging::message_type::get_business_unit_types_request:
        co_return co_await handle_get_business_unit_types_request(payload, remote_address);
    case comms::messaging::message_type::save_business_unit_type_request:
        co_return co_await handle_save_business_unit_type_request(payload, remote_address);
    case comms::messaging::message_type::delete_business_unit_type_request:
        co_return co_await handle_delete_business_unit_type_request(payload, remote_address);
    case comms::messaging::message_type::get_business_unit_type_history_request:
        co_return co_await handle_get_business_unit_type_history_request(payload, remote_address);
    // Portfolio handlers
    case comms::messaging::message_type::get_portfolios_request:
        co_return co_await handle_get_portfolios_request(payload, remote_address);
    case comms::messaging::message_type::save_portfolio_request:
        co_return co_await handle_save_portfolio_request(payload, remote_address);
    case comms::messaging::message_type::delete_portfolio_request:
        co_return co_await handle_delete_portfolio_request(payload, remote_address);
    case comms::messaging::message_type::get_portfolio_history_request:
        co_return co_await handle_get_portfolio_history_request(payload, remote_address);
    // Book handlers
    case comms::messaging::message_type::get_books_request:
        co_return co_await handle_get_books_request(payload, remote_address);
    case comms::messaging::message_type::save_book_request:
        co_return co_await handle_save_book_request(payload, remote_address);
    case comms::messaging::message_type::delete_book_request:
        co_return co_await handle_delete_book_request(payload, remote_address);
    case comms::messaging::message_type::get_book_history_request:
        co_return co_await handle_get_book_history_request(payload, remote_address);
    // Book status handlers
    case comms::messaging::message_type::get_book_statuses_request:
        co_return co_await handle_get_book_statuses_request(payload, remote_address);
    case comms::messaging::message_type::save_book_status_request:
        co_return co_await handle_save_book_status_request(payload, remote_address);
    case comms::messaging::message_type::delete_book_status_request:
        co_return co_await handle_delete_book_status_request(payload, remote_address);
    case comms::messaging::message_type::get_book_status_history_request:
        co_return co_await handle_get_book_status_history_request(payload, remote_address);
    // Purpose type handlers
    case comms::messaging::message_type::get_purpose_types_request:
        co_return co_await handle_get_purpose_types_request(payload, remote_address);
    case comms::messaging::message_type::save_purpose_type_request:
        co_return co_await handle_save_purpose_type_request(payload, remote_address);
    case comms::messaging::message_type::delete_purpose_type_request:
        co_return co_await handle_delete_purpose_type_request(payload, remote_address);
    case comms::messaging::message_type::get_purpose_type_history_request:
        co_return co_await handle_get_purpose_type_history_request(payload, remote_address);
    // Rounding type handlers
    case comms::messaging::message_type::get_rounding_types_request:
        co_return co_await handle_get_rounding_types_request(payload, remote_address);
    case comms::messaging::message_type::save_rounding_type_request:
        co_return co_await handle_save_rounding_type_request(payload, remote_address);
    case comms::messaging::message_type::delete_rounding_type_request:
        co_return co_await handle_delete_rounding_type_request(payload, remote_address);
    case comms::messaging::message_type::get_rounding_type_history_request:
        co_return co_await handle_get_rounding_type_history_request(payload, remote_address);
    // Currency asset class handlers
    case comms::messaging::message_type::get_monetary_natures_request:
        co_return co_await handle_get_monetary_natures_request(payload, remote_address);
    case comms::messaging::message_type::save_monetary_nature_request:
        co_return co_await handle_save_monetary_nature_request(payload, remote_address);
    case comms::messaging::message_type::delete_monetary_nature_request:
        co_return co_await handle_delete_monetary_nature_request(payload, remote_address);
    case comms::messaging::message_type::get_monetary_nature_history_request:
        co_return co_await handle_get_monetary_nature_history_request(payload, remote_address);
    // Currency market tier handlers
    case comms::messaging::message_type::get_currency_market_tiers_request:
        co_return co_await handle_get_currency_market_tiers_request(payload, remote_address);
    case comms::messaging::message_type::save_currency_market_tier_request:
        co_return co_await handle_save_currency_market_tier_request(payload, remote_address);
    case comms::messaging::message_type::delete_currency_market_tier_request:
        co_return co_await handle_delete_currency_market_tier_request(payload, remote_address);
    case comms::messaging::message_type::get_currency_market_tier_history_request:
        co_return co_await handle_get_currency_market_tier_history_request(payload, remote_address);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown refdata message type " << std::hex
                                   << static_cast<std::uint16_t>(type);
        co_return std::unexpected(ores::utility::serialization::error_code::invalid_message_type);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_currency_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_currency_request.";

    // Require authentication
    auto auth = require_authentication(remote_address, "Save currency");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth);

    // Create per-request service
    service::currency_service currency_service(ctx);

    // Deserialize request
    auto request_result = save_currency_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_currency_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.currencies.size()
                              << " currency(ies)";

    // Override modified_by with authenticated username
    for (auto& c : request.currencies) {
        c.modified_by = auth->username;
        c.performed_by.clear();
    }

    save_currency_response response;
    try {
        currency_service.save_currencies(request.currencies);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.currencies.size()
                                  << " currency(ies) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save currencies: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving currencies: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_currencies_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_currencies_request.";

    // Require authentication
    auto auth = require_authentication(remote_address, "Get currencies");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth);

    // Create per-request service
    service::currency_service currency_service(ctx);

    // Deserialize request
    auto request_result = get_currencies_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_currencies_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;

    // Validate pagination parameters
    constexpr std::uint32_t max_limit = 1000;
    if (request.limit == 0 || request.limit > max_limit) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid limit: " << request.limit
                                  << ". Must be between 1 and " << max_limit;
        co_return std::unexpected(ores::utility::serialization::error_code::limit_exceeded);
    }

    BOOST_LOG_SEV(lg(), debug) << "Fetching currencies with offset: "
                               << request.offset << ", limit: " << request.limit;

    auto currencies = currency_service.list_currencies(request.offset, request.limit);
    auto total_count = currency_service.count_currencies();

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << currencies.size()
                              << " currencies (total available: " << total_count << ")";

    // TEMP: Log first currency as JSON for debugging
    if (!currencies.empty()) {
        std::ostringstream oss;
        oss << "First currency JSON: " << rfl::json::write(currencies[0]);
        BOOST_LOG_SEV(lg(), debug) << oss.str();
    }

    // Create and serialize response
    get_currencies_response response{
        .currencies = std::move(currencies),
        .total_available_count = total_count
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_currency_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_currency_request.";

    // Require authentication
    auto auth = require_authentication(remote_address, "Delete currency");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth);

    // Create per-request service
    service::currency_service currency_service(ctx);

    // Deserialize request
    auto request_result = delete_currency_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_currency_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.iso_codes.size()
                              << " currency/currencies";

    delete_currency_response response;
    try {
        for (const auto& iso_code : request.iso_codes) {
            currency_service.delete_currency(iso_code);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.iso_codes.size() << " currency(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete currency: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_currency_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_currency_history_request.";

    // Require authentication
    auto auth = require_authentication(remote_address, "Get currency history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth);

    // Create per-request service
    service::currency_service currency_service(ctx);

    // Deserialize request
    auto request_result = get_currency_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_currency_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for currency: " << request.iso_code;

    get_currency_history_response response;
    try {
        auto history_opt = currency_service.get_currency_version_history(request.iso_code);

        if (!history_opt) {
            response.success = false;
            response.message = "Currency not found: " + request.iso_code;
            BOOST_LOG_SEV(lg(), warn) << "No history found for currency: " << request.iso_code;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.history = std::move(*history_opt);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.history.versions.size()
                                  << " versions for currency: " << request.iso_code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for "
                                   << request.iso_code << ": " << e.what();
    }

    co_return response.serialize();
}

// Business centre handlers

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_business_centres_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_business_centres_request.";

    auto auth = require_authentication(remote_address, "Get business centres");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::business_centre_service svc(ctx);

    auto request_result = get_business_centres_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_business_centres_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;

    constexpr std::uint32_t max_limit = 1000;
    if (request.limit == 0 || request.limit > max_limit) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid limit: " << request.limit
                                  << ". Must be between 1 and " << max_limit;
        co_return std::unexpected(ores::utility::serialization::error_code::limit_exceeded);
    }

    BOOST_LOG_SEV(lg(), debug) << "Fetching business centres with offset: "
                               << request.offset << ", limit: " << request.limit;

    auto business_centres = svc.list_business_centres(request.offset, request.limit);
    auto total_count = svc.count_business_centres();

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << business_centres.size()
                              << " business centres (total available: " << total_count << ")";

    get_business_centres_response response{
        .business_centres = std::move(business_centres),
        .total_available_count = total_count
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_business_centre_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_business_centre_request.";

    auto auth = require_authentication(remote_address, "Save business centre");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::business_centre_service svc(ctx);

    auto request_result = save_business_centre_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_business_centre_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.business_centres.size()
                              << " business centre(s)";
    for (auto& e : request.business_centres) {
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_business_centre_response response;
    try {
        svc.save_business_centres(request.business_centres);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.business_centres.size()
                                  << " business centre(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save business centres: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving business centres: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_business_centre_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_business_centre_request.";

    auto auth = require_authentication(remote_address, "Delete business centre");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::business_centre_service svc(ctx);

    auto request_result = delete_business_centre_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_business_centre_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.codes.size()
                              << " business centre(s)";

    delete_business_centre_response response;
    try {
        for (const auto& code : request.codes) {
            svc.delete_business_centre(code);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.codes.size() << " business centre(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete business centre: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_business_centre_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_business_centre_history_request.";

    auto auth = require_authentication(remote_address, "Get business centre history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::business_centre_service svc(ctx);

    auto request_result = get_business_centre_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_business_centre_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for business centre: " << request.code;

    get_business_centre_history_response response;
    try {
        auto history = svc.get_business_centre_history(request.code);

        if (history.empty()) {
            response.success = false;
            response.message = "Business centre not found: " + request.code;
            BOOST_LOG_SEV(lg(), warn) << "No history found for business centre: " << request.code;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.history = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.history.size()
                                  << " versions for business centre: " << request.code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for business centre "
                                   << request.code << ": " << e.what();
    }

    co_return response.serialize();
}

// Country handlers

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_countries_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_countries_request.";

    // Require authentication
    auto auth = require_authentication(remote_address, "Get countries");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth);

    // Create per-request service
    service::country_service country_service(ctx);

    // Deserialize request
    auto request_result = get_countries_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_countries_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;

    // Validate pagination parameters
    constexpr std::uint32_t max_limit = 1000;
    if (request.limit == 0 || request.limit > max_limit) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid limit: " << request.limit
                                  << ". Must be between 1 and " << max_limit;
        co_return std::unexpected(ores::utility::serialization::error_code::limit_exceeded);
    }

    BOOST_LOG_SEV(lg(), debug) << "Fetching countries with offset: "
                               << request.offset << ", limit: " << request.limit;

    auto countries = country_service.list_countries(request.offset, request.limit);
    auto total_count = country_service.count_countries();

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << countries.size()
                              << " countries (total available: " << total_count << ")";

    // Create and serialize response
    get_countries_response response{
        .countries = std::move(countries),
        .total_available_count = total_count
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_country_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_country_request.";

    // Require authentication
    auto auth = require_authentication(remote_address, "Save country");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth);

    // Create per-request service
    service::country_service country_service(ctx);

    // Deserialize request
    auto request_result = save_country_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_country_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.countries.size() << " country(ies)";

    // Override modified_by with authenticated username
    for (auto& e : request.countries) {
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_country_response response;
    try {
        country_service.save_countries(request.countries);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.countries.size()
                                  << " country(ies) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save countries: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving countries: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_country_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_country_request.";

    // Require authentication
    auto auth = require_authentication(remote_address, "Delete country");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth);

    // Create per-request service
    service::country_service country_service(ctx);

    // Deserialize request
    auto request_result = delete_country_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_country_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.alpha2_codes.size()
                              << " country/countries";

    delete_country_response response;
    try {
        for (const auto& alpha2_code : request.alpha2_codes) {
            country_service.delete_country(alpha2_code);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.alpha2_codes.size() << " country(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete country: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_country_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_country_history_request.";

    // Require authentication
    auto auth = require_authentication(remote_address, "Get country history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth);

    // Create per-request service
    service::country_service country_service(ctx);

    // Deserialize request
    auto request_result = get_country_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_country_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for country: " << request.alpha2_code;

    get_country_history_response response;
    try {
        auto history = country_service.get_country_history(request.alpha2_code);

        if (history.empty()) {
            response.success = false;
            response.message = "Country not found: " + request.alpha2_code;
            BOOST_LOG_SEV(lg(), warn) << "No history found for country: " << request.alpha2_code;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.history = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.history.size()
                                  << " versions for country: " << request.alpha2_code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for "
                                   << request.alpha2_code << ": " << e.what();
    }

    co_return response.serialize();
}

// Party type handlers

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_party_types_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_party_types_request.";

    auto auth = require_authentication(remote_address, "Get party types");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_type_service svc(ctx);

    auto request_result = get_party_types_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_party_types_request";
        co_return std::unexpected(request_result.error());
    }

    auto types = svc.list_types();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << types.size() << " party types";

    get_party_types_response response{
        .types = std::move(types)
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_party_type_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_party_type_request.";

    auto auth = require_authentication(remote_address, "Save party type");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_type_service svc(ctx);

    auto request_result = save_party_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_party_type_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.types.size() << " party type(s)";
    for (auto& e : request.types) {
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_party_type_response response;
    try {
        svc.save_types(request.types);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.types.size()
                                  << " party type(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save party types: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving party types: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_party_type_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_party_type_request.";

    auto auth = require_authentication(remote_address, "Delete party type");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_type_service svc(ctx);

    auto request_result = delete_party_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_party_type_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.codes.size() << " party type(s)";

    delete_party_type_response response;
    try {
        for (const auto& code : request.codes) {
            svc.remove_type(code);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.codes.size() << " party type(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete party type: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_party_type_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_party_type_history_request.";

    auto auth = require_authentication(remote_address, "Get party type history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_type_service svc(ctx);

    auto request_result = get_party_type_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_party_type_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for party type: " << request.code;

    get_party_type_history_response response;
    try {
        auto history = svc.get_type_history(request.code);

        if (history.empty()) {
            response.success = false;
            response.message = "Party type not found: " + request.code;
            BOOST_LOG_SEV(lg(), warn) << "No history found for party type: " << request.code;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for party type: " << request.code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for party type "
                                   << request.code << ": " << e.what();
    }

    co_return response.serialize();
}

// Party status handlers

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_party_statuses_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_party_statuses_request.";

    auto auth = require_authentication(remote_address, "Get party statuses");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_status_service svc(ctx);

    auto request_result = get_party_statuses_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_party_statuses_request";
        co_return std::unexpected(request_result.error());
    }

    auto statuses = svc.list_statuses();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << statuses.size() << " party statuses";

    get_party_statuses_response response{
        .statuses = std::move(statuses)
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_party_status_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_party_status_request.";

    auto auth = require_authentication(remote_address, "Save party status");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_status_service svc(ctx);

    auto request_result = save_party_status_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_party_status_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.statuses.size() << " party status(es)";
    for (auto& e : request.statuses) {
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_party_status_response response;
    try {
        svc.save_statuses(request.statuses);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.statuses.size()
                                  << " party status(es) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save party statuses: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving party statuses: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_party_status_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_party_status_request.";

    auto auth = require_authentication(remote_address, "Delete party status");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_status_service svc(ctx);

    auto request_result = delete_party_status_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_party_status_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.codes.size() << " party status(es)";

    delete_party_status_response response;
    try {
        for (const auto& code : request.codes) {
            svc.remove_status(code);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.codes.size() << " party status(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete party status: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_party_status_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_party_status_history_request.";

    auto auth = require_authentication(remote_address, "Get party status history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_status_service svc(ctx);

    auto request_result = get_party_status_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_party_status_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for party status: " << request.code;

    get_party_status_history_response response;
    try {
        auto history = svc.get_status_history(request.code);

        if (history.empty()) {
            response.success = false;
            response.message = "Party status not found: " + request.code;
            BOOST_LOG_SEV(lg(), warn) << "No history found for party status: " << request.code;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for party status: " << request.code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for party status "
                                   << request.code << ": " << e.what();
    }

    co_return response.serialize();
}

// Party ID scheme handlers

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_party_id_schemes_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_party_id_schemes_request.";

    auto auth = require_authentication(remote_address, "Get party ID schemes");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_id_scheme_service svc(ctx);

    auto request_result = get_party_id_schemes_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_party_id_schemes_request";
        co_return std::unexpected(request_result.error());
    }

    auto schemes = svc.list_schemes();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << schemes.size() << " party ID schemes";

    get_party_id_schemes_response response{
        .schemes = std::move(schemes)
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_party_id_scheme_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_party_id_scheme_request.";

    auto auth = require_authentication(remote_address, "Save party ID scheme");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_id_scheme_service svc(ctx);

    auto request_result = save_party_id_scheme_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_party_id_scheme_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.schemes.size() << " party ID scheme(s)";
    for (auto& e : request.schemes) {
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_party_id_scheme_response response;
    try {
        svc.save_schemes(request.schemes);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.schemes.size()
                                  << " party ID scheme(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save party ID schemes: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving party ID schemes: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_party_id_scheme_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_party_id_scheme_request.";

    auto auth = require_authentication(remote_address, "Delete party ID scheme");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_id_scheme_service svc(ctx);

    auto request_result = delete_party_id_scheme_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_party_id_scheme_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.codes.size() << " party ID scheme(s)";

    delete_party_id_scheme_response response;
    try {
        for (const auto& code : request.codes) {
            svc.remove_scheme(code);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.codes.size() << " party id scheme(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete party id scheme: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_party_id_scheme_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_party_id_scheme_history_request.";

    auto auth = require_authentication(remote_address, "Get party ID scheme history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_id_scheme_service svc(ctx);

    auto request_result = get_party_id_scheme_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_party_id_scheme_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for party ID scheme: " << request.code;

    get_party_id_scheme_history_response response;
    try {
        auto history = svc.get_scheme_history(request.code);

        if (history.empty()) {
            response.success = false;
            response.message = "Party ID scheme not found: " + request.code;
            BOOST_LOG_SEV(lg(), warn) << "No history found for party ID scheme: " << request.code;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for party ID scheme: " << request.code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for party ID scheme "
                                   << request.code << ": " << e.what();
    }

    co_return response.serialize();
}

// Contact type handlers

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_contact_types_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_contact_types_request.";

    auto auth = require_authentication(remote_address, "Get contact types");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::contact_type_service svc(ctx);

    auto request_result = get_contact_types_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_contact_types_request";
        co_return std::unexpected(request_result.error());
    }

    auto types = svc.list_types();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << types.size() << " contact types";

    get_contact_types_response response{
        .types = std::move(types)
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_contact_type_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_contact_type_request.";

    auto auth = require_authentication(remote_address, "Save contact type");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::contact_type_service svc(ctx);

    auto request_result = save_contact_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_contact_type_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.types.size() << " contact type(s)";
    for (auto& e : request.types) {
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_contact_type_response response;
    try {
        svc.save_types(request.types);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.types.size()
                                  << " contact type(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save contact types: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving contact types: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_contact_type_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_contact_type_request.";

    auto auth = require_authentication(remote_address, "Delete contact type");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::contact_type_service svc(ctx);

    auto request_result = delete_contact_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_contact_type_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.codes.size() << " contact type(s)";

    delete_contact_type_response response;
    try {
        for (const auto& code : request.codes) {
            svc.remove_type(code);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.codes.size() << " contact type(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete contact type: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_contact_type_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_contact_type_history_request.";

    auto auth = require_authentication(remote_address, "Get contact type history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::contact_type_service svc(ctx);

    auto request_result = get_contact_type_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_contact_type_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for contact type: " << request.code;

    get_contact_type_history_response response;
    try {
        auto history = svc.get_type_history(request.code);

        if (history.empty()) {
            response.success = false;
            response.message = "Contact type not found: " + request.code;
            BOOST_LOG_SEV(lg(), warn) << "No history found for contact type: " << request.code;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for contact type: " << request.code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for contact type "
                                   << request.code << ": " << e.what();
    }

    co_return response.serialize();
}

// Party handlers

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_parties_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_parties_request.";

    auto auth = require_authentication(remote_address, "Get parties");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_service svc(ctx);

    auto request_result = get_parties_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_parties_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;

    // Validate pagination parameters
    constexpr std::uint32_t max_limit = 1000;
    if (request.limit == 0 || request.limit > max_limit) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid limit: " << request.limit
                                  << ". Must be between 1 and " << max_limit;
        co_return std::unexpected(ores::utility::serialization::error_code::limit_exceeded);
    }

    BOOST_LOG_SEV(lg(), debug) << "Fetching parties with offset: "
                               << request.offset << ", limit: " << request.limit;

    auto parties = svc.list_parties(request.offset, request.limit);
    auto total_count = svc.count_parties();

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << parties.size()
                              << " parties (total available: " << total_count << ")";

    get_parties_response response{
        .parties = std::move(parties),
        .total_available_count = total_count
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_party_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_party_request.";

    auto auth = require_authentication(remote_address, "Save party");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_service svc(ctx);

    auto request_result = save_party_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_party_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.parties.size() << " party(ies)";
    for (auto& e : request.parties) {
        e.tenant_id = auth->tenant_id.to_string();
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_party_response response;
    try {
        svc.save_parties(request.parties);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.parties.size()
                                  << " party(ies) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save parties: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving parties: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_party_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_party_request.";

    auto auth = require_authentication(remote_address, "Delete party");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_service svc(ctx);

    auto request_result = delete_party_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_party_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.ids.size() << " party/parties";

    delete_party_response response;
    try {
        for (const auto& id : request.ids) {
            svc.remove_party(id);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.ids.size() << " party(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete party: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_party_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_party_history_request.";

    auto auth = require_authentication(remote_address, "Get party history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_service svc(ctx);

    auto request_result = get_party_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_party_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for party: " << request.id;

    get_party_history_response response;
    try {
        auto history = svc.get_party_history(request.id);

        if (history.empty()) {
            response.success = false;
            response.message = "Party not found: " + boost::uuids::to_string(request.id);
            BOOST_LOG_SEV(lg(), warn) << "No history found for party: " << request.id;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for party: " << request.id;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for party "
                                   << request.id << ": " << e.what();
    }

    co_return response.serialize();
}

// Counterparty handlers

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_counterparties_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_counterparties_request.";

    auto auth = require_authentication(remote_address, "Get counterparties");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::counterparty_service svc(ctx);

    auto request_result = get_counterparties_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_counterparties_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;

    // Validate pagination parameters
    constexpr std::uint32_t max_limit = 1000;
    if (request.limit == 0 || request.limit > max_limit) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid limit: " << request.limit
                                  << ". Must be between 1 and " << max_limit;
        co_return std::unexpected(ores::utility::serialization::error_code::limit_exceeded);
    }

    BOOST_LOG_SEV(lg(), debug) << "Fetching counterparties with offset: "
                               << request.offset << ", limit: " << request.limit;

    auto counterparties = svc.list_counterparties(request.offset, request.limit);
    auto total_count = svc.count_counterparties();

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << counterparties.size()
                              << " counterparties (total available: " << total_count << ")";

    get_counterparties_response response{
        .counterparties = std::move(counterparties),
        .total_available_count = total_count
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_counterparty_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_counterparty_request.";

    auto auth = require_authentication(remote_address, "Save counterparty");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::counterparty_service svc(ctx);

    auto request_result = save_counterparty_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_counterparty_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.counterparties.size() << " counterparty(ies)";
    for (auto& e : request.counterparties) {
        e.tenant_id = auth->tenant_id.to_string();
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_counterparty_response response;
    try {
        svc.save_counterparties(request.counterparties);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.counterparties.size()
                                  << " counterparty(ies) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save counterparties: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving counterparties: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_counterparty_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_counterparty_request.";

    auto auth = require_authentication(remote_address, "Delete counterparty");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::counterparty_service svc(ctx);

    auto request_result = delete_counterparty_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_counterparty_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.ids.size()
                              << " counterparty/counterparties";

    delete_counterparty_response response;
    try {
        for (const auto& id : request.ids) {
            svc.remove_counterparty(id);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.ids.size() << " counterparty(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete counterparty: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_counterparty_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_counterparty_history_request.";

    auto auth = require_authentication(remote_address, "Get counterparty history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::counterparty_service svc(ctx);

    auto request_result = get_counterparty_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_counterparty_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for counterparty: " << request.id;

    get_counterparty_history_response response;
    try {
        auto history = svc.get_counterparty_history(request.id);

        if (history.empty()) {
            response.success = false;
            response.message = "Counterparty not found: " +
                boost::uuids::to_string(request.id);
            BOOST_LOG_SEV(lg(), warn) << "No history found for counterparty: " << request.id;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for counterparty: " << request.id;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for counterparty "
                                   << request.id << ": " << e.what();
    }

    co_return response.serialize();
}

// Business unit handlers

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_business_units_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_business_units_request.";

    auto auth = require_authentication(remote_address, "Get business units");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::business_unit_service svc(ctx);

    auto request_result = get_business_units_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_business_units_request";
        co_return std::unexpected(request_result.error());
    }

    auto business_units = svc.list_business_units();

    const auto count = static_cast<std::uint32_t>(business_units.size());
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << count << " business units";

    get_business_units_response response{
        .business_units = std::move(business_units),
        .total_available_count = count
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_business_unit_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_business_unit_request.";

    auto auth = require_authentication(remote_address, "Save business unit");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::business_unit_service svc(ctx);

    auto request_result = save_business_unit_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_business_unit_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.business_units.size() << " business unit(s)";
    for (auto& e : request.business_units) {
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_business_unit_response response;
    try {
        svc.save_business_units(request.business_units);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.business_units.size()
                                  << " business unit(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save business units: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving business units: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_business_unit_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_business_unit_request.";

    auto auth = require_authentication(remote_address, "Delete business unit");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::business_unit_service svc(ctx);

    auto request_result = delete_business_unit_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_business_unit_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.ids.size()
                              << " business unit(s)";

    delete_business_unit_response response;
    try {
        for (const auto& id : request.ids) {
            svc.remove_business_unit(id);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.ids.size() << " business unit(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete business unit: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_business_unit_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_business_unit_history_request.";

    auto auth = require_authentication(remote_address, "Get business unit history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::business_unit_service svc(ctx);

    auto request_result = get_business_unit_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_business_unit_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for business unit: " << request.id;

    get_business_unit_history_response response;
    try {
        auto history = svc.get_business_unit_history(request.id);

        if (history.empty()) {
            response.success = false;
            response.message = "Business unit not found: " +
                boost::uuids::to_string(request.id);
            BOOST_LOG_SEV(lg(), warn) << "No history found for business unit: " << request.id;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for business unit: " << request.id;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for business unit "
                                   << request.id << ": " << e.what();
    }

    co_return response.serialize();
}

// Business unit type handlers

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_business_unit_types_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_business_unit_types_request.";

    auto auth = require_authentication(remote_address, "Get business unit types");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    repository::business_unit_type_repository repo(ctx);

    auto request_result = get_business_unit_types_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_business_unit_types_request";
        co_return std::unexpected(request_result.error());
    }

    auto types = repo.read_latest();

    const auto count = static_cast<std::uint32_t>(types.size());
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << count << " business unit types";

    get_business_unit_types_response response{
        .types = std::move(types)
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_business_unit_type_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_business_unit_type_request.";

    auto auth = require_authentication(remote_address, "Save business unit type");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    repository::business_unit_type_repository repo(ctx);

    auto request_result = save_business_unit_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_business_unit_type_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.types.size() << " business unit type(s)";
    for (auto& e : request.types) {
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_business_unit_type_response response;
    try {
        for (const auto& e : request.types) {
            repo.write(e);
        }
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.types.size()
                                  << " business unit type(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save business unit types: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving business unit types: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_business_unit_type_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_business_unit_type_request.";

    auto auth = require_authentication(remote_address, "Delete business unit type");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    repository::business_unit_type_repository repo(ctx);

    auto request_result = delete_business_unit_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_business_unit_type_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.ids.size()
                              << " business unit type(s)";

    delete_business_unit_type_response response;
    try {
        for (const auto& id : request.ids) {
            repo.remove(id);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.ids.size() << " business unit type(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete business unit type: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_business_unit_type_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_business_unit_type_history_request.";

    auto auth = require_authentication(remote_address, "Get business unit type history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    repository::business_unit_type_repository repo(ctx);

    auto request_result = get_business_unit_type_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_business_unit_type_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for business unit type: " << request.id;

    get_business_unit_type_history_response response;
    try {
        auto history = repo.read_all(request.id);

        if (history.empty()) {
            response.success = false;
            response.message = "Business unit type not found: " +
                boost::uuids::to_string(request.id);
            BOOST_LOG_SEV(lg(), warn) << "No history found for business unit type: "
                                      << request.id;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for business unit type: " << request.id;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for business unit type "
                                   << request.id << ": " << e.what();
    }

    co_return response.serialize();
}

// Portfolio handlers

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_portfolios_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_portfolios_request.";

    auto auth = require_authentication(remote_address, "Get portfolios");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::portfolio_service svc(ctx);

    auto request_result = get_portfolios_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_portfolios_request";
        co_return std::unexpected(request_result.error());
    }

    auto portfolios = svc.list_portfolios();

    const auto count = static_cast<std::uint32_t>(portfolios.size());
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << count << " portfolios";

    get_portfolios_response response{
        .portfolios = std::move(portfolios),
        .total_available_count = count
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_portfolio_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_portfolio_request.";

    auto auth = require_authentication(remote_address, "Save portfolio");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::portfolio_service svc(ctx);

    auto request_result = save_portfolio_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_portfolio_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.portfolios.size() << " portfolio(s)";
    for (auto& e : request.portfolios) {
        e.tenant_id = auth->tenant_id.to_string();
        e.modified_by = auth->username;
        e.performed_by.clear();
        e.party_id = auth->party_id;
    }

    save_portfolio_response response;
    try {
        svc.save_portfolios(request.portfolios);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.portfolios.size()
                                  << " portfolio(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save portfolios: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving portfolios: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_portfolio_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_portfolio_request.";

    auto auth = require_authentication(remote_address, "Delete portfolio");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::portfolio_service svc(ctx);

    auto request_result = delete_portfolio_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_portfolio_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.ids.size()
                              << " portfolio(s)";

    delete_portfolio_response response;
    try {
        for (const auto& id : request.ids) {
            svc.remove_portfolio(id);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.ids.size() << " portfolio(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete portfolio: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_portfolio_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_portfolio_history_request.";

    auto auth = require_authentication(remote_address, "Get portfolio history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::portfolio_service svc(ctx);

    auto request_result = get_portfolio_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_portfolio_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for portfolio: " << request.id;

    get_portfolio_history_response response;
    try {
        auto history = svc.get_portfolio_history(request.id);

        if (history.empty()) {
            response.success = false;
            response.message = "Portfolio not found: " +
                boost::uuids::to_string(request.id);
            BOOST_LOG_SEV(lg(), warn) << "No history found for portfolio: " << request.id;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for portfolio: " << request.id;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for portfolio "
                                   << request.id << ": " << e.what();
    }

    co_return response.serialize();
}

// Book handlers

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_books_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_books_request.";

    auto auth = require_authentication(remote_address, "Get books");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::book_service svc(ctx);

    auto request_result = get_books_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_books_request";
        co_return std::unexpected(request_result.error());
    }

    auto books = svc.list_books();

    const auto count = static_cast<std::uint32_t>(books.size());
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << count << " books";

    get_books_response response{
        .books = std::move(books),
        .total_available_count = count
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_book_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_book_request.";

    auto auth = require_authentication(remote_address, "Save book");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::book_service svc(ctx);

    auto request_result = save_book_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_book_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.books.size() << " book(s)";
    for (auto& e : request.books) {
        e.tenant_id = auth->tenant_id.to_string();
        e.party_id = auth->party_id;
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_book_response response;
    try {
        svc.save_books(request.books);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.books.size()
                                  << " book(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save books: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving books: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_book_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_book_request.";

    auto auth = require_authentication(remote_address, "Delete book");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::book_service svc(ctx);

    auto request_result = delete_book_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_book_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.ids.size() << " book(s)";

    delete_book_response response;
    try {
        for (const auto& id : request.ids) {
            svc.remove_book(id);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.ids.size() << " book(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete book: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_book_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_book_history_request.";

    auto auth = require_authentication(remote_address, "Get book history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::book_service svc(ctx);

    auto request_result = get_book_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_book_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for book: " << request.id;

    get_book_history_response response;
    try {
        auto history = svc.get_book_history(request.id);

        if (history.empty()) {
            response.success = false;
            response.message = "Book not found: " +
                boost::uuids::to_string(request.id);
            BOOST_LOG_SEV(lg(), warn) << "No history found for book: " << request.id;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for book: " << request.id;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for book "
                                   << request.id << ": " << e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Counterparty Identifier Handlers
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_counterparty_identifiers_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_counterparty_identifiers_request.";

    auto auth = require_authentication(remote_address, "Get counterparty identifiers");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::counterparty_identifier_service svc(ctx);

    auto request_result = get_counterparty_identifiers_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_counterparty_identifiers_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;

    std::vector<domain::counterparty_identifier> identifiers;
    if (request.counterparty_id.is_nil()) {
        identifiers = svc.list_counterparty_identifiers();
    } else {
        identifiers = svc.list_counterparty_identifiers_by_counterparty(request.counterparty_id);
    }

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << identifiers.size()
                              << " counterparty identifiers";

    get_counterparty_identifiers_response response{
        .counterparty_identifiers = std::move(identifiers)
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_counterparty_identifier_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_counterparty_identifier_request.";

    auto auth = require_authentication(remote_address, "Save counterparty identifier");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::counterparty_identifier_service svc(ctx);

    auto request_result = save_counterparty_identifier_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_counterparty_identifier_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.counterparty_identifiers.size()
                              << " counterparty identifier(s)";
    for (auto& e : request.counterparty_identifiers) {
        e.tenant_id = auth->tenant_id.to_string();
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_counterparty_identifier_response response;
    try {
        svc.save_counterparty_identifiers(request.counterparty_identifiers);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.counterparty_identifiers.size()
                                  << " counterparty identifier(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save counterparty identifiers: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving counterparty identifiers: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_counterparty_identifier_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_counterparty_identifier_request.";

    auto auth = require_authentication(remote_address, "Delete counterparty identifier");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::counterparty_identifier_service svc(ctx);

    auto request_result = delete_counterparty_identifier_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_counterparty_identifier_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.ids.size()
                              << " counterparty identifier(s)";

    delete_counterparty_identifier_response response;
    try {
        for (const auto& id : request.ids) {
            svc.remove_counterparty_identifier(id);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.ids.size() << " counterparty identifier(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete counterparty identifier: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_counterparty_identifier_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_counterparty_identifier_history_request.";

    auto auth = require_authentication(remote_address, "Get counterparty identifier history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::counterparty_identifier_service svc(ctx);

    auto request_result = get_counterparty_identifier_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_counterparty_identifier_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for counterparty identifier: " << request.id;

    get_counterparty_identifier_history_response response;
    try {
        auto history = svc.get_counterparty_identifier_history(request.id);

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for counterparty identifier: " << request.id;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for counterparty identifier "
                                   << request.id << ": " << e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Counterparty Contact Information Handlers
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_counterparty_contact_informations_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_counterparty_contact_informations_request.";

    auto auth = require_authentication(remote_address, "Get counterparty contact informations");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::counterparty_contact_information_service svc(ctx);

    auto request_result = get_counterparty_contact_informations_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_counterparty_contact_informations_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;

    std::vector<domain::counterparty_contact_information> contacts;
    if (request.counterparty_id.is_nil()) {
        contacts = svc.list_counterparty_contact_informations();
    } else {
        contacts = svc.list_counterparty_contact_informations_by_counterparty(request.counterparty_id);
    }

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << contacts.size()
                              << " counterparty contact informations";

    get_counterparty_contact_informations_response response{
        .counterparty_contact_informations = std::move(contacts)
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_counterparty_contact_information_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_counterparty_contact_information_request.";

    auto auth = require_authentication(remote_address, "Save counterparty contact information");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::counterparty_contact_information_service svc(ctx);

    auto request_result = save_counterparty_contact_information_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_counterparty_contact_information_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.counterparty_contact_informations.size()
                              << " counterparty contact information(s)";
    for (auto& e : request.counterparty_contact_informations) {
        e.tenant_id = auth->tenant_id.to_string();
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_counterparty_contact_information_response response;
    try {
        svc.save_counterparty_contact_informations(request.counterparty_contact_informations);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.counterparty_contact_informations.size()
                                  << " counterparty contact information(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save counterparty contact informations: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving counterparty contact informations: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_counterparty_contact_information_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_counterparty_contact_information_request.";

    auto auth = require_authentication(remote_address, "Delete counterparty contact information");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::counterparty_contact_information_service svc(ctx);

    auto request_result = delete_counterparty_contact_information_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_counterparty_contact_information_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.ids.size()
                              << " counterparty contact information(s)";

    delete_counterparty_contact_information_response response;
    try {
        for (const auto& id : request.ids) {
            svc.remove_counterparty_contact_information(id);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.ids.size() << " counterparty contact information(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete counterparty contact information: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_counterparty_contact_information_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_counterparty_contact_information_history_request.";

    auto auth = require_authentication(remote_address, "Get counterparty contact information history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::counterparty_contact_information_service svc(ctx);

    auto request_result = get_counterparty_contact_information_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_counterparty_contact_information_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for counterparty contact information: " << request.id;

    get_counterparty_contact_information_history_response response;
    try {
        auto history = svc.get_counterparty_contact_information_history(request.id);

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for counterparty contact information: " << request.id;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for counterparty contact information "
                                   << request.id << ": " << e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Party Identifier Handlers
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_party_identifiers_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_party_identifiers_request.";

    auto auth = require_authentication(remote_address, "Get party identifiers");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_identifier_service svc(ctx);

    auto request_result = get_party_identifiers_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_party_identifiers_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;

    std::vector<domain::party_identifier> identifiers;
    if (request.party_id.is_nil()) {
        identifiers = svc.list_party_identifiers();
    } else {
        identifiers = svc.list_party_identifiers_by_party(request.party_id);
    }

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << identifiers.size()
                              << " party identifiers";

    get_party_identifiers_response response{
        .party_identifiers = std::move(identifiers)
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_party_identifier_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_party_identifier_request.";

    auto auth = require_authentication(remote_address, "Save party identifier");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_identifier_service svc(ctx);

    auto request_result = save_party_identifier_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_party_identifier_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.party_identifiers.size()
                              << " party identifier(s)";
    for (auto& e : request.party_identifiers) {
        e.tenant_id = auth->tenant_id.to_string();
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_party_identifier_response response;
    try {
        svc.save_party_identifiers(request.party_identifiers);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.party_identifiers.size()
                                  << " party identifier(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save party identifiers: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving party identifiers: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_party_identifier_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_party_identifier_request.";

    auto auth = require_authentication(remote_address, "Delete party identifier");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_identifier_service svc(ctx);

    auto request_result = delete_party_identifier_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_party_identifier_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.ids.size()
                              << " party identifier(s)";

    delete_party_identifier_response response;
    try {
        for (const auto& id : request.ids) {
            svc.remove_party_identifier(id);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.ids.size() << " party identifier(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete party identifier: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_party_identifier_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_party_identifier_history_request.";

    auto auth = require_authentication(remote_address, "Get party identifier history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_identifier_service svc(ctx);

    auto request_result = get_party_identifier_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_party_identifier_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for party identifier: " << request.id;

    get_party_identifier_history_response response;
    try {
        auto history = svc.get_party_identifier_history(request.id);

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for party identifier: " << request.id;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for party identifier "
                                   << request.id << ": " << e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Party Contact Information Handlers
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_party_contact_informations_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_party_contact_informations_request.";

    auto auth = require_authentication(remote_address, "Get party contact informations");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_contact_information_service svc(ctx);

    auto request_result = get_party_contact_informations_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_party_contact_informations_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;

    std::vector<domain::party_contact_information> contacts;
    if (request.party_id.is_nil()) {
        contacts = svc.list_party_contact_informations();
    } else {
        contacts = svc.list_party_contact_informations_by_party(request.party_id);
    }

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << contacts.size()
                              << " party contact informations";

    get_party_contact_informations_response response{
        .party_contact_informations = std::move(contacts)
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_party_contact_information_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_party_contact_information_request.";

    auto auth = require_authentication(remote_address, "Save party contact information");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_contact_information_service svc(ctx);

    auto request_result = save_party_contact_information_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_party_contact_information_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.party_contact_informations.size()
                              << " party contact information(s)";
    for (auto& e : request.party_contact_informations) {
        e.tenant_id = auth->tenant_id.to_string();
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_party_contact_information_response response;
    try {
        svc.save_party_contact_informations(request.party_contact_informations);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.party_contact_informations.size()
                                  << " party contact information(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save party contact informations: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving party contact informations: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_party_contact_information_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_party_contact_information_request.";

    auto auth = require_authentication(remote_address, "Delete party contact information");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_contact_information_service svc(ctx);

    auto request_result = delete_party_contact_information_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_party_contact_information_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.ids.size()
                              << " party contact information(s)";

    delete_party_contact_information_response response;
    try {
        for (const auto& id : request.ids) {
            svc.remove_party_contact_information(id);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.ids.size() << " party contact information(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete party contact information: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_party_contact_information_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_party_contact_information_history_request.";

    auto auth = require_authentication(remote_address, "Get party contact information history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::party_contact_information_service svc(ctx);

    auto request_result = get_party_contact_information_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_party_contact_information_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for party contact information: " << request.id;

    get_party_contact_information_history_response response;
    try {
        auto history = svc.get_party_contact_information_history(request.id);

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for party contact information: " << request.id;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for party contact information "
                                   << request.id << ": " << e.what();
    }

    co_return response.serialize();
}

// Book status handlers

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_book_statuses_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_book_statuses_request.";

    auto auth = require_authentication(remote_address, "Get book statuses");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::book_status_service svc(ctx);

    auto request_result = get_book_statuses_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_book_statuses_request";
        co_return std::unexpected(request_result.error());
    }

    auto statuses = svc.list_statuses();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << statuses.size() << " book statuses";

    get_book_statuses_response response{
        .statuses = std::move(statuses)
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_book_status_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_book_status_request.";

    auto auth = require_authentication(remote_address, "Save book status");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::book_status_service svc(ctx);

    auto request_result = save_book_status_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_book_status_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.statuses.size() << " book status(es)";

    save_book_status_response response;
    try {
        svc.save_statuses(request.statuses);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.statuses.size()
                                  << " book status(es) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save book statuses: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving book statuses: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_book_status_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_book_status_request.";

    auto auth = require_authentication(remote_address, "Delete book status");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::book_status_service svc(ctx);

    auto request_result = delete_book_status_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_book_status_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.codes.size() << " book status(es)";

    delete_book_status_response response;
    try {
        for (const auto& code : request.codes) {
            svc.remove_status(code);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.codes.size() << " book status(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete book status: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_book_status_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_book_status_history_request.";

    auto auth = require_authentication(remote_address, "Get book status history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::book_status_service svc(ctx);

    auto request_result = get_book_status_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_book_status_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for book status: " << request.code;

    get_book_status_history_response response;
    try {
        auto history = svc.get_status_history(request.code);

        if (history.empty()) {
            response.success = false;
            response.message = "Book status not found: " + request.code;
            BOOST_LOG_SEV(lg(), warn) << "No history found for book status: " << request.code;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for book status: " << request.code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for book status "
                                   << request.code << ": " << e.what();
    }

    co_return response.serialize();
}

// Purpose type handlers

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_purpose_types_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_purpose_types_request.";

    auto auth = require_authentication(remote_address, "Get purpose types");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::purpose_type_service svc(ctx);

    auto request_result = get_purpose_types_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_purpose_types_request";
        co_return std::unexpected(request_result.error());
    }

    auto types = svc.list_types();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << types.size() << " purpose types";

    get_purpose_types_response response{
        .types = std::move(types)
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_purpose_type_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_purpose_type_request.";

    auto auth = require_authentication(remote_address, "Save purpose type");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::purpose_type_service svc(ctx);

    auto request_result = save_purpose_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_purpose_type_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.types.size() << " purpose type(s)";
    for (auto& e : request.types) {
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_purpose_type_response response;
    try {
        svc.save_types(request.types);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.types.size()
                                  << " purpose type(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save purpose types: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving purpose types: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_purpose_type_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_purpose_type_request.";

    auto auth = require_authentication(remote_address, "Delete purpose type");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::purpose_type_service svc(ctx);

    auto request_result = delete_purpose_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_purpose_type_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.codes.size() << " purpose type(s)";

    delete_purpose_type_response response;
    try {
        for (const auto& code : request.codes) {
            svc.remove_type(code);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.codes.size() << " purpose type(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete purpose type: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_purpose_type_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_purpose_type_history_request.";

    auto auth = require_authentication(remote_address, "Get purpose type history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::purpose_type_service svc(ctx);

    auto request_result = get_purpose_type_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_purpose_type_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for purpose type: " << request.code;

    get_purpose_type_history_response response;
    try {
        auto history = svc.get_type_history(request.code);

        if (history.empty()) {
            response.success = false;
            response.message = "Purpose type not found: " + request.code;
            BOOST_LOG_SEV(lg(), warn) << "No history found for purpose type: " << request.code;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for purpose type: " << request.code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for purpose type "
                                   << request.code << ": " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_rounding_types_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_rounding_types_request.";

    auto auth = require_authentication(remote_address, "Get rounding types");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::rounding_type_service svc(ctx);

    auto request_result = get_rounding_types_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_rounding_types_request";
        co_return std::unexpected(request_result.error());
    }

    auto types = svc.list_types();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << types.size() << " rounding types";

    get_rounding_types_response response{
        .types = std::move(types)
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_rounding_type_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_rounding_type_request.";

    auto auth = require_authentication(remote_address, "Save rounding type");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::rounding_type_service svc(ctx);

    auto request_result = save_rounding_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_rounding_type_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.types.size() << " rounding type(s)";
    for (auto& e : request.types) {
        e.modified_by = auth->username;
        e.performed_by.clear();
    }

    save_rounding_type_response response;
    try {
        svc.save_types(request.types);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.types.size()
                                  << " rounding type(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save rounding types: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving rounding types: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_rounding_type_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_rounding_type_request.";

    auto auth = require_authentication(remote_address, "Delete rounding type");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::rounding_type_service svc(ctx);

    auto request_result = delete_rounding_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_rounding_type_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.codes.size() << " rounding type(s)";

    delete_rounding_type_response response;
    try {
        for (const auto& code : request.codes) {
            svc.remove_type(code);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.codes.size() << " rounding type(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete rounding type: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_rounding_type_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_rounding_type_history_request.";

    auto auth = require_authentication(remote_address, "Get rounding type history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::rounding_type_service svc(ctx);

    auto request_result = get_rounding_type_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_rounding_type_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for rounding type: " << request.code;

    get_rounding_type_history_response response;
    try {
        auto history = svc.get_type_history(request.code);

        if (history.empty()) {
            response.success = false;
            response.message = "Rounding type not found: " + request.code;
            BOOST_LOG_SEV(lg(), warn) << "No history found for rounding type: " << request.code;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for rounding type: " << request.code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for rounding type "
                                   << request.code << ": " << e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Currency Asset Class handlers
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_monetary_natures_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_monetary_natures_request.";

    auto auth = require_authentication(remote_address, "Get monetary natures");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::monetary_nature_service svc(ctx);

    auto request_result = get_monetary_natures_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_monetary_natures_request";
        co_return std::unexpected(request_result.error());
    }

    auto types = svc.list_types();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << types.size() << " monetary natures";

    get_monetary_natures_response response{
        .types = std::move(types)
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_monetary_nature_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_monetary_nature_request.";

    auto auth = require_authentication(remote_address, "Save monetary nature");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::monetary_nature_service svc(ctx);

    auto request_result = save_monetary_nature_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_monetary_nature_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.types.size() << " monetary nature(s)";

    save_monetary_nature_response response;
    try {
        svc.save_types(request.types);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.types.size()
                                  << " monetary nature(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save monetary natures: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving monetary natures: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_monetary_nature_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_monetary_nature_request.";

    auto auth = require_authentication(remote_address, "Delete monetary nature");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::monetary_nature_service svc(ctx);

    auto request_result = delete_monetary_nature_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_monetary_nature_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.codes.size() << " monetary nature(s)";

    delete_monetary_nature_response response;
    try {
        for (const auto& code : request.codes) {
            svc.remove_type(code);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.codes.size() << " monetary nature(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete monetary nature: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_monetary_nature_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_monetary_nature_history_request.";

    auto auth = require_authentication(remote_address, "Get monetary nature history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::monetary_nature_service svc(ctx);

    auto request_result = get_monetary_nature_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_monetary_nature_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for monetary nature: " << request.code;

    get_monetary_nature_history_response response;
    try {
        auto history = svc.get_type_history(request.code);

        if (history.empty()) {
            response.success = false;
            response.message = "Monetary nature not found: " + request.code;
            BOOST_LOG_SEV(lg(), warn) << "No history found for monetary nature: " << request.code;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for monetary nature: " << request.code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for monetary nature "
                                   << request.code << ": " << e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Currency Market Tier handlers
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_currency_market_tiers_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_currency_market_tiers_request.";

    auto auth = require_authentication(remote_address, "Get currency market tiers");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::currency_market_tier_service svc(ctx);

    auto request_result = get_currency_market_tiers_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_currency_market_tiers_request";
        co_return std::unexpected(request_result.error());
    }

    auto types = svc.list_types();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << types.size() << " currency market tiers";

    get_currency_market_tiers_response response{
        .types = std::move(types)
    };
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_save_currency_market_tier_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_currency_market_tier_request.";

    auto auth = require_authentication(remote_address, "Save currency market tier");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::currency_market_tier_service svc(ctx);

    auto request_result = save_currency_market_tier_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_currency_market_tier_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving " << request.types.size()
                              << " currency market tier(s)";

    save_currency_market_tier_response response;
    try {
        svc.save_types(request.types);
        response.success = true;
        response.message = "Saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved "
                                  << request.types.size()
                                  << " currency market tier(s) by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save currency market tiers: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving currency market tiers: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_delete_currency_market_tier_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_currency_market_tier_request.";

    auto auth = require_authentication(remote_address, "Delete currency market tier");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::currency_market_tier_service svc(ctx);

    auto request_result = delete_currency_market_tier_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_currency_market_tier_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.codes.size() << " currency market tier(s)";

    delete_currency_market_tier_response response;
    try {
        for (const auto& code : request.codes) {
            svc.remove_type(code);
        }
        response.success = true;
        response.message = "Deleted successfully";
        BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
            << request.codes.size() << " currency market tier(s)";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to delete currency market tier: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error in batch delete: " << e.what();
    }
    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
refdata_message_handler::
handle_get_currency_market_tier_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_currency_market_tier_history_request.";

    auto auth = require_authentication(remote_address, "Get currency market tier history");
    if (!auth) {
        co_return std::unexpected(auth.error());
    }

    auto ctx = make_request_context(*auth);
    service::currency_market_tier_service svc(ctx);

    auto request_result = get_currency_market_tier_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_currency_market_tier_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for currency market tier: " << request.code;

    get_currency_market_tier_history_response response;
    try {
        auto history = svc.get_type_history(request.code);

        if (history.empty()) {
            response.success = false;
            response.message = "Currency market tier not found: " + request.code;
            BOOST_LOG_SEV(lg(), warn) << "No history found for currency market tier: " << request.code;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << response.versions.size()
                                  << " versions for currency market tier: " << request.code;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for currency market tier "
                                   << request.code << ": " << e.what();
    }

    co_return response.serialize();
}

}
