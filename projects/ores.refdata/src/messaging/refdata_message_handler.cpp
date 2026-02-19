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
#include "ores.refdata/service/portfolio_service.hpp"
#include "ores.refdata/service/book_service.hpp"
#include "ores.refdata/service/book_status_service.hpp"
#include "ores.refdata/messaging/book_status_protocol.hpp"
#include "ores.refdata/service/purpose_type_service.hpp"
#include "ores.refdata/messaging/purpose_type_protocol.hpp"

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
    BOOST_LOG_SEV(lg(), info) << "Saving currency: " << request.currency.iso_code;

    // Override modified_by with authenticated username
    request.currency.modified_by = auth->username;
    request.currency.performed_by.clear();

    save_currency_response response;
    try {
        currency_service.save_currency(request.currency);
        response.success = true;
        response.message = "Currency saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved currency: "
                                  << request.currency.iso_code
                                  << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save currency: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving currency "
                                   << request.currency.iso_code << ": " << e.what();
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

    // Process each currency in the batch
    for (const auto& iso_code : request.iso_codes) {
        delete_currency_result result;
        result.iso_code = iso_code;

        try {
            currency_service.delete_currency(iso_code);
            result.success = true;
            result.message = "Currency deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted currency: " << iso_code;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete currency: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting currency "
                                       << iso_code << ": " << e.what();
        }

        response.results.push_back(std::move(result));
    }

    BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
                              << response.results.size() << " results";

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
    BOOST_LOG_SEV(lg(), info) << "Saving business centre: " << request.business_centre.code;

    request.business_centre.modified_by = auth->username;
    request.business_centre.performed_by.clear();

    save_business_centre_response response;
    try {
        svc.save_business_centre(request.business_centre);
        response.success = true;
        response.message = "Business centre saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved business centre: "
                                  << request.business_centre.code
                                  << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save business centre: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving business centre "
                                   << request.business_centre.code << ": " << e.what();
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

    for (const auto& code : request.codes) {
        delete_business_centre_result result;
        result.code = code;

        try {
            svc.delete_business_centre(code);
            result.success = true;
            result.message = "Business centre deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted business centre: " << code;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete business centre: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting business centre "
                                       << code << ": " << e.what();
        }

        response.results.push_back(std::move(result));
    }

    BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
                              << response.results.size() << " results";

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
    BOOST_LOG_SEV(lg(), info) << "Saving country: " << request.country.alpha2_code;

    // Override modified_by with authenticated username
    request.country.modified_by = auth->username;
    request.country.performed_by.clear();

    save_country_response response;
    try {
        country_service.save_country(request.country);
        response.success = true;
        response.message = "Country saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved country: "
                                  << request.country.alpha2_code
                                  << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save country: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving country "
                                   << request.country.alpha2_code << ": " << e.what();
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

    // Process each country in the batch
    for (const auto& alpha2_code : request.alpha2_codes) {
        delete_country_result result;
        result.alpha2_code = alpha2_code;

        try {
            country_service.delete_country(alpha2_code);
            result.success = true;
            result.message = "Country deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted country: " << alpha2_code;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete country: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting country "
                                       << alpha2_code << ": " << e.what();
        }

        response.results.push_back(std::move(result));
    }

    BOOST_LOG_SEV(lg(), info) << "Batch delete completed: "
                              << response.results.size() << " results";

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
    BOOST_LOG_SEV(lg(), info) << "Saving party type: " << request.type.code;

    request.type.modified_by = auth->username;
    request.type.performed_by.clear();

    save_party_type_response response;
    try {
        svc.save_type(request.type);
        response.success = true;
        response.message = "Party type saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved party type: "
                                  << request.type.code << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save party type: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving party type "
                                   << request.type.code << ": " << e.what();
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
    for (const auto& code : request.codes) {
        delete_party_type_result result;
        result.code = code;

        try {
            svc.remove_type(code);
            result.success = true;
            result.message = "Party type deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted party type: " << code;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete party type: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting party type "
                                       << code << ": " << e.what();
        }

        response.results.push_back(std::move(result));
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
    BOOST_LOG_SEV(lg(), info) << "Saving party status: " << request.status.code;

    request.status.modified_by = auth->username;
    request.status.performed_by.clear();

    save_party_status_response response;
    try {
        svc.save_status(request.status);
        response.success = true;
        response.message = "Party status saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved party status: "
                                  << request.status.code << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save party status: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving party status "
                                   << request.status.code << ": " << e.what();
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
    for (const auto& code : request.codes) {
        delete_party_status_result result;
        result.code = code;

        try {
            svc.remove_status(code);
            result.success = true;
            result.message = "Party status deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted party status: " << code;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete party status: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting party status "
                                       << code << ": " << e.what();
        }

        response.results.push_back(std::move(result));
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
    BOOST_LOG_SEV(lg(), info) << "Saving party ID scheme: " << request.scheme.code;

    request.scheme.modified_by = auth->username;
    request.scheme.performed_by.clear();

    save_party_id_scheme_response response;
    try {
        svc.save_scheme(request.scheme);
        response.success = true;
        response.message = "Party ID scheme saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved party ID scheme: "
                                  << request.scheme.code << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save party ID scheme: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving party ID scheme "
                                   << request.scheme.code << ": " << e.what();
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
    for (const auto& code : request.codes) {
        delete_party_id_scheme_result result;
        result.code = code;

        try {
            svc.remove_scheme(code);
            result.success = true;
            result.message = "Party ID scheme deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted party ID scheme: " << code;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete party ID scheme: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting party ID scheme "
                                       << code << ": " << e.what();
        }

        response.results.push_back(std::move(result));
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
    BOOST_LOG_SEV(lg(), info) << "Saving contact type: " << request.type.code;

    request.type.modified_by = auth->username;
    request.type.performed_by.clear();

    save_contact_type_response response;
    try {
        svc.save_type(request.type);
        response.success = true;
        response.message = "Contact type saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved contact type: "
                                  << request.type.code << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save contact type: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving contact type "
                                   << request.type.code << ": " << e.what();
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
    for (const auto& code : request.codes) {
        delete_contact_type_result result;
        result.code = code;

        try {
            svc.remove_type(code);
            result.success = true;
            result.message = "Contact type deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted contact type: " << code;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete contact type: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting contact type "
                                       << code << ": " << e.what();
        }

        response.results.push_back(std::move(result));
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
    BOOST_LOG_SEV(lg(), info) << "Saving party: " << request.party.short_code;

    request.party.tenant_id = auth->tenant_id.to_string();
    request.party.modified_by = auth->username;
    request.party.performed_by.clear();

    save_party_response response;
    try {
        svc.save_party(request.party);
        response.success = true;
        response.message = "Party saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved party: "
                                  << request.party.short_code << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save party: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving party "
                                   << request.party.short_code << ": " << e.what();
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
    for (const auto& id : request.ids) {
        delete_party_result result;
        result.id = id;

        try {
            svc.remove_party(id);
            result.success = true;
            result.message = "Party deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted party: " << id;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete party: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting party "
                                       << id << ": " << e.what();
        }

        response.results.push_back(std::move(result));
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
    BOOST_LOG_SEV(lg(), info) << "Saving counterparty: " << request.counterparty.short_code;

    request.counterparty.tenant_id = auth->tenant_id.to_string();
    request.counterparty.modified_by = auth->username;
    request.counterparty.performed_by.clear();

    save_counterparty_response response;
    try {
        svc.save_counterparty(request.counterparty);
        response.success = true;
        response.message = "Counterparty saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved counterparty: "
                                  << request.counterparty.short_code
                                  << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save counterparty: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving counterparty "
                                   << request.counterparty.short_code << ": " << e.what();
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
    for (const auto& id : request.ids) {
        delete_counterparty_result result;
        result.id = id;

        try {
            svc.remove_counterparty(id);
            result.success = true;
            result.message = "Counterparty deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted counterparty: " << id;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete counterparty: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting counterparty "
                                       << id << ": " << e.what();
        }

        response.results.push_back(std::move(result));
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
    BOOST_LOG_SEV(lg(), info) << "Saving business unit: " << request.business_unit.id;

    request.business_unit.modified_by = auth->username;
    request.business_unit.performed_by.clear();

    save_business_unit_response response;
    try {
        svc.save_business_unit(request.business_unit);
        response.success = true;
        response.message = "Business unit saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved business unit: "
                                  << request.business_unit.id
                                  << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save business unit: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving business unit "
                                   << request.business_unit.id << ": " << e.what();
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
    for (const auto& id : request.ids) {
        delete_business_unit_result result;
        result.id = id;

        try {
            svc.remove_business_unit(id);
            result.success = true;
            result.message = "Business unit deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted business unit: " << id;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete business unit: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting business unit "
                                       << id << ": " << e.what();
        }

        response.results.push_back(std::move(result));
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
    BOOST_LOG_SEV(lg(), info) << "Saving portfolio: " << request.portfolio.id;

    request.portfolio.modified_by = auth->username;
    request.portfolio.performed_by.clear();

    save_portfolio_response response;
    try {
        svc.save_portfolio(request.portfolio);
        response.success = true;
        response.message = "Portfolio saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved portfolio: "
                                  << request.portfolio.id
                                  << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save portfolio: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving portfolio "
                                   << request.portfolio.id << ": " << e.what();
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
    for (const auto& id : request.ids) {
        delete_portfolio_result result;
        result.id = id;

        try {
            svc.remove_portfolio(id);
            result.success = true;
            result.message = "Portfolio deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted portfolio: " << id;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete portfolio: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting portfolio "
                                       << id << ": " << e.what();
        }

        response.results.push_back(std::move(result));
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
    BOOST_LOG_SEV(lg(), info) << "Saving book: " << request.book.id;

    request.book.modified_by = auth->username;
    request.book.performed_by.clear();

    save_book_response response;
    try {
        svc.save_book(request.book);
        response.success = true;
        response.message = "Book saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved book: "
                                  << request.book.id
                                  << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save book: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving book "
                                   << request.book.id << ": " << e.what();
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
    for (const auto& id : request.ids) {
        delete_book_result result;
        result.id = id;

        try {
            svc.remove_book(id);
            result.success = true;
            result.message = "Book deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted book: " << id;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete book: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting book "
                                       << id << ": " << e.what();
        }

        response.results.push_back(std::move(result));
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
    BOOST_LOG_SEV(lg(), info) << "Saving counterparty identifier: "
                              << request.counterparty_identifier.id;

    request.counterparty_identifier.tenant_id = auth->tenant_id.to_string();
    request.counterparty_identifier.modified_by = auth->username;
    request.counterparty_identifier.performed_by.clear();

    save_counterparty_identifier_response response;
    try {
        svc.save_counterparty_identifier(request.counterparty_identifier);
        response.success = true;
        response.message = "Counterparty identifier saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved counterparty identifier: "
                                  << request.counterparty_identifier.id
                                  << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save counterparty identifier: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving counterparty identifier "
                                   << request.counterparty_identifier.id << ": " << e.what();
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
    for (const auto& id : request.ids) {
        delete_counterparty_identifier_result result;
        result.id = id;

        try {
            svc.remove_counterparty_identifier(id);
            result.success = true;
            result.message = "Counterparty identifier deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted counterparty identifier: " << id;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete counterparty identifier: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting counterparty identifier "
                                       << id << ": " << e.what();
        }

        response.results.push_back(std::move(result));
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
    BOOST_LOG_SEV(lg(), info) << "Saving counterparty contact information: "
                              << request.counterparty_contact_information.id;

    request.counterparty_contact_information.tenant_id = auth->tenant_id.to_string();
    request.counterparty_contact_information.modified_by = auth->username;
    request.counterparty_contact_information.performed_by.clear();

    save_counterparty_contact_information_response response;
    try {
        svc.save_counterparty_contact_information(request.counterparty_contact_information);
        response.success = true;
        response.message = "Counterparty contact information saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved counterparty contact information: "
                                  << request.counterparty_contact_information.id
                                  << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save counterparty contact information: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving counterparty contact information "
                                   << request.counterparty_contact_information.id << ": " << e.what();
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
    for (const auto& id : request.ids) {
        delete_counterparty_contact_information_result result;
        result.id = id;

        try {
            svc.remove_counterparty_contact_information(id);
            result.success = true;
            result.message = "Counterparty contact information deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted counterparty contact information: " << id;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete counterparty contact information: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting counterparty contact information "
                                       << id << ": " << e.what();
        }

        response.results.push_back(std::move(result));
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
    BOOST_LOG_SEV(lg(), info) << "Saving party identifier: "
                              << request.party_identifier.id;

    request.party_identifier.tenant_id = auth->tenant_id.to_string();
    request.party_identifier.modified_by = auth->username;
    request.party_identifier.performed_by.clear();

    save_party_identifier_response response;
    try {
        svc.save_party_identifier(request.party_identifier);
        response.success = true;
        response.message = "Party identifier saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved party identifier: "
                                  << request.party_identifier.id
                                  << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save party identifier: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving party identifier "
                                   << request.party_identifier.id << ": " << e.what();
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
    for (const auto& id : request.ids) {
        delete_party_identifier_result result;
        result.id = id;
        try {
            svc.remove_party_identifier(id);
            result.success = true;
            result.message = "Party identifier deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted party identifier: "
                                      << id << " by " << auth->username;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete party identifier: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting party identifier "
                                       << id << ": " << e.what();
        }
        response.results.push_back(std::move(result));
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
    BOOST_LOG_SEV(lg(), info) << "Saving party contact information: "
                              << request.party_contact_information.id;

    request.party_contact_information.tenant_id = auth->tenant_id.to_string();
    request.party_contact_information.modified_by = auth->username;
    request.party_contact_information.performed_by.clear();

    save_party_contact_information_response response;
    try {
        svc.save_party_contact_information(request.party_contact_information);
        response.success = true;
        response.message = "Party contact information saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved party contact information: "
                                  << request.party_contact_information.id
                                  << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save party contact information: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving party contact information "
                                   << request.party_contact_information.id << ": " << e.what();
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
    for (const auto& id : request.ids) {
        delete_party_contact_information_result result;
        result.id = id;
        try {
            svc.remove_party_contact_information(id);
            result.success = true;
            result.message = "Party contact information deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted party contact information: "
                                      << id << " by " << auth->username;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete party contact information: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting party contact information "
                                       << id << ": " << e.what();
        }
        response.results.push_back(std::move(result));
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
    BOOST_LOG_SEV(lg(), info) << "Saving book status: " << request.status.code;

    request.status.modified_by = auth->username;
    request.status.performed_by.clear();

    save_book_status_response response;
    try {
        svc.save_status(request.status);
        response.success = true;
        response.message = "Book status saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved book status: "
                                  << request.status.code << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save book status: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving book status "
                                   << request.status.code << ": " << e.what();
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
    for (const auto& code : request.codes) {
        delete_book_status_result result;
        result.code = code;

        try {
            svc.remove_status(code);
            result.success = true;
            result.message = "Book status deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted book status: " << code;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete book status: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting book status "
                                       << code << ": " << e.what();
        }

        response.results.push_back(std::move(result));
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
    BOOST_LOG_SEV(lg(), info) << "Saving purpose type: " << request.type.code;

    request.type.modified_by = auth->username;
    request.type.performed_by.clear();

    save_purpose_type_response response;
    try {
        svc.save_type(request.type);
        response.success = true;
        response.message = "Purpose type saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved purpose type: "
                                  << request.type.code << " by " << auth->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save purpose type: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving purpose type "
                                   << request.type.code << ": " << e.what();
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
    for (const auto& code : request.codes) {
        delete_purpose_type_result result;
        result.code = code;

        try {
            svc.remove_type(code);
            result.success = true;
            result.message = "Purpose type deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted purpose type: " << code;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete purpose type: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting purpose type "
                                       << code << ": " << e.what();
        }

        response.results.push_back(std::move(result));
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

}
