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
#ifndef ORES_QT_LOOKUP_FETCHER_HPP
#define ORES_QT_LOOKUP_FETCHER_HPP

#include "ores.dq.api/domain/coding_scheme.hpp"
#include "ores.qt/export.hpp"
#include "ores.refdata.api/domain/book_status.hpp"
#include "ores.refdata.api/domain/currency_market_tier.hpp"
#include "ores.refdata.api/domain/monetary_nature.hpp"
#include "ores.refdata.api/domain/regulatory_book_type.hpp"
#include "ores.refdata.api/domain/rounding_type.hpp"
#include "ores.refdata.api/domain/tenor_anchor.hpp"
#include "ores.refdata.api/domain/tenor_kind.hpp"
#include "ores.refdata.api/domain/tenor_resolution_algorithm.hpp"
#include "ores.refdata.api/domain/tenor_unit.hpp"
#include <QString>
#include <expected>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ores::qt {

class ClientManager;

/**
 * @brief Maximum number of items fetched by each synchronous lookup call.
 *
 * All lookup fetchers (currencies, business centres, portfolios, etc.) use
 * this single limit so the behaviour is consistent and easy to change.
 * A server-side search/filter mechanism should replace bulk fetching if
 * any entity class grows beyond this threshold.
 */
inline constexpr int lookup_fetch_limit = 1000;

/**
 * @brief Fixed party category values (foundation data, not server-fetched).
 */
namespace party_categories {
inline constexpr std::string_view operational = "Operational";
inline constexpr std::string_view system = "System";
inline constexpr std::string_view internal = "Internal";
}

/**
 * @brief Result of fetching type and status lookup codes from the server.
 *
 * Used by party, counterparty, and tenant detail dialogs to populate
 * combo boxes.
 */
struct lookup_result {
    std::vector<std::string> type_codes;
    std::vector<std::string> status_codes;
    std::vector<std::string> business_centre_codes;
};

/**
 * @brief Fetches party type and status codes from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Returns empty vectors on failure.
 */
ORES_QT_API lookup_result fetch_party_lookups(ClientManager* cm);

/**
 * @brief Fetches tenant type and status codes from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Returns empty vectors on failure.
 */
ORES_QT_API lookup_result fetch_tenant_lookups(ClientManager* cm);

/**
 * @brief Fetches currency ISO codes from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by detail dialogs that need a currency combo box.
 * Returns empty vector on failure.
 */
ORES_QT_API std::vector<std::string> fetch_currency_codes(ClientManager* cm);

/**
 * @brief Fetches all country alpha-2 codes from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by detail dialogs that need a country combo box.
 * Returns empty vector on failure.
 */
ORES_QT_API std::vector<std::string> fetch_country_codes(ClientManager* cm);

/**
 * @brief Fetches currency pair codes (e.g. "EUR/USD") from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by detail dialogs that need a currency pair combo box (e.g.
 * currency_pair_convention's pair_code field).
 * Returns empty vector on failure.
 */
ORES_QT_API std::vector<std::string> fetch_currency_pair_codes(ClientManager* cm);

/**
 * @brief Fetches a currency ISO code -> display name mapping from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used to render friendly currency names (e.g. "GBP" -> "British Pound").
 * Returns empty map on failure.
 */
ORES_QT_API std::unordered_map<std::string, std::string> fetch_currency_names(ClientManager* cm);

/**
 * @brief Fetches business centre code to country alpha-2 code mapping from
 * the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by list models that display country flag icons for business centres
 * (via ImageCache::getCountryFlagIcon).
 * Returns empty map on failure.
 */
ORES_QT_API std::unordered_map<std::string, std::string>
fetch_business_centre_country_map(ClientManager* cm);

/**
 * @brief Fetches business centre codes (e.g. "GBLO", "USNY") from the
 * server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by detail dialogs that need a plain business centre combo box (e.g.
 * book's rates_centre_code field) -- the business-centre counterpart to
 * fetch_currency_codes.
 * Returns empty vector on failure.
 */
ORES_QT_API std::vector<std::string> fetch_business_centre_codes(ClientManager* cm);

/**
 * @brief A name/id pair for a portfolio, used to populate parent combos.
 */
struct portfolio_entry {
    std::string id; // UUID as string
    std::string name;
};

/**
 * @brief Fetches all portfolio name/id pairs from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Returns empty vector on failure.
 */
ORES_QT_API std::vector<portfolio_entry> fetch_portfolio_entries(ClientManager* cm);

/**
 * @brief A name/id pair for a business unit, used to populate owner-unit combos.
 */
struct business_unit_entry {
    std::string id; // UUID as string
    std::string name;
};

/**
 * @brief Fetches all business unit name/id pairs from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Returns empty vector on failure.
 */
ORES_QT_API std::vector<business_unit_entry> fetch_business_unit_entries(ClientManager* cm);

/**
 * @brief Fetches all rounding types from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by DynamicComboSetup to populate currency's rounding_type combo.
 * Returns an error message on failure, distinguishing it from a
 * legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::rounding_type>, QString>
fetch_rounding_types(ClientManager* cm);

/**
 * @brief Fetches all monetary natures from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by DynamicComboSetup to populate currency's monetary_nature combo.
 * Returns an error message on failure, distinguishing it from a
 * legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::monetary_nature>, QString>
fetch_monetary_natures(ClientManager* cm);

/**
 * @brief Fetches all currency market tiers from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by DynamicComboSetup to populate currency's market_tier combo.
 * Returns an error message on failure, distinguishing it from a
 * legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::currency_market_tier>, QString>
fetch_currency_market_tiers(ClientManager* cm);

/**
 * @brief Fetches all book statuses from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by Book's book_status combo. Returns an error message on
 * failure, distinguishing it from a legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::book_status>, QString>
fetch_book_statuses(ClientManager* cm);

/**
 * @brief Fetches all regulatory book types from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by Book's regulatory_book_type combo. Returns an error message
 * on failure, distinguishing it from a legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::regulatory_book_type>, QString>
fetch_regulatory_book_types(ClientManager* cm);

/**
 * @brief Fetches all tenor kinds from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by Tenor's kind combo. Returns an error message on failure,
 * distinguishing it from a legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::tenor_kind>, QString>
fetch_tenor_kinds(ClientManager* cm);

/**
 * @brief Fetches all tenor units from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by Tenor's unit combo. Returns an error message on failure,
 * distinguishing it from a legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::tenor_unit>, QString>
fetch_tenor_units(ClientManager* cm);

/**
 * @brief Fetches all tenor resolution algorithms from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by Tenor Convention's resolution_algorithm combo. Returns an
 * error message on failure, distinguishing it from a legitimately-empty
 * result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::tenor_resolution_algorithm>, QString>
fetch_tenor_resolution_algorithms(ClientManager* cm);

/**
 * @brief Fetches all tenor anchors from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by Tenor Convention's measured_from combo. Returns an error
 * message on failure, distinguishing it from a legitimately-empty
 * result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::tenor_anchor>, QString>
fetch_tenor_anchors(ClientManager* cm);

/**
 * @brief Fetches all coding schemes from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by BusinessCentre's coding_scheme_code combo. Returns an error
 * message on failure, distinguishing it from a legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<dq::domain::coding_scheme>, QString>
fetch_coding_schemes(ClientManager* cm);

}

#endif
