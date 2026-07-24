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
#include "ores.refdata.api/domain/asset_class_code.hpp"
#include "ores.refdata.api/domain/book_status.hpp"
#include "ores.refdata.api/domain/business_unit_type.hpp"
#include "ores.refdata.api/domain/calendar_type.hpp"
#include "ores.refdata.api/domain/contact_type.hpp"
#include "ores.refdata.api/domain/counterparty.hpp"
#include "ores.refdata.api/domain/country.hpp"
#include "ores.refdata.api/domain/crm_topology_config.hpp"
#include "ores.refdata.api/domain/currency_market_tier.hpp"
#include "ores.refdata.api/domain/curve_role.hpp"
#include "ores.refdata.api/domain/instrument_code.hpp"
#include "ores.refdata.api/domain/monetary_nature.hpp"
#include "ores.refdata.api/domain/party.hpp"
#include "ores.refdata.api/domain/party_id_scheme.hpp"
#include "ores.refdata.api/domain/party_status.hpp"
#include "ores.refdata.api/domain/party_type.hpp"
#include "ores.refdata.api/domain/purpose_type.hpp"
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
 * @brief Fetches QuantLib/ORE calendar codes from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by the reusable calendar combo picker (setup_calendar_combo)
 * and any detail dialog that needs a plain calendar combo box.
 * Returns empty vector on failure.
 */
ORES_QT_API std::vector<std::string> fetch_calendar_codes(ClientManager* cm);

/**
 * @brief Fetches all country alpha-2 codes from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by detail dialogs that need a country combo box.
 * Returns empty vector on failure.
 */
ORES_QT_API std::vector<std::string> fetch_country_codes(ClientManager* cm);

/**
 * @brief Fetches all contact type codes from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by Party's and Counterparty's contact information "Type" combo.
 * Returns empty vector on failure.
 */
ORES_QT_API std::vector<std::string> fetch_contact_type_codes(ClientManager* cm);

/**
 * @brief Fetches all party id scheme codes (e.g. "LEI", "BIC") from the
 * server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by Party's and Counterparty's identifier "Scheme" combo.
 * Returns empty vector on failure.
 */
ORES_QT_API std::vector<std::string> fetch_party_id_scheme_codes(ClientManager* cm);

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
 * @brief Fetches all calendar types from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by DynamicComboSetup to populate calendar's calendar_type combo.
 * Returns an error message on failure, distinguishing it from a
 * legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::calendar_type>, QString>
fetch_calendar_types(ClientManager* cm);

/**
 * @brief Fetches all countries from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by DynamicComboSetup to populate calendar's country_code combo.
 * Returns an error message on failure, distinguishing it from a
 * legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::country>, QString>
fetch_countries(ClientManager* cm);

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
 * @brief Fetches book statuses as they stood at a specific timepoint.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used to resolve a historical book version's status badge correctly
 * even if the code has since been renamed or deleted -- see the
 * As-of lookup resolution codegen facet story. Returns an error
 * message on failure, distinguishing it from a legitimately-empty
 * result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::book_status>, QString>
fetch_book_statuses_at_timepoint(ClientManager* cm, const QString& as_of);

/**
 * @brief Fetches all business unit types from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by BusinessUnit's unit_type_id combo. Returns an error message
 * on failure, distinguishing it from a legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::business_unit_type>, QString>
fetch_business_unit_types(ClientManager* cm);

/**
 * @brief Fetches all purpose types from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by Portfolio's purpose_type combo. Returns an error message on
 * failure, distinguishing it from a legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::purpose_type>, QString>
fetch_purpose_types(ClientManager* cm);

/**
 * @brief Fetches all party types from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by Party's and Counterparty's party_type combo. Returns an
 * error message on failure, distinguishing it from a legitimately-
 * empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::party_type>, QString>
fetch_party_types(ClientManager* cm);

/**
 * @brief Fetches all party statuses from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by Party's and Counterparty's status combo. Returns an error
 * message on failure, distinguishing it from a legitimately-empty
 * result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::party_status>, QString>
fetch_party_statuses(ClientManager* cm);

/**
 * @brief Fetches all party id schemes from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by PartyIdentifier's and CounterpartyIdentifier's id_scheme
 * combo. Returns an error message on failure, distinguishing it from
 * a legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::party_id_scheme>, QString>
fetch_party_id_schemes(ClientManager* cm);

/**
 * @brief Fetches all contact types from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by PartyContactInformation's and CounterpartyContactInformation's
 * contact_type combo. Returns an error message on failure, distinguishing
 * it from a legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::contact_type>, QString>
fetch_contact_types(ClientManager* cm);

/**
 * @brief Fetches parties from the server for use as a parent-party picker.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by Party's (and Book's org-hierarchy) parent party combo. Fetches
 * a large-but-bounded page (matching PartyChildEntityTables' existing
 * limit=1000 precedent) rather than building a searchable/paginated
 * combo. Returns an error message on failure, distinguishing it from a
 * legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::party>, QString>
fetch_parties(ClientManager* cm);

/**
 * @brief Fetches counterparties from the server for use as a
 * parent-counterparty picker.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by Counterparty's parent counterparty combo. Fetches a
 * large-but-bounded page (matching PartyChildEntityTables' existing
 * limit=1000 precedent) rather than building a searchable/paginated
 * combo. Returns an error message on failure, distinguishing it from a
 * legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::counterparty>, QString>
fetch_counterparties(ClientManager* cm);

/**
 * @brief Fetches all CRM topology configs from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by CRM Driver Pair's and CRM Enabled Derived Pair's config_id
 * combo (the parent config a pair belongs to, otherwise unreachable
 * from either dialog since party_id/config_id are session/parent-
 * implicit and there is no Explorer/parent-relationship wiring for
 * CRM yet). Returns an error message on failure, distinguishing it
 * from a legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::crm_topology_config>, QString>
fetch_crm_topology_configs(ClientManager* cm);

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
 * @brief Fetches regulatory book types as they stood at a specific timepoint.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used to resolve a historical book version's type badge correctly
 * even if the code has since been renamed or deleted -- see the
 * As-of lookup resolution codegen facet story. Returns an error
 * message on failure, distinguishing it from a legitimately-empty
 * result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::regulatory_book_type>, QString>
fetch_regulatory_book_types_at_timepoint(ClientManager* cm, const QString& as_of);

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

/**
 * @brief Fetches all asset class codes from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by InstrumentCode's asset_class combo. Returns an error message
 * on failure, distinguishing it from a legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::asset_class_code>, QString>
fetch_asset_class_codes(ClientManager* cm);

/**
 * @brief Fetches all curve roles from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Used by InstrumentCode's curve_role combo. Returns an error message
 * on failure, distinguishing it from a legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::curve_role>, QString>
fetch_curve_roles(ClientManager* cm);

/**
 * @brief Fetches all instrument codes from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Returns an error message on failure, distinguishing it from a
 * legitimately-empty result.
 */
ORES_QT_API std::expected<std::vector<refdata::domain::instrument_code>, QString>
fetch_instrument_codes(ClientManager* cm);

}

#endif
