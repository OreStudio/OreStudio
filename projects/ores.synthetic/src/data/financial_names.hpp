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
#ifndef ORES_SYNTHETIC_DATA_FINANCIAL_NAMES_HPP
#define ORES_SYNTHETIC_DATA_FINANCIAL_NAMES_HPP

#include <array>
#include <string_view>

namespace ores::synthetic::data {

/**
 * @brief English surnames suitable for financial institution naming.
 *
 * Curated list of surnames that sound plausible as foundations for
 * bank and financial firm names. All picking is done via the seeded
 * generation_engine to ensure reproducibility.
 */
inline constexpr std::array<std::string_view, 30> gb_surnames = {{
    "Rutherford", "Whitfield", "Crawford", "Henderson", "Ashworth",
    "Pemberton", "Thornton", "Hargreaves", "Kingsley", "Montague",
    "Blackwell", "Cavendish", "Fitzroy", "Drummond", "Baring",
    "Coutts", "Hambro", "Schroeder", "Warburg", "Rothschild",
    "Fleming", "Guinness", "Lloyds", "Jardine", "Matheson",
    "Laidlaw", "Gresham", "Dunbar", "Sinclair", "Mackenzie"
}};

inline constexpr std::array<std::string_view, 30> us_surnames = {{
    "Hamilton", "Jefferson", "Morgan", "Rockefeller", "Carnegie",
    "Vanderbilt", "Astor", "Whitney", "Mellon", "Harriman",
    "Goldman", "Lehman", "Merrill", "Salomon", "Warburg",
    "Loeb", "Kuhn", "Schiff", "Stillman", "Baker",
    "Aldrich", "Peabody", "Perkins", "Sears", "Wainwright",
    "Prescott", "Thayer", "Cabot", "Forbes", "Fidelity"
}};

/**
 * @brief Street name components for deterministic address generation.
 */
inline constexpr std::array<std::string_view, 20> gb_street_names = {{
    "King Street", "Queen Street", "High Street", "Threadneedle Street",
    "Lombard Street", "Cornhill", "Moorgate", "Bishopsgate",
    "Cannon Street", "Cheapside", "Leadenhall Street", "Fenchurch Street",
    "Gracechurch Street", "Poultry", "Old Broad Street", "Throgmorton Street",
    "Bartholomew Lane", "Princes Street", "Gresham Street", "Coleman Street"
}};

inline constexpr std::array<std::string_view, 20> us_street_names = {{
    "Wall Street", "Broadway", "Park Avenue", "Madison Avenue",
    "Lexington Avenue", "Fifth Avenue", "Sixth Avenue", "Water Street",
    "Broad Street", "Pine Street", "Liberty Street", "Maiden Lane",
    "Exchange Place", "Fulton Street", "William Street", "Nassau Street",
    "State Street", "Federal Street", "Congress Street", "Franklin Street"
}};

/**
 * @brief BIC code prefixes for deterministic BIC generation.
 */
inline constexpr std::array<std::string_view, 10> bic_prefixes = {{
    "RUTH", "WHIT", "CRAW", "HEND", "ASHW",
    "PEMB", "THOR", "HARG", "KING", "MONT"
}};

inline constexpr std::string_view gb_bic_suffix = "GB2L";
inline constexpr std::string_view us_bic_suffix = "US33";

/**
 * @brief GB-style party name suffixes for legal entities.
 */
inline constexpr std::array<std::string_view, 8> gb_party_suffixes = {{
    "Bank Plc",
    "Capital Ltd",
    "Securities Ltd",
    "Asset Management LLP",
    "Wealth Management Ltd",
    "Holdings Plc",
    "Investments Ltd",
    "Partners LLP"
}};

/**
 * @brief US-style party name suffixes for legal entities.
 */
inline constexpr std::array<std::string_view, 8> us_party_suffixes = {{
    "Financial Inc",
    "Capital LLC",
    "Securities Corp",
    "Asset Management LLC",
    "Wealth Management Inc",
    "Holdings Corp",
    "Investments LLC",
    "Partners LP"
}};

/**
 * @brief GB-style counterparty name patterns.
 *
 * {0} is replaced with a surname, {1} with a city.
 */
inline constexpr std::array<std::string_view, 8> gb_counterparty_patterns = {{
    "{0} Brothers",
    "{0} & Co",
    "{0} Capital",
    "{0} Bank Plc",
    "Royal {0} Group",
    "{0} Investment Trust",
    "{0} Wealth Management",
    "{0} Securities"
}};

/**
 * @brief US-style counterparty name patterns.
 */
inline constexpr std::array<std::string_view, 8> us_counterparty_patterns = {{
    "{0} Stanley",
    "{0} Sachs",
    "{0} Lynch",
    "{0} Financial Group",
    "First {0} Bank",
    "{0} Asset Management",
    "{0} Brothers & Co",
    "{1} Trust Company"
}};

/**
 * @brief Counterparty party types used in financial services.
 */
inline constexpr std::array<std::string_view, 4> counterparty_types = {{
    "Bank",
    "Securities Firm",
    "Insurance Co",
    "Fund Manager"
}};

/**
 * @brief Regional names for organisational divisions.
 */
inline constexpr std::array<std::string_view, 3> region_names = {{
    "EMEA",
    "Americas",
    "APAC"
}};

/**
 * @brief Asset classes for portfolio and business unit naming.
 */
inline constexpr std::array<std::string_view, 4> asset_classes = {{
    "Rates",
    "Credit",
    "FX",
    "Equities"
}};

/**
 * @brief Trading book product types.
 */
inline constexpr std::array<std::string_view, 7> product_types = {{
    "Vanilla Swaps",
    "Options",
    "Exotic",
    "Spot Forward",
    "CDS",
    "Bonds",
    "NDF"
}};

/**
 * @brief Purpose types for portfolio nodes.
 */
inline constexpr std::array<std::string_view, 3> portfolio_purposes = {{
    "Risk",
    "Regulatory",
    "Hedging"
}};

/**
 * @brief Financial centre city with associated business centre code.
 */
struct financial_centre {
    std::string_view city;
    std::string_view business_centre_code;
    std::string_view state;        // county (GB) or state (US)
    std::string_view postal_prefix; // for realistic postal codes
};

/**
 * @brief GB financial centres.
 */
inline constexpr std::array<financial_centre, 6> gb_financial_centres = {{
    {"London",      "GBLO", "Greater London",  "EC"},
    {"Edinburgh",   "GBED", "Midlothian",      "EH"},
    {"Birmingham",  "GBLO", "West Midlands",   "B"},
    {"Manchester",  "GBLO", "Greater Manchester", "M"},
    {"Leeds",       "GBLO", "West Yorkshire",  "LS"},
    {"Glasgow",     "GBLO", "Glasgow",         "G"}
}};

/**
 * @brief US financial centres.
 */
inline constexpr std::array<financial_centre, 6> us_financial_centres = {{
    {"New York",      "USNY", "New York",     "10"},
    {"Chicago",       "USCH", "Illinois",     "60"},
    {"Boston",        "USBO", "Massachusetts","02"},
    {"San Francisco", "USSF", "California",   "94"},
    {"Charlotte",     "USNY", "North Carolina","28"},
    {"Houston",       "USNY", "Texas",        "77"}
}};

/**
 * @brief Currencies commonly used in each region.
 */
inline constexpr std::array<std::string_view, 3> region_currencies = {{
    "EUR",  // EMEA
    "USD",  // Americas
    "JPY"   // APAC
}};

/**
 * @brief Per-asset-class currencies for leaf portfolios under EMEA.
 */
inline constexpr std::array<std::string_view, 4> emea_leaf_currencies = {{
    "GBP",  // Rates
    "EUR",  // Credit
    "EUR",  // FX
    "EUR"   // Equities
}};

/**
 * @brief Per-asset-class currencies for leaf portfolios under Americas.
 */
inline constexpr std::array<std::string_view, 4> americas_leaf_currencies = {{
    "USD",  // Rates
    "USD",  // Credit
    "USD",  // FX
    "USD"   // Equities
}};

/**
 * @brief Per-asset-class currencies for leaf portfolios under APAC.
 */
inline constexpr std::array<std::string_view, 4> apac_leaf_currencies = {{
    "JPY",  // Rates
    "JPY",  // Credit
    "JPY",  // FX
    "JPY"   // Equities
}};

/**
 * @brief Contact types for address records.
 */
inline constexpr std::array<std::string_view, 4> contact_types = {{
    "Legal",
    "Operations",
    "Settlement",
    "Billing"
}};

/**
 * @brief Web domain suffixes by country.
 */
inline constexpr std::string_view gb_domain_suffix = "co.uk";
inline constexpr std::string_view us_domain_suffix = "com";

/**
 * @brief Phone country prefixes.
 */
inline constexpr std::string_view gb_phone_prefix = "+44";
inline constexpr std::string_view us_phone_prefix = "+1";

}

#endif
