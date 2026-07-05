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

#ifndef ORES_REFDATA_API_DOMAIN_PARTY_ID_SCHEME_CONSTANTS_HPP
#define ORES_REFDATA_API_DOMAIN_PARTY_ID_SCHEME_CONSTANTS_HPP

#include <array>
#include <string_view>

namespace ores::refdata::domain::party_id_scheme_constants {

/**
 * @brief Party identifier scheme codes used throughout the system.
 *
 * These codes must match entries in the ores_refdata_party_id_schemes_tbl
 * table (see refdata_party_id_schemes_populate.sql). Some schemes carry a
 * max_cardinality of 1, meaning a given party/counterparty may only ever
 * have one identifier of that scheme.
 */
namespace codes {

constexpr std::string_view lei = "LEI";
constexpr std::string_view bic = "BIC";
constexpr std::string_view mic = "MIC";
constexpr std::string_view national_id = "NATIONAL_ID";
constexpr std::string_view cedb = "CEDB";
constexpr std::string_view natural_person = "NATURAL_PERSON";
constexpr std::string_view acer = "ACER";
constexpr std::string_view dtcc_participant_id = "DTCC_PARTICIPANT_ID";
constexpr std::string_view mpid = "MPID";
constexpr std::string_view internal = "INTERNAL";

} // namespace codes

/**
 * @brief All known party identifier scheme codes, in populate-script order.
 *
 * Useful for synthetic-data generators that must cycle through schemes
 * (e.g. to generate multiple identifiers for the same party/counterparty
 * without violating the max_cardinality=1 constraint some schemes carry).
 */
constexpr std::array<std::string_view, 10> all = {
    codes::lei,
    codes::bic,
    codes::mic,
    codes::national_id,
    codes::cedb,
    codes::natural_person,
    codes::acer,
    codes::dtcc_participant_id,
    codes::mpid,
    codes::internal,
};

} // namespace ores::refdata::domain::party_id_scheme_constants

#endif
