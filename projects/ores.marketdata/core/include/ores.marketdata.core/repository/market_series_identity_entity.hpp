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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_entity.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_MARKET_SERIES_IDENTITY_ENTITY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_MARKET_SERIES_IDENTITY_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::marketdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a market series identity in the database.
 */
struct market_series_identity_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_marketdata_market_series_identity_tbl";

    sqlgen::PrimaryKey<std::string> series_id;
    std::string tenant_id;
    std::string party_id;
    std::string identity_kind;
    std::optional<std::string> asset_class;
    std::optional<std::string> instrument_type;
    std::optional<std::string> quote_type;
    std::optional<std::string> atm;
    std::optional<std::string> cap_floor;
    std::optional<std::string> ccy;
    std::optional<std::string> cds_index_name;
    std::optional<std::string> commodity_name;
    std::optional<std::string> contract;
    std::optional<std::string> contract_name;
    std::optional<std::string> curve_id;
    std::optional<std::string> day_counter;
    std::optional<std::string> doc_clause;
    std::optional<std::string> dst;
    std::optional<std::string> eq_name;
    std::optional<std::string> fixed_ccy;
    std::optional<std::string> fixed_tenor;
    std::optional<std::string> flat_ccy;
    std::optional<std::string> flat_term;
    std::optional<std::string> float_ccy;
    std::optional<std::string> float_tenor;
    std::optional<std::string> future_contract;
    std::optional<std::string> fwd_start;
    std::optional<std::string> identifier;
    std::optional<std::string> index;
    std::optional<std::string> index1;
    std::optional<std::string> index2;
    std::optional<std::string> index_name;
    std::optional<std::string> index_tenor;
    std::optional<std::string> index_term;
    std::optional<std::string> offset;
    std::optional<std::string> option_type;
    std::optional<std::string> payer_receiver;
    std::optional<std::string> qualifier;
    std::optional<std::string> quote_name;
    std::optional<std::string> quote_tag;
    std::optional<std::string> rating_name;
    std::optional<std::string> relative;
    std::optional<std::string> running_spread;
    std::optional<std::string> seasonality_type;
    std::optional<std::string> security_id;
    std::optional<std::string> seniority;
    std::optional<std::string> side;
    std::optional<std::string> tenor;
    std::optional<std::string> term;
    std::optional<std::string> time_unit;
    std::optional<std::string> underlying_name;
    std::optional<std::string> unit_ccy;
};

std::ostream& operator<<(std::ostream& s, const market_series_identity_entity& v);

}

#endif
