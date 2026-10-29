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

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::marketdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a market series identity in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct market_series_identity_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_marketdata_market_series_identity_tbl";

    boost::uuids::uuid series_id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    boost::uuids::uuid party_id;
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
    std::optional<std::string> delivery;
    std::optional<std::string> delivery_end;
    std::optional<std::string> delivery_start;
    std::optional<std::string> doc_clause;
    std::optional<std::string> dst;
    std::optional<std::string> eq_name;
    std::optional<std::string> expiry;
    std::optional<std::string> family;
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
    std::optional<std::string> spread_offset;
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
    std::optional<std::string> source;
    std::optional<std::string> tenor;
    std::optional<std::string> term;
    std::optional<std::string> time_unit;
    std::optional<std::string> underlying_name;
    std::optional<std::string> unit_ccy;

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = market_series_identity_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("series_id", &e::series_id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("identity_kind", &e::identity_kind) |
               query_builder::bind("asset_class", &e::asset_class) |
               query_builder::bind("instrument_type", &e::instrument_type) |
               query_builder::bind("quote_type", &e::quote_type) |
               query_builder::bind("atm", &e::atm) |
               query_builder::bind("cap_floor", &e::cap_floor) |
               query_builder::bind("ccy", &e::ccy) |
               query_builder::bind("cds_index_name", &e::cds_index_name) |
               query_builder::bind("commodity_name", &e::commodity_name) |
               query_builder::bind("contract", &e::contract) |
               query_builder::bind("contract_name", &e::contract_name) |
               query_builder::bind("curve_id", &e::curve_id) |
               query_builder::bind("day_counter", &e::day_counter) |
               query_builder::bind("delivery", &e::delivery) |
               query_builder::bind("delivery_end", &e::delivery_end) |
               query_builder::bind("delivery_start", &e::delivery_start) |
               query_builder::bind("doc_clause", &e::doc_clause) |
               query_builder::bind("dst", &e::dst) | query_builder::bind("eq_name", &e::eq_name) |
               query_builder::bind("expiry", &e::expiry) |
               query_builder::bind("family", &e::family) |
               query_builder::bind("fixed_ccy", &e::fixed_ccy) |
               query_builder::bind("fixed_tenor", &e::fixed_tenor) |
               query_builder::bind("flat_ccy", &e::flat_ccy) |
               query_builder::bind("flat_term", &e::flat_term) |
               query_builder::bind("float_ccy", &e::float_ccy) |
               query_builder::bind("float_tenor", &e::float_tenor) |
               query_builder::bind("future_contract", &e::future_contract) |
               query_builder::bind("fwd_start", &e::fwd_start) |
               query_builder::bind("identifier", &e::identifier) |
               query_builder::bind("index", &e::index) | query_builder::bind("index1", &e::index1) |
               query_builder::bind("index2", &e::index2) |
               query_builder::bind("index_name", &e::index_name) |
               query_builder::bind("index_tenor", &e::index_tenor) |
               query_builder::bind("index_term", &e::index_term) |
               query_builder::bind("spread_offset", &e::spread_offset) |
               query_builder::bind("option_type", &e::option_type) |
               query_builder::bind("payer_receiver", &e::payer_receiver) |
               query_builder::bind("qualifier", &e::qualifier) |
               query_builder::bind("quote_name", &e::quote_name) |
               query_builder::bind("quote_tag", &e::quote_tag) |
               query_builder::bind("rating_name", &e::rating_name) |
               query_builder::bind("relative", &e::relative) |
               query_builder::bind("running_spread", &e::running_spread) |
               query_builder::bind("seasonality_type", &e::seasonality_type) |
               query_builder::bind("security_id", &e::security_id) |
               query_builder::bind("seniority", &e::seniority) |
               query_builder::bind("side", &e::side) | query_builder::bind("source", &e::source) |
               query_builder::bind("tenor", &e::tenor) | query_builder::bind("term", &e::term) |
               query_builder::bind("time_unit", &e::time_unit) |
               query_builder::bind("underlying_name", &e::underlying_name) |
               query_builder::bind("unit_ccy", &e::unit_ccy);
    }
};

std::ostream& operator<<(std::ostream& s, const market_series_identity_entity& v);

}

#endif
