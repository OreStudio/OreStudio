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
#ifndef ORES_REFDATA_CORE_REPOSITORY_DEFAULT_CURVE_CONFIGURATION_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_DEFAULT_CURVE_CONFIGURATION_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a default curve configuration in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct default_curve_configuration_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_default_curve_configurations_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid party_id;
    boost::uuids::uuid curve_definition_id;
    bool is_inline = false;
    std::optional<int> priority;
    std::optional<std::string> default_curve_type;
    std::optional<std::string> discount_curve;
    std::optional<std::string> day_counter;
    std::optional<std::string> recovery_rate;
    std::optional<std::string> start_date;
    bool has_quotes = false;
    std::optional<std::string> benchmark_curve;
    std::optional<std::string> reinterpreted_yield_curve;
    std::optional<std::string> source_curve;
    std::optional<std::string> pillars;
    std::optional<int> spot_lag;
    std::optional<std::string> calendar;
    std::optional<std::string> conventions;
    std::optional<std::string> extrapolation;
    std::optional<ores::utility::decimal::decimal> running_spread;
    std::optional<std::string> index_term;
    std::optional<std::string> imply_default_from_market;
    std::optional<std::string> allow_negative_rates;
    std::optional<std::string> price_is_upfront;
    std::optional<std::string> initial_state;
    std::optional<std::string> states;
    int position = 0;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = db_timestamp::max();
    db_timestamp valid_to = db_timestamp::max();

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = default_curve_configuration_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("curve_definition_id", &e::curve_definition_id) |
               query_builder::bind("is_inline", &e::is_inline) |
               query_builder::bind("priority", &e::priority) |
               query_builder::bind("default_curve_type", &e::default_curve_type) |
               query_builder::bind("discount_curve", &e::discount_curve) |
               query_builder::bind("day_counter", &e::day_counter) |
               query_builder::bind("recovery_rate", &e::recovery_rate) |
               query_builder::bind("start_date", &e::start_date) |
               query_builder::bind("has_quotes", &e::has_quotes) |
               query_builder::bind("benchmark_curve", &e::benchmark_curve) |
               query_builder::bind("reinterpreted_yield_curve", &e::reinterpreted_yield_curve) |
               query_builder::bind("source_curve", &e::source_curve) |
               query_builder::bind("pillars", &e::pillars) |
               query_builder::bind("spot_lag", &e::spot_lag) |
               query_builder::bind("calendar", &e::calendar) |
               query_builder::bind("conventions", &e::conventions) |
               query_builder::bind("extrapolation", &e::extrapolation) |
               query_builder::bind("running_spread", &e::running_spread) |
               query_builder::bind("index_term", &e::index_term) |
               query_builder::bind("imply_default_from_market", &e::imply_default_from_market) |
               query_builder::bind("allow_negative_rates", &e::allow_negative_rates) |
               query_builder::bind("price_is_upfront", &e::price_is_upfront) |
               query_builder::bind("initial_state", &e::initial_state) |
               query_builder::bind("states", &e::states) |
               query_builder::bind("position", &e::position) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const default_curve_configuration_entity& v);

}

#endif
