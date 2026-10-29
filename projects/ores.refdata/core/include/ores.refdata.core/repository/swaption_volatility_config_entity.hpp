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
#ifndef ORES_REFDATA_CORE_REPOSITORY_SWAPTION_VOLATILITY_CONFIG_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_SWAPTION_VOLATILITY_CONFIG_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a swaption volatility config in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct swaption_volatility_config_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_swaption_volatility_configs_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid party_id;
    boost::uuids::uuid curve_definition_id;
    std::optional<std::string> dimension;
    std::optional<std::string> volatility_type;
    std::optional<std::string> interpolation;
    std::optional<std::string> extrapolation;
    std::optional<std::string> output_volatility_type;
    std::optional<std::string> model_shift;
    std::optional<std::string> output_shift;
    std::optional<std::string> day_counter;
    std::optional<std::string> calendar;
    std::optional<std::string> business_day_convention;
    std::optional<std::string> option_tenors;
    std::optional<std::string> swap_tenors;
    std::optional<std::string> short_swap_index_base;
    std::optional<std::string> swap_index_base;
    std::optional<std::string> smile_option_tenors;
    std::optional<std::string> smile_swap_tenors;
    std::optional<std::string> smile_spreads;
    std::optional<std::string> quote_tag;
    bool has_proxy_config = false;
    std::optional<std::string> proxy_source_curve_id;
    std::optional<std::string> proxy_source_short_swap_index_base;
    std::optional<std::string> proxy_source_swap_index_base;
    std::optional<std::string> proxy_target_short_swap_index_base;
    std::optional<std::string> proxy_target_swap_index_base;
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
        using e = swaption_volatility_config_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("curve_definition_id", &e::curve_definition_id) |
               query_builder::bind("dimension", &e::dimension) |
               query_builder::bind("volatility_type", &e::volatility_type) |
               query_builder::bind("interpolation", &e::interpolation) |
               query_builder::bind("extrapolation", &e::extrapolation) |
               query_builder::bind("output_volatility_type", &e::output_volatility_type) |
               query_builder::bind("model_shift", &e::model_shift) |
               query_builder::bind("output_shift", &e::output_shift) |
               query_builder::bind("day_counter", &e::day_counter) |
               query_builder::bind("calendar", &e::calendar) |
               query_builder::bind("business_day_convention", &e::business_day_convention) |
               query_builder::bind("option_tenors", &e::option_tenors) |
               query_builder::bind("swap_tenors", &e::swap_tenors) |
               query_builder::bind("short_swap_index_base", &e::short_swap_index_base) |
               query_builder::bind("swap_index_base", &e::swap_index_base) |
               query_builder::bind("smile_option_tenors", &e::smile_option_tenors) |
               query_builder::bind("smile_swap_tenors", &e::smile_swap_tenors) |
               query_builder::bind("smile_spreads", &e::smile_spreads) |
               query_builder::bind("quote_tag", &e::quote_tag) |
               query_builder::bind("has_proxy_config", &e::has_proxy_config) |
               query_builder::bind("proxy_source_curve_id", &e::proxy_source_curve_id) |
               query_builder::bind("proxy_source_short_swap_index_base",
                                   &e::proxy_source_short_swap_index_base) |
               query_builder::bind("proxy_source_swap_index_base",
                                   &e::proxy_source_swap_index_base) |
               query_builder::bind("proxy_target_short_swap_index_base",
                                   &e::proxy_target_short_swap_index_base) |
               query_builder::bind("proxy_target_swap_index_base",
                                   &e::proxy_target_swap_index_base) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const swaption_volatility_config_entity& v);

}

#endif
