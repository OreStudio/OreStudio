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
#ifndef ORES_REFDATA_CORE_REPOSITORY_CROSS_CURRENCY_FIX_FLOAT_CONVENTION_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_CROSS_CURRENCY_FIX_FLOAT_CONVENTION_ENTITY_HPP

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
 * @brief Represents a cross-currency fix-float convention in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct cross_currency_fix_float_convention_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename =
        "ores_refdata_cross_currency_fix_float_conventions_tbl";

    std::string id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid party_id;
    int settlement_days = 0;
    std::string settlement_calendar;
    std::string settlement_convention;
    std::string fixed_currency;
    std::string fixed_frequency;
    std::string fixed_convention;
    std::string fixed_day_count_fraction;
    std::string index;
    std::optional<bool> eom;
    std::optional<bool> is_resettable;
    std::optional<bool> float_index_is_resettable;
    std::optional<bool> include_spread;
    std::optional<std::string> lookback;
    std::optional<int> fixing_days;
    std::optional<int> rate_cutoff;
    std::optional<bool> is_averaged;
    std::optional<bool> observation_shift;
    std::optional<std::string> oresmd_uri;
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
        using e = cross_currency_fix_float_convention_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("settlement_days", &e::settlement_days) |
               query_builder::bind("settlement_calendar", &e::settlement_calendar) |
               query_builder::bind("settlement_convention", &e::settlement_convention) |
               query_builder::bind("fixed_currency", &e::fixed_currency) |
               query_builder::bind("fixed_frequency", &e::fixed_frequency) |
               query_builder::bind("fixed_convention", &e::fixed_convention) |
               query_builder::bind("fixed_day_count_fraction", &e::fixed_day_count_fraction) |
               query_builder::bind("index", &e::index) | query_builder::bind("eom", &e::eom) |
               query_builder::bind("is_resettable", &e::is_resettable) |
               query_builder::bind("float_index_is_resettable", &e::float_index_is_resettable) |
               query_builder::bind("include_spread", &e::include_spread) |
               query_builder::bind("lookback", &e::lookback) |
               query_builder::bind("fixing_days", &e::fixing_days) |
               query_builder::bind("rate_cutoff", &e::rate_cutoff) |
               query_builder::bind("is_averaged", &e::is_averaged) |
               query_builder::bind("observation_shift", &e::observation_shift) |
               query_builder::bind("oresmd_uri", &e::oresmd_uri) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const cross_currency_fix_float_convention_entity& v);

}

#endif
