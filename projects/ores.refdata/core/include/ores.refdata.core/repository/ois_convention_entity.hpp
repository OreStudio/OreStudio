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
#ifndef ORES_REFDATA_CORE_REPOSITORY_OIS_CONVENTION_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_OIS_CONVENTION_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a ois convention in the database.
 */
struct ois_convention_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_ois_conventions_tbl";

    std::string id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;
    int spot_lag = 0;
    std::string index;
    std::string fixed_day_count_fraction;
    std::optional<std::string> fixed_calendar;
    std::optional<int> payment_lag;
    std::optional<bool> end_of_month;
    std::optional<std::string> fixed_frequency;
    std::optional<std::string> fixed_convention;
    std::optional<std::string> fixed_payment_convention;
    std::optional<std::string> rule;
    std::optional<std::string> payment_calendar;
    std::optional<int> rate_cutoff;
    std::optional<std::string> oresmd_uri;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = ois_convention_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("spot_lag", &e::spot_lag) |
               query_builder::bind("index", &e::index) |
               query_builder::bind("fixed_day_count_fraction", &e::fixed_day_count_fraction) |
               query_builder::bind("fixed_calendar", &e::fixed_calendar) |
               query_builder::bind("payment_lag", &e::payment_lag) |
               query_builder::bind("end_of_month", &e::end_of_month) |
               query_builder::bind("fixed_frequency", &e::fixed_frequency) |
               query_builder::bind("fixed_convention", &e::fixed_convention) |
               query_builder::bind("fixed_payment_convention", &e::fixed_payment_convention) |
               query_builder::bind("rule", &e::rule) |
               query_builder::bind("payment_calendar", &e::payment_calendar) |
               query_builder::bind("rate_cutoff", &e::rate_cutoff) |
               query_builder::bind("oresmd_uri", &e::oresmd_uri) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const ois_convention_entity& v);

}

#endif
