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
#ifndef ORES_TRADING_CORE_REPOSITORY_BOND_ISSUE_LEG_RATE_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_BOND_ISSUE_LEG_RATE_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::trading::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a bond issue leg rate in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct bond_issue_leg_rate_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_bond_issue_leg_rates_tbl";

    boost::uuids::uuid issue_id;
    int leg_number;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    std::string rate_kind;
    std::optional<std::string> index;
    std::optional<bool> is_in_arrears;
    std::optional<std::int64_t> fixing_days;
    std::optional<std::string> fixing_calendar;
    std::optional<std::string> last_recent_period;
    std::optional<std::string> last_recent_period_calendar;
    std::optional<std::string> lookback;
    std::optional<std::int64_t> rate_cutoff;
    std::optional<bool> is_averaged;
    std::optional<bool> has_sub_periods;
    std::optional<bool> include_spread;
    std::optional<bool> is_not_resetting_xccy;
    std::optional<bool> naked_option;
    std::optional<bool> local_cap_floor;
    std::optional<bool> stub_use_original_curve;
    std::optional<bool> observation_shift;
    std::optional<std::string> front_stub_short_index;
    std::optional<std::string> front_stub_long_index;
    std::optional<std::string> front_stub_rounding_type;
    std::optional<std::int64_t> front_stub_rounding_precision;
    std::optional<std::string> back_stub_short_index;
    std::optional<std::string> back_stub_long_index;
    std::optional<std::string> back_stub_rounding_type;
    std::optional<std::int64_t> back_stub_rounding_precision;
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
        using e = bond_issue_leg_rate_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("issue_id", &e::issue_id) |
               query_builder::key("leg_number", &e::leg_number) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("rate_kind", &e::rate_kind) |
               query_builder::bind("index", &e::index) |
               query_builder::bind("is_in_arrears", &e::is_in_arrears) |
               query_builder::bind("fixing_days", &e::fixing_days) |
               query_builder::bind("fixing_calendar", &e::fixing_calendar) |
               query_builder::bind("last_recent_period", &e::last_recent_period) |
               query_builder::bind("last_recent_period_calendar", &e::last_recent_period_calendar) |
               query_builder::bind("lookback", &e::lookback) |
               query_builder::bind("rate_cutoff", &e::rate_cutoff) |
               query_builder::bind("is_averaged", &e::is_averaged) |
               query_builder::bind("has_sub_periods", &e::has_sub_periods) |
               query_builder::bind("include_spread", &e::include_spread) |
               query_builder::bind("is_not_resetting_xccy", &e::is_not_resetting_xccy) |
               query_builder::bind("naked_option", &e::naked_option) |
               query_builder::bind("local_cap_floor", &e::local_cap_floor) |
               query_builder::bind("stub_use_original_curve", &e::stub_use_original_curve) |
               query_builder::bind("observation_shift", &e::observation_shift) |
               query_builder::bind("front_stub_short_index", &e::front_stub_short_index) |
               query_builder::bind("front_stub_long_index", &e::front_stub_long_index) |
               query_builder::bind("front_stub_rounding_type", &e::front_stub_rounding_type) |
               query_builder::bind("front_stub_rounding_precision",
                                   &e::front_stub_rounding_precision) |
               query_builder::bind("back_stub_short_index", &e::back_stub_short_index) |
               query_builder::bind("back_stub_long_index", &e::back_stub_long_index) |
               query_builder::bind("back_stub_rounding_type", &e::back_stub_rounding_type) |
               query_builder::bind("back_stub_rounding_precision",
                                   &e::back_stub_rounding_precision) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const bond_issue_leg_rate_entity& v);

}

#endif
