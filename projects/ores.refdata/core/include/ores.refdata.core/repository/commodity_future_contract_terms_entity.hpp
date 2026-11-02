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
#ifndef ORES_REFDATA_CORE_REPOSITORY_COMMODITY_FUTURE_CONTRACT_TERMS_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_COMMODITY_FUTURE_CONTRACT_TERMS_ENTITY_HPP

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
 * @brief Represents a commodity future contract terms in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct commodity_future_contract_terms_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_commodity_future_contract_terms_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid convention_id;

    boost::uuids::uuid party_id;
    std::string contract_frequency;
    std::string calendar;
    std::optional<std::string> expiry_calendar;
    std::optional<int> expiry_month_lag;
    std::optional<std::string> one_contract_month;
    std::optional<int> offset_days;
    std::optional<std::string> business_day_convention;
    std::optional<bool> adjust_before_offset;
    std::optional<bool> is_averaging;
    std::optional<std::string> valid_contract_months;
    std::optional<int> hours_per_day;
    std::optional<std::string> index_name;
    std::optional<std::string> savings_time;
    std::optional<std::string> delivery_location;
    std::optional<bool> balance_of_the_month;
    std::optional<std::string> balance_of_the_month_pricing_calendar;
    std::optional<std::string> prohibited_expiries;
    std::optional<std::string> future_continuation_mappings;
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
        using e = commodity_future_contract_terms_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("convention_id", &e::convention_id) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("contract_frequency", &e::contract_frequency) |
               query_builder::bind("calendar", &e::calendar) |
               query_builder::bind("expiry_calendar", &e::expiry_calendar) |
               query_builder::bind("expiry_month_lag", &e::expiry_month_lag) |
               query_builder::bind("one_contract_month", &e::one_contract_month) |
               query_builder::bind("offset_days", &e::offset_days) |
               query_builder::bind("business_day_convention", &e::business_day_convention) |
               query_builder::bind("adjust_before_offset", &e::adjust_before_offset) |
               query_builder::bind("is_averaging", &e::is_averaging) |
               query_builder::bind("valid_contract_months", &e::valid_contract_months) |
               query_builder::bind("hours_per_day", &e::hours_per_day) |
               query_builder::bind("index_name", &e::index_name) |
               query_builder::bind("savings_time", &e::savings_time) |
               query_builder::bind("delivery_location", &e::delivery_location) |
               query_builder::bind("balance_of_the_month", &e::balance_of_the_month) |
               query_builder::bind("balance_of_the_month_pricing_calendar",
                                   &e::balance_of_the_month_pricing_calendar) |
               query_builder::bind("prohibited_expiries", &e::prohibited_expiries) |
               query_builder::bind("future_continuation_mappings",
                                   &e::future_continuation_mappings) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const commodity_future_contract_terms_entity& v);

}

#endif
