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
#ifndef ORES_TRADING_CORE_REPOSITORY_BOND_ISSUE_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_BOND_ISSUE_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::trading::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a bond issue in the database.
 */
struct bond_issue_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_bond_issues_tbl";

    std::string issue_id;
    std::string tenant_id;
    int version = 0;
    std::string security_id;
    std::optional<std::string> issuer;
    std::optional<std::string> face_value;
    std::optional<std::string> issue_date;
    std::optional<int> settlement_days;
    std::optional<std::string> calendar;
    std::optional<std::string> credit_curve_id;
    std::optional<std::string> reference_curve_id;
    std::optional<std::string> income_curve_id;
    std::optional<std::string> credit_group;
    std::optional<std::string> volatility_curve_id;
    std::optional<std::string> price_quote_method;
    std::optional<std::string> price_quote_base_value;
    std::optional<std::string> sub_type;
    std::optional<std::string> price_type;
    std::optional<std::string> payer;
    std::optional<std::string> credit_risk;
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
        using e = bond_issue_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("issue_id", &e::issue_id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("security_id", &e::security_id) |
               query_builder::bind("issuer", &e::issuer) |
               query_builder::bind("face_value", &e::face_value) |
               query_builder::bind("issue_date", &e::issue_date) |
               query_builder::bind("settlement_days", &e::settlement_days) |
               query_builder::bind("calendar", &e::calendar) |
               query_builder::bind("credit_curve_id", &e::credit_curve_id) |
               query_builder::bind("reference_curve_id", &e::reference_curve_id) |
               query_builder::bind("income_curve_id", &e::income_curve_id) |
               query_builder::bind("credit_group", &e::credit_group) |
               query_builder::bind("volatility_curve_id", &e::volatility_curve_id) |
               query_builder::bind("price_quote_method", &e::price_quote_method) |
               query_builder::bind("price_quote_base_value", &e::price_quote_base_value) |
               query_builder::bind("sub_type", &e::sub_type) |
               query_builder::bind("price_type", &e::price_type) |
               query_builder::bind("payer", &e::payer) |
               query_builder::bind("credit_risk", &e::credit_risk) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const bond_issue_entity& v);

}

#endif
