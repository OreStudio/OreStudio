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
#ifndef ORES_TRADING_CORE_REPOSITORY_CREDIT_DEFAULT_SWAP_INSTRUMENT_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_CREDIT_DEFAULT_SWAP_INSTRUMENT_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::trading::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a credit default swap instrument in the database.
 */
struct credit_default_swap_instrument_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_cds_instruments_tbl";

    std::string trade_id;
    std::string tenant_id;
    int version = 0;
    std::string trade_activity_id;
    std::optional<std::string> credit_curve_id;
    std::optional<std::string> issuer_id;
    std::optional<double> recovery_rate;
    std::optional<std::string> reference_obligation;
    std::optional<bool> settles_accrual;
    std::optional<bool> rebates_accrual;
    std::optional<bool> pays_at_default_time;
    std::optional<std::string> protection_payment_time;
    std::optional<std::string> protection_start_date;
    std::optional<std::string> upfront_date;
    std::optional<std::string> upfront_fee;
    std::optional<std::string> trade_date;
    std::optional<int> cash_settlement_days;
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
        using e = credit_default_swap_instrument_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("trade_id", &e::trade_id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("trade_activity_id", &e::trade_activity_id) |
               query_builder::bind("credit_curve_id", &e::credit_curve_id) |
               query_builder::bind("issuer_id", &e::issuer_id) |
               query_builder::bind("recovery_rate", &e::recovery_rate) |
               query_builder::bind("reference_obligation", &e::reference_obligation) |
               query_builder::bind("settles_accrual", &e::settles_accrual) |
               query_builder::bind("rebates_accrual", &e::rebates_accrual) |
               query_builder::bind("pays_at_default_time", &e::pays_at_default_time) |
               query_builder::bind("protection_payment_time", &e::protection_payment_time) |
               query_builder::bind("protection_start_date", &e::protection_start_date) |
               query_builder::bind("upfront_date", &e::upfront_date) |
               query_builder::bind("upfront_fee", &e::upfront_fee) |
               query_builder::bind("trade_date", &e::trade_date) |
               query_builder::bind("cash_settlement_days", &e::cash_settlement_days) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const credit_default_swap_instrument_entity& v);

}

#endif
