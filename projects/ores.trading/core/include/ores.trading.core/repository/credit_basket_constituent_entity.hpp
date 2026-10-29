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
#ifndef ORES_TRADING_CORE_REPOSITORY_CREDIT_BASKET_CONSTITUENT_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_CREDIT_BASKET_CONSTITUENT_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::trading::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a credit basket constituent in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct credit_basket_constituent_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_credit_basket_constituents_tbl";

    boost::uuids::uuid trade_id;
    int sequence_number;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid trade_activity_id;
    std::string issuer_id;
    std::optional<std::string> qualifier;
    std::optional<std::string> credit_curve_id;
    std::optional<ores::utility::decimal::decimal> notional;
    std::optional<double> weight;
    std::optional<std::string> currency;
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
        using e = credit_basket_constituent_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("trade_id", &e::trade_id) |
               query_builder::key("sequence_number", &e::sequence_number) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("trade_activity_id", &e::trade_activity_id) |
               query_builder::bind("issuer_id", &e::issuer_id) |
               query_builder::bind("qualifier", &e::qualifier) |
               query_builder::bind("credit_curve_id", &e::credit_curve_id) |
               query_builder::bind("notional", &e::notional) |
               query_builder::bind("weight", &e::weight) |
               query_builder::bind("currency", &e::currency) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const credit_basket_constituent_entity& v);

}

#endif
