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
#ifndef ORES_TRADING_CORE_REPOSITORY_TRADE_SUMMARY_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_TRADE_SUMMARY_ENTITY_HPP

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
 * @brief Represents a trade summary in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct trade_summary_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_trade_summaries_vw";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    std::string row_kind;
    boost::uuids::uuid party_id;
    std::optional<boost::uuids::uuid> counterparty_id;
    std::optional<boost::uuids::uuid> book_id;
    std::string instrument_type;
    std::optional<std::string> status;
    std::string authorisation_status;
    std::optional<std::chrono::year_month_day> trade_date;
    int trade_count = 0;
    std::optional<std::string> structure_id;
    bool is_leg = false;
    std::string search_text;

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = trade_summary_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("row_kind", &e::row_kind) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("counterparty_id", &e::counterparty_id) |
               query_builder::bind("book_id", &e::book_id) |
               query_builder::bind("instrument_type", &e::instrument_type) |
               query_builder::bind("status", &e::status) |
               query_builder::bind("authorisation_status", &e::authorisation_status) |
               query_builder::bind("trade_date", &e::trade_date) |
               query_builder::bind("trade_count", &e::trade_count) |
               query_builder::bind("structure_id", &e::structure_id) |
               query_builder::bind("is_leg", &e::is_leg) |
               query_builder::bind("search_text", &e::search_text);
    }
};

std::ostream& operator<<(std::ostream& s, const trade_summary_entity& v);

}

#endif
