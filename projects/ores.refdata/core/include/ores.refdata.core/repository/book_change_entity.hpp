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
#ifndef ORES_REFDATA_CORE_REPOSITORY_BOOK_CHANGE_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_BOOK_CHANGE_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a book change in the database.
 */
struct book_change_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_book_changes_tbl";

    std::string id;
    std::string tenant_id;
    int version = 0;
    std::string request_id;


    int line_no = 0;

    std::string operation;
    int base_version = 0;
    bool applied = false;
    std::string entity_id;
    std::string party_id;
    std::string name;
    std::optional<std::string> description;
    std::string parent_portfolio_id;
    std::optional<std::string> owner_unit_id;
    std::string functional_currency;
    std::optional<std::string> gl_account_ref;
    std::optional<std::string> cost_center;
    std::string book_status = "Active";
    std::string regulatory_book_type = "Trading";
    std::string book_purpose_type = "Trading";
    std::string ledger_feed_type = "None";
    bool is_sweepable = false;
    std::string rates_centre_code = "WRLD";
    std::optional<std::string> sandbox_id;
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
        using e = book_change_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("request_id", &e::request_id) |
               query_builder::bind("line_no", &e::line_no) |
               query_builder::bind("operation", &e::operation) |
               query_builder::bind("base_version", &e::base_version) |
               query_builder::bind("applied", &e::applied) |
               query_builder::bind("entity_id", &e::entity_id) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("name", &e::name) |
               query_builder::bind("description", &e::description) |
               query_builder::bind("parent_portfolio_id", &e::parent_portfolio_id) |
               query_builder::bind("owner_unit_id", &e::owner_unit_id) |
               query_builder::bind("functional_currency", &e::functional_currency) |
               query_builder::bind("gl_account_ref", &e::gl_account_ref) |
               query_builder::bind("cost_center", &e::cost_center) |
               query_builder::bind("book_status", &e::book_status) |
               query_builder::bind("regulatory_book_type", &e::regulatory_book_type) |
               query_builder::bind("book_purpose_type", &e::book_purpose_type) |
               query_builder::bind("ledger_feed_type", &e::ledger_feed_type) |
               query_builder::bind("is_sweepable", &e::is_sweepable) |
               query_builder::bind("rates_centre_code", &e::rates_centre_code) |
               query_builder::bind("sandbox_id", &e::sandbox_id) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const book_change_entity& v);

}

#endif
