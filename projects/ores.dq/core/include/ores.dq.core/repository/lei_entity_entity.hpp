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
#ifndef ORES_DQ_CORE_REPOSITORY_LEI_ENTITY_ENTITY_HPP
#define ORES_DQ_CORE_REPOSITORY_LEI_ENTITY_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::dq::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a lei entity in the database.
 */
struct lei_entity_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_dq_lei_entities_artefact_tbl";

    std::string lei;
    std::string tenant_id;
    std::string entity_legal_name;
    std::string entity_entity_category;
    std::optional<std::string> entity_entity_sub_category;
    std::string entity_entity_status;
    std::optional<std::string> entity_legal_form_entity_legal_form_code;
    std::optional<std::string> entity_legal_form_other_legal_form;
    std::optional<std::string> entity_legal_jurisdiction;
    std::optional<std::string> entity_legal_address_first_address_line;
    std::optional<std::string> entity_legal_address_city;
    std::optional<std::string> entity_legal_address_region;
    std::string entity_legal_address_country;
    std::optional<std::string> entity_legal_address_postal_code;
    std::optional<std::string> entity_headquarters_address_first_address_line;
    std::optional<std::string> entity_headquarters_address_city;
    std::optional<std::string> entity_headquarters_address_region;
    std::optional<std::string> entity_headquarters_address_country;
    std::optional<std::string> entity_headquarters_address_postal_code;
    std::optional<db_timestamp> entity_entity_creation_date;
    std::optional<db_timestamp> registration_initial_registration_date;
    std::optional<db_timestamp> registration_last_update_date;
    std::optional<db_timestamp> registration_next_renewal_date;
    std::optional<std::string> registration_registration_status;
    std::optional<std::string> entity_transliterated_name_1;
    std::optional<std::string> entity_transliterated_name_1_type;

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = lei_entity_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("lei", &e::lei) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("entity_legal_name", &e::entity_legal_name) |
               query_builder::bind("entity_entity_category", &e::entity_entity_category) |
               query_builder::bind("entity_entity_sub_category", &e::entity_entity_sub_category) |
               query_builder::bind("entity_entity_status", &e::entity_entity_status) |
               query_builder::bind("entity_legal_form_entity_legal_form_code",
                                   &e::entity_legal_form_entity_legal_form_code) |
               query_builder::bind("entity_legal_form_other_legal_form",
                                   &e::entity_legal_form_other_legal_form) |
               query_builder::bind("entity_legal_jurisdiction", &e::entity_legal_jurisdiction) |
               query_builder::bind("entity_legal_address_first_address_line",
                                   &e::entity_legal_address_first_address_line) |
               query_builder::bind("entity_legal_address_city", &e::entity_legal_address_city) |
               query_builder::bind("entity_legal_address_region", &e::entity_legal_address_region) |
               query_builder::bind("entity_legal_address_country",
                                   &e::entity_legal_address_country) |
               query_builder::bind("entity_legal_address_postal_code",
                                   &e::entity_legal_address_postal_code) |
               query_builder::bind("entity_headquarters_address_first_address_line",
                                   &e::entity_headquarters_address_first_address_line) |
               query_builder::bind("entity_headquarters_address_city",
                                   &e::entity_headquarters_address_city) |
               query_builder::bind("entity_headquarters_address_region",
                                   &e::entity_headquarters_address_region) |
               query_builder::bind("entity_headquarters_address_country",
                                   &e::entity_headquarters_address_country) |
               query_builder::bind("entity_headquarters_address_postal_code",
                                   &e::entity_headquarters_address_postal_code) |
               query_builder::bind("entity_entity_creation_date", &e::entity_entity_creation_date) |
               query_builder::bind("registration_initial_registration_date",
                                   &e::registration_initial_registration_date) |
               query_builder::bind("registration_last_update_date",
                                   &e::registration_last_update_date) |
               query_builder::bind("registration_next_renewal_date",
                                   &e::registration_next_renewal_date) |
               query_builder::bind("registration_registration_status",
                                   &e::registration_registration_status) |
               query_builder::bind("entity_transliterated_name_1",
                                   &e::entity_transliterated_name_1) |
               query_builder::bind("entity_transliterated_name_1_type",
                                   &e::entity_transliterated_name_1_type);
    }
};

std::ostream& operator<<(std::ostream& s, const lei_entity_entity& v);

}

#endif
