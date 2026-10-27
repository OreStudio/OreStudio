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
#ifndef ORES_DQ_CORE_REPOSITORY_DATASET_ENTITY_HPP
#define ORES_DQ_CORE_REPOSITORY_DATASET_ENTITY_HPP

#include "ores.database/q/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::dq::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a dataset in the database.
 */
struct dataset_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_dq_datasets_tbl";

    std::string id;
    std::string tenant_id;
    int version = 0;

    std::string code;

    std::optional<std::string> catalog_name;
    std::string subject_area_name;
    std::string domain_name;
    std::optional<std::string> coding_scheme_code;
    std::string origin_code;
    std::string nature_code;
    std::string treatment_code;
    std::optional<std::string> methodology_id;
    std::string name;
    std::string description;
    std::string source_system_id;
    std::string business_context;
    std::optional<std::string> upstream_derivation_id;
    int lineage_depth = 0;
    std::string as_of_date;
    std::string ingestion_timestamp;
    std::optional<std::string> license_info;
    std::string artefact_type;
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
        using e = dataset_entity;
        namespace q = ores::database::q;
        return q::columns<e>() | q::key("id", &e::id) | q::bind("tenant_id", &e::tenant_id) |
               q::bind("version", &e::version) | q::bind("code", &e::code) |
               q::bind("catalog_name", &e::catalog_name) |
               q::bind("subject_area_name", &e::subject_area_name) |
               q::bind("domain_name", &e::domain_name) |
               q::bind("coding_scheme_code", &e::coding_scheme_code) |
               q::bind("origin_code", &e::origin_code) | q::bind("nature_code", &e::nature_code) |
               q::bind("treatment_code", &e::treatment_code) |
               q::bind("methodology_id", &e::methodology_id) | q::bind("name", &e::name) |
               q::bind("description", &e::description) |
               q::bind("source_system_id", &e::source_system_id) |
               q::bind("business_context", &e::business_context) |
               q::bind("upstream_derivation_id", &e::upstream_derivation_id) |
               q::bind("lineage_depth", &e::lineage_depth) | q::bind("as_of_date", &e::as_of_date) |
               q::bind("ingestion_timestamp", &e::ingestion_timestamp) |
               q::bind("license_info", &e::license_info) |
               q::bind("artefact_type", &e::artefact_type) |
               q::bind("modified_by", &e::modified_by) | q::bind("performed_by", &e::performed_by) |
               q::bind("change_reason_code", &e::change_reason_code) |
               q::bind("change_commentary", &e::change_commentary) |
               q::bind("valid_from", &e::valid_from) | q::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const dataset_entity& v);

}

#endif
