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

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::dq::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a dataset in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct dataset_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_dq_datasets_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;

    std::string code;

    std::optional<std::string> catalog_name;
    std::string subject_area_name;
    std::string domain_name;
    std::optional<std::string> coding_scheme_code;
    std::string origin_code;
    std::string nature_code;
    std::string treatment_code;
    std::optional<boost::uuids::uuid> methodology_id;
    std::string name;
    std::string description;
    std::string source_system_id;
    std::string business_context;
    std::optional<boost::uuids::uuid> upstream_derivation_id;
    int lineage_depth = 0;
    db_timestamp as_of_date;
    db_timestamp ingestion_timestamp;
    std::optional<std::string> license_info;
    std::string artefact_type;
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
        using e = dataset_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) | query_builder::bind("code", &e::code) |
               query_builder::bind("catalog_name", &e::catalog_name) |
               query_builder::bind("subject_area_name", &e::subject_area_name) |
               query_builder::bind("domain_name", &e::domain_name) |
               query_builder::bind("coding_scheme_code", &e::coding_scheme_code) |
               query_builder::bind("origin_code", &e::origin_code) |
               query_builder::bind("nature_code", &e::nature_code) |
               query_builder::bind("treatment_code", &e::treatment_code) |
               query_builder::bind("methodology_id", &e::methodology_id) |
               query_builder::bind("name", &e::name) |
               query_builder::bind("description", &e::description) |
               query_builder::bind("source_system_id", &e::source_system_id) |
               query_builder::bind("business_context", &e::business_context) |
               query_builder::bind("upstream_derivation_id", &e::upstream_derivation_id) |
               query_builder::bind("lineage_depth", &e::lineage_depth) |
               query_builder::bind("as_of_date", &e::as_of_date) |
               query_builder::bind("ingestion_timestamp", &e::ingestion_timestamp) |
               query_builder::bind("license_info", &e::license_info) |
               query_builder::bind("artefact_type", &e::artefact_type) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const dataset_entity& v);

}

#endif
