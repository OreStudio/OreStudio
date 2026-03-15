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
#ifndef ORES_DQ_MESSAGING_DATA_ORGANIZATION_PROTOCOL_HPP
#define ORES_DQ_MESSAGING_DATA_ORGANIZATION_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include "ores.dq/domain/catalog.hpp"
#include "ores.dq/domain/data_domain.hpp"
#include "ores.dq/domain/methodology.hpp"
#include "ores.dq/domain/subject_area.hpp"
#include "ores.dq/domain/nature_dimension.hpp"
#include "ores.dq/domain/origin_dimension.hpp"
#include "ores.dq/domain/treatment_dimension.hpp"

namespace ores::dq::messaging {

// =============================================================================
// Catalog Protocol
// =============================================================================

struct get_catalogs_request {
    using response_type = struct get_catalogs_response;
    static constexpr std::string_view nats_subject = "dq.v1.catalogs.list";
    int offset = 0;
    int limit = 100;
};

struct get_catalogs_response {
    std::vector<ores::dq::domain::catalog> catalogs;
    int total_available_count = 0;
};

struct save_catalog_request {
    using response_type = struct save_catalog_response;
    static constexpr std::string_view nats_subject = "dq.v1.catalogs.save";
    ores::dq::domain::catalog data;
};

struct save_catalog_response {
    bool success = false;
    std::string message;
};

struct delete_catalog_request {
    using response_type = struct delete_catalog_response;
    static constexpr std::string_view nats_subject = "dq.v1.catalogs.delete";
    std::vector<std::string> codes;
};

struct delete_catalog_response {
    bool success = false;
    std::string message;
};

struct get_catalog_history_request {
    using response_type = struct get_catalog_history_response;
    static constexpr std::string_view nats_subject = "dq.v1.catalogs.history";
    std::string code;
};

struct get_catalog_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::domain::catalog> history;
};

// =============================================================================
// Data Domain Protocol
// =============================================================================

struct get_data_domains_request {
    using response_type = struct get_data_domains_response;
    static constexpr std::string_view nats_subject = "dq.v1.data-domains.list";
    int offset = 0;
    int limit = 100;
};

struct get_data_domains_response {
    std::vector<ores::dq::domain::data_domain> domains;
    int total_available_count = 0;
};

struct save_data_domain_request {
    using response_type = struct save_data_domain_response;
    static constexpr std::string_view nats_subject = "dq.v1.data-domains.save";
    ores::dq::domain::data_domain data;
};

struct save_data_domain_response {
    bool success = false;
    std::string message;
};

struct delete_data_domain_request {
    using response_type = struct delete_data_domain_response;
    static constexpr std::string_view nats_subject = "dq.v1.data-domains.delete";
    std::vector<std::string> names;
};

struct delete_data_domain_response {
    bool success = false;
    std::string message;
};

struct get_data_domain_history_request {
    using response_type = struct get_data_domain_history_response;
    static constexpr std::string_view nats_subject = "dq.v1.data-domains.history";
    std::string name;
};

struct get_data_domain_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::domain::data_domain> history;
};

// =============================================================================
// Methodology Protocol
// =============================================================================

struct get_methodologies_request {
    using response_type = struct get_methodologies_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.list";
    int offset = 0;
    int limit = 100;
};

struct get_methodologies_response {
    std::vector<ores::dq::domain::methodology> methodologies;
    int total_available_count = 0;
};

struct save_methodology_request {
    using response_type = struct save_methodology_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.save";
    ores::dq::domain::methodology data;
};

struct save_methodology_response {
    bool success = false;
    std::string message;
};

struct delete_methodology_request {
    using response_type = struct delete_methodology_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.delete";
    std::vector<std::string> codes;
};

struct delete_methodology_response {
    bool success = false;
    std::string message;
};

struct get_methodology_history_request {
    using response_type = struct get_methodology_history_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.history";
    std::string code;
};

struct get_methodology_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::domain::methodology> history;
};

// =============================================================================
// Subject Area Protocol
// =============================================================================

struct subject_area_key {
    std::string name;
    std::string domain_name;
};

struct get_subject_areas_request {
    using response_type = struct get_subject_areas_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject-areas.list";
    int offset = 0;
    int limit = 100;
};

struct get_subject_areas_response {
    std::vector<ores::dq::domain::subject_area> subject_areas;
    int total_available_count = 0;
};

struct save_subject_area_request {
    using response_type = struct save_subject_area_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject-areas.save";
    ores::dq::domain::subject_area data;
};

struct save_subject_area_response {
    bool success = false;
    std::string message;
};

struct delete_subject_area_request {
    using response_type = struct delete_subject_area_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject-areas.delete";
    std::vector<subject_area_key> keys;
};

struct delete_subject_area_response {
    bool success = false;
    std::string message;
};

struct get_subject_area_history_request {
    using response_type = struct get_subject_area_history_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject-areas.history";
    subject_area_key key;
};

struct get_subject_area_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::domain::subject_area> history;
};

// =============================================================================
// Nature Dimension Protocol
// =============================================================================

struct get_nature_dimensions_request {
    using response_type = struct get_nature_dimensions_response;
    static constexpr std::string_view nats_subject = "dq.v1.nature-dimensions.list";
    int offset = 0;
    int limit = 100;
};

struct get_nature_dimensions_response {
    std::vector<ores::dq::domain::nature_dimension> nature_dimensions;
    int total_available_count = 0;
};

struct save_nature_dimension_request {
    using response_type = struct save_nature_dimension_response;
    static constexpr std::string_view nats_subject = "dq.v1.nature-dimensions.save";
    ores::dq::domain::nature_dimension data;
};

struct save_nature_dimension_response {
    bool success = false;
    std::string message;
};

struct delete_nature_dimension_request {
    using response_type = struct delete_nature_dimension_response;
    static constexpr std::string_view nats_subject = "dq.v1.nature-dimensions.delete";
    std::vector<std::string> codes;
};

struct delete_nature_dimension_response {
    bool success = false;
    std::string message;
};

struct get_nature_dimension_history_request {
    using response_type = struct get_nature_dimension_history_response;
    static constexpr std::string_view nats_subject = "dq.v1.nature-dimensions.history";
    std::string code;
};

struct get_nature_dimension_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::domain::nature_dimension> history;
};

// =============================================================================
// Origin Dimension Protocol
// =============================================================================

struct get_origin_dimensions_request {
    using response_type = struct get_origin_dimensions_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin-dimensions.list";
    int offset = 0;
    int limit = 100;
};

struct get_origin_dimensions_response {
    std::vector<ores::dq::domain::origin_dimension> origin_dimensions;
    int total_available_count = 0;
};

struct save_origin_dimension_request {
    using response_type = struct save_origin_dimension_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin-dimensions.save";
    ores::dq::domain::origin_dimension data;
};

struct save_origin_dimension_response {
    bool success = false;
    std::string message;
};

struct delete_origin_dimension_request {
    using response_type = struct delete_origin_dimension_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin-dimensions.delete";
    std::vector<std::string> codes;
};

struct delete_origin_dimension_response {
    bool success = false;
    std::string message;
};

struct get_origin_dimension_history_request {
    using response_type = struct get_origin_dimension_history_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin-dimensions.history";
    std::string code;
};

struct get_origin_dimension_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::domain::origin_dimension> history;
};

// =============================================================================
// Treatment Dimension Protocol
// =============================================================================

struct get_treatment_dimensions_request {
    using response_type = struct get_treatment_dimensions_response;
    static constexpr std::string_view nats_subject = "dq.v1.treatment-dimensions.list";
    int offset = 0;
    int limit = 100;
};

struct get_treatment_dimensions_response {
    std::vector<ores::dq::domain::treatment_dimension> treatment_dimensions;
    int total_available_count = 0;
};

struct save_treatment_dimension_request {
    using response_type = struct save_treatment_dimension_response;
    static constexpr std::string_view nats_subject = "dq.v1.treatment-dimensions.save";
    ores::dq::domain::treatment_dimension data;
};

struct save_treatment_dimension_response {
    bool success = false;
    std::string message;
};

struct delete_treatment_dimension_request {
    using response_type = struct delete_treatment_dimension_response;
    static constexpr std::string_view nats_subject = "dq.v1.treatment-dimensions.delete";
    std::vector<std::string> codes;
};

struct delete_treatment_dimension_response {
    bool success = false;
    std::string message;
};

struct get_treatment_dimension_history_request {
    using response_type = struct get_treatment_dimension_history_response;
    static constexpr std::string_view nats_subject = "dq.v1.treatment-dimensions.history";
    std::string code;
};

struct get_treatment_dimension_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::domain::treatment_dimension> history;
};

}

#endif
