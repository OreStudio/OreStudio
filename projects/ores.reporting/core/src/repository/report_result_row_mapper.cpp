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
 * Template: cpp_domain_type_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.reporting.core/repository/report_result_row_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/domain/report_result_row.hpp"
#include "ores.reporting.api/domain/report_result_row_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.core/repository/report_result_row_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <string_view>
#include <vector>

namespace ores::reporting::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::report_result_row report_result_row_mapper::map(const report_result_row_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::report_result_row r;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);

    r.report_instance_id = boost::lexical_cast<boost::uuids::uuid>(v.report_instance_id);

    r.as_of = timestamp_to_timepoint(std::string_view{v.as_of});


    r.subject_type = v.subject_type;

    r.subject_id = boost::lexical_cast<boost::uuids::uuid>(v.subject_id);


    r.measure = v.measure;


    r.currency = v.currency;

    r.report_definition_id = boost::lexical_cast<boost::uuids::uuid>(v.report_definition_id);
    r.figure = v.figure;
    r.source = v.source;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

report_result_row_entity report_result_row_mapper::map(const domain::report_result_row& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    report_result_row_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.party_id = boost::uuids::to_string(v.party_id);

    r.report_instance_id = boost::uuids::to_string(v.report_instance_id);

    r.as_of = ores::platform::time::datetime::to_iso8601_utc(v.as_of);


    r.subject_type = v.subject_type;

    r.subject_id = boost::uuids::to_string(v.subject_id);


    r.measure = v.measure;


    r.currency = v.currency;

    r.report_definition_id = boost::uuids::to_string(v.report_definition_id);
    r.figure = v.figure;
    r.source = v.source;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::report_result_row>
report_result_row_mapper::map(const std::vector<report_result_row_entity>& v) {
    return map_vector<report_result_row_entity, domain::report_result_row>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<report_result_row_entity>
report_result_row_mapper::map(const std::vector<domain::report_result_row>& v) {
    return map_vector<domain::report_result_row, report_result_row_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
