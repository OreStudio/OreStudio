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
#include "ores.reporting.core/repository/run_result_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/domain/run_result.hpp"
#include "ores.reporting.api/domain/run_result_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.core/repository/run_result_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <string_view>
#include <vector>

namespace ores::reporting::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::run_result run_result_mapper::map(const run_result_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::run_result r;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.party_id = v.party_id;
    r.report_instance_id = v.report_instance_id;
    r.report_definition_id = v.report_definition_id;
    r.as_of = v.as_of;
    r.subject_type = v.subject_type;
    r.subject_id = v.subject_id;
    r.measure = v.measure;
    r.currency = v.currency;
    r.counterparty_id = v.counterparty_id;
    r.figure = v.figure;
    r.source = v.source;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

run_result_entity run_result_mapper::map(const domain::run_result& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    run_result_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.party_id = v.party_id;
    r.report_instance_id = v.report_instance_id;
    r.report_definition_id = v.report_definition_id;
    r.as_of = v.as_of;
    r.subject_type = v.subject_type;
    r.subject_id = v.subject_id;
    r.measure = v.measure;
    r.currency = v.currency;
    r.counterparty_id = v.counterparty_id;
    r.figure = v.figure;
    r.source = v.source;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::run_result> run_result_mapper::map(const std::vector<run_result_entity>& v) {
    return map_vector<run_result_entity, domain::run_result>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<run_result_entity> run_result_mapper::map(const std::vector<domain::run_result>& v) {
    return map_vector<domain::run_result, run_result_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
