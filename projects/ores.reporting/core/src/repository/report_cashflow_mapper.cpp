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
#include "ores.reporting.core/repository/report_cashflow_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/domain/report_cashflow.hpp"
#include "ores.reporting.api/domain/report_cashflow_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.core/repository/report_cashflow_entity.hpp"
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

domain::report_cashflow report_cashflow_mapper::map(const report_cashflow_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::report_cashflow r;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.party_id = v.party_id;
    r.report_instance_id = v.report_instance_id;
    r.family = v.family;
    r.subject_type = v.subject_type;
    r.subject_id = v.subject_id;
    r.scenario = v.scenario;
    r.cashflow_no = v.cashflow_no;
    r.leg_no = v.leg_no;
    r.as_of = v.as_of;
    r.measure = v.measure;
    r.part = v.part;
    r.report_definition_id = v.report_definition_id;
    r.pay_date = v.pay_date;
    r.flow_type = v.flow_type.value_or("");
    r.currency = v.currency.value_or("");
    r.qualifiers = v.qualifiers.value_or("");
    r.value = v.value;
    r.source = v.source;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

report_cashflow_entity report_cashflow_mapper::map(const domain::report_cashflow& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    report_cashflow_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.party_id = v.party_id;
    r.report_instance_id = v.report_instance_id;
    r.family = v.family;
    r.subject_type = v.subject_type;
    r.subject_id = v.subject_id;
    r.scenario = v.scenario;
    r.cashflow_no = v.cashflow_no;
    r.leg_no = v.leg_no;
    r.as_of = v.as_of;
    r.measure = v.measure;
    r.part = v.part;
    r.report_definition_id = v.report_definition_id;
    r.pay_date = v.pay_date;
    r.flow_type = v.flow_type.empty() ? std::nullopt : std::optional(v.flow_type);
    r.currency = v.currency.empty() ? std::nullopt : std::optional(v.currency);
    r.qualifiers = v.qualifiers.empty() ? std::nullopt : std::optional(v.qualifiers);
    r.value = v.value;
    r.source = v.source;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::report_cashflow>
report_cashflow_mapper::map(const std::vector<report_cashflow_entity>& v) {
    return map_vector<report_cashflow_entity, domain::report_cashflow>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<report_cashflow_entity>
report_cashflow_mapper::map(const std::vector<domain::report_cashflow>& v) {
    return map_vector<domain::report_cashflow, report_cashflow_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
