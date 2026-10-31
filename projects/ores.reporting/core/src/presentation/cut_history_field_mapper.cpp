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
 * Template: cpp_history_field_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.reporting.core/presentation/cut_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/domain/cut.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::reporting::presentation {

std::vector<ores::diff::domain::field_value> render_cut_fields(const domain::cut& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Kind Code", .value = v.kind_code});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.party_id)});
    fields.push_back({.name = "Scope Code", .value = v.scope_code});
    fields.push_back({.name = "Business Centre Code", .value = v.business_centre_code});
    fields.push_back({.name = "Timepoint",
                      .value = ores::platform::time::datetime::to_iso8601_utc(v.timepoint)});
    fields.push_back({.name = "Take Ordinal", .value = std::to_string(v.take_ordinal)});
    fields.push_back({.name = "Timepoint Timezone", .value = v.timepoint_timezone});
    fields.push_back({.name = "Cut Time Code", .value = v.cut_time_code.value_or(std::string{})});
    fields.push_back({.name = "Market Data Snapshot ID",
                      .value = v.market_data_snapshot_id ?
                                   boost::uuids::to_string(*v.market_data_snapshot_id) :
                                   std::string{}});
    fields.push_back({.name = "Deal Population ID",
                      .value = v.deal_population_id ?
                                   boost::uuids::to_string(*v.deal_population_id) :
                                   std::string{}});
    fields.push_back({.name = "Reference Data Version",
                      .value = v.reference_data_version.value_or(std::string{})});
    fields.push_back({.name = "Engine Version", .value = v.engine_version.value_or(std::string{})});
    fields.push_back({.name = "Code Version", .value = v.code_version.value_or(std::string{})});
    fields.push_back({.name = "Taken By", .value = v.taken_by});
    fields.push_back({.name = "Taken At",
                      .value = v.taken_at ?
                                   ores::platform::time::datetime::to_iso8601_utc(*v.taken_at) :
                                   std::string{}});
    fields.push_back(
        {.name = "Requires Authorisation", .value = v.requires_authorisation ? "true" : "false"});
    fields.push_back({.name = "Approval Request ID",
                      .value = v.approval_request_id ?
                                   boost::uuids::to_string(*v.approval_request_id) :
                                   std::string{}});
    fields.push_back({.name = "Approved By", .value = v.approved_by});
    fields.push_back({.name = "Approved At",
                      .value = v.approved_at ?
                                   ores::platform::time::datetime::to_iso8601_utc(*v.approved_at) :
                                   std::string{}});
    fields.push_back({.name = "Refused At",
                      .value = v.refused_at ?
                                   ores::platform::time::datetime::to_iso8601_utc(*v.refused_at) :
                                   std::string{}});
    fields.push_back({.name = "Refusal Comment", .value = v.refusal_comment});
    fields.push_back({.name = "Retention Months", .value = std::to_string(v.retention_months)});
    fields.push_back({.name = "Retention Destination Code", .value = v.retention_destination_code});
    fields.push_back({.name = "Legal Hold", .value = v.legal_hold ? "true" : "false"});
    fields.push_back(
        {.name = "Output Is Indicative", .value = v.output_is_indicative ? "true" : "false"});
    using ores::history::domain::provenance_fields;
    fields.push_back({.name = provenance_fields::modified_by, .value = v.modified_by});
    fields.push_back({.name = provenance_fields::performed_by, .value = v.performed_by});
    fields.push_back(
        {.name = provenance_fields::change_reason_code, .value = v.change_reason_code});
    fields.push_back({.name = provenance_fields::change_commentary, .value = v.change_commentary});
    fields.push_back({.name = provenance_fields::recorded_at,
                      .value = ores::platform::time::datetime::to_iso8601_utc(v.recorded_at)});

    return fields;
}

}
