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
#include "ores.refdata.core/presentation/csa_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/csa.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value> render_csa_fields(const domain::csa& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back(
        {.name = "Netting Set ID", .value = boost::uuids::to_string(v.netting_set_id)});
    fields.push_back({.name = "Is Active", .value = v.is_active ? "true" : "false"});
    fields.push_back({.name = "Bilateral", .value = v.bilateral.value_or(std::string{})});
    fields.push_back({.name = "Csa Currency", .value = v.csa_currency.value_or(std::string{})});
    fields.push_back({.name = "Index Name", .value = v.index_name.value_or(std::string{})});
    fields.push_back({.name = "Threshold Pay",
                      .value = v.threshold_pay ? std::to_string(*v.threshold_pay) : std::string{}});
    fields.push_back(
        {.name = "Threshold Receive",
         .value = v.threshold_receive ? std::to_string(*v.threshold_receive) : std::string{}});
    fields.push_back({.name = "Minimum Transfer Amount Pay",
                      .value = v.minimum_transfer_amount_pay ?
                                   std::to_string(*v.minimum_transfer_amount_pay) :
                                   std::string{}});
    fields.push_back({.name = "Minimum Transfer Amount Receive",
                      .value = v.minimum_transfer_amount_receive ?
                                   std::to_string(*v.minimum_transfer_amount_receive) :
                                   std::string{}});
    fields.push_back({.name = "Independent Amount Held",
                      .value = v.independent_amount_held ?
                                   std::to_string(*v.independent_amount_held) :
                                   std::string{}});
    fields.push_back({.name = "Independent Amount Type",
                      .value = v.independent_amount_type.value_or(std::string{})});
    fields.push_back({.name = "Call Frequency", .value = v.call_frequency.value_or(std::string{})});
    fields.push_back({.name = "Post Frequency", .value = v.post_frequency.value_or(std::string{})});
    fields.push_back({.name = "Margin Period Of Risk",
                      .value = v.margin_period_of_risk.value_or(std::string{})});
    fields.push_back({.name = "Collateral Compounding Spread Receive",
                      .value = v.collateral_compounding_spread_receive ?
                                   std::to_string(*v.collateral_compounding_spread_receive) :
                                   std::string{}});
    fields.push_back({.name = "Collateral Compounding Spread Pay",
                      .value = v.collateral_compounding_spread_pay ?
                                   std::to_string(*v.collateral_compounding_spread_pay) :
                                   std::string{}});
    fields.push_back({.name = "Apply Initial Margin",
                      .value = v.apply_initial_margin ?
                                   (*v.apply_initial_margin ? "true" : "false") :
                                   std::string{}});
    fields.push_back(
        {.name = "Initial Margin Type", .value = v.initial_margin_type.value_or(std::string{})});
    fields.push_back({.name = "Calculate Im Amount",
                      .value = v.calculate_im_amount ? (*v.calculate_im_amount ? "true" : "false") :
                                                       std::string{}});
    fields.push_back({.name = "Calculate Vm Amount",
                      .value = v.calculate_vm_amount ? (*v.calculate_vm_amount ? "true" : "false") :
                                                       std::string{}});
    fields.push_back({.name = "Non Exempt Im Regulations",
                      .value = v.non_exempt_im_regulations.value_or(std::string{})});
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
