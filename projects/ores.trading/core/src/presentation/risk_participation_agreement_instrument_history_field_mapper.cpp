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
#include "ores.trading.core/presentation/risk_participation_agreement_instrument_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/risk_participation_agreement_instrument.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value> render_risk_participation_agreement_instrument_fields(
    const domain::risk_participation_agreement_instrument& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Trade ID", .value = boost::uuids::to_string(v.trade_id)});
    fields.push_back(
        {.name = "Trade Activity ID", .value = boost::uuids::to_string(v.trade_activity_id)});
    fields.push_back({.name = "Credit Curve ID", .value = v.credit_curve_id});
    fields.push_back({.name = "Issuer ID", .value = v.issuer_id.value_or(std::string{})});
    fields.push_back({.name = "Recovery Rate",
                      .value = v.recovery_rate ? std::to_string(*v.recovery_rate) : std::string{}});
    fields.push_back({.name = "Participation Rate", .value = std::to_string(v.participation_rate)});
    fields.push_back(
        {.name = "Settles Accrual",
         .value = v.settles_accrual ? (*v.settles_accrual ? "true" : "false") : std::string{}});
    fields.push_back(
        {.name = "Naked Option",
         .value = v.naked_option ? (*v.naked_option ? "true" : "false") : std::string{}});
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
