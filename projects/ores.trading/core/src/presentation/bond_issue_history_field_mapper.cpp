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
#include "ores.trading.core/presentation/bond_issue_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value> render_bond_issue_fields(const domain::bond_issue& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Issue ID", .value = boost::uuids::to_string(v.issue_id)});
    fields.push_back({.name = "Security ID", .value = v.security_id});
    fields.push_back({.name = "Issuer", .value = v.issuer});
    fields.push_back({.name = "Currency", .value = v.currency});
    fields.push_back({.name = "Face Value", .value = std::to_string(v.face_value)});
    fields.push_back({.name = "Coupon Rate", .value = std::to_string(v.coupon_rate)});
    fields.push_back({.name = "Coupon Frequency Code", .value = v.coupon_frequency_code});
    fields.push_back({.name = "Day Count Code", .value = v.day_count_code});
    fields.push_back({.name = "Issue Date", .value = v.issue_date});
    fields.push_back({.name = "Settlement Days", .value = std::to_string(v.settlement_days)});
    fields.push_back({.name = "Calendar", .value = v.calendar.value_or(std::string{})});
    fields.push_back(
        {.name = "Credit Curve ID", .value = v.credit_curve_id.value_or(std::string{})});
    fields.push_back(
        {.name = "Reference Curve ID", .value = v.reference_curve_id.value_or(std::string{})});
    fields.push_back(
        {.name = "Income Curve ID", .value = v.income_curve_id.value_or(std::string{})});
    fields.push_back({.name = "Bond Notional", .value = v.bond_notional.value_or(std::string{})});
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
