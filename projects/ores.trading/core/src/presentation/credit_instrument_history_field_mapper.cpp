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
#include "ores.trading.core/presentation/credit_instrument_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/credit_instrument.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value>
render_credit_instrument_fields(const domain::credit_instrument& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Trade ID", .value = boost::uuids::to_string(v.identity.trade_id)});
    fields.push_back({.name = "Trade Type Code", .value = v.identity.trade_type_code});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.identity.party_id)});
    fields.push_back({.name = "Reference Entity", .value = v.reference_entity});
    fields.push_back({.name = "Currency", .value = v.currency});
    fields.push_back({.name = "Notional", .value = v.notional.to_string()});
    fields.push_back({.name = "Spread", .value = std::to_string(v.spread)});
    fields.push_back({.name = "Recovery Rate", .value = std::to_string(v.recovery_rate)});
    fields.push_back({.name = "Tenor", .value = v.tenor});
    fields.push_back({.name = "Start Date",
                      .value = ores::platform::time::datetime::to_iso8601_date(v.start_date)});
    fields.push_back({.name = "Maturity Date",
                      .value = ores::platform::time::datetime::to_iso8601_date(v.maturity_date)});
    fields.push_back({.name = "Day Count Fraction Code", .value = v.day_count_fraction_code});
    fields.push_back({.name = "Payment Frequency Code", .value = v.payment_frequency_code});
    fields.push_back({.name = "Index Name", .value = v.index_name});
    fields.push_back({.name = "Index Series",
                      .value = v.index_series ? std::to_string(*v.index_series) : std::string{}});
    fields.push_back({.name = "Seniority", .value = v.seniority});
    fields.push_back({.name = "Restructuring", .value = v.restructuring});
    fields.push_back({.name = "Description", .value = v.description});
    fields.push_back({.name = "Option Type", .value = v.option_type});
    fields.push_back(
        {.name = "Option Expiry Date",
         .value = v.option_expiry_date ?
                      ores::platform::time::datetime::to_iso8601_date(*v.option_expiry_date) :
                      std::string{}});
    fields.push_back({.name = "Option Strike",
                      .value = v.option_strike ? std::to_string(*v.option_strike) : std::string{}});
    fields.push_back({.name = "Linked Asset Code", .value = v.linked_asset_code});
    fields.push_back(
        {.name = "Tranche Attachment",
         .value = v.tranche_attachment ? std::to_string(*v.tranche_attachment) : std::string{}});
    fields.push_back(
        {.name = "Tranche Detachment",
         .value = v.tranche_detachment ? std::to_string(*v.tranche_detachment) : std::string{}});
    using ores::history::domain::provenance_fields;
    fields.push_back({.name = provenance_fields::modified_by, .value = v.audit.modified_by});
    fields.push_back({.name = provenance_fields::performed_by, .value = v.audit.performed_by});
    fields.push_back(
        {.name = provenance_fields::change_reason_code, .value = v.audit.change_reason_code});
    fields.push_back(
        {.name = provenance_fields::change_commentary, .value = v.audit.change_commentary});
    fields.push_back(
        {.name = provenance_fields::recorded_at,
         .value = ores::platform::time::datetime::to_iso8601_utc(v.audit.recorded_at)});

    return fields;
}

}
