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
#include "ores.trading.core/presentation/trade_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value> render_trade_fields(const domain::trade& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.identity.id)});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.identity.party_id)});
    fields.push_back({.name = "External ID", .value = v.identity.external_id});
    fields.push_back({.name = "Book ID", .value = boost::uuids::to_string(v.parties.book_id)});
    fields.push_back(
        {.name = "Portfolio ID", .value = boost::uuids::to_string(v.parties.portfolio_id)});
    fields.push_back({.name = "Successor Trade ID",
                      .value = v.parties.successor_trade_id ?
                                   boost::uuids::to_string(*v.parties.successor_trade_id) :
                                   std::string{}});
    fields.push_back({.name = "Trade Type", .value = v.classification.trade_type});
    fields.push_back({.name = "Counterparty ID",
                      .value = v.parties.counterparty_id ?
                                   boost::uuids::to_string(*v.parties.counterparty_id) :
                                   std::string{}});
    fields.push_back(
        {.name = "Product Type", .value = rfl::enum_to_string(v.classification.product_type)});
    fields.push_back({.name = "Instrument ID",
                      .value = v.classification.instrument_id ?
                                   boost::uuids::to_string(*v.classification.instrument_id) :
                                   std::string{}});
    fields.push_back(
        {.name = "Asset Class", .value = v.classification.asset_class.value_or(std::string{})});
    fields.push_back({.name = "Netting Set ID", .value = v.classification.netting_set_id});
    fields.push_back({.name = "Activity Type Code", .value = v.classification.activity_type_code});
    fields.push_back(
        {.name = "Status ID", .value = boost::uuids::to_string(v.classification.status_id)});
    fields.push_back(
        {.name = "Trade Date", .value = v.lifecycle.trade_date.value_or(std::string{})});
    fields.push_back({.name = "Execution Timestamp",
                      .value = v.lifecycle.execution_timestamp.value_or(std::string{})});
    fields.push_back(
        {.name = "Effective Date", .value = v.lifecycle.effective_date.value_or(std::string{})});
    fields.push_back({.name = "Termination Date",
                      .value = v.lifecycle.termination_date.value_or(std::string{})});
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
