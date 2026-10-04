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
#include "ores.trading.core/repository/bond_issue_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/bond_issue_json_io.hpp" // IWYU pragma: keep.
#include "ores.utility/decimal/decimal.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::bond_issue bond_issue_mapper::map(const bond_issue_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::bond_issue r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.issue_id = boost::lexical_cast<boost::uuids::uuid>(v.issue_id.value());
    r.security_id = v.security_id;
    r.issuer = v.issuer.value_or("");
    r.face_value =
        v.face_value.has_value() ?
            std::optional(ores::utility::decimal::decimal::from_string(*v.face_value).value()) :
            std::nullopt;
    r.issue_date =
        v.issue_date.has_value() ?
            std::optional(ores::platform::time::datetime::from_iso8601_date(*v.issue_date)) :
            std::nullopt;
    r.settlement_days = v.settlement_days.value_or(0);
    r.calendar = v.calendar;
    r.credit_curve_id = v.credit_curve_id;
    r.reference_curve_id = v.reference_curve_id;
    r.income_curve_id = v.income_curve_id;
    r.credit_group = v.credit_group;
    r.volatility_curve_id = v.volatility_curve_id;
    r.price_quote_method = v.price_quote_method;
    r.price_quote_base_value = v.price_quote_base_value;
    r.sub_type = v.sub_type;
    r.price_type = v.price_type;
    r.payer = v.payer;
    r.credit_risk = v.credit_risk;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

bond_issue_entity bond_issue_mapper::map(const domain::bond_issue& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    bond_issue_entity r;
    r.issue_id = boost::uuids::to_string(v.issue_id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.security_id = v.security_id;
    r.issuer = v.issuer.empty() ? std::nullopt : std::optional(v.issuer);
    r.face_value =
        v.face_value.has_value() ? std::optional(v.face_value->to_string()) : std::nullopt;
    r.issue_date =
        v.issue_date.has_value() ?
            std::optional(ores::platform::time::datetime::to_iso8601_date(*v.issue_date)) :
            std::nullopt;
    r.settlement_days = v.settlement_days == 0 ? std::nullopt : std::optional(v.settlement_days);
    r.calendar = v.calendar;
    r.credit_curve_id = v.credit_curve_id;
    r.reference_curve_id = v.reference_curve_id;
    r.income_curve_id = v.income_curve_id;
    r.credit_group = v.credit_group;
    r.volatility_curve_id = v.volatility_curve_id;
    r.price_quote_method = v.price_quote_method;
    r.price_quote_base_value = v.price_quote_base_value;
    r.sub_type = v.sub_type;
    r.price_type = v.price_type;
    r.payer = v.payer;
    r.credit_risk = v.credit_risk;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::bond_issue> bond_issue_mapper::map(const std::vector<bond_issue_entity>& v) {
    return map_vector<bond_issue_entity, domain::bond_issue>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<bond_issue_entity> bond_issue_mapper::map(const std::vector<domain::bond_issue>& v) {
    return map_vector<domain::bond_issue, bond_issue_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
