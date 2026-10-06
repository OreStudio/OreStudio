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
#include "ores.dq.core/repository/csa_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.dq.api/domain/csa.hpp"
#include "ores.dq.api/domain/csa_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/csa_entity.hpp"
#include "ores.logging/boost_severity.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <vector>

namespace ores::dq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::csa csa_mapper::map(const csa_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::csa r;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.netting_set_code = v.netting_set_code.value();
    r.is_active = v.is_active;
    r.bilateral = v.bilateral;
    r.csa_currency = v.csa_currency;
    r.index_name = v.index_name;
    r.threshold_pay = v.threshold_pay;
    r.threshold_receive = v.threshold_receive;
    r.minimum_transfer_amount_pay = v.minimum_transfer_amount_pay;
    r.minimum_transfer_amount_receive = v.minimum_transfer_amount_receive;
    r.independent_amount_held = v.independent_amount_held;
    r.independent_amount_type = v.independent_amount_type;
    r.call_frequency = v.call_frequency;
    r.post_frequency = v.post_frequency;
    r.margin_period_of_risk = v.margin_period_of_risk;
    r.collateral_compounding_spread_receive = v.collateral_compounding_spread_receive;
    r.collateral_compounding_spread_pay = v.collateral_compounding_spread_pay;
    r.apply_initial_margin = v.apply_initial_margin;
    r.initial_margin_type = v.initial_margin_type;
    r.calculate_im_amount = v.calculate_im_amount;
    r.calculate_vm_amount = v.calculate_vm_amount;
    r.non_exempt_im_regulations = v.non_exempt_im_regulations;
    r.eligible_currencies = v.eligible_currencies;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

csa_entity csa_mapper::map(const domain::csa& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    csa_entity r;
    r.netting_set_code = v.netting_set_code;
    r.tenant_id = v.tenant_id.to_string();
    r.is_active = v.is_active;
    r.bilateral = v.bilateral;
    r.csa_currency = v.csa_currency;
    r.index_name = v.index_name;
    r.threshold_pay = v.threshold_pay;
    r.threshold_receive = v.threshold_receive;
    r.minimum_transfer_amount_pay = v.minimum_transfer_amount_pay;
    r.minimum_transfer_amount_receive = v.minimum_transfer_amount_receive;
    r.independent_amount_held = v.independent_amount_held;
    r.independent_amount_type = v.independent_amount_type;
    r.call_frequency = v.call_frequency;
    r.post_frequency = v.post_frequency;
    r.margin_period_of_risk = v.margin_period_of_risk;
    r.collateral_compounding_spread_receive = v.collateral_compounding_spread_receive;
    r.collateral_compounding_spread_pay = v.collateral_compounding_spread_pay;
    r.apply_initial_margin = v.apply_initial_margin;
    r.initial_margin_type = v.initial_margin_type;
    r.calculate_im_amount = v.calculate_im_amount;
    r.calculate_vm_amount = v.calculate_vm_amount;
    r.non_exempt_im_regulations = v.non_exempt_im_regulations;
    r.eligible_currencies = v.eligible_currencies;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::csa> csa_mapper::map(const std::vector<csa_entity>& v) {
    return map_vector<csa_entity, domain::csa>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<csa_entity> csa_mapper::map(const std::vector<domain::csa>& v) {
    return map_vector<domain::csa, csa_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
