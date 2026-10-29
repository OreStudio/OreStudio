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
#include "ores.refdata.core/repository/tenor_basis_swap_long_short_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_long_short.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_long_short_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/tenor_basis_swap_long_short_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::tenor_basis_swap_long_short
tenor_basis_swap_long_short_mapper::map(const tenor_basis_swap_long_short_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::tenor_basis_swap_long_short r;
    r.identity.version = v.version;
    r.identity.tenant_id = v.tenant_id;
    r.identity.id = v.id;
    r.identity.convention_id = v.convention_id;
    r.identity.party_id = v.party_id;
    r.long_leg.long_index = v.long_index;
    r.long_leg.long_pay_tenor = v.long_pay_tenor;
    r.short_leg.short_index = v.short_index;
    r.short_leg.short_pay_tenor = v.short_pay_tenor;
    r.short_leg.spread_on_short = v.spread_on_short;
    r.coupon.include_spread = v.include_spread;
    r.coupon.sub_periods_coupon_type = v.sub_periods_coupon_type;
    r.audit.modified_by = v.modified_by;
    r.audit.performed_by = v.performed_by;
    r.audit.change_reason_code = v.change_reason_code;
    r.audit.change_commentary = v.change_commentary;
    r.audit.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

tenor_basis_swap_long_short_entity
tenor_basis_swap_long_short_mapper::map(const domain::tenor_basis_swap_long_short& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    tenor_basis_swap_long_short_entity r;
    r.id = v.identity.id;
    r.tenant_id = v.identity.tenant_id;
    r.version = v.identity.version;
    r.convention_id = v.identity.convention_id;
    r.party_id = v.identity.party_id;
    r.long_index = v.long_leg.long_index;
    r.long_pay_tenor = v.long_leg.long_pay_tenor;
    r.short_index = v.short_leg.short_index;
    r.short_pay_tenor = v.short_leg.short_pay_tenor;
    r.spread_on_short = v.short_leg.spread_on_short;
    r.include_spread = v.coupon.include_spread;
    r.sub_periods_coupon_type = v.coupon.sub_periods_coupon_type;
    r.modified_by = v.audit.modified_by;
    r.performed_by = v.audit.performed_by;
    r.change_reason_code = v.audit.change_reason_code;
    r.change_commentary = v.audit.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_mapper::map(const std::vector<tenor_basis_swap_long_short_entity>& v) {
    return map_vector<tenor_basis_swap_long_short_entity, domain::tenor_basis_swap_long_short>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<tenor_basis_swap_long_short_entity>
tenor_basis_swap_long_short_mapper::map(const std::vector<domain::tenor_basis_swap_long_short>& v) {
    return map_vector<domain::tenor_basis_swap_long_short, tenor_basis_swap_long_short_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
