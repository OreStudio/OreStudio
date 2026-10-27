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
#include "ores.refdata.core/repository/tenor_basis_swap_pay_receive_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_pay_receive.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_pay_receive_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/tenor_basis_swap_pay_receive_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::tenor_basis_swap_pay_receive
tenor_basis_swap_pay_receive_mapper::map(const tenor_basis_swap_pay_receive_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::tenor_basis_swap_pay_receive r;
    r.identity.version = v.version;
    r.identity.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.identity.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.identity.convention_id = boost::lexical_cast<boost::uuids::uuid>(v.convention_id);
    r.identity.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.pay_leg.pay_index = v.pay_index;
    r.pay_leg.pay_frequency = v.pay_frequency;
    r.rec_leg.receive_index = v.receive_index;
    r.rec_leg.receive_frequency = v.receive_frequency;
    r.coupon.spread_on_rec = v.spread_on_rec;
    r.coupon.include_spread = v.include_spread;
    r.coupon.sub_periods_coupon_type = v.sub_periods_coupon_type;
    r.pay_leg.pay_is_averaged = v.pay_is_averaged;
    r.rec_leg.rec_is_averaged = v.rec_is_averaged;
    r.audit.modified_by = v.modified_by;
    r.audit.performed_by = v.performed_by;
    r.audit.change_reason_code = v.change_reason_code;
    r.audit.change_commentary = v.change_commentary;
    r.audit.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

tenor_basis_swap_pay_receive_entity
tenor_basis_swap_pay_receive_mapper::map(const domain::tenor_basis_swap_pay_receive& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    tenor_basis_swap_pay_receive_entity r;
    r.id = boost::uuids::to_string(v.identity.id);
    r.tenant_id = v.identity.tenant_id.to_string();
    r.version = v.identity.version;
    r.convention_id = boost::uuids::to_string(v.identity.convention_id);
    r.party_id = boost::uuids::to_string(v.identity.party_id);
    r.pay_index = v.pay_leg.pay_index;
    r.pay_frequency = v.pay_leg.pay_frequency;
    r.receive_index = v.rec_leg.receive_index;
    r.receive_frequency = v.rec_leg.receive_frequency;
    r.spread_on_rec = v.coupon.spread_on_rec;
    r.include_spread = v.coupon.include_spread;
    r.sub_periods_coupon_type = v.coupon.sub_periods_coupon_type;
    r.pay_is_averaged = v.pay_leg.pay_is_averaged;
    r.rec_is_averaged = v.rec_leg.rec_is_averaged;
    r.modified_by = v.audit.modified_by;
    r.performed_by = v.audit.performed_by;
    r.change_reason_code = v.audit.change_reason_code;
    r.change_commentary = v.audit.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::tenor_basis_swap_pay_receive> tenor_basis_swap_pay_receive_mapper::map(
    const std::vector<tenor_basis_swap_pay_receive_entity>& v) {
    return map_vector<tenor_basis_swap_pay_receive_entity, domain::tenor_basis_swap_pay_receive>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<tenor_basis_swap_pay_receive_entity> tenor_basis_swap_pay_receive_mapper::map(
    const std::vector<domain::tenor_basis_swap_pay_receive>& v) {
    return map_vector<domain::tenor_basis_swap_pay_receive, tenor_basis_swap_pay_receive_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
