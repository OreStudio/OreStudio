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
#include "ores.reporting.core/repository/cut_approval_detail_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/domain/cut_approval_detail.hpp"
#include "ores.reporting.api/domain/cut_approval_detail_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.core/repository/cut_approval_detail_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <vector>

namespace ores::reporting::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::cut_approval_detail cut_approval_detail_mapper::map(const cut_approval_detail_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::cut_approval_detail r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.request_id = v.request_id;
    r.party_id = v.party_id;
    r.cut_id = v.cut_id;
    r.deal_count = v.deal_count;
    r.market_data_set_count = v.market_data_set_count;
    r.gap_count = v.gap_count;
    r.applied_by = v.applied_by;
    r.applied_at = v.applied_at;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

cut_approval_detail_entity cut_approval_detail_mapper::map(const domain::cut_approval_detail& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    cut_approval_detail_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.request_id = v.request_id;
    r.party_id = v.party_id;
    r.cut_id = v.cut_id;
    r.deal_count = v.deal_count;
    r.market_data_set_count = v.market_data_set_count;
    r.gap_count = v.gap_count;
    r.applied_by = v.applied_by;
    r.applied_at = v.applied_at;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::cut_approval_detail>
cut_approval_detail_mapper::map(const std::vector<cut_approval_detail_entity>& v) {
    return map_vector<cut_approval_detail_entity, domain::cut_approval_detail>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<cut_approval_detail_entity>
cut_approval_detail_mapper::map(const std::vector<domain::cut_approval_detail>& v) {
    return map_vector<domain::cut_approval_detail, cut_approval_detail_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
