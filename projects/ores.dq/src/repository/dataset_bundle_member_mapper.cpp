/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.dq/repository/dataset_bundle_member_mapper.hpp"

#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.dq/domain/dataset_bundle_member_json_io.hpp" // IWYU pragma: keep.

namespace ores::dq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::dataset_bundle_member
dataset_bundle_member_mapper::map(const dataset_bundle_member_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::dataset_bundle_member r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.bundle_code = v.bundle_code.value();
    r.dataset_code = v.dataset_code;
    r.display_order = v.display_order;
    r.optional = v.optional;
    r.recorded_by = v.modified_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

dataset_bundle_member_entity
dataset_bundle_member_mapper::map(const domain::dataset_bundle_member& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    dataset_bundle_member_entity r;
    r.bundle_code = v.bundle_code;
    r.tenant_id = v.tenant_id;
    r.dataset_code = v.dataset_code;
    r.version = v.version;
    r.display_order = v.display_order;
    r.optional = v.optional;
    r.modified_by = v.recorded_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::dataset_bundle_member>
dataset_bundle_member_mapper::map(const std::vector<dataset_bundle_member_entity>& v) {
    return map_vector<dataset_bundle_member_entity, domain::dataset_bundle_member>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<dataset_bundle_member_entity>
dataset_bundle_member_mapper::map(const std::vector<domain::dataset_bundle_member>& v) {
    return map_vector<domain::dataset_bundle_member, dataset_bundle_member_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
