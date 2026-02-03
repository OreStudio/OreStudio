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
#include "ores.dq/repository/change_reason_mapper.hpp"

#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.dq/domain/change_reason_json_io.hpp" // IWYU pragma: keep.

namespace ores::dq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::change_reason
change_reason_mapper::map(const change_reason_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::change_reason r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.code = v.code.value();
    r.description = v.description;
    r.category_code = v.category_code;
    r.applies_to_amend = v.applies_to_amend;
    r.applies_to_delete = v.applies_to_delete;
    r.requires_commentary = v.requires_commentary;
    r.display_order = v.display_order;
    r.recorded_by = v.modified_by;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

change_reason_entity
change_reason_mapper::map(const domain::change_reason& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    change_reason_entity r;
    r.code = v.code;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.description = v.description;
    r.category_code = v.category_code;
    r.applies_to_amend = v.applies_to_amend;
    r.applies_to_delete = v.applies_to_delete;
    r.requires_commentary = v.requires_commentary;
    r.display_order = v.display_order;
    r.modified_by = v.recorded_by;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::change_reason>
change_reason_mapper::map(const std::vector<change_reason_entity>& v) {
    return map_vector<change_reason_entity, domain::change_reason>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<change_reason_entity>
change_reason_mapper::map(const std::vector<domain::change_reason>& v) {
    return map_vector<domain::change_reason, change_reason_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
