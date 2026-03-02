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
#include "ores.trading/repository/activity_type_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading/domain/activity_type_json_io.hpp" // IWYU pragma: keep.

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::activity_type
activity_type_mapper::map(const activity_type_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::activity_type r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.code = v.code.value();
    r.category = v.category;
    r.requires_confirmation = v.requires_confirmation;
    r.description = v.description.value_or("");
    r.fpml_event_type_code = v.fpml_event_type_code.value_or("");
    r.fsm_transition_id = v.fsm_transition_id.has_value()
        ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.fsm_transition_id))
        : std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    if (!v.valid_from)
        throw std::logic_error("Cannot map entity with null valid_from to domain object.");
    r.recorded_at = timestamp_to_timepoint(*v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

activity_type_entity
activity_type_mapper::map(const domain::activity_type& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    activity_type_entity r;
    r.code = v.code;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.category = v.category;
    r.requires_confirmation = v.requires_confirmation;
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.fpml_event_type_code = v.fpml_event_type_code.empty()
        ? std::nullopt : std::optional(v.fpml_event_type_code);
    r.fsm_transition_id = v.fsm_transition_id.has_value()
        ? std::optional(boost::uuids::to_string(*v.fsm_transition_id))
        : std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::activity_type>
activity_type_mapper::map(const std::vector<activity_type_entity>& v) {
    return map_vector<activity_type_entity, domain::activity_type>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<activity_type_entity>
activity_type_mapper::map(const std::vector<domain::activity_type>& v) {
    return map_vector<domain::activity_type, activity_type_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
