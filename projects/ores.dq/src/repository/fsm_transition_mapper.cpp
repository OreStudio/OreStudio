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
#include "ores.dq/repository/fsm_transition_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"

namespace ores::dq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::fsm_transition
fsm_transition_mapper::map(const fsm_transition_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::fsm_transition r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.machine_id = boost::lexical_cast<boost::uuids::uuid>(v.machine_id);
    r.from_state_id = v.from_state_id.has_value()
        ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.from_state_id))
        : std::nullopt;
    r.to_state_id = boost::lexical_cast<boost::uuids::uuid>(v.to_state_id);
    r.name = v.name;
    r.guard_function = v.guard_function.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    if (!v.valid_from)
        throw std::logic_error("Cannot map entity with null valid_from to domain object.");
    r.recorded_at = timestamp_to_timepoint(*v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result name: " << r.name;
    return r;
}

fsm_transition_entity
fsm_transition_mapper::map(const domain::fsm_transition& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v.name;

    fsm_transition_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.machine_id = boost::uuids::to_string(v.machine_id);
    r.from_state_id = v.from_state_id.has_value()
        ? std::optional(boost::uuids::to_string(*v.from_state_id))
        : std::nullopt;
    r.to_state_id = boost::uuids::to_string(v.to_state_id);
    r.name = v.name;
    r.guard_function = v.guard_function.empty()
        ? std::nullopt : std::optional(v.guard_function);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result name: " << r.name;
    return r;
}

std::vector<domain::fsm_transition>
fsm_transition_mapper::map(const std::vector<fsm_transition_entity>& v) {
    return map_vector<fsm_transition_entity, domain::fsm_transition>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<fsm_transition_entity>
fsm_transition_mapper::map(const std::vector<domain::fsm_transition>& v) {
    return map_vector<domain::fsm_transition, fsm_transition_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
