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
#include "ores.trading.core/repository/script_twin_input_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/script_twin_input.hpp"
#include "ores.trading.api/domain/script_twin_input_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/script_twin_input_entity.hpp"
#include <string>
#include <vector>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::script_twin_input script_twin_input_mapper::map(const script_twin_input_entity& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::script_twin_input r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.trade_type_code = v.trade_type_code;
    r.sequence_number = v.sequence_number;
    r.script_name = v.script_name;
    r.input_name = v.input_name;
    r.data_type = v.data_type;
    r.is_array = v.is_array;
    r.source = v.source;
    r.transform = v.transform;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    ORES_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

script_twin_input_entity script_twin_input_mapper::map(const domain::script_twin_input& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    script_twin_input_entity r;
    r.trade_type_code = v.trade_type_code;
    r.sequence_number = v.sequence_number;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.script_name = v.script_name;
    r.input_name = v.input_name;
    r.data_type = v.data_type;
    r.is_array = v.is_array;
    r.source = v.source;
    r.transform = v.transform;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    ORES_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::script_twin_input>
script_twin_input_mapper::map(const std::vector<script_twin_input_entity>& v) {
    return map_vector<script_twin_input_entity, domain::script_twin_input>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<script_twin_input_entity>
script_twin_input_mapper::map(const std::vector<domain::script_twin_input>& v) {
    return map_vector<domain::script_twin_input, script_twin_input_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
