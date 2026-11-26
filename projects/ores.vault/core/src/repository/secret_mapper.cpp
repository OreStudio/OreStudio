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
#include "ores.vault.core/repository/secret_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.vault.api/domain/secret.hpp"
#include "ores.vault.api/domain/secret_json_io.hpp" // IWYU pragma: keep.
#include "ores.vault.core/repository/secret_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <vector>

namespace ores::vault::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::secret secret_mapper::map(const secret_entity& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::secret r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.name = v.name;
    r.owner = v.owner;
    r.sealed_value = v.sealed_value;
    r.key_version = v.key_version;
    r.description = v.description.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    ORES_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

secret_entity secret_mapper::map(const domain::secret& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    secret_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.name = v.name;
    r.owner = v.owner;
    r.sealed_value = v.sealed_value;
    r.key_version = v.key_version;
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    ORES_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::secret> secret_mapper::map(const std::vector<secret_entity>& v) {
    return map_vector<secret_entity, domain::secret>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<secret_entity> secret_mapper::map(const std::vector<domain::secret>& v) {
    return map_vector<domain::secret, secret_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
