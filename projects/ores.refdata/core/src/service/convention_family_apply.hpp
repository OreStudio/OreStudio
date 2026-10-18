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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_REFDATA_CORE_SERVICE_CONVENTION_FAMILY_APPLY_HPP
#define ORES_REFDATA_CORE_SERVICE_CONVENTION_FAMILY_APPLY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.refdata.core/export.hpp"
#include <string>

namespace ores::refdata::messaging {
struct conventions_document;
}

namespace ores::refdata::service {

struct conventions_save_result;

// Declarations for the generated bodies in convention_family_apply.inc, which
// are owned by convention_family_apply.cpp. The generator emits the bodies; if
// a signature here stops matching, the link fails, which is the point.

void apply_party_families(const ores::database::context& ctx, messaging::conventions_document& v);
void apply_families_by_id(const ores::database::context& ctx, messaging::conventions_document& v);
void apply_world_families(const ores::database::context& ctx,
                          messaging::conventions_document& v,
                          conventions_save_result& result);

void read_party_families(const ores::database::context& ctx, messaging::conventions_document& v);
void read_families_by_id(const ores::database::context& ctx, messaging::conventions_document& v);
/**
 * @brief Reads one family's live rows as the payloads the wire carries them in.
 *
 * The bytes are the bytes a stage message carries, so an in-process caller and
 * a remote caller see a family the same way. The generated dispatch gives each
 * family its own branch.
 *
 * @throws std::logic_error when the registry does not know the family.
 */
ORES_REFDATA_CORE_EXPORT std::vector<std::string>
read_family_payloads(const ores::database::context& ctx, const std::string& family);

void read_world_families(const ores::database::context& ctx, messaging::conventions_document& v);

bool apply_staged_family(const ores::database::context& ctx,
                         const std::string& family,
                         const std::string& payload,
                         conventions_save_result& result);

}

#endif
