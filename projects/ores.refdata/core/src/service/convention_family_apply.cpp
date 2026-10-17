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
#include "ores.database/domain/context.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.api/domain/convention_family_registry.hpp"
#include "ores.refdata.core/service/conventions_document_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "convention_family_apply.hpp"
#include "conventions_row_helpers.hpp"
#include <rfl/json.hpp>
// One repository per family the generated apply fragment calls.
#include "convention_family_apply_includes.inc"
#include <map>
#include <utility>
#include <vector>

namespace ores::refdata::service {

using namespace ores::refdata::repository;

// The generated dispatch owns this translation unit so the exported
// apply_staged_family does not depend on which service happens to include the
// fragment. The helpers the fragment calls are included above it, outside this
// namespace, because they name their own fully qualified types.
#include "convention_family_apply.inc"

}
