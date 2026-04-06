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
#include "ores.workflow/service/workflow_registry.hpp"

namespace ores::workflow::service {

void workflow_registry::register_definition(workflow_definition def) {
    definitions_.insert_or_assign(def.type_name, std::move(def));
}

const workflow_definition*
workflow_registry::find(const std::string& type_name) const {
    const auto it = definitions_.find(type_name);
    return it != definitions_.end() ? &it->second : nullptr;
}

}
