/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.utility/log/lifecycle_manager.hpp"
#include "ores.utility/log/scoped_lifecycle_manager.hpp"

namespace ores::utility::log {

scoped_lifecycle_manager::scoped_lifecycle_manager()
    : is_initialised_(false) {}

scoped_lifecycle_manager::
scoped_lifecycle_manager(const std::optional<logging_configuration>& ocfg) {
    initialise(ocfg);
}

scoped_lifecycle_manager::~scoped_lifecycle_manager() {
    if (is_initialised_)
        lifecycle_manager::shutdown();
}

void scoped_lifecycle_manager::
initialise(const std::optional<logging_configuration>& ocfg) {
    is_initialised_ = true;
    lifecycle_manager::initialise(ocfg);
}

bool scoped_lifecycle_manager::is_initialised() const {
    return is_initialised_;
}

}
