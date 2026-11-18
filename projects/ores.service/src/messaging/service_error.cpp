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
#include "ores.service/messaging/service_error.hpp"
#include <string>
#include <string_view>
#include <utility>

namespace ores::service::messaging {

/*
 * Defined here rather than in the header, so the shared library owns the vtable
 * and exports the members its consumers import. A class that is exported and
 * whose members are all inline is emitted only in a translation unit that
 * includes its header; without this one the DLL exported nothing and every
 * Windows link failed on service_error.
 */
service_error::service_error(std::string error_code, std::string_view subject)
    : std::runtime_error("Request to " + std::string(subject) + " rejected by server: " +
                         error_code)
    , error_code_(std::move(error_code)) {}

service_error::~service_error() = default;

}
