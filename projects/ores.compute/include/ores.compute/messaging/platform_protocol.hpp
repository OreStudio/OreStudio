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
#ifndef ORES_COMPUTE_MESSAGING_PLATFORM_PROTOCOL_HPP
#define ORES_COMPUTE_MESSAGING_PLATFORM_PROTOCOL_HPP

#include <vector>
#include <string_view>
#include "ores.compute/domain/compute_platform.hpp"

namespace ores::compute::messaging {

struct list_platforms_request {
    using response_type = struct list_platforms_response;
    static constexpr std::string_view nats_subject =
        "compute.v1.platforms.list";
};

struct list_platforms_response {
    std::vector<ores::compute::domain::compute_platform> platforms;
    bool success = false;
    std::string message;
};

}

#endif
