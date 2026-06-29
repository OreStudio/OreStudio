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
#ifndef ORES_SYNTHETIC_API_MESSAGING_GMM_COMPONENT_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_GMM_COMPONENT_PROTOCOL_HPP

#include "ores.synthetic.api/domain/gmm_component.hpp"
#include <string>
#include <vector>

namespace ores::synthetic::messaging {

struct get_gmm_components_request {
    using response_type = struct get_gmm_components_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.gmm_components.list";
    int offset = 0;
    int limit = 100;
};

struct get_gmm_components_response {
    std::vector<ores::synthetic::domain::gmm_component> components;
    int total_available_count = 0;
};

struct save_gmm_component_request {
    using response_type = struct save_gmm_component_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.gmm_components.save";
    ores::synthetic::domain::gmm_component data;

    static save_gmm_component_request from(ores::synthetic::domain::gmm_component c) {
        return {.data = std::move(c)};
    }
};

struct save_gmm_component_response {
    bool success = false;
    std::string message;
};

struct delete_gmm_component_request {
    using response_type = struct delete_gmm_component_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.gmm_components.delete";
    std::vector<std::string> ids;
};

struct delete_gmm_component_response {
    bool success = false;
    std::string message;
};

}

#endif
