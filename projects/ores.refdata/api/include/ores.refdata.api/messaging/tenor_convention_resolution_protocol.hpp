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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_API_MESSAGING_TENOR_CONVENTION_RESOLUTION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_TENOR_CONVENTION_RESOLUTION_PROTOCOL_HPP

#include "ores.refdata.api/domain/tenor_convention_resolution.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief The tenor convention resolution row enriched with the joined row's
 * display fields, so a screen needs one request for the whole set rather
 * than one per row. The by-side read returns this view.
 */
struct tenor_convention_resolution_view {
    ores::refdata::domain::tenor_convention_resolution tenor_convention_resolution;
};

struct get_tenor_convention_resolutions_request {
    using response_type = struct get_tenor_convention_resolutions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_convention_resolutions.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_tenor_convention_resolutions_response {
    std::vector<ores::refdata::domain::tenor_convention_resolution> tenor_convention_resolutions;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct get_tenor_convention_resolutions_by_convention_request {
    using response_type = struct get_tenor_convention_resolutions_by_convention_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_convention_resolutions.list_by_convention_code";
    std::string convention_code;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_tenor_convention_resolutions_by_convention_response {
    std::vector<tenor_convention_resolution_view> tenor_convention_resolutions;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct count_tenor_convention_resolutions_by_convention_request {
    using response_type = struct count_tenor_convention_resolutions_by_convention_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_convention_resolutions.count_by_convention_code";
    std::string convention_code;
};

struct count_tenor_convention_resolutions_by_convention_response {
    int total_available_count = 0;
};

struct count_tenor_convention_resolutions_by_tenor_request {
    using response_type = struct count_tenor_convention_resolutions_by_tenor_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_convention_resolutions.count_by_tenor_code";
    std::string tenor_code;
};

struct count_tenor_convention_resolutions_by_tenor_response {
    int total_available_count = 0;
};
}

#endif
