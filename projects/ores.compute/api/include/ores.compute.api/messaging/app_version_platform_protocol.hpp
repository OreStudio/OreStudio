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
#ifndef ORES_COMPUTE_API_MESSAGING_APP_VERSION_PLATFORM_PROTOCOL_HPP
#define ORES_COMPUTE_API_MESSAGING_APP_VERSION_PLATFORM_PROTOCOL_HPP

#include "ores.compute.api/domain/app_version_platform.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::compute::messaging {

struct get_app_version_platforms_by_app_version_request {
    using response_type = struct get_app_version_platforms_by_app_version_response;
    static constexpr std::string_view nats_subject =
        "compute.v1.app_version_platforms.list_by_app_version_id";
    std::string app_version_id;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_app_version_platforms_by_app_version_response {
    std::vector<ores::compute::domain::app_version_platform> app_version_platforms;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct replace_app_version_platforms_by_app_version_request {
    using response_type = struct replace_app_version_platforms_by_app_version_response;
    static constexpr std::string_view nats_subject =
        "compute.v1.app_version_platforms.replace_by_app_version_id";
    std::string app_version_id;
    std::vector<ores::compute::domain::app_version_platform> app_version_platforms;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
};

struct replace_app_version_platforms_by_app_version_response {
    bool success = false;
    std::string message;
};

/**
 * @brief Opens one pair in the relation, creating the row when the pair is
 * not already open and leaving it unchanged when it is.
 *
 * Link is the incremental verb: a screen toggling one association does not
 * rewrite the whole set to change it, and does not need to know whether the
 * pair already exists.
 */
struct link_app_version_platform_request {
    using response_type = struct link_app_version_platform_response;
    static constexpr std::string_view nats_subject =
        "compute.v1.app_version_platforms.link_app_version_platform";
    std::string app_version_id;
    std::string platform_id;
    std::string package_uri;
    std::string sha256;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
};

struct link_app_version_platform_response {
    bool success = false;
    std::string message;
};

/**
 * @brief Closes one pair in the relation, leaving every other pair of the
 * set untouched.
 */
struct unlink_app_version_platform_request {
    using response_type = struct unlink_app_version_platform_response;
    static constexpr std::string_view nats_subject =
        "compute.v1.app_version_platforms.unlink_app_version_platform";
    std::string app_version_id;
    std::string platform_id;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
};

struct unlink_app_version_platform_response {
    bool success = false;
    std::string message;
};

struct count_app_version_platforms_by_app_version_request {
    using response_type = struct count_app_version_platforms_by_app_version_response;
    static constexpr std::string_view nats_subject =
        "compute.v1.app_version_platforms.count_by_app_version_id";
    std::string app_version_id;
};

struct count_app_version_platforms_by_app_version_response {
    int total_available_count = 0;
};

struct count_app_version_platforms_by_platform_request {
    using response_type = struct count_app_version_platforms_by_platform_response;
    static constexpr std::string_view nats_subject =
        "compute.v1.app_version_platforms.count_by_platform_id";
    std::string platform_id;
};

struct count_app_version_platforms_by_platform_response {
    int total_available_count = 0;
};

/**
 * @brief The app version platform row enriched with the joined row's
 * display fields, so a screen needs one request for the whole set rather
 * than one per row.
 */
struct app_version_platform_view {
    ores::compute::domain::app_version_platform app_version_platform;
    std::string platform_code;
};
}

#endif
