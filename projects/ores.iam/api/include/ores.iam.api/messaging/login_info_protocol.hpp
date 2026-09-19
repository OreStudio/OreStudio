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
#ifndef ORES_IAM_API_MESSAGING_LOGIN_INFO_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_LOGIN_INFO_PROTOCOL_HPP

#include "ores.iam.api/domain/login_info.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct get_login_info_request {
    using response_type = struct get_login_info_response;
    static constexpr std::string_view nats_subject = "iam.v1.login_info.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_login_info_response {
    std::vector<ores::iam::domain::login_info> login_info;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_login_info_request {
    using response_type = struct save_login_info_response;
    static constexpr std::string_view nats_subject = "iam.v1.login_info.save";
    ores::iam::domain::login_info data;

    static save_login_info_request from(ores::iam::domain::login_info v) {
        return {.data = std::move(v)};
    }
};

struct save_login_info_response {
    bool success = false;
    std::string message;
};

struct delete_login_info_request {
    using response_type = struct delete_login_info_response;
    static constexpr std::string_view nats_subject = "iam.v1.login_info.delete";
    std::vector<std::string> ids;
};

struct delete_login_info_response {
    bool success = false;
    std::string message;
};


}

#endif
