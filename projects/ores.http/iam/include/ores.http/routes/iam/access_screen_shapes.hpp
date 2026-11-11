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
#ifndef ORES_HTTP_ROUTES_IAM_ACCESS_SCREEN_SHAPES_HPP
#define ORES_HTTP_ROUTES_IAM_ACCESS_SCREEN_SHAPES_HPP

#include "ores.iam.api/messaging/authorization_protocol.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

/**
 * @brief The shapes the access screens send and read, as the BFF stated them.
 *
 * The access screens name every field in camelCase, where IAM's protocol names
 * them in snake_case, so each answer is restated in the screen's own shape and
 * each body is read in it. The shapes are one vocabulary across the access
 * routes, and live here once.
 */
namespace ores::http::routes::iam {

struct held_role final {
    std::string roleId;
    std::string name;
    std::string description;
    std::vector<std::string> permissionCodes;
    std::string givenBy;
    std::chrono::system_clock::time_point givenAt;
    std::string reasonCode;
    std::string commentary;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<held_role>) {
    using ores::utility::codec::field;
    return std::tuple{field{"roleId", &held_role::roleId},
                      field{"name", &held_role::name},
                      field{"description", &held_role::description},
                      field{"permissionCodes", &held_role::permissionCodes},
                      field{"givenBy", &held_role::givenBy},
                      field{"givenAt", &held_role::givenAt},
                      field{"reasonCode", &held_role::reasonCode},
                      field{"commentary", &held_role::commentary}};
}

struct access_view final {
    std::vector<held_role> roles;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<access_view>) {
    using ores::utility::codec::field;
    return std::tuple{field{"roles", &access_view::roles}};
}

struct permission_page_view final {
    std::string area;
    std::vector<ores::iam::messaging::permission_resource_row> rows;
    int totalCount = 0;
    std::vector<ores::iam::messaging::permission_area_count> areas;
    bool areaWhole = false;
    bool everything = false;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<permission_page_view>) {
    using ores::utility::codec::field;
    return std::tuple{field{"area", &permission_page_view::area},
                      field{"rows", &permission_page_view::rows},
                      field{"totalCount", &permission_page_view::totalCount},
                      field{"areas", &permission_page_view::areas},
                      field{"areaWhole", &permission_page_view::areaWhole},
                      field{"everything", &permission_page_view::everything}};
}

struct role_row_view final {
    std::string id;
    int version = 0;
    std::string name;
    std::string description;
    bool service = false;
    bool registrationDefault = false;
    bool requestable = false;
    int permissionCount = 0;
    bool everything = false;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_row_view>) {
    using ores::utility::codec::field;
    return std::tuple{field{"id", &role_row_view::id},
                      field{"version", &role_row_view::version},
                      field{"name", &role_row_view::name},
                      field{"description", &role_row_view::description},
                      field{"service", &role_row_view::service},
                      field{"registrationDefault", &role_row_view::registrationDefault},
                      field{"requestable", &role_row_view::requestable},
                      field{"permissionCount", &role_row_view::permissionCount},
                      field{"everything", &role_row_view::everything}};
}

struct role_page_view final {
    std::vector<role_row_view> roles;
    int totalCount = 0;
    int serviceHidden = 0;
    std::vector<ores::iam::messaging::permission_area_count> areas;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_page_view>) {
    using ores::utility::codec::field;
    return std::tuple{field{"roles", &role_page_view::roles},
                      field{"totalCount", &role_page_view::totalCount},
                      field{"serviceHidden", &role_page_view::serviceHidden},
                      field{"areas", &role_page_view::areas}};
}

struct default_party_body final {
    std::string partyId;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<default_party_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"partyId", &default_party_body::partyId}};
}


}

#endif
