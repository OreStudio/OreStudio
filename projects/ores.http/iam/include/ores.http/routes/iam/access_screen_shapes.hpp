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
#include <cstdint>
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


/**
 * @brief A role given to an account: the role, the reason and a note.
 */
struct give_role_body final {
    std::string roleId;
    std::string reasonCode;
    std::optional<std::string> note;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<give_role_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"roleId", &give_role_body::roleId},
                      field{"reasonCode", &give_role_body::reasonCode},
                      field{"note", &give_role_body::note}};
}

/**
 * @brief A role as the editor states it: a new one names itself, an edited one
 * also the version it was read at and its two flags.
 */
struct role_body final {
    std::string name;
    std::optional<std::string> description;
    std::optional<std::int64_t> version;
    std::optional<bool> registrationDefault;
    std::optional<bool> requestable;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"name", &role_body::name},
                      field{"description", &role_body::description},
                      field{"version", &role_body::version},
                      field{"registrationDefault", &role_body::registrationDefault},
                      field{"requestable", &role_body::requestable}};
}

/**
 * @brief Every permission a role grants, and why.
 */
struct permission_bundle_body final {
    std::vector<std::string> codes;
    std::optional<std::string> note;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<permission_bundle_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"codes", &permission_bundle_body::codes},
                      field{"note", &permission_bundle_body::note}};
}

/**
 * @brief Permissions to add to a role and to remove from it, and why.
 */
struct permission_changes_body final {
    std::optional<std::vector<std::string>> add;
    std::optional<std::vector<std::string>> remove;
    std::string note;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<permission_changes_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"add", &permission_changes_body::add},
                      field{"remove", &permission_changes_body::remove},
                      field{"note", &permission_changes_body::note}};
}


/**
 * @brief A profile as the screen writes it; a field it leaves out is empty.
 */
struct profile_body final {
    std::optional<std::string> fullName;
    std::optional<std::string> jobTitle;
    std::optional<std::string> imageId;
    std::optional<std::string> reasonCode;
    std::optional<std::string> commentary;
    std::optional<std::string> expectedVersion;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<profile_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"fullName", &profile_body::fullName},
                      field{"jobTitle", &profile_body::jobTitle},
                      field{"imageId", &profile_body::imageId},
                      field{"reasonCode", &profile_body::reasonCode},
                      field{"commentary", &profile_body::commentary},
                      field{"expectedVersion", &profile_body::expectedVersion}};
}

/**
 * @brief A contact record as the screen writes it, with the version an
 * administrator's panel read, or none; a field it leaves out is empty.
 */
struct contact_body final {
    std::optional<std::string> streetLine1;
    std::optional<std::string> streetLine2;
    std::optional<std::string> city;
    std::optional<std::string> state;
    std::optional<std::string> countryCode;
    std::optional<std::string> postalCode;
    std::optional<std::string> phone;
    std::optional<std::string> email;
    std::optional<std::string> webPage;
    std::optional<std::string> reasonCode;
    std::optional<std::string> commentary;
    std::optional<std::int64_t> version;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<contact_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"streetLine1", &contact_body::streetLine1},
                      field{"streetLine2", &contact_body::streetLine2},
                      field{"city", &contact_body::city},
                      field{"state", &contact_body::state},
                      field{"countryCode", &contact_body::countryCode},
                      field{"postalCode", &contact_body::postalCode},
                      field{"phone", &contact_body::phone},
                      field{"email", &contact_body::email},
                      field{"webPage", &contact_body::webPage},
                      field{"reasonCode", &contact_body::reasonCode},
                      field{"commentary", &contact_body::commentary},
                      field{"version", &contact_body::version}};
}


/**
 * @brief Who one account reports to, or nothing to clear it, with the version
 * the screen read and why.
 */
struct reporting_line_body final {
    std::string reportsToAccountId;
    std::optional<std::string> expectedVersion;
    std::optional<std::string> reasonCode;
    std::optional<std::string> commentary;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<reporting_line_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"reportsToAccountId", &reporting_line_body::reportsToAccountId},
                      field{"expectedVersion", &reporting_line_body::expectedVersion},
                      field{"reasonCode", &reporting_line_body::reasonCode},
                      field{"commentary", &reporting_line_body::commentary}};
}

}

#endif
