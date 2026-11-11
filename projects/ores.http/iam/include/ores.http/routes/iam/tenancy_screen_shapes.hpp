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
#ifndef ORES_HTTP_ROUTES_IAM_TENANCY_SCREEN_SHAPES_HPP
#define ORES_HTTP_ROUTES_IAM_TENANCY_SCREEN_SHAPES_HPP

#include "ores.utility/codec/wire_fields.hpp"
#include <map>
#include <optional>
#include <string>
#include <tuple>
#include <type_traits>

/**
 * @brief The bodies the tenancy and setup screens send, as the BFF stated them.
 *
 * The screens name every field in camelCase, where IAM's protocol names them in
 * snake_case, so each body is read in the screen's own shape. The shapes are
 * one vocabulary across the tenancy routes, and live here once.
 */
namespace ores::http::routes::iam {

/**
 * @brief A tenant to provision: the starting point, the tenant and its first
 * administrator, and the values the starting point's form asked for.
 */
struct provision_tenant_body final {
    std::string profileCode;
    std::string tenantCode;
    std::string tenantName;
    std::string tenantHostname;
    std::optional<std::string> tenantDescription;
    std::string adminUsername;
    std::string adminEmail;
    std::string adminPassword;
    std::optional<std::map<std::string, std::string>> parameters;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<provision_tenant_body>) {
    using ores::utility::codec::field;
    using b = provision_tenant_body;
    return std::tuple{field{"profileCode", &b::profileCode},
                      field{"tenantCode", &b::tenantCode},
                      field{"tenantName", &b::tenantName},
                      field{"tenantHostname", &b::tenantHostname},
                      field{"tenantDescription", &b::tenantDescription},
                      field{"adminUsername", &b::adminUsername},
                      field{"adminEmail", &b::adminEmail},
                      field{"adminPassword", &b::adminPassword},
                      field{"parameters", &b::parameters}};
}

/**
 * @brief A party to add: its legal name, its short code, and the LEI of the
 * legal entity it was built from, when it was.
 */
struct provision_party_body final {
    std::string fullName;
    std::string shortCode;
    std::optional<std::string> lei;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<provision_party_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"fullName", &provision_party_body::fullName},
                      field{"shortCode", &provision_party_body::shortCode},
                      field{"lei", &provision_party_body::lei}};
}

/**
 * @brief A tenant's removal, confirmed by typing its code again.
 */
struct confirm_removal_body final {
    std::string confirmCode;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<confirm_removal_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"confirmCode", &confirm_removal_body::confirmCode}};
}

/**
 * @brief The first administrator: a username, a password and an email address.
 */
struct initial_administrator_body final {
    std::string principal;
    std::string password;
    std::string email;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<initial_administrator_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"principal", &initial_administrator_body::principal},
                      field{"password", &initial_administrator_body::password},
                      field{"email", &initial_administrator_body::email}};
}

}

#endif
