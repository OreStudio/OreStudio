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
#ifndef ORES_IAM_API_DOMAIN_ROLE_CODES_HPP
#define ORES_IAM_API_DOMAIN_ROLE_CODES_HPP

namespace ores::iam::domain {

/**
 * @brief Well-known role names used throughout the system.
 *
 * These names must match the rows seeded in ores_iam_roles_tbl. They lived in
 * the hand-written role.hpp; that header is now generated from the role
 * entity model, and codegen has no way to express "also emit this extra
 * namespace of constants", so a regeneration would silently drop them. They
 * therefore live in this own hand-maintained header, beside the generated
 * role.hpp, following the permission_codes.hpp precedent.
 */
namespace roles {
constexpr auto super_admin = "SuperAdmin";
constexpr auto tenant_admin = "TenantAdmin";
constexpr auto trading = "Trading";
constexpr auto sales = "Sales";
constexpr auto operations = "Operations";
constexpr auto support = "Support";
constexpr auto viewer = "Viewer";
}

}

#endif
