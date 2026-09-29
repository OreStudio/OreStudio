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
#ifndef ORES_IAM_CORE_SERVICE_TENANT_HOSTNAME_CHECK_HPP
#define ORES_IAM_CORE_SERVICE_TENANT_HOSTNAME_CHECK_HPP

#include <string>

namespace ores::iam::service {

/**
 * @brief Why a tenant hostname is not one this deployment accepts, or nothing.
 *
 * The hostname routes a request to its tenant: an administrator is addressed
 * as =username@hostname=, the part after the @ is what a login looks the tenant
 * up by, and the lookup is an exact match on the stored hostname. A hostname
 * that states a port therefore produces a principal that resolves to no tenant,
 * and the tenant's own administrator cannot sign in to the tenant they were
 * just handed. A port is where a deployment listens, not which tenant somebody
 * belongs to, so it is refused when the tenant is created rather than carried
 * into an identity that does not resolve.
 *
 * The answer is a sentence rather than an exception, because the caller is a
 * service that has to tell a person why their request was refused.
 */
[[nodiscard]] inline std::string check_tenant_hostname(const std::string& hostname) {
    if (hostname.empty()) {
        return "A tenant hostname is required.";
    }
    if (hostname.find(':') != std::string::npos) {
        return "A tenant hostname may not state a port.";
    }
    return {};
}

} // namespace ores::iam::service

#endif
