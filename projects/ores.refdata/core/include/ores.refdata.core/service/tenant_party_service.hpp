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
#ifndef ORES_REFDATA_CORE_SERVICE_TENANT_PARTY_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_TENANT_PARTY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/messaging/tenant_party_protocol.hpp"
#include "ores.refdata.core/export.hpp"

namespace ores::refdata::service {

/**
 * @brief Reads the parties of one named tenant for a system administrator.
 *
 * Every other party read answers the caller's own tenant. This one names the
 * tenant, so it is served only to a caller in the system tenant. It reads with
 * a context scoped to the named tenant, and the party table's policy then
 * admits that tenant's rows and no others. The system tenant is refused as a
 * target, because the policy admits every tenant's rows to it.
 */
class ORES_REFDATA_CORE_EXPORT tenant_party_service {
public:
    using context = ores::database::context;

    /// The most parties one page may ask for.
    static constexpr std::uint32_t max_limit = 1000;

    /**
     * @brief Constructs the service for a caller.
     *
     * @param caller The caller's request context; its tenant decides whether
     * the read is served.
     */
    explicit tenant_party_service(context caller);

    messaging::list_tenant_parties_response
    list_tenant_parties(const messaging::list_tenant_parties_request& request);

private:
    context caller_;
};

}

#endif
