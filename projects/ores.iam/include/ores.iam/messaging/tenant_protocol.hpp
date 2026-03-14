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
#ifndef ORES_IAM_MESSAGING_TENANT_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_TENANT_PROTOCOL_HPP

#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.iam/domain/tenant.hpp"

namespace ores::iam::messaging {

struct get_tenants_request {
    bool include_deleted = false;
};

struct get_tenants_response {
    std::vector<ores::iam::domain::tenant> tenants;
};

struct save_tenant_request {
    ores::iam::domain::tenant data;

    static save_tenant_request from(ores::iam::domain::tenant t) {
        return { .data = std::move(t) };
    }
};

struct save_tenant_response {
    bool success = false;
    std::string message;
};

struct delete_tenant_request {
    std::vector<boost::uuids::uuid> ids;
};

struct delete_tenant_response {
    bool success = false;
    std::string message;
};

struct get_tenant_history_request {
    boost::uuids::uuid id;
};

struct get_tenant_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::iam::domain::tenant> versions;
};

}

#endif
