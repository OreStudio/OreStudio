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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_DATABASE_PQ_SESSION_CONTEXT_HPP
#define ORES_DATABASE_PQ_SESSION_CONTEXT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <vector>

namespace ores::database::pq {

/**
 * @brief Who a transaction acts for. The policies of row-level security read
 * these settings, and the audit columns name the actor and the service.
 */
struct session_context final {
    utility::uuid::tenant_id tenant = utility::uuid::tenant_id::system();
    std::optional<boost::uuids::uuid> party;
    std::vector<boost::uuids::uuid> visible_parties;
    std::string actor;
    std::string service;
};

}

#endif
