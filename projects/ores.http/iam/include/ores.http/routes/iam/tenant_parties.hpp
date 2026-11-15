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
#ifndef ORES_HTTP_ROUTES_IAM_TENANT_PARTIES_HPP
#define ORES_HTTP_ROUTES_IAM_TENANT_PARTIES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/domain/party.hpp"
#include <vector>

namespace ores::http::routes::iam {

/**
 * @brief Every party of the caller's own tenant.
 *
 * Read to the end rather than one page, because the questions it answers are
 * about all of them: which party sits at the top of the tenant, and whether the
 * tenant holds the party a session is moving to. A tenant's parties are its own
 * structure and not its trading data, so reading them all is a small read.
 */
class tenant_parties final {
public:
    /**
     * @throws record_routes::refusal when a page is refused, as the BFF refused
     * a failed operation.
     */
    [[nodiscard]] static std::vector<ores::refdata::domain::party>
    every(const ores::http::domain::http_request& req, ores::nats::service::nats_client& session);
};

}

#endif
