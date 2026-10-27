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
#include "ores.database/domain/context.hpp"

namespace ores::database {

pq::pool& context::pq_pool() const {
    std::scoped_lock lock(pq_->mutex);
    if (!pq_->pool) {
        pq_->pool = std::make_unique<pq::pool>(
            pq::connection_options{.host = credentials_.host,
                                   .port = static_cast<std::uint16_t>(credentials_.port),
                                   .dbname = credentials_.dbname,
                                   .user = credentials_.user,
                                   .password = credentials_.password,
                                   .application_name = service_account_},
            pq_->size);
    }
    return *pq_->pool;
}

pq::session_context context::pq_session() const {
    return {.tenant = tenant_id(),
            .party = party_id(),
            .visible_parties = visible_party_ids(),
            .actor = actor(),
            .service = service_account_};
}

}
