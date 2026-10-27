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
#ifndef ORES_DATABASE_DOMAIN_CONTEXT_TRANSACT_HPP
#define ORES_DATABASE_DOMAIN_CONTEXT_TRANSACT_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/pq/store_error.hpp"
#include "ores.database/pq/unit_of_work.hpp"
#include <utility>

namespace ores::database {

/**
 * @brief Runs @p work in one libpq transaction for the tenant, party, actor
 * and service that @p ctx names.
 *
 * A repository that has moved to the libpq layer calls this, so its callers
 * keep passing a context. A context bound to a sqlgen unit of work is refused:
 * the work would run outside that transaction, and a rollback of the unit of
 * work would not undo it.
 */
template <typename Work>
auto transact(const context& ctx, Work&& work) {
    if (ctx.active_transaction())
        throw pq::store_error("A repository on the libpq layer cannot join a sqlgen unit of work",
                              "25000",
                              pq::error_kind::refused);
    return pq::transact(ctx.pq_pool(), ctx.pq_session(), std::forward<Work>(work));
}

}

#endif
