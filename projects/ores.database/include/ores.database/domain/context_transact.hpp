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
 * A context bound to a unit of work runs the work on that unit of work's
 * transaction, which commits or rolls back with the unit of work; any other
 * runs it in a transaction of its own.
 */
template <typename Work>
auto transact(const context& ctx, Work&& work) {
    if (auto* open = ctx.active_transaction()) {
        if (open->ended())
            throw pq::store_error(
                "The unit of work's transaction has ended", "25000", pq::error_kind::refused);
        return std::forward<Work>(work)(*open);
    }
    return pq::transact(ctx.pq_pool(), ctx.pq_session(), std::forward<Work>(work));
}

}

#endif
