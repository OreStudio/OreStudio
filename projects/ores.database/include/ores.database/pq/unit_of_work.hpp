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
#ifndef ORES_DATABASE_PQ_UNIT_OF_WORK_HPP
#define ORES_DATABASE_PQ_UNIT_OF_WORK_HPP

#include "ores.database/pq/pool.hpp"
#include "ores.database/pq/transaction.hpp"
#include <type_traits>
#include <utility>

namespace ores::database::pq {

/**
 * @brief Runs @p work in one transaction: the only place that leases,
 * begins, commits, rolls back and gives back.
 *
 * The work receives the open transaction and never sees the connection or the
 * credentials. It commits when the work returns, and rolls back when it
 * throws, after which the exception goes on to the caller. The lease returns
 * the connection on every path.
 */
template <typename Work>
auto transact(pool& p, const session_context& context, Work&& work)
    -> std::invoke_result_t<Work, transaction&> {
    auto lease = p.acquire();
    transaction tx(lease.get(), context);
    try {
        if constexpr (std::is_void_v<std::invoke_result_t<Work, transaction&>>) {
            std::forward<Work>(work)(tx);
            tx.commit();
        } else {
            auto out = std::forward<Work>(work)(tx);
            tx.commit();
            return out;
        }
    } catch (...) {
        tx.rollback();
        throw;
    }
}

}

#endif
