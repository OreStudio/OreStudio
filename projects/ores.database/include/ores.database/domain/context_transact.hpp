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

namespace detail {

/**
 * @brief The connection of an open sqlgen transaction.
 *
 * A unit of work that has committed still holds its connection, outside any
 * transaction, so a call joined to it would run unguarded: it is refused.
 */
inline PGconn* open_connection_of(const context::transaction_type& open) {
    PGconn* c = open.conn()->conn()->native_handle();
    const auto status = c ? PQtransactionStatus(c) : PQTRANS_UNKNOWN;
    if (status != PQTRANS_INTRANS && status != PQTRANS_INERROR)
        throw pq::store_error("The sqlgen unit of work has no open transaction to join",
                              "25000",
                              pq::error_kind::refused);
    return c;
}

}

/**
 * @brief Runs @p work in one libpq transaction for the tenant, party, actor
 * and service that @p ctx names.
 *
 * A repository that has moved to the libpq layer calls this, so its callers
 * keep passing a context. A context bound to a sqlgen unit of work joins that
 * transaction on its connection, so the work commits or rolls back with the
 * unit of work, and sees the tenant and party set on that connection, as a
 * sqlgen repository does. This lets components move one at a time, and goes
 * with sqlgen.
 */
template <typename Work>
auto transact(const context& ctx, Work&& work) {
    if (const auto& open = ctx.active_transaction()) {
        auto c = pq::connection::borrow(detail::open_connection_of(**open));
        auto tx = pq::transaction::join(c);
        return std::forward<Work>(work)(tx);
    }
    return pq::transact(ctx.pq_pool(), ctx.pq_session(), std::forward<Work>(work));
}

}

#endif
