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
#ifndef ORES_DATABASE_REPOSITORY_UNIT_OF_WORK_HPP
#define ORES_DATABASE_REPOSITORY_UNIT_OF_WORK_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/export.hpp"
#include "ores.database/repository/repository_exception.hpp"
#include <sqlgen/Transaction.hpp>

namespace ores::database::repository {

/**
 * @brief One transaction, shared by every repository write made through it.
 *
 * A unit of work owns one pooled connection and one transaction. Its
 * ctx() returns a context bound to that transaction, and every repository
 * call made with that context joins it. A use case that must write
 * several rows together opens one unit of work, writes through it, and
 * calls commit once.
 *
 * The transaction ends on commit(). When the caller destroys the unit of
 * work without committing, the destructor rolls the transaction back, so
 * a thrown exception between the writes leaves none of them.
 *
 * The tenant and the party are set on the connection when it is acquired.
 * A held connection cannot change them, so a caller that needs another
 * tenant or party builds that context before it opens the unit of work.
 *
 * A unit of work does not move or copy: its address is the lifetime of
 * the transaction, and repository calls must happen inside that lifetime.
 *
 * @example
 * unit_of_work uow(ctx_);
 * const auto& ctx = uow.ctx();
 * account_repository{}.write(ctx, account);
 * audit_repository{}.insert(ctx, entry);
 * uow.commit();
 */
class ORES_DATABASE_EXPORT unit_of_work {
public:
    /**
     * @brief Opens one connection and one transaction on it.
     *
     * @throws repository_exception when no connection can be acquired or
     * the transaction cannot begin.
     */
    explicit unit_of_work(context ctx);

    /**
     * @brief Rolls the transaction back when the caller did not commit.
     */
    ~unit_of_work();

    unit_of_work(const unit_of_work&) = delete;
    unit_of_work& operator=(const unit_of_work&) = delete;
    unit_of_work(unit_of_work&&) = delete;
    unit_of_work& operator=(unit_of_work&&) = delete;

    /**
     * @brief Gets the context bound to this transaction.
     *
     * Pass it to every repository call that must join the transaction.
     */
    [[nodiscard]] const context& ctx() const {
        return ctx_;
    }

    /**
     * @brief Commits the transaction.
     *
     * Does nothing on a second call.
     *
     * @throws repository_exception when the commit fails.
     */
    void commit();

private:
    static sqlgen::Ref<context::transaction_type> begin(context& base);

    context base_;
    sqlgen::Ref<context::transaction_type> transaction_;
    context ctx_;
    bool committed_ = false;
};

}

#endif
