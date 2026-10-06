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
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.database/domain/tenant_aware_pool.hpp"
#include <boost/exception/exception.hpp>
#include <format>
#include <sqlgen/postgres.hpp>

namespace ores::database::repository {

unit_of_work::unit_of_work(context ctx)
    : base_(std::move(ctx))
    , transaction_(begin(base_))
    , ctx_(base_.with_transaction(transaction_)) {}

unit_of_work::~unit_of_work() {
    if (!committed_)
        (void)transaction_->rollback();
}

void unit_of_work::commit() {
    if (committed_)
        return;

    const auto r = transaction_->commit();
    if (!r)
        BOOST_THROW_EXCEPTION(repository_exception(
            std::format("Cannot commit the transaction: {}", r.error().what())));
    committed_ = true;
}

sqlgen::Ref<context::transaction_type> unit_of_work::begin(context& base) {
    const auto r = sqlgen::session(base.connection_pool()).and_then(sqlgen::begin_transaction);
    if (!r)
        BOOST_THROW_EXCEPTION(
            repository_exception(std::format("Cannot begin a transaction: {}", r.error().what())));
    return *r;
}

}
