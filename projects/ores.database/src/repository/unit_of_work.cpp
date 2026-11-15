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
#include "ores.database/pq/store_error.hpp"
#include <boost/exception/exception.hpp>
#include <format>

namespace ores::database::repository {

unit_of_work::unit_of_work(const context& ctx)
    : base_(ctx)
    , lease_(base_.pq_pool().acquire())
    , transaction_(std::make_unique<pq::transaction>(lease_->get(), base_.pq_session()))
    , ctx_(base_.with_transaction(*transaction_)) {}

unit_of_work::~unit_of_work() {
    if (!committed_)
        transaction_->rollback();
}

void unit_of_work::commit() {
    if (committed_)
        return;
    // The transaction has ended whether the commit succeeds or fails, and an
    // ended transaction refuses every statement, so the connection goes back to
    // the pool here rather than when the unit of work goes out of scope. Work
    // after a commit then never holds this connection while it waits on another.
    try {
        transaction_->commit();
    } catch (const pq::store_error& e) {
        lease_.reset();
        BOOST_THROW_EXCEPTION(
            repository_exception(std::format("Cannot commit the transaction: {}", e.what())));
    }
    committed_ = true;
    lease_.reset();
}

}
