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

unit_of_work::unit_of_work(context ctx)
    : base_(std::move(ctx))
    , lease_(base_.pq_pool().acquire())
    , transaction_(std::make_unique<pq::transaction>(lease_.get(), base_.pq_session()))
    , ctx_(base_.with_transaction(*transaction_)) {}

unit_of_work::~unit_of_work() {
    if (!committed_)
        transaction_->rollback();
}

void unit_of_work::commit() {
    if (committed_)
        return;
    try {
        transaction_->commit();
    } catch (const pq::store_error& e) {
        BOOST_THROW_EXCEPTION(
            repository_exception(std::format("Cannot commit the transaction: {}", e.what())));
    }
    committed_ = true;
}

}
