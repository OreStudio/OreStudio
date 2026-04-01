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
#include "ores.marketdata.core/service/market_fixing_service.hpp"

namespace ores::marketdata::service {

market_fixing_service::market_fixing_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::market_fixing>
market_fixing_service::list(const boost::uuids::uuid& series_id) {
    return repo_.read_latest(ctx_, series_id);
}

std::vector<domain::market_fixing>
market_fixing_service::list(const boost::uuids::uuid& series_id,
    const std::chrono::year_month_day& from_date,
    const std::chrono::year_month_day& to_date) {
    return repo_.read_latest(ctx_, series_id, from_date, to_date);
}

void market_fixing_service::save(const domain::market_fixing& v) {
    repo_.write(ctx_, v);
}

void market_fixing_service::save(const std::vector<domain::market_fixing>& v) {
    repo_.write(ctx_, v);
}

void market_fixing_service::remove(const boost::uuids::uuid& series_id) {
    repo_.remove(ctx_, series_id);
}

}
