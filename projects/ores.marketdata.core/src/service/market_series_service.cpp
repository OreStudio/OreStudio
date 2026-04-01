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
#include "ores.marketdata.core/service/market_series_service.hpp"

namespace ores::marketdata::service {

market_series_service::market_series_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::market_series> market_series_service::list() {
    return repo_.read_latest(ctx_);
}

std::vector<domain::market_series> market_series_service::find_by_type(
    const std::string& series_type, const std::string& metric,
    const std::string& qualifier) {
    return repo_.read_latest_by_type(ctx_, series_type, metric, qualifier);
}

void market_series_service::save(const domain::market_series& v) {
    repo_.write(ctx_, v);
}

void market_series_service::save(const std::vector<domain::market_series>& v) {
    repo_.write(ctx_, v);
}

void market_series_service::remove(const boost::uuids::uuid& id) {
    repo_.remove(ctx_, id);
}

}
