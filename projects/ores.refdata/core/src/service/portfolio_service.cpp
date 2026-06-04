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
#include "ores.refdata.core/service/portfolio_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

portfolio_service::portfolio_service(context ctx)
    : ctx_(ctx) {}

std::vector<domain::portfolio> portfolio_service::list_portfolios() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all portfolios";
    return repo_.read_latest(ctx_);
}

std::optional<domain::portfolio> portfolio_service::find_portfolio(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding portfolio: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

void portfolio_service::save_portfolio(const domain::portfolio& portfolio) {
    if (portfolio.id.is_nil()) {
        throw std::invalid_argument("Portfolio ID cannot be nil.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving portfolio: " << portfolio.id;
    auto p = portfolio;
    stamp(p, ctx_);
    repo_.write(ctx_, p);
    BOOST_LOG_SEV(lg(), info) << "Saved portfolio: " << portfolio.id;
}

void portfolio_service::save_portfolios(const std::vector<domain::portfolio>& portfolios) {
    for (const auto& p : portfolios) {
        if (p.id.is_nil())
            throw std::invalid_argument("Portfolio ID cannot be nil.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << portfolios.size() << " portfolios";
    auto stamped = portfolios;
    for (auto& p : stamped)
        stamp(p, ctx_);
    repo_.write(ctx_, stamped);
}

void portfolio_service::remove_portfolio(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing portfolio: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed portfolio: " << id;
}

std::vector<domain::portfolio> portfolio_service::get_portfolio_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for portfolio: " << id;
    return repo_.read_all(ctx_, id);
}

}
