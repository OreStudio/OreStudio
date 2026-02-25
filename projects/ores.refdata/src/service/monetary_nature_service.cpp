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
#include "ores.refdata/service/monetary_nature_service.hpp"

#include <stdexcept>

namespace ores::refdata::service {

using namespace ores::logging;

monetary_nature_service::monetary_nature_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::monetary_nature> monetary_nature_service::list_types() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all currency asset classes";
    return repo_.read_latest(ctx_);
}

std::optional<domain::monetary_nature>
monetary_nature_service::find_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding currency asset class: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty()) return std::nullopt;
    return results.front();
}

void monetary_nature_service::save_type(const domain::monetary_nature& v) {
    if (v.code.empty())
        throw std::invalid_argument("Currency Asset Class code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving currency asset class: " << v.code;
    repo_.write(ctx_, v);
    BOOST_LOG_SEV(lg(), info) << "Saved currency asset class: " << v.code;
}

void monetary_nature_service::remove_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency asset class: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed currency asset class: " << code;
}

std::vector<domain::monetary_nature>
monetary_nature_service::get_type_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for currency asset class: " << code;
    return repo_.read_all(ctx_, code);
}

}
