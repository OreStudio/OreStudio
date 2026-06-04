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
#include "ores.refdata.core/service/swap_convention_service.hpp"
#include <stdexcept>

namespace ores::refdata::service {

using namespace ores::logging;

swap_convention_service::swap_convention_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::swap_convention> swap_convention_service::list_swap_conventions() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all swap conventions";
    return repo_.read_latest(ctx_);
}

std::optional<domain::swap_convention>
swap_convention_service::get_swap_convention(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting swap convention: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void swap_convention_service::save_swap_convention(const domain::swap_convention& v) {
    if (v.id.empty())
        throw std::invalid_argument("Swap Convention id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving swap convention: " << v.id;
    repo_.write(ctx_, v);
    BOOST_LOG_SEV(lg(), info) << "Saved swap convention: " << v.id;
}

void swap_convention_service::remove_swap_convention(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing swap convention: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed swap convention: " << id;
}

std::vector<domain::swap_convention>
swap_convention_service::get_swap_convention_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for swap convention: " << id;
    return repo_.read_all(ctx_, id);
}

}
