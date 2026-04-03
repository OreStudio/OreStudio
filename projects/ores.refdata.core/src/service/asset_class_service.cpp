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
#include "ores.refdata.core/service/asset_class_service.hpp"

namespace ores::refdata::service {

using namespace ores::logging;

std::vector<domain::asset_class_info>
asset_class_service::list_asset_classes(
    const std::string& coding_scheme,
    std::uint32_t offset,
    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug)
        << "Listing asset classes. scheme=" << coding_scheme;
    return repo_.read_latest(ctx_, coding_scheme, offset, limit);
}

std::uint32_t
asset_class_service::count_asset_classes(
    const std::string& coding_scheme) {
    return repo_.count_latest(ctx_, coding_scheme);
}

}
