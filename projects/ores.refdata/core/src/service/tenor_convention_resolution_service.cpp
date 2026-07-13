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
#include "ores.refdata.core/service/tenor_convention_resolution_service.hpp"

namespace ores::refdata::service {

using namespace ores::logging;

tenor_convention_resolution_service::tenor_convention_resolution_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_service::list_resolutions() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all tenor convention resolutions";
    return repo_.read_all(ctx_);
}

}
