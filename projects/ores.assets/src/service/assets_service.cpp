/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.assets/service/assets_service.hpp"

namespace ores::assets::service {

using namespace ores::telemetry::log;

assets_service::assets_service(context ctx)
    : ctx_(std::move(ctx)) {
}

std::vector<domain::currency_image> assets_service::get_currency_images() {
    BOOST_LOG_SEV(lg(), debug) << "Getting all currency-image mappings";
    return currency_image_repo_.read_latest(ctx_);
}

std::vector<domain::image> assets_service::get_images(
    const std::vector<std::string>& image_ids) {
    BOOST_LOG_SEV(lg(), debug) << "Getting images by IDs, count=" << image_ids.size();
    return image_repo_.read_latest_by_ids(ctx_, image_ids);
}

}
