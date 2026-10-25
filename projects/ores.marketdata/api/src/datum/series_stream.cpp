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
#include "ores.marketdata.api/datum/series_stream.hpp"
#include <boost/uuid/name_generator_sha1.hpp>
#include <string>

namespace ores::marketdata::datum {

boost::uuids::uuid stream_id(std::string_view provider_code, std::string_view series_uri) {
    static const boost::uuids::name_generator_sha1 generator(series_stream_namespace);
    std::string name = std::to_string(provider_code.size());
    name += ':';
    name += provider_code;
    name += series_uri;
    return generator(name);
}

}
