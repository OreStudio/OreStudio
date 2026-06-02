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
#ifndef ORES_REFDATA_API_MESSAGING_ASSET_CLASS_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_ASSET_CLASS_PROTOCOL_HPP

#include <string>
#include <vector>
#include "ores.refdata.api/domain/asset_class_info.hpp"

namespace ores::refdata::messaging {

/**
 * @brief Request published asset class entries from the refdata service.
 *
 * An optional @c coding_scheme_code filter narrows the result to a single
 * coding scheme (e.g. "ORE_ASSET_CLASS"). If empty, all schemes are returned.
 */
struct get_asset_classes_request {
    using response_type = struct get_asset_classes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset-classes.list";
    std::string coding_scheme_code; ///< Optional filter; empty = all schemes
    int offset = 0;
    int limit = 200;
};

struct get_asset_classes_response {
    std::vector<ores::refdata::domain::asset_class_info> asset_classes;
    int total_available_count = 0;
};

}

#endif
