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
#ifndef ORES_MARKETDATA_CORE_SERVICE_ORESMD_URI_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_ORESMD_URI_SERVICE_HPP

#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include <cstddef>

namespace ores::marketdata::service {

/**
 * @brief Reads an oresmd URI into its parts and writes one from its parts.
 *
 * Both calls go through the codec, so the browser never decides whether a URI is
 * valid and never spells one. A URI the codec refuses is an answer and not a
 * fault: the response says why, in the codec's words.
 *
 * The inputs are bounded. A longer URI, more fields or a longer text is refused
 * before the codec sees it, because the request comes from a browser.
 */
class ORES_MARKETDATA_CORE_EXPORT oresmd_uri_service final {
public:
    /// The most characters a URI, a subject or a field's text may hold.
    static constexpr std::size_t max_text = 2048;
    /// The most fields a compose request may state.
    static constexpr std::size_t max_fields = 64;

    /// The parts of @p request's URI, or the codec's reason for refusing it.
    static messaging::parse_oresmd_uri_response
    parse(const messaging::parse_oresmd_uri_request& request);

    /// The URI the codec writes for @p request's parts, or why they make none.
    static messaging::compose_oresmd_uri_response
    compose(const messaging::compose_oresmd_uri_request& request);
};

}

#endif
