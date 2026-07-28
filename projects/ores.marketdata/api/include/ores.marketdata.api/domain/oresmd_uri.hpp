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
#ifndef ORES_MARKETDATA_API_DOMAIN_ORESMD_URI_HPP
#define ORES_MARKETDATA_API_DOMAIN_ORESMD_URI_HPP

#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief The oresmd URI scheme name (RFC 3986 scheme component).
 *
 * `oresmd` names a URI scheme, not a C++ namespace -- every oresmd type/function lives
 * under the ordinary `ores::marketdata` namespace, per
 * id:C3E053CA-0D4B-480B-9119-E11530160EC1, "Why a URI, and why query parameters".
 */
inline constexpr std::string_view oresmd_scheme = "oresmd";

/**
 * @brief A strongly-typed wrapper for a serialised oresmd URI string, distinguishing "an
 * oresmd URI" from an arbitrary `std::string` at the type level.
 */
struct oresmd_uri final {
    std::string value;
};

}

#endif
