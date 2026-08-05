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
#ifndef ORES_SYNTHETIC_API_DOMAIN_BINDING_MODE_HPP
#define ORES_SYNTHETIC_API_DOMAIN_BINDING_MODE_HPP

namespace ores::synthetic::domain {

/**
 * @brief Whether a market_data_generation_config's generated data is
 * authoritative for real system behaviour.
 *
 * Orthogonal to scope, which decides how widely the data is shared.
 */
enum class binding_mode {
    bound,    ///< Resolved into the feed real consumers see. At most one
              ///< bound config may be active per (instrument, scope).
    sandboxed ///< Generated and published under a distinct namespace,
              ///< reachable only by explicit selection; never resolved
              ///< into the bound feed.
};

}

#endif
