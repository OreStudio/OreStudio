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
#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include "ores.marketdata.core/vendor/bloomberg_notation.hpp"
#include "ores.marketdata.core/vendor/demo_notation.hpp"
#include "ores.marketdata.core/vendor/lseg_notation.hpp"

namespace ores::marketdata::vendor {

const provider_notation* notation_of(std::string_view provider) {
    static const bloomberg_notation bloomberg;
    static const lseg_notation lseg;
    static const demo_notation demo;
    for (const provider_notation* candidate : {static_cast<const provider_notation*>(&bloomberg),
                                               static_cast<const provider_notation*>(&lseg),
                                               static_cast<const provider_notation*>(&demo)})
        if (candidate->provider() == provider)
            return candidate;
    return nullptr;
}

}
