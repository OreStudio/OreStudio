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
#ifndef ORES_UTILITY_CODEC_WIRE_TRAITS_FWD_HPP
#define ORES_UTILITY_CODEC_WIRE_TRAITS_FWD_HPP

namespace ores::utility::codec {

/**
 * @brief How a type reaches the wire, in each format the wire states.
 *
 * The primary template has no definition, so a type with no codec does not
 * compile. A type with a codec states a specialisation in its own header, and
 * its translation unit defines the four functions. This declaration exists so
 * a header can state that specialisation without pulling a codec behind it.
 */
template <typename T>
struct wire_traits;

}

#endif
