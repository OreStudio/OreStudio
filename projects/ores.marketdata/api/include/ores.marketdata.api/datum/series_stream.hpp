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
#ifndef ORES_MARKETDATA_API_DATUM_SERIES_STREAM_HPP
#define ORES_MARKETDATA_API_DATUM_SERIES_STREAM_HPP

#include "ores.marketdata.api/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <string_view>

namespace ores::marketdata::datum {

/**
 * @brief The namespace every stream id is derived in.
 *
 * The value belongs to the generator, not to a deployment. Every store derives
 * the id of a stream in this namespace, so the id is a function of the provider
 * and the series URI alone and no table has to say what it is.
 */
inline constexpr boost::uuids::uuid series_stream_namespace = {{0x7d,
                                                                0x41,
                                                                0xc9,
                                                                0x36,
                                                                0x5b,
                                                                0xa0,
                                                                0x4e,
                                                                0x28,
                                                                0x9f,
                                                                0x13,
                                                                0xe6,
                                                                0x0b,
                                                                0x72,
                                                                0x85,
                                                                0xd4,
                                                                0x1a}};

/**
 * @brief The id of the stream that holds the ticks of one series of one
 * provider.
 *
 * The id is derived from the provider code and the series URI, and stored
 * nowhere. A tick row names its stream by this id, so the ingest that writes
 * the tick and the history read that finds it compute the same id from the same
 * two values. A party reads the stream through its own series: the series
 * carries the provider code and the series URI the id comes from.
 *
 * The name the id is derived from is the length of @p provider_code in decimal
 * digits, a colon, the provider code, and then @p series_uri. The length keeps
 * two different pairs apart without a separator that either part might hold.
 * A new namespace or a new spelling renames every stream, so the ticks written
 * before the change are no longer found under the new ids.
 */
ORES_MARKETDATA_API_EXPORT boost::uuids::uuid stream_id(std::string_view provider_code,
                                                        std::string_view series_uri);

}

#endif
