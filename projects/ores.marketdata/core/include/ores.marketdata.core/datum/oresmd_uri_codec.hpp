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
#ifndef ORES_MARKETDATA_CORE_DATUM_ORESMD_URI_CODEC_HPP
#define ORES_MARKETDATA_CORE_DATUM_ORESMD_URI_CODEC_HPP

#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.core/export.hpp"
#include <expected>
#include <string>
#include <string_view>

namespace ores::marketdata::datum {

/**
 * @brief Writes a market datum as an oresmd URI and reads it back.
 *
 * The codec walks the datum's schema row and has no code per instrument type:
 *
 * @code
 * oresmd://credit/ACME?type=quote&instrument=cds&quote=credit_spread
 *     &seniority=SNRFOR&ccy=USD&doc_clause=XR14&term=5Y
 * @endcode
 *
 * The authority is the row's asset class and the path is its subject field,
 * percent-encoded with its case kept. The query starts with type (quote for a
 * datum, series for a series), instrument and quote, the ORE instrument and
 * quote types in lower case. Each other field follows under its schema name, in
 * schema order, with the case of its value kept. A field that holds none is
 * left out.
 *
 * The reader is strict. It refuses a missing key for a field that may not be
 * none, an unknown key, a repeated key, an empty value, a coordinate field in a
 * series, and a quote URI whose datum has no ORE key, so a quote URI maps one
 * to one onto the canonical ORE key. It also refuses any text other than the
 * writer's own spelling, such as a reordered query, so one datum has one URI
 * and stored URIs compare as text.
 */
class ORES_MARKETDATA_CORE_EXPORT oresmd_uri_codec final {
public:
    /// The datum or series @p uri names, or why the grammar does not admit it.
    [[nodiscard]] static std::expected<market_datum, std::string> read(std::string_view uri);

    /// The URI of @p datum, or why it has none: a datum with no ORE key has no
    /// quote URI.
    [[nodiscard]] static std::expected<std::string, std::string> write(const market_datum& datum);
};

}

#endif
