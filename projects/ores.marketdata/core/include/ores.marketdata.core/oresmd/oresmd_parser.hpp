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
#ifndef ORES_MARKETDATA_CORE_ORESMD_ORESMD_PARSER_HPP
#define ORES_MARKETDATA_CORE_ORESMD_ORESMD_PARSER_HPP

#include "ores.marketdata.api/domain/market_data_identifier.hpp"
#include "ores.marketdata.api/domain/oresmd_uri.hpp"
#include "ores.marketdata.core/export.hpp"
#include <optional>
#include <set>
#include <string>

namespace ores::marketdata::core {

/**
 * @brief Canonical spellings of the free-text identifier components, supplied by the
 * caller from refdata.
 *
 * The URI builder matches the identifier's tenor and point values against these sets
 * and rejects unknown spellings; oresmd keeps no dependency on the refdata
 * repositories. Values use the parser's spelling (lowercase, as parse() stores them);
 * a typed "6m" and a program-built "6M" are two strings unless the caller's refdata
 * container says which is canonical.
 */
struct canonical_values final {
    /** @brief Canonical tenor spellings (e.g. "3m", "1d"). */
    std::set<std::string> tenor;

    /** @brief Canonical point spellings (e.g. "5y", "sr,5y", "5y,2y,atm"). */
    std::set<std::string> point;
};

/**
 * @brief Parses oresmd URIs into market_data_identifier and serialises them back, per
 * id:C3E053CA-0D4B-480B-9119-E11530160EC1 ("oresmd: ORE Studio Market Data URI").
 *
 * Dispatches on the URI's `asset_class` authority component to construct the matching
 * concrete struct inside the market_data_identifier variant -- there is no virtual call
 * here, just a `std::visit`/switch keyed on that one field. Throws oresmd_exception on
 * an unrecognised scheme, asset class, or an asset class's mandatory field missing.
 */
class ORES_MARKETDATA_CORE_EXPORT oresmd_parser final {
public:
    /** @brief Parses @p uri into the matching concrete identifier inside the variant. */
    [[nodiscard]] static domain::market_data_identifier parse(const domain::oresmd_uri& uri);

    /** @brief Serialises @p identifier back into an oresmd URI, the reverse of parse(). */
    [[nodiscard]] static domain::oresmd_uri
    to_uri(const domain::market_data_identifier& identifier);

    /**
     * @brief Serialises @p identifier as the series it belongs to rather than as the
     * datum it is: the same URI with the observation's coordinate dropped.
     *
     * Which fields hold that coordinate is a fact about each family, and the
     * decomposition's own split is the authority for it: a quote's point and a
     * surface's expiry and strike are dropped, a discount curve's maturity is dropped
     * because its series is the currency and the curve, and a future's contract code
     * and underlying tenor are dropped because its series is the currency and the
     * contract month. Every point of one series therefore projects to one identity.
     *
     * A series is what a market_series row holds, and its points are the observation
     * rows beneath it (see the market_series model's oresmd_uri column). A datum's key
     * carries its own point -- IR_SWAP/RATE/USD/2D/1D/10Y names the 10Y point of the
     * USD swap series -- so projecting the datum would put one of the series' points
     * into the series' own identity, and two databases built from the same files in a
     * different order would disagree about the identity of one series.
     */
    [[nodiscard]] static domain::oresmd_uri
    to_series_uri(const domain::market_data_identifier& identifier);

    /**
     * @brief The series @p identifier with one observation's coordinate put back,
     * the inverse of to_series_uri() for the families whose coordinate is a point.
     *
     * A writer that holds a series identity and one observation's point -- the
     * decomposition's own `point_id`, which is the key's remaining segments joined
     * with `/` -- needs the datum's identifier before it can project a quote key,
     * and which field the point belongs in is a fact about each family. This is
     * the one place that fact is read back: a discount curve's maturity goes to
     * its tenor, a money-market future's coordinate splits into its contract code
     * and underlying tenor, an overnight-index future's into its contract month,
     * contract code and underlying tenor, and every other indexed family takes the
     * point as the point.
     *
     * A surface is refused, because its coordinate is the identifier's vol point
     * rather than one string, and to_series_uri() drops that point rather than
     * leaving a field to put back. A fixing is refused, because its key is an
     * index name rather than a series and a point, and a class with no point at
     * all is refused by the same rule. A refused call returns nullopt.
     */
    [[nodiscard]] static std::optional<domain::market_data_identifier>
    with_point(const domain::market_data_identifier& identifier, const std::string& point);

    /**
     * @brief Serialises @p identifier, matching its tenor and point values against
     * @p canonical and rejecting unknown spellings with oresmd_exception.
     *
     * The canonical form of the stored string: values the container knows are emitted
     * verbatim, values it does not know are an error -- the caller (refdata) owns the
     * spelling. Identifiers without a tenor or point (scalar series, correlation) are
     * unaffected.
     */
    [[nodiscard]] static domain::oresmd_uri to_uri(const domain::market_data_identifier& identifier,
                                                   const canonical_values& canonical);
};

}

#endif
