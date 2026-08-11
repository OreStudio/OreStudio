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
#ifndef ORES_MARKETDATA_CORE_ORESMD_ORESMD_PROJECTIONS_HPP
#define ORES_MARKETDATA_CORE_ORESMD_ORESMD_PROJECTIONS_HPP

#include "ores.marketdata.api/domain/market_data_identifier.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.ore.core/market/fx_quote_convention_checker.hpp"
#include <optional>
#include <string>

namespace ores::marketdata::core {

/**
 * @brief The three market_series columns a projected ORE key string
 * (e.g. "FX/RATE/EUR/USD") splits into: series_type, metric, and qualifier.
 * qualifier absorbs every remaining '/'-delimited segment after the first
 * two, e.g. "IR_SWAP/RATE/USD/2D/3M/PAR_RATE" splits into
 * qualifier="USD/2D/3M/PAR_RATE".
 */
struct market_series_key {
    std::string series_type;
    std::string metric;
    std::string qualifier;
};

/**
 * @brief Deterministic projections from a market_data_identifier into ORE's own index
 * name, curve key, and quote key strings, per
 * id:C3E053CA-0D4B-480B-9119-E11530160EC1's "Projection rules" section.
 *
 * Each function returns std::nullopt when @p identifier's asset class/type combination
 * does not produce that kind of key (e.g. to_index_name() on an equity identifier, or
 * to_curve_key() on a `type=quote` identifier).
 *
 * Known simplification: the IR par-rate quote key's settlement/spot-lag dimension
 * (documented in the design doc as "2D" for USD/EUR) is a per-currency market
 * convention this library does not yet look up -- it is hardcoded to "2D" here, the
 * same simplification the design doc's own worked examples use. Replacing it with a
 * real per-currency lookup is follow-on work for the migration tasks that give this
 * library a refdata dependency.
 */
class ORES_MARKETDATA_CORE_EXPORT oresmd_projections final {
public:
    [[nodiscard]] static std::optional<std::string>
    to_index_name(const domain::market_data_identifier& identifier);

    [[nodiscard]] static std::optional<std::string>
    to_curve_key(const domain::market_data_identifier& identifier);

    [[nodiscard]] static std::optional<std::string>
    to_quote_key(const domain::market_data_identifier& identifier);

    /**
     * @brief The reverse of to_quote_key()/to_curve_key(): splits an already-projected
     * ORE key string (e.g. "FX/RATE/EUR/USD") into the three columns market_series
     * stores them under. Returns std::nullopt if @p key has fewer than 3 '/'-delimited
     * segments. Consolidates what were three separately hand-written, identical
     * ad-hoc parsers (in ores.synthetic.service's feed_controller.hpp and
     * ores.marketdata.service's feed_ingest_loop.cpp) into one shared, tested
     * implementation.
     */
    [[nodiscard]] static std::optional<market_series_key>
    split_market_series_key(const std::string& key);

    /**
     * @brief The inverse projection: an ORE quote key string (e.g. "FX/RATE/EUR/USD")
     * becomes the oresmd identifier the forward projection would have emitted it from,
     * per id:C3E053CA-0D4B-480B-9119-E11530160EC1's "Projection rules" section, followed
     * in reverse.
     *
     * Seeded by the series_key_registry's decomposition table: every series type that
     * table knows is either mapped here or has no oresmd identifier at all (BOND, the
     * option and capfloor families) and yields std::nullopt. Unknown types and malformed
     * keys (wrong segment count for the type, unrecognised metric/index/model) also
     * yield std::nullopt -- the caller decides whether to reject or drop the row. The
     * qualifier/point boundaries mirror the registry's per-type rows; the segment-to-field
     * mapping follows what to_quote_key() emits, exactly.
     */
    [[nodiscard]] static std::optional<domain::market_data_identifier>
    from_ore_key(const std::string& key);

    /**
     * @brief from_ore_key() with the FX/RATE convention correction applied at parse time.
     *
     * For an FX spot key the two currency segments are checked against the supplied
     * checker's known canonical pairs; a reversed pair is swapped in the identifier's
     * =pair= field before it is built -- the same fx_quote_convention_checker the
     * import boundary uses today, now targeting the identifier instead of the qualifier
     * string. Every other key is unaffected.
     */
    [[nodiscard]] static std::optional<domain::market_data_identifier>
    from_ore_key(const std::string& key,
                 const ores::ore::market::fx_quote_convention_checker& checker);
};

}

#endif
