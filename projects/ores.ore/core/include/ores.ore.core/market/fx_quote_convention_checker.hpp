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
#ifndef ORES_ORE_CORE_MARKET_FX_QUOTE_CONVENTION_CHECKER_HPP
#define ORES_ORE_CORE_MARKET_FX_QUOTE_CONVENTION_CHECKER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/export.hpp"
#include <set>
#include <string>
#include <utility>

namespace ores::ore::market {

/**
 * @brief What, if anything, fx_quote_convention_checker::check did to a quote.
 */
enum class fx_quote_status {
    /** Currency pair not found in either order — nothing to check against. */
    unknown_pair,
    /** Pair matches the known canonical base/quote order; nothing changed. */
    unchanged,
    /**
     * @brief Pair was reversed relative to the known canonical order.
     *
     * The qualifier is swapped (base and quote traded); the value is left
     * untouched. Some vendored ORE example files store a value that is
     * mathematically correct under the reversed key — see
     * ores.marketdata's import_service, which is the one place this class
     * is currently wired up, for the real-world case this guards against.
     */
    key_swapped,
};

/**
 * @brief Result of checking one FX spot quote against known pair conventions.
 */
struct fx_quote_check_result {
    /** Base currency after the check (same as input unless key_swapped). */
    std::string base_currency;
    /** Quote currency after the check (same as input unless key_swapped). */
    std::string quote_currency;
    fx_quote_status status = fx_quote_status::unknown_pair;
};

/**
 * @brief Checks a single FX spot quote's currency order against a known set
 * of canonical (base, quote) pairs, and corrects it if reversed.
 *
 * A deliberately narrow, dependency-free heuristic: it only ever swaps a
 * qualifier's two currencies (never touches the quoted value, so it can
 * never introduce floating-point error) and only when the *reversed* pair
 * is the one actually present in the known set — an unrecognised pair is
 * left alone rather than guessed at. Construct with the known canonical
 * pairs (e.g. from ores.refdata's currency_pair reference data, which
 * already stores each pair in canonical base-currency-precedence order);
 * everything else is pure, in-memory lookup, independent of any I/O, so it
 * can be exhaustively unit tested without a database or NATS connection.
 */
class ORES_ORE_CORE_EXPORT fx_quote_convention_checker {
private:
    inline static std::string_view logger_name = "ores.ore.core.fx_quote_convention_checker";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using currency_pair = std::pair<std::string, std::string>; // (base, quote)

    /**
     * @param known_pairs Canonical (base, quote) pairs, e.g. {"EUR","USD"}.
     *        Only the order actually present in this set is treated as
     *        canonical — the checker never infers an order on its own.
     */
    explicit fx_quote_convention_checker(std::set<currency_pair> known_pairs);

    /**
     * @brief Checks one (base, quote) quote and corrects it if reversed.
     *
     * @param base First currency in the quote's key as given, e.g. "USD".
     * @param quote Second currency in the quote's key as given, e.g. "GBP".
     */
    [[nodiscard]] fx_quote_check_result check(const std::string& base,
                                              const std::string& quote) const;

private:
    std::set<currency_pair> known_pairs_;
};

}

#endif
