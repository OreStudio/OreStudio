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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_MARKETDATA_API_PROJECTION_ORE_PROJECTION_HPP
#define ORES_MARKETDATA_API_PROJECTION_ORE_PROJECTION_HPP

#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.marketdata.api/export.hpp"
#include <cstdint>
#include <expected>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>

namespace ores::marketdata::projection {

/**
 * @brief Why an oresmd reference has no ORE name in this projection.
 *
 * The reasons stay apart rather than folded into one, because they call for
 * different answers: one is a gap in the run's market configuration, the other
 * two are references ORE cannot read at all.
 */
enum class projection_refusal_kind : std::uint8_t {
    /// The stored text looks like an oresmd address and the grammar refuses it,
    /// so nothing can say what datum, series or index it names.
    address_the_grammar_refuses,
    /// A curve series the run's market configuration gives no name.
    configuration_names_no_curve,
    /// A datum ORE has no market datum of its type for, so the key codec cannot
    /// spell it.
    ore_has_no_name
};

/// The reference the projection could not name, and why, in the codec's words.
struct projection_refusal final {
    projection_refusal_kind kind;
    std::string reference;
    std::string reason;
};

/**
 * @brief A stored reference the export can neither write as an ORE name nor
 * leave as stored.
 *
 * Thrown where the alternative is an ORE document carrying a value the engine
 * cannot read, so the export names the value and the reason, rather than a
 * valuation failing later with a message about a curve.
 */
class ORES_MARKETDATA_API_EXPORT ore_projection_error final : public std::runtime_error {
public:
    explicit ore_projection_error(projection_refusal refusal);

    [[nodiscard]] projection_refusal_kind kind() const noexcept {
        return kind_;
    }
    [[nodiscard]] const std::string& reference() const noexcept {
        return reference_;
    }

private:
    projection_refusal_kind kind_;
    std::string reference_;
};

/**
 * @brief The vendor projection to ORE: an oresmd identifier written as ORE
 * names it.
 *
 * The page Market Data Identifier defines it as one-directional, an
 * identifier rendered in a vendor's notation; reading an ORE name back is
 * ingestion, which the importer owns. An index is written by its ORE index
 * name and a quote by its ORE market data key, both fixed by the codecs. A
 * curve has no fixed ORE name: ORE finds it through the market configuration
 * a run reads, so the run gives the projection the name its configuration
 * gives each curve series. A projection built with no names writes no curve.
 */
class ORES_MARKETDATA_API_EXPORT ore_projection final {
public:
    ore_projection() = default;

    /// @param curve_names The ORE name of each curve series, by its URI.
    explicit ore_projection(std::unordered_map<std::string, std::string> curve_names);

    /// The ORE name of @p reference, or why it has none here.
    [[nodiscard]] std::expected<std::string, projection_refusal>
    ore_name_of(const datum::market_reference& reference) const;

    /**
     * @brief The ORE name of what a stored column holds, or the text as stored.
     *
     * A stored column is in one of four states, and each is answered apart:
     *
     * - It holds no address, but an ORE name or another vendor's name, as every
     *   reference column does until stored references are identifiers. It is
     *   written as it stands, and nothing is wrong.
     * - It holds a curve series the run's market configuration gives no name.
     *   It is written as it stands and the projection warns, because a run that
     *   forwards no market writes what its trades hold: an export with no
     *   configuration leaves every curve alone.
     * - It looks like an oresmd address and the grammar refuses it. No ORE
     *   document can read it, so the projection logs the reason and throws.
     * - It holds a datum ORE has no key for. Same answer: logged and thrown.
     *
     * @throws ore_projection_error on the last two, naming the value and why.
     */
    std::string ore_name_or_stored(std::string_view stored) const;

    /**
     * @brief The address of an ORE index name, or the name as it stands.
     *
     * The name stays as it stands when ORE's grammar does not read it, or when
     * the address could not write the same spelling back, so that an imported
     * document loses nothing.
     */
    [[nodiscard]] static std::string address_of_index_or_stored(std::string_view ore_name);

private:
    std::unordered_map<std::string, std::string> curve_names_;
};

}

#endif
