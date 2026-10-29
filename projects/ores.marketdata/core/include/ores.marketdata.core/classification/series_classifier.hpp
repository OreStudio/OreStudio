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
#ifndef ORES_MARKETDATA_CORE_CLASSIFICATION_SERIES_CLASSIFIER_HPP
#define ORES_MARKETDATA_CORE_CLASSIFICATION_SERIES_CLASSIFIER_HPP

#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/market_index.hpp"
#include "ores.marketdata.api/domain/series_classification_rule.hpp"
#include "ores.marketdata.core/export.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ores::marketdata::core {

/** What a rule is keyed by: the oresmd identity of a datum, or of an index. */
enum class key_kind : std::uint8_t {
    /// A market datum: its instrument and its quote.
    datum,
    /// A market index, which is what a fixing series is: its family.
    index
};

/// The text the catalogue writes for @p kind under =key_kind=.
[[nodiscard]] ORES_MARKETDATA_CORE_EXPORT std::string_view name_of(key_kind kind);

/// The kind the catalogue's text @p name names, or nothing.
[[nodiscard]] ORES_MARKETDATA_CORE_EXPORT std::optional<key_kind>
key_kind_named(std::string_view name);

/**
 * @brief What a classification rule is looked up by: a kind, an instrument and a
 * quote.
 *
 * The instrument is the text the oresmd URI writes under =instrument=, such as
 * =ir_swap=, =discount= or =correlation=; for an index it is the family's name,
 * such as =ibor=, or the empty string for every family. The quote is the text
 * the URI writes under =quote=, or the empty string for a rule that applies to
 * every quote of its instrument. An index has no quote.
 */
struct classification_key final {
    key_kind kind = key_kind::datum;
    std::string instrument;
    std::string quote;

    /**
     * @brief A correlation's two operands, as the datum's own fields state them;
     * empty for every other identity.
     *
     * The classes of a pairwise correlation are read from these and not from the
     * key, because the pair belongs to the two classes it relates and to no class
     * of its own.
     */
    std::vector<std::string> operands;

    friend bool operator==(const classification_key&, const classification_key&) = default;
};

/**
 * @brief The classification key of @p d: its instrument and its quote.
 *
 * No ORE key is composed or read: the identity is the oresmd one, so the key is
 * the spelling the URI writes.
 */
[[nodiscard]] ORES_MARKETDATA_CORE_EXPORT classification_key
classification_key_of(const datum::market_datum& d);

/**
 * @brief The classification key of @p i: the index family it belongs to.
 */
[[nodiscard]] ORES_MARKETDATA_CORE_EXPORT classification_key
classification_key_of(const datum::market_index& i);

/**
 * @brief Taxonomy assigned to one market series.
 */
struct series_classification {
    /**
     * @brief Codes from refdata.asset_class_code naming the classes the series
     *        belongs to.
     *
     * One element for every series that measures a single class. A pairwise
     * correlation names the two classes it relates, which is why this is a
     * collection and not a single code. Empty when no rule matches any of a
     * correlation's operands, or when an index family's authority names no class
     * -- states the market_series_asset_classes junction allows and a not-null
     * column would not.
     */
    std::vector<std::string> asset_classes;

    /**
     * @brief Code from refdata.series_subclass_code.
     */
    std::string series_subclass;
};

/**
 * @brief Maps a market series onto our classification taxonomy.
 *
 * Build one from the rows of the series classification rule table and pass it
 * down. Nothing here reads the database or reaches for a compiled table, so it
 * is a plain value that a test can build without a database, NATS or an import
 * pipeline. The table is the taxonomy: an instrument a corpus adds later, or one
 * a user brings, is an inserted row rather than a rebuild.
 *
 * The input is the series identity as the oresmd URI writes it -- the instrument
 * and the quote of a datum, or the family of an index -- and not an ORE key. An
 * ORE key is a boundary format: it exists where ORE XML is read and written, and
 * the runtime never asks what ORE would have called a series.
 *
 * The asset class and series subclass codes this emits are validated by
 * ores.marketdata's tables against the refdata catalogues, so a code that exists
 * only here is a defect the corpus test catches.
 */
class ORES_MARKETDATA_CORE_EXPORT series_classifier final {
public:
    /**
     * @brief Builds the classifier from the series classification rule rows.
     *
     * @param rules Rows of the series classification rule table, as the
     *        repository returns them. A row whose key repeats replaces the
     *        earlier one.
     * @throws std::invalid_argument if rules is empty, if a row names a
     *         key_kind or an asset_class_source the table does not allow, or if
     *         a row's source and its asset class code contradict each other.
     */
    explicit series_classifier(std::vector<domain::series_classification_rule> rules);

    /**
     * @brief Classifies a series, throwing when the table has no rule.
     *
     * @param key The series identity, as classification_key_of() states it.
     * @return The taxonomy the series belongs to.
     * @throws std::invalid_argument if no rule matches.
     */
    [[nodiscard]] series_classification classify(const classification_key& key) const;

    /**
     * @brief Classifies a series, returning nullopt when the table has no rule.
     *
     * The form a corpus sweep uses: an unrecognised series is a finding to
     * report, not an exception to abort the walk with.
     */
    [[nodiscard]] std::optional<series_classification>
    try_classify(const classification_key& key) const;

    /**
     * @brief Every instrument the table carries for one kind, in ascending
     * order.
     */
    [[nodiscard]] std::vector<std::string> known_instruments(key_kind kind) const;

private:
    using by_quote = std::unordered_map<std::string, domain::series_classification_rule>;
    using by_instrument = std::unordered_map<std::string, by_quote>;

    std::unordered_map<std::uint8_t, by_instrument> by_kind_;
};

}

#endif
