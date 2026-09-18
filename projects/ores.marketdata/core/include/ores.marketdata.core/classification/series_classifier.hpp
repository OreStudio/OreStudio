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

#include "ores.marketdata.api/domain/series_classification_rule.hpp"
#include "ores.marketdata.core/export.hpp"
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace ores::marketdata::core {

/**
 * @brief Taxonomy assigned to one ORE market data series.
 */
struct series_classification {
    /**
     * @brief Codes from refdata.asset_class_code naming the classes the series
     *        belongs to.
     *
     * One element for every series that measures a single class. A pairwise
     * correlation names the two classes it relates, which is why this is a
     * collection and not a single code. Empty when no rule matches any of a
     * correlation's operands — a state the market_series_asset_classes
     * junction allows and a not-null column would not.
     */
    std::vector<std::string> asset_classes;

    /**
     * @brief Code from refdata.series_subclass_code.
     */
    std::string series_subclass;
};

/**
 * @brief Maps an ORE market data series onto our classification taxonomy.
 *
 * The ORE vocabulary is the input side and ours is the output side, so this
 * mapping is the boundary between them and belongs in one place.
 *
 * Build one from the rows of the series classification rule table and pass it
 * down. Nothing here reads the database or reaches for a compiled table, so it
 * is a plain value that a test can build without a database, NATS or an import
 * pipeline. The table is the taxonomy: a type ORE adds later, or one a user
 * brings, is an inserted row rather than a rebuild.
 *
 * The input is the series identity as the market_series table stores it — the
 * natural key of series_type, metric and qualifier. The qualifier carries the
 * operands of a correlation, so the type alone is not enough to classify one.
 *
 * The asset class and series subclass codes this emits are validated by
 * ores.marketdata's tables against the refdata catalogues, so a code that
 * exists only here is a defect the corpus test catches.
 *
 * Structural facts about the key — whether it carries a point dimension, and
 * so whether its observations name a point_id of their own — are not
 * classification and are not decided here. ores.ore.core's
 * series_key_registry owns them.
 */
class ORES_MARKETDATA_CORE_EXPORT series_classifier final {
public:
    /**
     * @brief Builds the classifier from the series classification rule rows.
     *
     * @param rules Rows of the series classification rule table, as the
     *        repository returns them. A row whose type and metric repeat
     *        replaces the earlier one.
     * @throws std::invalid_argument if rules is empty, if a row names an
     *         asset_class_source other than the two the table allows, or if a
     *         row's source and its asset class code contradict each other.
     */
    explicit series_classifier(std::vector<domain::series_classification_rule> rules);

    /**
     * @brief Classifies a series, throwing when the table has no rule.
     *
     * @param series_type ORE key type (e.g. FX, DISCOUNT, CORRELATION).
     * @param metric ORE metric component (e.g. RATE, PRICE, RATE_LNVOL).
     * @param qualifier Series qualifier, operands included.
     * @return The taxonomy the series belongs to.
     * @throws std::invalid_argument if no rule matches.
     */
    [[nodiscard]] series_classification classify(const std::string& series_type,
                                                 const std::string& metric,
                                                 const std::string& qualifier) const;

    /**
     * @brief Classifies a series, returning nullopt when the table has no rule.
     *
     * The form a corpus sweep uses: an unrecognised series is a finding to
     * report, not an exception to abort the walk with.
     */
    [[nodiscard]] std::optional<series_classification>
    try_classify(const std::string& series_type,
                 const std::string& metric,
                 const std::string& qualifier) const;

    /**
     * @brief Every ORE series type the table carries, in ascending order.
     */
    [[nodiscard]] std::vector<std::string> known_series_types() const;

private:
    std::unordered_map<std::string,
                       std::unordered_map<std::string, domain::series_classification_rule>>
        by_type_;
};

}

#endif
