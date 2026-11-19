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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_SERIES_IDENTITY_SHAPE_CHECK_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_SERIES_IDENTITY_SHAPE_CHECK_HPP

#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/domain/market_series_identity.hpp"
#include "ores.marketdata.api/domain/series_identity_shape.hpp"
#include "ores.marketdata.api/domain/series_identity_shape_entry.hpp"
#include "ores.marketdata.core/export.hpp"
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief The text one column of an identity row holds, read by name.
 *
 * The name is the column the shape table states, and the identity type's own field
 * list is what turns it into a member, so no second mapping has to be kept in step.
 * A name the type does not hold as text gives no value.
 */
[[nodiscard]] ORES_MARKETDATA_CORE_EXPORT std::optional<std::string>
text_of(const domain::market_series_identity& row, std::string_view column);

/**
 * @brief Refuses a row its family's shape does not admit, naming the family and the field.
 *
 * A family is the asset class and the series subclass the taxonomy names, narrowed
 * by the index family for an index series. A shape names the family, and its
 * entries name the fields. A missing required field is not an identity of that
 * family; a forbidden one states something untrue about it. A family no shape
 * names is not enforced: the shapes are a catalogue that grows, and refusing every
 * family nobody has stated yet would stop an ingest for a reason that is the
 * catalogue's.
 *
 * @throws std::invalid_argument naming the series, the family and the field.
 * @throws std::logic_error when an entry names a field no identity holds as text.
 * That is a fault of the catalogue, not of the series, so it is not an
 * @c std::invalid_argument and a caller that counts refused series does not count it.
 * An entry whose shape is not stated speaks for no family.
 */
ORES_MARKETDATA_CORE_EXPORT void
check_identity_shape(const std::vector<domain::series_identity_shape>& shapes,
                     const std::vector<domain::series_identity_shape_entry>& entries,
                     const domain::market_series& series,
                     const domain::market_series_identity& row);

/**
 * @brief The stated shapes, grouped by family once, so a batch checks every row
 * against one map.
 */
class ORES_MARKETDATA_CORE_EXPORT identity_shape_book final {
public:
    identity_shape_book(const std::vector<domain::series_identity_shape>& shapes,
                        const std::vector<domain::series_identity_shape_entry>& entries);

    /// As @c check_identity_shape, against the shapes this book holds.
    void check(const domain::market_series& series,
               const domain::market_series_identity& row) const;

    /// As @c states_identity_shape, against the shapes this book holds.
    [[nodiscard]] bool states(const domain::market_series& series,
                              const domain::market_series_identity& row) const;

private:
    struct family_shape final {
        std::vector<std::string> required;
        std::vector<std::string> forbidden;
    };
    std::map<std::tuple<std::string, std::string, std::string>, family_shape> families_;
};

/**
 * @brief Refuses a series whose identity is an index and whose subclass says otherwise.
 *
 * A fixing is an index and not a datum, and the taxonomy gives every index series
 * the one subclass, because a rule keyed by an index family speaks for all of them.
 * The shape below is keyed on that subclass, so a series that states a different one
 * is held to no shape at all and the guard cannot see the lie: this is the part of
 * the shape that has to be checked before a family can be looked up.
 *
 * @throws std::invalid_argument naming the series, its index family and both subclasses.
 */
ORES_MARKETDATA_CORE_EXPORT void check_identity_kind(const domain::market_series& series,
                                                     const domain::market_series_identity& row);

/**
 * @brief Whether a shape names this series' family, so a caller can count the series
 * nobody has stated one for.
 */
[[nodiscard]] ORES_MARKETDATA_CORE_EXPORT bool
states_identity_shape(const std::vector<domain::series_identity_shape>& shapes,
                      const std::vector<domain::series_identity_shape_entry>& entries,
                      const domain::market_series& series,
                      const domain::market_series_identity& row);

}

#endif
