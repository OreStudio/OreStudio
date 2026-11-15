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
#include "ores.marketdata.core/export.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief The text one column of an identity row holds, read by name.
 *
 * The name is the column the shape table states, and the identity type's own field
 * list is what turns it into a member, so a name the type does not hold reads as
 * empty rather than needing a second mapping to keep in step.
 */
[[nodiscard]] ORES_MARKETDATA_CORE_EXPORT std::string
text_of(const domain::market_series_identity& row, std::string_view column);

/**
 * @brief Refuses a row its family's shape does not admit, naming the family and the field.
 *
 * A family is the asset class and the series subclass the taxonomy names, narrowed
 * by the index family for an index series. A missing required field is not an
 * identity of that family; a forbidden one states something untrue about it. A
 * family no row mentions is not enforced: the shape is a catalogue that grows, and
 * refusing every family nobody has stated yet would stop an ingest for a reason
 * that is the catalogue's.
 *
 * @throws std::invalid_argument naming the series, the family and the field.
 */
ORES_MARKETDATA_CORE_EXPORT void
check_identity_shape(const std::vector<domain::series_identity_shape>& stated,
                     const domain::market_series& series,
                     const domain::market_series_identity& row);

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
 * @brief Whether any stated row speaks for this series' family, so a caller can count
 * the series nobody has stated one for.
 */
[[nodiscard]] ORES_MARKETDATA_CORE_EXPORT bool
states_identity_shape(const std::vector<domain::series_identity_shape>& stated,
                      const domain::market_series& series,
                      const domain::market_series_identity& row);

}

#endif
