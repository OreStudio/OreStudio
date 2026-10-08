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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_MARKET_SERIES_IDENTITY_READER_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_MARKET_SERIES_IDENTITY_READER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief Resolves a series from its typed identity by reading the identity
 * projection.
 *
 * A series holds its identity only inside its oresmd URI, which SQL cannot
 * compare field by field, so the projection writes those fields into columns of
 * their own. This read filters on those columns and joins the result to the
 * series, so the narrowing happens in the store and no URI is parsed here.
 *
 * The projection is written from the codec's own parse, so a field means here
 * what it means in the URI. What this read still decides is which fields the
 * request may name: the instrument and quote type must be ones the codec knows
 * and state the spelling the URI writes, the asset class must be the type's,
 * and each field must be one the type's schema row marks as identity and is not
 * the scope, which the request states on its own.
 *
 * A request that fails any of those is refused with =std::invalid_argument=.
 * Nothing is guessed, so a malformed request cannot silently resolve to the
 * wrong series, and no field name a caller invents reaches the store as a
 * column.
 */
class ORES_MARKETDATA_CORE_EXPORT market_series_identity_reader final {
public:
    /**
     * @brief The series that carry @p identity, at most one per owning party.
     *
     * @throws std::invalid_argument when the identity is not one the codec can
     * spell.
     */
    [[nodiscard]] static std::vector<domain::market_series>
    read(ores::database::context ctx, const messaging::resolve_series_identity_request& identity);
};

}

#endif
